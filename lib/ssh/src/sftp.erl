%%%-------------------------------------------------------------------
%%% File    : sftp.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : SFTP frontend
%%%
%%% Created : 24 Aug 2004 by Tony Rogvall <tony@bix.hemma.se>
%%%-------------------------------------------------------------------
-module(sftp).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").

-include("../include/ssh_xfer.hrl").

-import(lists, [member/2, map/2, foldl/3, reverse/1]).
%%--------------------------------------------------------------------
%% External exports
-export([start/3, start_link/3]).
-export([start/2, start_link/2]).
-export([start/1, start_link/1]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, 
	{
	  xf,
	  reqid
	 }).

-define(FILEOP_TIMEOUT, 60000).

-define(NEXT_REQID(S),
	S#state { reqid = (S#state.reqid + 1) band 16#ffffffff}).

-define(XF(S), S#state.xf).

-define(REQID(S), S#state.reqid).

%%====================================================================
%% External functions
%%====================================================================
open(Pid, File, Mode) ->
    gen_server:call(Pid, {open, File, Mode}, ?FILEOP_TIMEOUT).

close(Pid, Handle) ->
    gen_server:call(Pid, {close, Handle}, ?FILEOP_TIMEOUT).

pread(Pid, Handle, Offset, Len) ->
    gen_server:call(Pid, {pread, Handle, Offset, Len}, ?FILEOP_TIMEOUT).

read(Pid, Handle, Len) ->
    gen_server:call(Pid, {read, Handle, Len}, ?FILEOP_TIMEOUT).    

pwrite(Pid, Handle, Offset, Data) ->
    gen_server:call(Pid, {pwrite, Handle, Offset, Data}, ?FILEOP_TIMEOUT).

write(Pid, Handle, Data) ->
    gen_server:call(Pid, {write, Handle, Data}, ?FILEOP_TIMEOUT).

position(Pid, Handle, Pos) ->
    gen_server:call(Pid, {position, Handle, Pos}, ?FILEOP_TIMEOUT).

read_file_info(Pid, Name) ->
    gen_server:call(Pid, {read_file_info, Name}, ?FILEOP_TIMEOUT).

write_file_info(Pid, Name, Info) ->
    gen_server:call(Pid, {write_file_info, Name}, ?FILEOP_TIMEOUT).

read_link_info(Pid, Name) ->
    gen_server:call(Pid, {read_link_info, Name}, ?FILEOP_TIMEOUT).

make_symlink(Pid, Old, New) ->
    gen_server:call(Pid, {mak_symlink, Old, New}, ?FILEOP_TIMEOUT).    

rename(Pid, FromFile, ToFile) ->
    gen_server:call(Pid, {rename, FromFile, ToFile}, ?FILEOP_TIMEOUT).

delete(Pid, Name) ->
    gen_server:call(Pid, {delete, Name}, ?FILEOP_TIMEOUT).

make_dir(Pid, Name) ->
    gen_server:call(Pid, {make_dir, Name}, ?FILEOP_TIMEOUT).

del_dir(Pid, Name) ->
    gen_server:call(Pid, {del_dir, Name}, ?FILEOP_TIMEOUT).

list_dir(Pid, Name) ->
    gen_server:call(Pid, {list_dir, Name}, 60000).

stop(Pid) ->
    gen_server:call(Pid, stop).

send_window(Pid) ->
    gen_server:call(Pid, send_window).

recv_window(Pid) ->
    gen_server:call(Pid, recv_window).

read_file(Pid, Name) ->
    case open(Pid, Name, [read, binary]) of
	{ok, Handle} ->
	    {ok,{WindowSz,PacketSz}} = recv_window(Pid),
	    Res = read_file_loop(Pid, Handle, PacketSz, []),
	    close(Pid, Handle),
	    Res;
	Error ->
	    Error
    end.

read_file_loop(Pid, Handle, PacketSz, Acc) ->
    case read(Pid, Handle, PacketSz) of
	{ok, Data}  ->
	    read_file_loop(Pid, Handle, PacketSz, [Data|Acc]);
	eof ->
	    {ok, list_to_binary(reverse(Acc))};
	Error ->
	    Error
    end.

write_file(Pid, Name, List) when list(List) ->
    write_file(Pid, Name, list_to_binary(List));
write_file(Pid, Name, Bin) ->
    case open(Pid, Name, [write, binary]) of
	{ok, Handle} ->
	    {ok,{Window,Packet}} = send_window(Pid),
	    Res = write_file_loop(Pid, Handle, 0, Bin, size(Bin), Packet),
	    close(Pid, Handle),
	    Res;
	Error ->
	    Error
    end.

write_file_loop(Pid, Handle, Pos, Bin, 0, PacketSz) ->
    ok;
write_file_loop(Pid, Handle, Pos, Bin, Remain, PacketSz) ->
    if Remain >= PacketSz ->
	    <<_:Pos/binary, Data:PacketSz/binary, _/binary>> = Bin,
	    case write(Pid, Handle, Data) of
		ok ->
		    write_file_loop(Pid, Handle, 
				    Pos+PacketSz, Bin, Remain-PacketSz,
				    PacketSz);
		Error ->
		    Error
	    end;
       true ->
	    <<_:Pos/binary, Data/binary>> = Bin,
	    write(Pid, Handle, Data)
    end.


%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(CM) ->
    gen_server:start_link(?MODULE, [CM], []).

start_link(Host, Opts) ->
    gen_server:start_link(?MODULE, [Host,22,Opts], []).
    
start_link(Host, Port, Opts) ->
    gen_server:start_link(?MODULE, [Host,Port,Opts], []).

start(CM) ->
    gen_server:start(?MODULE, [CM], []).

start(Host, Opts) ->
    gen_server:start(?MODULE, [Host,22,Opts], []).
    
start(Host, Port, Opts) ->
    gen_server:start(?MODULE, [Host,Port,Opts], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([CM]) ->
    case ssh_xfer:attach(CM) of
	{ok,Xf} ->
	    {ok, #state { reqid = 0, xf = Xf }};
	Error ->
	    {stop, Error }
    end;
init([Host,Port,Auth]) ->
    case ssh_xfer:connect(Host, Port, Auth) of
	{ok, Xf} ->
	    {ok, #state { reqid = 0, xf = Xf }};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({open, FileName, Mode}, From, State) ->
    XF = State#state.xf,
    {Access,Flags,Attrs} = open_mode(XF#ssh_xfer.vsn, Mode),
    ReqID = State#state.reqid,
    case ssh_xfer:open(XF, ReqID, FileName, Access, Flags, Attrs) of
	{handle,ReqID, Handle} ->
	    put({offset,Handle}, 0),
	    ReqID1 = (ReqID + 1) band 16#ffffffff,
	    case ssh_xfer:stat(State#state.xf, ReqID1, FileName, [size]) of
		{attrs,ReqID1, A} ->
		    if A#ssh_xfer_attr.size == undefined ->
			    put({size,Handle}, 0);
		       true ->
			    put({size,Handle}, A#ssh_xfer_attr.size)
		    end;
		_ ->
		    put({size,Handle}, 0)
	    end,
	    case member(binary, Mode) orelse member(raw, Mode) of
		true -> put({mode,Handle}, binary);
		false -> put({mode,Handle}, text)
	    end,
	    ReqID2 = (ReqID1 + 1) band 16#ffffffff,
	    {reply,{ok,Handle}, State#state {reqid = ReqID2}};
	{status, ReqID, {Status,Err,Lang,Reply}} ->
	    {reply,{error,Status}, ?NEXT_REQID(State)};
	_ ->
	    {reply,{error,einval}, ?NEXT_REQID(State)}
    end;
handle_call({close, Handle}, From, State) ->
    case lseek_position(Handle, cur) of
	{ok,_} ->
	    ReqID = State#state.reqid,
	    Rep = ssh_xfer:close(State#state.xf, ReqID, Handle),
	    erase({offset,Handle}),
	    erase({size,Handle}),
	    erase({mode,Handle}),
	    xreply(Rep, ReqID, State);
	Error ->
	    {reply,Error, State}
    end;

handle_call({pread, Handle, At, Length}, From, State) ->
    case lseek_position(Handle, At) of
	{ok,Offset} ->
	    ReqID = State#state.reqid,
	    case ssh_xfer:read(State#state.xf,ReqID, Handle, Offset, Length) of
		{data, ReqID, Data} ->
		    update_size(Handle, Offset+size(Data)),
		    case get({mode,Handle}) of
			binary ->
			    {reply, {ok,Data}, ?NEXT_REQID(State)};
			text ->
			    {reply, {ok,binary_to_list(Data)},
			     ?NEXT_REQID(State)}
		    end;
		{status, ReqID, {Status, Err, Lang, Reply}} ->
		    {reply, {error,Status}, ?NEXT_REQID(State)};
		_ ->
		    {reply, {error,einval}, ?NEXT_REQID(State)}
	    end;
	Error ->
	    {reply, Error, State}
    end;

handle_call({read, Handle, Length}, From, State) ->
    case lseek_position(Handle, cur) of
	{ok,Offset} ->
	    ReqID = ?REQID(State),
	    case ssh_xfer:read(?XF(State),ReqID, Handle, Offset, Length) of
		{data, ReqID, Data} ->
		    update_offset(Handle, Offset+size(Data)),
		    case get({mode,Handle}) of
			binary ->
			    {reply, {ok,Data}, ?NEXT_REQID(State)};
			text ->
			    {reply, {ok,binary_to_list(Data)},
			     ?NEXT_REQID(State)}
		    end;
		{status, ReqID, {eof, Err, Lang, Reply}} ->
		    {reply, eof, ?NEXT_REQID(State)};
		{status, ReqID, {Status, Err, Lang, Reply}} ->
		    {reply, {error,Status}, ?NEXT_REQID(State)};
		_ ->
		    {reply, {error,einval}, ?NEXT_REQID(State)}
	    end;
	Error ->
	    {reply, Error, State}
    end;

handle_call({pwrite, Handle, At, Data0}, From, State) ->
    case lseek_position(Handle, At) of
	{ok,Offset} ->
	    Data = if binary(Data0) -> Data0;
		      list(Data0) -> list_to_binary(Data0)
		   end,
	    ReqID = State#state.reqid,
	    case ssh_xfer:write(?XF(State), ReqID, Handle, Offset, Data) of
		{status, ReqID, {ok, Err, Lang, Reply}} ->
		    update_size(Handle, Offset+size(Data)),
		    {reply, ok, ?NEXT_REQID(State)};
		{status, ReqID, {Status, Err, Lang, Reply}} ->
		    {reply, {error,Status}, ?NEXT_REQID(State)};
		_ ->
		    {reply, {error,einval}, ?NEXT_REQID(State)}
	    end;
	Error ->
	    {reply, Error, State}
    end;

handle_call({write, Handle, Data0}, From,  State) ->
    case lseek_position(Handle, cur) of
	{ok,Offset} ->
	    Data = if binary(Data0) -> Data0;
		      list(Data0) -> list_to_binary(Data0)
		   end,
	    ReqID = State#state.reqid,
	    case ssh_xfer:write(?XF(State),ReqID,Handle,Offset,Data) of
		{status, ReqID, {ok, Err, Lang, Reply}} ->
		    update_offset(Handle, Offset+size(Data)),
		    {reply, ok, ?NEXT_REQID(State)};
		{status, ReqID, {Status, Err, Lang, Reply}} ->
		    {reply, {error,Status}, ?NEXT_REQID(State)};
		_ ->
		    {reply, {error,einval}, ?NEXT_REQID(State)}
	    end;
	Error ->
	    {reply, Error, State}
    end;

handle_call({position,Handle,At}, From, State) ->
    case lseek_position(Handle, At) of
	{ok,Offset} ->
	    update_offset(Handle, Offset),
	    {reply, {ok, Offset}, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call({rename, FromFile, ToFile}, From, State) ->
    ReqID = State#state.reqid,
    Rep = ssh_xfer:rename(State#state.xf,ReqID,FromFile,ToFile,[overwrite]),
    xreply(Rep, ReqID, State);

handle_call({delete, Name}, From, State) ->
    ReqID = State#state.reqid,
    Rep = ssh_xfer:remove(State#state.xf, ReqID, Name),
    xreply(Rep, ReqID, State);        

handle_call({make_dir, Name}, From, State) ->
    ReqID = State#state.reqid,
    Rep = ssh_xfer:mkdir(State#state.xf, ReqID, Name,
			 #ssh_xfer_attr{ type = directory }),
    xreply(Rep, ReqID, State);    

handle_call({del_dir, Name}, From, State) ->
    ReqID = State#state.reqid,
    Rep = ssh_xfer:rmdir(State#state.xf, ReqID, Name),
    xreply(Rep, ReqID, State);

handle_call({list_dir, Name}, From, State) ->
    ReqID = State#state.reqid,
    case do_list_dir(State#state.xf, ReqID, Name) of
	{{ok, List},NReqID} ->
	    %% Strip list from file_info
	    NList = foldl(fun({Nm, _Info},Acc) -> [Nm|Acc] end, [],List),
	    {reply, {ok, NList}, State#state { reqid = NReqID}};
	{Error, NReqID} ->
	    {reply, Error, State#state { reqid = NReqID}}
    end;

handle_call({read_file_info, Name}, From, State) ->
    ReqID = State#state.reqid,
    Rep = ssh_xfer:stat(State#state.xf, ReqID, Name, all),
    xreply(Rep, ReqID, State);

handle_call({read_link_info, Name}, From, State) ->
    ReqID = State#state.reqid,
    Rep = ssh_xfer:lstat(State#state.xf, ReqID, Name, all),
    xreply(Rep, ReqID, State);

handle_call({write_file_info, Name, Info}, From, State) ->
    ReqID = State#state.reqid,
    A = info_to_attr(Info),
    Rep = ssh_xfer:setstat(State#state.xf, ReqID, Name, A),
    xreply(Rep, ReqID, State);

handle_call(send_window, From, State) ->
    XF = State#state.xf,
    {reply, ssh_cm:send_window(XF#ssh_xfer.cm, XF#ssh_xfer.channel), State};

handle_call(recv_window, From, State) ->
    XF = State#state.xf,
    {reply, ssh_cm:recv_window(XF#ssh_xfer.cm, XF#ssh_xfer.channel), State};

handle_call(stop, From, State) ->    
    {stop, normal, ok, State};

handle_call(Call, From, State) ->    
    {reply, {error, bad_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

xreply(Reply, ReqID, State) ->
    NReqID = (ReqID + 1) band 16#ffffffff,
    case Reply of
	{data, ReqID, Data} ->
	    {reply, {ok,Data}, State#state { reqid = NReqID }};
	{handle, ReqID, Handle} ->
	    {reply, {ok,Handle}, State#state { reqid = NReqID  }};
	{attrs, ReqID, A} ->
	    {reply, {ok, attr_to_info(A)}, State#state { reqid = NReqID }};
	{status, ReqID, {ok, Err, _Lang, _Rep}} ->
	    {reply, ok, State#state { reqid = NReqID }};
	{status, ReqID, {Status, Err, _Lang, _Rep}} ->
	    {reply, {error,Status}, State#state { reqid = NReqID }};
	Rep ->
	    {reply, {error,Rep}, State#state { reqid = NReqID }}
    end.

do_list_dir(Xf, ReqID, Name) ->
    NReqID = (ReqID + 1) band 16#ffffffff,
    case ssh_xfer:opendir(Xf, ReqID, Name) of
	{handle, ReqID, Handle} ->
	    do_read_dir(Xf, NReqID, Handle, []);
	{status, ReqID, {Status, Err, Lang, Reply}} ->
	    {{error,Status}, NReqID};
	Rep ->
	    {{error,Rep}, NReqID}
    end.


do_read_dir(Xf, ReqID, Handle, Acc) ->
    NReqID = (ReqID + 1) band 16#ffffffff,
    Rep = ssh_xfer:readdir(Xf, ReqID, Handle),
    case Rep of
	{name, ReqID, Names} ->
	    do_read_dir(Xf, NReqID, Handle, Acc ++ Names);
	{status, ReqID, {eof, _, _, _}} ->
	    {{ok, Acc}, NReqID};
	{status, ReqID, {Status, Err, Lang, Reply}} ->
	    {{error,Status}, NReqID };
	Rep ->
	    {{error,Rep}}
    end.

%% convert: file_info -> ssh_xfer_attr
info_to_attr(I) ->
    #ssh_xfer_attr { permissions = I#file_info.mode,
		     size = I#file_info.size,
		     type = I#file_info.type,
		     owner = I#file_info.uid,
		     group = I#file_info.gid,
		     atime = datetime_to_unix(I#file_info.atime),
		     mtime = datetime_to_unix(I#file_info.mtime),
		     createtime = datetime_to_unix(I#file_info.ctime)}.

%% convert: ssh_xfer_attr -> file_info
attr_to_info(A) ->
    #file_info { size   = A#ssh_xfer_attr.size,
		 type   = A#ssh_xfer_attr.type,
		 access = read_write, %% FIXME: read/write/read_write/none
		 atime = unix_to_datetime(A#ssh_xfer_attr.atime),
		 mtime = unix_to_datetime(A#ssh_xfer_attr.mtime),
		 ctime = unix_to_datetime(A#ssh_xfer_attr.createtime),
		 mode = A#ssh_xfer_attr.permissions,
		 links = 1,
		 major_device = 0,
		 minor_device = 0,
		 inode = 0,
		 uid  = A#ssh_xfer_attr.owner,
		 gid  = A#ssh_xfer_attr.group
		 }.


unix_to_datetime(undefined) ->
    undefined;
unix_to_datetime(Sec) ->
    calendar:gregorian_seconds_to_datetime(Sec + 62167219200).

datetime_to_unix(undefined) ->
    undefined;
datetime_to_unix(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.


open_mode(Vsn,Modes) ->
    open_mode(Vsn, Modes, [], [], #ssh_xfer_attr{ type = regular }).

open_mode(3, [read|Mode], Access, Flags, Attrs) ->
    open_mode(3,Mode,[read_data,read_attributes|Access], [read|Flags], Attrs);
open_mode(5, [read|Mode], Access, Flags, Attrs) ->
    Flags1 =
	case member(write, Mode) orelse member(truncate_existing, Flags) of
	    false -> [open_existing | Flags];
	    true  -> Flags
	end,
    open_mode(5, Mode, [read_data,read_attributes|Access], Flags1, Attrs);
open_mode(3, [write|Mode], Access, Flags, Attrs) ->
    open_mode(3, Mode, [write_data,write_attributes|Access], 
	      [write,creat,trunc|Flags], Attrs);
open_mode(5, [write|Mode], Access, Flags, Attrs) ->
    Flags1 =
	case member(read, Mode) orelse member(existing, Flags) of
	    true -> Flags;
	    false -> [create_truncate|Flags]
	end,
    open_mode(5, Mode, [write_data,write_attributes|Access], 
	      Flags1, Attrs);
open_mode(3, [append|Mode],Access, Flags, Attrs) ->
    open_mode(3, Mode, [write_data,write_attributes|Access], 
	      [write,creat,trunc,append|Flags], Attrs);
open_mode(5, [append|Mode],Access, Flags, Attrs) ->
    open_mode(5, Mode, [write_data,write_attributes,append_data|Access], 
	      [open_or_create,write_data,write_attributes,append_data|Flags],
	      Attrs);
open_mode(Vsn, [raw|Mode],Access, Flags, Attrs) ->
    open_mode(Vsn, Mode, Access, Flags, Attrs);
open_mode(Vsn, [binary|Mode],Access, Flags, Attrs) ->
    open_mode(Vsn, Mode, Access, Flags, Attrs);
open_mode(_, [], Access, Flags, Attrs) ->
    {Access, Flags, Attrs}.


update_size(Handle, NewSize) ->
    OldSize = get({size,Handle}),
    if NewSize > OldSize ->
	    put({size,Handle}, NewSize);
       true ->
	    ok
    end.

update_offset(Handle, NewOffset) ->
    put({offset,Handle}, NewOffset),
    update_size(Handle, NewOffset).

%%
%% Caluclate a integer offset
%%
lseek_position(Handle, Pos) ->
    lseek_pos(Pos, get({offset,Handle}), get({size,Handle})).

lseek_pos(Pos, undefined, _) ->
    {error, einval};
lseek_pos(Pos, CurOffset, CurSize)
  when integer(Pos), 0 =< Pos, Pos < ?SSH_FILEXFER_LARGEFILESIZE ->
    {ok,Pos};
lseek_pos(bof, CurOffset, CurSize) ->
    {ok,0};
lseek_pos(cur, CurOffset, CurSize) ->
    {ok,CurOffset};
lseek_pos(eof, CurOffset, CurSize) ->
    {ok,CurSize};
lseek_pos({bof, Offset}, CurOffset, CurSize)
  when integer(Offset), 0 =< Offset, Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    {ok, Offset};
lseek_pos({cur, Offset}, CurOffset, CurSize)
  when integer(Offset), -(?SSH_FILEXFER_LARGEFILESIZE) =< Offset, 
       Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    NewOffset = CurOffset + Offset,
    if NewOffset < 0 ->
	    {ok, 0};
       true ->
	    {ok, NewOffset}
    end;
lseek_pos({eof, Offset}, CurOffset, CurSize) 
  when integer(Offset), -(?SSH_FILEXFER_LARGEFILESIZE) =< Offset, 
       Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    NewOffset = CurSize + Offset,
    if NewOffset < 0 ->
	    {ok, 0};
       true ->
	    {ok, NewOffset}
    end;
lseek_pos(_, _, _) ->
    {error, einval}.

    
