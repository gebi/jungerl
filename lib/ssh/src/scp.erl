%%% File    : scp.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : scp client & server
%%% Created : 20 Oct 2005 by Tony Rogvall <tony@iMac.local>

-module(scp).

-include_lib("kernel/include/file.hrl").
-include("../include/ssh_connect.hrl").

-compile(export_all).

-define(SCP_OK,      0).
-define(SCP_ERROR,   1).
-define(SCP_FATAL,   2).

copy_to_server(_SSH, CM, Channel, Target) ->
    recv_files(CM, Channel, Target).
    
recv_files(CM, Channel, Target) ->
    %% Verify Target (check if is directory)
    send_ok(CM, Channel),
    recv_files(CM, Channel, Target, undefined).

%%
%% Control records:
%% E        -  done
%% C<mode> <size> <file>
%% D<mode> <size> <dir>
%% T<m-sec> <m-usec> <a-sec> <a-usec>
%%
recv_files(CM, Channel, Target, FileTm) ->
    io:format("scp: recv_files target=~s\n", [Target]),
    case read_line(CM, Channel) of
	{error, Reason} ->
	    io:format("scp: error: ~p\n", [Reason]),
	    Reason;
	{ok, Line} ->
	    io:format("scp: got control line ~s\n", [Line]),
	    case string:tokens(Line, " ") of
		["E"] ->
		    send_ok(CM, Channel),
		    ok;
		["C"++Mode,Size,File] ->
		    case catch {list_to_integer(Size),
				erlang:list_to_integer(Mode,8),
				filename:join(Target,File)} of
			{'EXIT', _} ->
			    send_error(CM,Channel,"bad control record");
			{FileSize,FileMode,FileName} ->
			    server_recv_file(CM,Channel,FileName,
					     FileSize,FileMode,FileTm),
			    recv_files(CM,Channel,Target,FileTm)
		    end;
		["D"++Mode,Size,Dir] ->
		    case catch {list_to_integer(Size),
				erlang:list_to_integer(Mode,8),
				filename:join(Target,Dir)} of
			{'EXIT', _} ->
			    send_error(CM,Channel,"bad control record");
			{_DirSize,DirMode,DirName} ->
			    case file:make_dir(DirName) of
				{error, eexist} ->
				    _DRes = recv_files(CM, Channel, DirName),
				    set_mode(DirName,DirMode,FileTm),
				    recv_files(CM,Channel,Target,FileTm);
				ok ->
				    _DRes = recv_files(CM, Channel, DirName),
				    set_mode(DirName,DirMode,FileTm),
				    recv_files(CM,Channel,Target,FileTm);
				{error, Reason} ->
				    send_error(CM,Channel,erl_posix_msg:message(Reason))
			    end
		    end;

		["T"++Ms,Mus,As,Aus] ->
		    case catch {{list_to_integer(Ms),list_to_integer(Mus)},
				{list_to_integer(As),list_to_integer(Aus)}} of
			{'EXIT', _} ->
			    send_error(CM,Channel,"bad control record");
			{{UxMs,_},{UxAs,_}} ->
			    ssh_cm:send(CM, Channel, [?SCP_OK]),
			    GMs = 62167219200 + UxMs,
			    GAs = 62167219200 + UxAs,
			    Mtime = calendar:gregorian_seconds_to_datetime(GMs),
			    Atime = calendar:gregorian_seconds_to_datetime(GAs),
			    recv_files(CM, Channel, Target, {Mtime,Atime})
		    end;
		_ ->
		    send_error(CM, Channel,"expeced control record")
	    end
    end.

set_mode(Path, Mode, undefined) ->
    file:change_mode(Path, Mode);    
set_mode(Path, Mode, {Mtime,Atime}) ->
    file:change_time(Path,Atime,Mtime),
    file:change_mode(Path, Mode).
    


server_recv_file(CM,Channel,FileName,FileSize,FileMode,FileTm) ->
    case file:open(FileName, [write]) of
	{ok, Fd} ->
	    send_ok(CM, Channel),
	    case server_recv_file(CM,Channel,Fd,FileSize) of
		ok ->
		    file:position(Fd, FileSize),
		    file:truncate(Fd),
		    set_mode(FileName,FileMode,FileTm),
		    wait_ok(CM,Channel),
		    io:format("got ok\n",[]),
		    send_ok(CM,Channel);

		{error,closed} ->
		    exit(closed);
		
		{error,Reason} ->
		    send_error(CM,Channel,erl_posix_msg:message(Reason))
	    end;

	{error,Reason} ->
	    send_error(CM,Channel,erl_posix_msg:message(Reason)),
	    error
    end.

server_recv_file(CM,Channel,Fd,Remain) ->
    server_recv_file(CM,Channel,Fd,Remain,ok).

server_recv_file(CM,Channel,Fd,Remain,WRes) ->
    receive
	{ssh_cm, CM, {exit_signal,Channel,_SIG, _Err, _Lang}} ->
	    io:format("scp: exit_signal\n", []),
	    ssh_cm:close(CM, Channel),
	    {error,closed};

	{ssh_cm, CM, {eof, Channel}} ->
	    io:format("scp: eof\n", []),
	    {error,closed};

	{ssh_cm, CM, {closed, Channel}} ->
	    io:format("scp: closed\n", []),
	    ssh_cm:detach(CM),
	    {error,closed};

	{ssh_cm, CM, {data, Channel, Type, Data}} ->
	    DSize = size(Data),
	    ssh_cm:adjust_window(CM, Channel,DSize),
	    io:format("scp: Type:~p, Remain:~p, Size=~p, Data: ~p\n", 
		      [Type,Remain,DSize,Data]),
	    if Type == ?SSH_EXTENDED_DATA_STDERR ->
		    io:format("scp: ~s", [binary_to_list(Data)]),
		    server_recv_file(CM,Channel,Fd,Remain,WRes);
	       DSize > Remain ->
		    <<Data1:Remain/binary,Data2/binary>> = Data,
		    WRes1 = case file:write(Fd,Data1) of
				ok -> WRes;
				Error -> Error
			    end,
		    self() ! {ssh_cm,CM,{push_data,Channel,Type,Data2}},
		    WRes1;
	       DSize == Remain ->
		    case file:write(Fd,Data) of
			ok -> WRes;
			Error -> Error
		    end;
	       true ->
		    case file:write(Fd,Data) of
			ok ->
			    server_recv_file(CM,Channel,Fd,
					     Remain-DSize,WRes);
			Error ->
			    server_recv_file(CM,Channel,Fd,
					     Remain-DSize,Error)
		    end
	    end
    end.
    

send_error(CM, Channel, Error) ->
    ssh_cm:send(CM, Channel, [?SCP_ERROR,"scp: ",Error,"\n"]),
    error.

send_ok(CM, Channel) ->  
    ssh_cm:send(CM, Channel, [?SCP_OK]),
    ok.

wait_ok(CM, Channel) ->
    case read_data(CM,Channel,1) of
	{ok, [?SCP_OK]} ->
	    ok;
	{ok, _} ->
	    {error,data};
	Error -> Error
    end.

read_line(CM,Channel) ->
    read_data(CM, Channel, -1).

read_raw(CM,Channel) ->
    read_data(CM, Channel, 0).

read_data(CM, Channel, Len) ->
    read_data(CM, Channel, Len, []).

read_data(CM,Channel,Remain,Buf) ->
    receive
	{ssh_cm, CM, {push_data, Channel, Type, Data}} ->
	    read_buf(CM,Channel,Remain,Buf,Type,Data)
    after 0 ->
	    receive
		{ssh_cm, CM, {exit_signal,Channel, SIG, _Err, _Lang}} ->
		    ssh_cm:close(CM, Channel),
		    {error, {exit, SIG}};
		
		{ssh_cm, CM, {eof, Channel}} ->
		    {error, eof};
	
		{ssh_cm, CM, {closed, Channel}} ->
		    ssh_cm:detach(CM),
		    {error, closed};
		
		{ssh_cm, CM, {data, Channel, Type, Data}} ->
		    ssh_cm:adjust_window(CM, Channel, size(Data)),
		    read_buf(CM,Channel,Remain,Buf,Type,Data);
		
		Other ->
		    io:format("scp: read_data got ~p\n", [Other]),
		    read_data(CM, Channel,Remain,Buf)
	    end
    end.


read_buf(CM,Channel,Remain,Buf,?SSH_EXTENDED_DATA_STDERR,Data) ->
    io:format("scp: ~s", [binary_to_list(Data)]),
    read_data(CM, Channel,Remain,Buf);    
read_buf(CM,Channel,-1,Buf,Type,Data) ->
    List = binary_to_list(Data),
    case string:chr(List, $\n) of
	0 -> read_data(CM, Channel, -1, Buf++List);
	I ->
	    {Before,[_|After]} = lists:split(I-1,List),
	    self() ! {ssh_cm,CM,{push_data,Channel,Type,list_to_binary(After)}},
	    {ok,Buf++Before}
    end;
read_buf(_CM,_Channel,0,Buf,_Type,Data) ->
    {ok,Buf++binary_to_list(Data)};    
read_buf(CM,Channel,Remain,Buf,Type,Data) ->
    DSize = size(Data),
    if
	DSize == 0 ->
	    read_data(CM, Channel,Remain,Buf);
	DSize == Remain ->
	    {ok,Buf++binary_to_list(Data)};
       DSize > Remain ->
	    <<Data1:Remain/binary,Data2/binary>> = Data,
	    self() ! {ssh_cm,CM,{push_data,Channel,Type,Data2}},
	    {ok,Buf++binary_to_list(Data1)};
       DSize < Remain ->
	    read_data(CM,Channel,Remain-DSize,Buf++binary_to_list(Data))
    end.

    
    
