%%% File    : ucs_data.erl
%%% Author  : Lon Willett <Lon.Willett@sse.ie>
%%%           Johan Blom <johan@localhost.localdomain>
%%% Description : Database support for ucs module.

-module(ucs_data).

-vsn('0.3').
-author('Lon.Willett@sse.ie').
-modified_by('johan.blom@mobilearts.se').

-behaviour(gen_server).
-compile([verbose,report_warnings,warn_unused_vars]).

%%% The main exports, for the "ucs" module.
-export([code_to_mnemonic/1, mnemonic_to_code/1, unicode_data/1,
	 rebuild_database/0, rebuild_database/1, rebuild_database/2]).

%%% Conversion to/from IANA recognised character sets
-export([to_unicode/2, from_unicode/2,
	 getMIB/1, getCharset/1, getIANAname/1]).

%%% unicode_fixed_data_blocks/0 is exported for used by the
%%% ucs_data_build module.  Its definition needs to match the
%%% the current unicode database ("UnicodeData.txt").
-export([unicode_fixed_data_blocks/0]).

%%% Server start and stop; note that the server will be started
%%% when needed under the "kernel_safe_sup" supervisor.
-export([start_link/0, stop/0]).

%%% Starts the server unless it is not already started.
-export([ensure_started/0]).

%%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("kernel/include/file.hrl").
-include("unidata.hrl").

%%% Server state
-record(state,{dir,
	       unicode_data,
	       unicode_mappings,
	       ucs_mnemonics_bycode,
	       ucs_mnemonics_bymnem}).

%%% ----------------------------------------------------------------------------
%%% Internal API
code_to_mnemonic(Code) when integer(Code), Code >= 0, Code =< 16#7FFFFFFF ->
    ensure_started(),
    gen_server:call(ucs_data,{code_to_mnemonic,Code}).

mnemonic_to_code(Mnem) ->
    ensure_started(),
    gen_server:call(ucs_data,{mnemonic_to_code,Mnem}).

unicode_data(Code) when integer(Code), Code >= 0, Code =< 16#7FFFFFFF ->
    BlockStart = block_start(Code),
    Key = if BlockStart =/= false -> BlockStart; true -> Code end,
    ensure_started(),
    case gen_server:call(ucs_data,{code_to_unidata,Key}) of
	undefined ->
	    #unidata{code=Code,name="<U+" ++ ucs:code_to_string(Code) ++ ">"};
	Unidata when record(Unidata,unidata) ->
	    if
		BlockStart =:= false -> Unidata;
		true ->
		    Name = Unidata#unidata.name ++
			", U+" ++ ucs:code_to_string(Code) ++ ">",
		    Unidata#unidata{code=Code,name=Name}
	    end
    end.

to_unicode(Input,Charset) ->
    gen_server:call(ucs_data,{to_unicode,Input,Charset}).


from_unicode(Input,Charset) ->
    gen_server:call(ucs_data,{from_unicode,Input,Charset}).


getMIB(Charset) ->
    gen_server:call(ucs_data,{getMIB,Charset}).
    

getCharset(MIBnum) ->
    gen_server:call(ucs_data,{getCharset,MIBnum}).

getIANAname(CharsetName) ->
    gen_server:call(ucs_data,{getIANAname,CharsetName}).


rebuild_database() ->
    rebuild_database(all).

rebuild_database([Which,MappingDir]) ->
    rebuild_database(Which,atom_to_list(MappingDir));
rebuild_database(Which) ->
    rebuild_database(Which,".").

rebuild_database(Which,MappingDir) ->
    Dir=get_data_dir(),
    case catch ucs_data_build:rebuild(Which,Dir,MappingDir) of
	ok ->
	    ok;
	{'EXIT',Reason} ->
	    io:format("ERROR: ~p",[Reason]),
	    {error,Reason}
    end.


ensure_started() ->
    case whereis(ucs_data) of
	undefined ->
	    application:start(ucs);
	Pid ->
	    {ok,Pid}
    end.


stop() ->
    case whereis(ucs_data) of
	undefined -> ok;
	_ -> gen_server:call(ucs_data,stop)
    end.


start_link() ->
    gen_server:start_link({local,ucs_data},?MODULE,[],[]).


init([]) ->
    process_flag(trap_exit,true),
    Dir=filename:join([get_data_dir(),"db"]),
    case open_ucs_mibenum(Dir) of
	ok ->
	    {ok,Dirlist}=file:list_dir(Dir),
	    State = #state{dir=Dir,
			   unicode_data=closed,
			   unicode_mappings=scan_dir(Dir,Dirlist),
			   ucs_mnemonics_bycode=closed,
			   ucs_mnemonics_bymnem=closed},
	    {ok,State};
	{error,Reason} ->
	    {stop,Reason}
    end.


terminate(_Reason,State) ->
    close_all(State).


open_ucs_mibenum(Dir) ->
    Path = filename:join(Dir,"mibenum.dets"),
    case dets:open_file(mibenum_data,
			[{type,set},{keypos,1},
			 {file,Path},{access,read},
			 {cache_size,256}]) of
	{ok,mibenum_data} ->
	    ok;
	{error,Reason} ->
	    io:format("WARNING: Cannot open mibenum data ~p~n",[Reason]),
	    {error,Reason}
    end.



scan_dir(_Dir,[]) ->
    [];
scan_dir(Dir,["unicode-"++CharsetFile|Rest]) ->
    case lists:reverse(CharsetFile) of
	"bat."++RevCharset ->
	    File=filename:join(Dir,"unicode-"++CharsetFile),
	    {ok,_Tab}=ets:file2tab(File),
	    [list_to_atom(lists:reverse(RevCharset)) | scan_dir(Dir,Rest)];
	_ ->
	    scan_dir(Dir,Rest)
    end;
scan_dir(Dir,[_|Rest]) ->
    scan_dir(Dir,Rest).


handle_call({code_to_unidata,Code},_From,State) ->
    handle_code_to_unidata(Code,State);
handle_call({code_to_mnemonic,Code},_From,State) ->
    handle_code_to_mnemonic(Code,State);
handle_call({mnemonic_to_code,Mnem},_From,State) ->
    handle_mnemonic_to_code(Mnem,State);
handle_call({to_unicode,Input,Charset},_From,State) ->
    Out=case get_IANAname(Charset) of
	    CharsetName when atom(CharsetName) ->
		case lists:member(CharsetName,State#state.unicode_mappings) of
		    true ->
			use_mapping_to(Input,CharsetName,[]);
		    _ ->
			{error,undefined}
		end;
	    {error,Reason} ->
		{error,Reason};
	    undefined -> 
		{error,unsupported_charset}
	end,
    {reply,Out,State};
handle_call({from_unicode,Input,Charset},_From,State) ->
    Out=case get_IANAname(Charset) of
	    CharsetName when atom(CharsetName) ->
		case lists:member(CharsetName,State#state.unicode_mappings) of
		    true ->
			use_mapping_from(Input,CharsetName,[]);
		    _ ->
			{error,undefined}
		end;
	    {error,Reason} ->
		{error,Reason};
	    undefined -> 
		{error,unsupported_charset}
	end,
    {reply,Out,State};
handle_call({getMIB,Charset},_From,State) ->
    handle_getMIB(Charset,State);
handle_call({getCharset,MIBnum},_From,State) ->
    handle_getCharset(MIBnum,State);
handle_call({getIANAname,CharsetName},_From,State) ->
    IANAname=get_IANAname(CharsetName),
    {reply,IANAname,State};
handle_call(stop,_From,State) ->
    {stop, normal, ok, State}.

handle_cast(_,State) ->
    {noreply,State}.

handle_info(_,State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Where is our database located?

get_data_dir() ->
    Dir = get_data_dir1(),
    %%error_logger:info_msg("ucs_data:get_data_dir() -> ~s~n",[Dir]),
    Dir.

get_data_dir1() ->
    %% 1st try: code:priv_dir(ucs)
    case code:priv_dir(ucs) of
	{error,_} -> get_data_dir2();
	Dir -> case is_dir(Dir) of true -> Dir; false -> get_data_dir2() end
    end.
get_data_dir2() ->
    %% 2nd try: code:lib_dir(ucs)
    case code:lib_dir(ucs) of
	{error,_} -> get_data_dir3();
	Dir -> case is_dir(Dir) of true -> Dir; false -> get_data_dir3() end
    end.
get_data_dir3() ->
    %% 3rd try: starting at directory containing this modules code,
    %% look around for likely candidates
    case code:is_loaded(?MODULE) of
	{file,Filename} ->
	    Bin = filename:dirname(Filename),
	    Lib =
		case filename:basename(Bin) of
		    "ebin" -> filename:dirname(Bin);
		    _ -> Bin
		end,
	    Priv = filename:join(Lib,"priv"),
	    case is_dir(Priv) of
		true -> Priv;
		false -> Lib
	    end;
	_ ->
	    get_data_dir4()
    end.
get_data_dir4() ->
    %% Final try (shouldn't ever get here)
    ".".

is_dir(Dir) ->
    case file:read_file_info(Dir) of
	{ok,#file_info{type=directory}} -> true;
	_ -> false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_code_to_unidata(Code,State)
  when integer(Code), Code >= 0, Code =< 16#7FFFFFFF ->
    State1 = open_unicode_data(State),
    case State1#state.unicode_data of
	open ->
	    case dets:lookup(unicode_data,Code) of
		[] ->
		    {reply,undefined,State1};
		[Unidata] when record(Unidata,unidata) ->
		    {reply,Unidata,State1}
	    end;
	error ->
	    {reply,undefined,State1}
    end.

handle_code_to_mnemonic(Code,State)
  when integer(Code), Code >= 0, Code =< 16#7FFFFFFF ->
    State1 = open_ucs_mnemonics_bycode(State),
    case State1#state.ucs_mnemonics_bycode of
	open ->
	    case dets:lookup(ucs_mnemonics_bycode,Code) of
		[] ->
		    {reply,undefined,State1};
		[{Code,Mnem}] ->
		    {reply,Mnem,State1}
	    end;
	error ->
	    {reply,undefined,State1}
    end.

handle_mnemonic_to_code(Mnem,State) ->
    State1 = open_ucs_mnemonics_bymnem(State),
    case State1#state.ucs_mnemonics_bymnem of
	open ->
	    case dets:lookup(ucs_mnemonics_bymnem,Mnem) of
		[] ->
		    {reply,undefined,State1};
		[{Code,Mnem}] ->
		    {reply,Code,State1}
	    end;
	error ->
	    {reply,undefined,State1}
    end.

open_unicode_data(State) ->
    case State#state.unicode_data of
	closed ->
	    Path = filename:join(State#state.dir,"unidata.dets"),
	    case dets:open_file(unicode_data,
				[{type,set},
				 {keypos,#unidata.code},
				 {file,Path},
				 {access,read},
				 {cache_size,512}]) of
		{ok,unicode_data} ->
		    State#state{unicode_data=open};
		{error,_} ->
		    State#state{unicode_data=error}
	    end;
	open ->
	    State;
	error ->
	    State
    end.

open_ucs_mnemonics_bycode(State) ->
    case State#state.ucs_mnemonics_bycode of
	closed ->
	    Path = filename:join(State#state.dir,"ucsmnem1.dets"),
	    case dets:open_file(ucs_mnemonics_bycode,
				[{type,set},
				 {keypos,1},
				 {file,Path},
				 {access,read},
				 {cache_size,256}]) of
		{ok,ucs_mnemonics_bycode} ->
		    State#state{ucs_mnemonics_bycode=open};
		{error,_} ->
		    State#state{ucs_mnemonics_bycode=error}
	    end;
	open ->
	    State;
	error ->
	    State
    end.

open_ucs_mnemonics_bymnem(State) ->
    case State#state.ucs_mnemonics_bymnem of
	closed ->
	    Path = filename:join(State#state.dir,"ucsmnem2.dets"),
	    case dets:open_file(ucs_mnemonics_bymnem,
				[{type,set},
				 {keypos,2},
				 {file,Path},
				 {access,read},
				 {cache_size,256}]) of
		{ok,ucs_mnemonics_bymnem} ->
		    State#state{ucs_mnemonics_bymnem=open};
		{error,_} ->
		    State#state{ucs_mnemonics_bymnem=error}
	    end;
	open ->
	    State;
	error ->
	    State
    end.


%%% FIXME gets {error,not_owner} trap when trying to close the dets tables below
close_all(State) ->
    case catch close_all2(State) of
	{'EXIT',{{badmatch,{error,not_owner}},_}} ->
	    State;
	State1 ->
	    State1
    end.

close_all2(State) ->
    ok = case State#state.unicode_data of
	     open -> dets:close(unicode_data);
	     _ -> ok
	 end,
    ok = case State#state.ucs_mnemonics_bycode of
	     open -> dets:close(ucs_mnemonics_bycode);
	     _ -> ok
	 end,
    ok = case State#state.ucs_mnemonics_bymnem of
	     open -> dets:close(ucs_mnemonics_bymnem);
	     _ -> ok
	 end,
    dets:close(ucs_mibenum),
    State#state{unicode_data=closed,
		ucs_mnemonics_bycode=closed,
		ucs_mnemonics_bymnem=closed}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% This fun returns a sorted list containing {Low,High} code
%%% pairs of the fixed-data Unicode blocks.  It must match the
%%% current database (UnicodeData.txt).
unicode_fixed_data_blocks() ->
    [{16#3400,16#4DB5},		% CJK Ideograph Extension A
     {16#4E00,16#9FA5},		% CJK Ideograph
     {16#AC00,16#D7A3},		% Hangul Syllable
     {16#D800,16#DB7F},		% Non Private Use High Surrogate
     {16#DB80,16#DBFF},		% Private Use High Surrogate
     {16#DC00,16#DFFF},		% Low Surrogate
     {16#E000,16#F8FF},		% Private Use
     {16#F0000,16#FFFFD},	% Plane 15 Private Use
     {16#100000,16#10FFFD}].	% Plane 16 Private Use


%%% block_start(Char) -- Return code of start of block that Char is in,
%%% or false if Char is not part of a fixed-data block.  This must match
%%% the definition of unicode_data_blocks().

block_start(Char) when Char < 16#3400 -> false;
block_start(Char) when Char < 16#A000 ->
    if
	Char =< 16#4DB5 -> 16#3400;	% CJK Ideograph Extension A
	Char  < 16#4E00 -> false;
	Char =< 16#9FA5 -> 16#4E00;	% CJK Ideograph
	true -> false
    end;
block_start(Char) when Char < 16#AC00 -> false;
block_start(Char) when Char < 16#F900 ->
    if
	Char =< 16#D7A3 -> 16#AC00;	% Hangul Syllable
	Char  < 16#D800 -> false;
	Char  < 16#DB80 -> 16#D800;	% Non Private Use High Surrogate
	Char  < 16#DC00 -> 16#DB80;	% Private Use High Surrogate
	Char  < 16#E000 -> 16#DC00;	% Low Surrogate
	true            -> 16#E000	% Private Use
    end;
block_start(Char) when Char < 16#F0000 -> false;
block_start(Char) when Char < 16#110000 ->
    if
	Char =<  16#FFFFD -> 16#F0000;	% Plane 15 Private Use
	Char  < 16#100000 -> false;
	Char =< 16#10FFFD -> 16#100000;	% Plane 16 Private Use
	true -> false
    end;
block_start(_) ->
    %% Not a Unicode char
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Use the ets table to match each character from Input (using CharsetName)
%%% into Unicode.
use_mapping_to([],_CharsetName,Out) ->
    lists:reverse(Out);
use_mapping_to([Char|Input],CharsetName,Out) ->
    case ets:lookup(CharsetName,Char) of
	[] ->
	    {error,undefined_char};
	[{Char,UniChar}] ->
	    use_mapping_to(Input,CharsetName,[UniChar|Out])
    end.


%%% Use the dets table to match each character from Input (using Unicode) into
%%% CharsetName.
use_mapping_from([],_CharsetName,Out) ->
    lists:reverse(Out);
use_mapping_from([UniChar|Input],CharsetName,Out) ->
    case ets:lookup(CharsetName,{UniChar}) of
	[] ->
	    {error,undefined_char};
	[{_,Char}] ->
	    use_mapping_from(Input,CharsetName,[Char|Out])
    end.



get_IANAname(Charset) ->
    case dets:lookup(mibenum_data,Charset) of
	[] ->
	    {error,undefined_charset};
	[{Charset,MIBnum}] when atom(Charset),integer(MIBnum) ->
	    case dets:lookup(mibenum_data,MIBnum) of
		[] ->
		    {error,undefined_mibenum};
		[{MIBnum,CharsetName}] when atom(CharsetName),integer(MIBnum) ->
		    CharsetName
	    end;
	[{Charset,Charset}] -> % Not yet registred with IANA
	    Charset
    end.

    
handle_getMIB(Charset,State) when atom(Charset) ->
    case dets:lookup(mibenum_data,Charset) of
	[] ->
	    {reply,{error,undefined_charset},State};
	[{Charset,MIBnum}] when atom(Charset),integer(MIBnum) ->
	    {reply,MIBnum,State};
	[{Charset,Charset}] when atom(Charset) ->
	    {reply,not_IANA_registered,State}
    end.


handle_getCharset(MIBnum,State) ->
    case dets:lookup(mibenum_data,MIBnum) of
	[] ->
	    {reply,{error,undefined_mibenum},State};
	[{MIBnum,Charset}] when atom(Charset),integer(MIBnum) ->
	    {reply,Charset,State}
    end.
