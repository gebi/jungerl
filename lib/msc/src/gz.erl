%%%----------------------------------------------------------------------
%%% File    : gz.erl
%%% Author  : Martin Bjorklund <mbj@bluetail.com>
%%% Purpose : API to streaming gz driver.
%%% Created : 11 Dec 2000 by Martin Bjorklund <mbj@bluetail.com>
%%%----------------------------------------------------------------------

-module(gz).
-author('mbj@bluetail.com').

-export([load_driver/0, open/1, stream/2, close/1]).


%% op codes
-define(GZ_OPEN,       $o).
-define(GZ_READ,       $r).
-define(GZ_WRITE,      $w).
-define(GZ_CONV,       $v).
-define(GZ_CLOSE,      $c).

%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------
load_driver() ->
    erl_ddll:load_driver(code:priv_dir(msc), "gz_drv").

%%-----------------------------------------------------------------
%% Func: open(Mode) -> {ok, Handle} | {error, Reason}
%% Types: Mode = compress | decompress
%% Purpose: Opens a streaming gz port.
%%-----------------------------------------------------------------
open(Mode) ->  
    M = if Mode == decompress -> ?GZ_READ;
	   Mode == compress -> ?GZ_WRITE
	end,
    P = open_port({spawn, 'gz_drv'}, [binary]),
    erlang:port_command(P, [?GZ_OPEN, M]),
    case recv(P) of
	ok ->
	    {ok, {gz, P}};
	Err ->
	    Err
    end.

%%-----------------------------------------------------------------
%% Func: stream(Handle, Data) -> {ok, RData} | {error, Reason}
%% Types: Data = RData = io_list()
%% Purpose: Depenign on the mode of handle, compresses or
%%          decompresses the Data.  Note that RData might be empty.
%%-----------------------------------------------------------------
stream({gz, P}, Bin) ->
    erlang:port_command(P, [?GZ_CONV, Bin]),
    recv_all(P, []).

%%-----------------------------------------------------------------
%% Func: close(Handle) -> {ok, RData} | {error, Reason}
%% Types: RData = io_list()
%% Purpose: Closes the gz port, and finalizes the (de)compression.
%%          Note that more data might be returned!
%%-----------------------------------------------------------------
close({gz, P}) ->
    erlang:port_command(P, [?GZ_CLOSE]),
    case recv_all(P, []) of
	ok ->
	    unlink(P),
	    catch erlang:port_close(P),
	    {ok, []};
        Else ->
	    unlink(P),
	    catch erlang:port_close(P),
	    Else
    end.
    

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
recv_all(P, Ack) ->
    case recv(P) of
	Bin when binary(Bin) ->
	    recv_all(P, [Bin | Ack]);
	L when list(L) ->
	    recv_all(P, [L | Ack]);
	eof ->
	    {ok, lists:reverse(Ack)};
	Else ->
	    Else
    end.


recv(P) ->
    receive
	{P, value, Bin} ->
	    Bin;
	{P, ok} ->
	    ok;
	{P, error, ErrAtom} ->
	    unlink(P),
	    erlang:port_close(P),
	    {error, ErrAtom};
	{P, eof} ->
	    eof
    end.

