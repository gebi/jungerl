%%%----------------------------------------------------------------------
%%% File    : cb_template.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : template for panScan callback function
%%% Created :  2 Mar 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------
%%% Message - {Tag, {Pid, Name}, Data, TimeStamp}
%%%
%%% Tag				  Data 
%%% 'receive',			  Message 
%%% call,			  {M,F,A} 
%%% exit,			  Reason 
%%% return_to,			  {M,F,A} 
%%% spawn,			  Pid2 
%%% link,			  Pid2 
%%% unlink,			  Pid2 
%%% getting_linked,		  Pid2 
%%% in,				  {M,F,A} 
%%% out,			  {M,F,A} 
%%% gc_start,			  Info 
%%% gc_end,			  Info 
%%% send,			  {Pid2, Msg} 
%%% send_to_non_existing_process, {Msg, Pid2} 
%%% return_from,		  {{M,F,A}, ReturnValue}
%%%----------------------------------------------------------------------

-module(cb_template).
-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).

-record(st, {}).

requires() -> [{flags, []}, {tps, []}].
doc() -> "prints everything".

go([Message, Line, Out, initial|_]) ->
    io:fwrite(Out, "~p: initializing~n", [?MODULE]),
    go([Message, Line, Out, #st{}]);
go([end_of_trace, Line, Out, St|_]) ->
    io:fwrite(Out, "~p: exiting - msgs: ~w~n", [?MODULE, Line-1]);
go([Message, Line, Out, St|_]) ->
    io:fwrite(Out, "~p: ~-5w - ~w~n", [?MODULE, Line, Message]),
    St.
