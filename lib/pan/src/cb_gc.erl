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

-module(cb_gc).
-author('etxmacr@avc386').

-define(UPD_FIELD(XLD, XField, XDiff),
	case (XLD)#ld.XField of 
	    no -> XLD;
	    _ -> (XLD)#ld{XField = ntadd((XLD)#ld.XField,{0,0,XDiff})}
	end).


-export([go/1, requires/0, doc/0]).

-record(ld, {diff = 500000, out, pid = no, in = no, gc = no, file_driver = no}).

requires() -> [{flags, [garbage_collection,running,procs]}, {tps, []}].
doc() -> "picks out gc's longer than 500 ms".

go([Message, Line, Out, initial|_]) ->
    go([Message, Line, Out, initial_state(Out)]);
go([end_of_trace, Line, Out, LD|_]) ->
    ok;

go([{out,_,0,TS}, Line, Out, LD|_]) ->		       %filedriver
    LD#ld{file_driver = TS};
go([{in,_,0,TS}, Line, Out, LD|_]) ->		       %filedriver
    Diff = out(LD, Line, file_driver, "file_driver", TS, LD#ld.file_driver),
    ?UPD_FIELD(?UPD_FIELD(LD#ld{file_driver = no}, gc, Diff), in, Diff);

go([{in,{Pid,PI},_,TS}, Line, Out, LD|_]) ->
    LD#ld{in = TS, pid = Pid};
go([{out,{Pid,PI},_,TS}, Line, Out, LD|_]) ->
    out(LD, Line, PI, "sched", TS, LD#ld.in),
    LD#ld{in = no, pid = no};
go([{exit,{Pid,PI},_,TS}, Line, Out, LD = #ld{pid = Pid}|_]) ->
    out(LD, Line, PI, "schedx", TS, LD#ld.in),
    LD#ld{in = no, pid = no};

go([{gc_start,{Pid,PI},_,TS}, Line, Out, LD|_]) ->     %gc
    LD#ld{gc = TS};
go([{gc_end,{Pid,PI},_,TS}, Line, Out, LD = #ld{gc = Gc}|_]) ->	%gc
    Diff = out(LD, Line, PI, "gc", TS, Gc),
    ?UPD_FIELD(LD#ld{gc = no}, in, Diff);

go(X = [_,_,_,LD|_]) -> %%    io:fwrite("~p: ~w~n", [?MODULE, X]),
    LD.

initial_state(Out) ->
    #ld{out = Out}.

out(#ld{diff = Diff, out = Out}, Line, PI, Lab, Now, In) ->
    case catch ntdiff(Now, In) of
	{'EXIT', R} -> 0;
	D when D > Diff ->
	    io:fwrite(Out, "~p: ~w ~s ~s ~w - ~w ms~n", 
		      [?MODULE, Line, ntform(Now), Lab, PI, D/1000]),
	    D;
	D -> D
    end.

ntadd({MSo, So, USo}, {MSi, Si, USi}) ->
    case USi+USo of
	US when US > 999999 -> 
	    case So+Si+1 of
		S when S > 999999 -> {MSo+MSi+1, S-1000000, US-1000000};
		S -> {MSo+MSi, S, US-1000000}
	    end;
	US ->
	    case So+Si of
		S when S > 999999 -> {MSo+MSi+1, S-1000000,US};
		S -> {MSo+MSi, S,US}
	    end
    end. 

		     
ntdiff({MS, S, USo}, {MS, S, USi}) -> 
    (USo-USi);
ntdiff({MS, So, USo}, {MS, Si, USi}) -> 
    (USo-USi)+(So-Si)*1000000;
ntdiff({MSo, So, USo}, {MSi, Si, USi}) ->
    (USo-USi)+(So-Si)*1000000+(MSo-MSi)*1000000000000.
ntform({Msec, Sec, Usec} = Now) ->
    T = tuple_to_list(element(2,calendar:now_to_datetime(Now)))++[Usec],
    io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w", T).

