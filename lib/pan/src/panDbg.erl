%%%----------------------------------------------------------------------
%%% File    : panDbg.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created :  1 Jun 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(panDbg).
-author('etxmacr@avc386').

-export([do/2,do/3,do/4]).

do(add, TPs) when list(TPs) ->
    case whereis(panHost) of
	undefined -> do(start, TPs);
	_ -> bang({add, TPs})
    end;
do(add, TP) -> do(add, [TP]);
do(del, TPs) when list(TPs) -> do_do(del, TPs);
do(del, TP) -> do(del, [TP]);
do(info, Args) -> do_do(info, Args);
do(stop, _) -> pan:stop();
do(start, TPs) -> do(start, TPs, '').

do(start, TPs, Node) -> do(start, TPs, Node, '').
do(start, TPs, Node, Proc) -> do(start, TPs, Node, Proc, []).
do(start, TPs, Node, Proc, Flags) -> pan:start(ip, Node, Proc, Flags, TPs).

do_do(Act, Args) ->
    case whereis(panHost) of
	undefined -> {error, not_started};
	_ -> bang({Act, Args})
    end.

bang(Term) -> 
    panHost ! {?MODULE, Term},
    ok.
