%%%----------------------------------------------------------------------
%%% File    : pan.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 15 Mar 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(pan).
-author('etxmacr@avc386').

-import(filename,[join/2,dirname/1]).

-export([help/0]).
-export([start/0,start/1,start/2,start/3,start/4,start/5]).
-export([mark/0]).
-export([stop/0]).
-export([scan/1,scan/2,scan/3,scan/4,scan/5]).
-export([perf/1, perf/2]).
-export([prof/1, prof/2, prof/3, prof/4]).
-export([dbg/0,dbg/1,dbg/2,dbg/3,dbg/4]).

-define(HELPFILE, "doc/pan.html").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> panHost:start().
start(A) -> panHost:start(A).
start(A,B) -> panHost:start(A,B).
start(A,B,C) -> panHost:start(A,B,C).
start(A,B,C,D) -> panHost:start(A,B,C,D).
start(A,B,C,D,E) -> panHost:start(A,B,C,D,E).

mark() -> panHost:mark().
stop() -> panHost:stop().

help() -> do_help().

scan(A) -> panScan:file(A).
scan(A, B) -> panScan:file(A, B).
scan(A, B, C) -> panScan:file(A, B, C).
scan(A, B, C, D) -> panScan:file(A, B, C, D).
scan(A, B, C, D, E) -> panScan:file(A, B, C, D, E).
perf(Dir) -> perf(Dir, '').
perf(Dir, File) -> panPerf:go(Dir, File).
prof(Dir) -> prof(Dir, '').
prof(Dir, Proc) -> prof(Dir, Proc, nostack).
prof(Dir, Proc, Stack) -> prof(Dir, Proc, Stack, '').
prof(Dir, Proc, Stack, File) -> panProf:proc(Dir, Proc, Stack, File).  
dbg() -> dbg(start).
dbg(Act) -> dbg(Act,[]).
dbg(Act, Arg) -> panDbg:do(Act, Arg).
dbg(start, TPs, Node) -> panDbg:do(start, TPs, Node).
dbg(start, TPs, Node, Proc) ->panDbg:do(start, TPs, Node, Proc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_help() ->
    Hfile = join(dirname(dirname(code:which(?MODULE))), ?HELPFILE),
    case file:open(Hfile, [read]) of
	{ok, FD} -> do_help(FD, io:get_line(FD, ''), 0);
	Err -> {no_help, Err, Hfile}
    end.
do_help(FD, eof, _) -> ok;
do_help(FD, [], N) -> do_help(FD, io:get_line(FD, ''), N);
do_help(FD, "&gt;"++Txt, 0) -> io:put_chars([$>]),do_help(FD, Txt, 0);
do_help(FD, "&lt;"++Txt, 0) -> io:put_chars([$<]),do_help(FD, Txt, 0);
do_help(FD, [$>|Txt], N) -> do_help(FD, Txt, N-1);
do_help(FD, [$<|Txt], N) -> do_help(FD, Txt, N+1);
do_help(FD, [Char|Txt], N) when N > 0 ->  do_help(FD, Txt, N);
do_help(FD, [Char|Txt], N) ->
    io:put_chars([Char]),
    do_help(FD, Txt, N).
