%%%----------------------------------------------------------------------
%%% File    : pan.hrl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created :  8 Mar 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-define(WAIT(N), receive after N*1000 -> ok end).
-define(MODULES, [panTarg, panLib]).
-define(TMPDIR, "/tmp").
-define(LOG(Tag, Info), panLib:log(Tag, ?MODULE, ?LINE, ?FUNCTION, Info)).
-define(FUNCTION, process_info(self(),current_function)).

%%% default arguments
-define(DEFTC, []).
-define(DEFFLAGS, [running,timestamp]).
-define(DEFPROCS, all).
-define(DEFTPS, []).

-define(DEFIPPORT, 9666).
-define(DEFIPQUE, 4096).

-define(DEFWRAPSIZE, 128*1024).
-define(DEFWRAPCNT, 8).


%%% mandatory args (added automagically)
-define(MANDFLAGS, [set_on_spawn,timestamp,call]).
-define(MANDTPS, []).

%%% flag aliases
-define(DBGFLAGS, []).
-define(GCFLAGS,  [garbage_collection]).
-define(PROCFLAGS, ?GCFLAGS++[running,send,'receive']).
-define(PERFFLAGS, ?GCFLAGS++[running,procs]).
-define(PROFFLAGS, ?PERFFLAGS++[return_to,arity]).
-define(ALLFLAGS, ?PROFFLAGS++[send,'receive']).

-record(timer, {state, start, stop, timeout}).
