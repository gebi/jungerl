% First working version of Erlang QuickCheck.

-module(qc).
-export([seeds/1, prop_independence/1,
         spawn/3,spawn_link/3,spawn/1,spawn_link/1,
	 return/1,bind/2,two/1,oneof/1,frequency/1,sized/1,resize/2,
         nat/0,int/0,bool/0,list/1,function0/1,function/1,function2/1,function3/1,
	 sample/1,test/2,
         implies/2, forall/2, eval/1, collect/2,within/2,testwithin/4,
	 quickcheck/1,quickcheck/3,
	 trace/2, controller/4, event/1, pause/0, echoer/0,
	 sample_traces/1, sample_traces/3,
	 rename_pids/1, nowaits/1,
	 tracetest/0,
	 next/1,empty/0,always/1,always/2,eventually/1,eventually/2,until/2,
	 tnot/1,tand/2,tor/2,timplies/2,matches/1,tafter/1,satisfies/2,
	 mortal/0,killer/3,killer/4]).
-compile(export_all).

% splitting the random number seed, for example in a new process.

newseed() -> {random:uniform(32768),random:uniform(32768),random:uniform(32768)}.
seed({A,B,C}) -> random:seed(A,B,C).

seeds(0) -> Seed = random:seed(), seed(Seed), [Seed];
seeds(N) when N>0 ->
  Seed = random:seed(), seed(Seed), Seed1 = newseed(),
  [Seed | seeds(N-1) ++ begin seed(Seed1), seeds(N-1) end].

% quickchecked prop_independence(20) -- 2 million generated seeds all different!
prop_independence(N) -> 
  Seeds = lists:sort(seeds(N)),
  collect(length(Seeds), Seeds == nodups(Seeds)).

nodups([X,X|Xs]) -> nodups([X|Xs]);
nodups([X|Xs]) -> [X|nodups(Xs)];
nodups([]) -> ([]).

% spawn a new process with an independent random number seed.

spawn(M,F,Args) -> 
  Seed = newseed(),
  erlang:spawn(fun() -> seed(Seed), apply(M,F,Args) end).

spawn(Fun) -> spawn(erlang,apply,[Fun,[]]).

spawn_link(M,F,Args) -> 
  Seed = newseed(),
  erlang:spawn_link(fun() -> seed(Seed), apply(M,F,Args) end).

spawn_link(Fun) -> spawn_link(erlang,apply,[Fun,[]]).

% make random number state depend on a value.

depend(X) -> [dependbits(B,8) || B<-binary_to_list(term_to_binary(X))].
dependbits(B,0) -> ok;
dependbits(B,N) when B rem 2==0 -> newseed(), dependbits(B div 2,N-1);
dependbits(B,N) when B rem 2==1 -> seed(newseed()), dependbits(B div 2,N-1).
% is this too slow? We could depend on a few randomly selected bytes instead.

% generators

gen(F,Size) when is_function(F) -> F(Size);
gen([H|T],Size) -> [gen(H,Size) | gen(T,Size)];
gen(T,Size) when is_tuple(T) -> list_to_tuple(gen(tuple_to_list(T),Size));
gen(X,Size) -> X.

return(X) -> fun(Size) -> X end.

bind(G,F) -> fun(Size) -> X = gen(G,Size), gen(F(X),Size) end.

two(G) -> {G,G}.

oneof(Gs) -> fun(Size) ->
  gen(lists:nth(random:uniform(length(Gs)),Gs),Size) end.

frequency(NGs) -> fun(Size) -> {N,V} = gen(pick(NGs),Size), V end.

pick([]) -> {0,{error,"frequency []"}};
pick([{N,G}|NGs]) when is_number(N) ->
  {N1,G1} = pick(NGs),
  I = random:uniform(N+N1),
  {N+N1,if I =< N -> G;
           true -> G1
        end};
pick([G|NGs]) -> pick([{1,G}|NGs]).

sized(F) -> fun(Size) -> gen(F(Size),Size) end.
resize(S,G) -> fun(Size) -> gen(G,S) end.

nat() -> fun(Size) -> random:uniform(Size+1) - 1 end.
int() -> fun(Size) -> random:uniform(2*Size+1) - Size - 1 end.
bool() -> oneof([true,false]).
list(G) -> fun(Size) -> 
  Len = random:uniform(Size+1) - 1,
  [gen(G,Size) || _ <- lists:duplicate(Len,0)]
end.

function0(G) -> fun(Size) ->
  F = gen(function(G),Size),
  fun() -> F({}) end end.
function(G) -> fun(Size) -> 
  Seed = newseed(),
  fun(X) -> S = seed(Seed), depend(X), Y = gen(G,Size), seed(S), Y end end.
function2(G) -> fun(Size) -> 
  F = gen(function(G),Size),
  fun(X,Y) -> F({X,Y}) end end.
function3(G) -> fun(Size) -> 
  F = gen(function(G),Size),
  fun(X,Y,Z) -> F({X,Y,Z}) end end.
function4(G) -> fun(Size) -> 
  F = gen(function(G),Size),
  fun(X,Y,Z,T) -> F({X,Y,Z,T}) end end.

sample(G) -> quickcheck(forall(G,fun(X)->collect(X,true)end)).

% given a size, a test returns {ok,list-of-collected-values} 
% or {fail,arguments,error} or skip.
test(true,Size) -> {ok, []};
test(false,Size) -> {fail, [], ok};
test(F,Size) when is_function(F) -> 
  case catch {ok,F(Size)} of
    {ok,X} -> X;
    {'EXIT',E} -> {fail,[],E}
  end.

implies(true,F) -> fun(Size) -> test(F,Size) end;
implies(false,F) -> fun(Size) -> skip end.

forall(G,F) -> fun(Size) ->
  Arg = gen(G,Size),
  case test(F(eval(Arg)),Size) of
    {fail,Args,E} -> {fail,[Arg|Args],E};
    T -> T
  end
end.

% Generated test data is evaluated before being passed to the property under
% test, permitting test cases to contain function calls.

eval({'@',Mod,Fun,Args}) -> apply(Mod,Fun,lists:map(fun eval/1,Args));
eval([H|T]) -> [eval(H)|eval(T)];
eval(Tup) when is_tuple(Tup) -> list_to_tuple(eval(tuple_to_list(Tup)));
eval(X) -> X.

collect(V,P) -> fun(Size) ->
  case test(P,Size) of
    {ok,Vals} -> {ok,[V|Vals]};
    T -> T
  end
end.

within(T,P) -> fun(Size) ->
  Seed = random:seed(),
  spawn_link(qc,testwithin,[Seed,self(),P,Size]),
  receive {testwithin,R} -> R
	  after T -> {fail,[],timeout}
  end end.

testwithin({A,B,C},Pid,P,Size) ->
  random:seed(A,B,C),

  Pid!{testwithin,test(P,Size)}.

quickcheck(P) -> quickcheck(100,1000,P).

quickcheck(N,Limit,P) -> quickcheck(0,0,[],N,Limit,P).

quickcheck(M,Size,Hist,N,0,P) ->
  io:format("Testing limit reached after ~w successful tests~n",[M]),
  hist(Hist);
quickcheck(M,Size,Hist,M,Limit,P) ->
  io:format("~nOK, passed ~w tests~n",[M]),
  hist(Hist);
quickcheck(M,Size,Hist,N,Limit,P) ->
  case test(P,Size div 5+2) of
    {ok,Vals} -> io:format("."),
		 quickcheck(M+1,Size+1,[Vals|Hist],N,Limit,P);
    {fail,Vals,E} -> io:format("~nFalsifiable, after ~w successful tests:~n",[M]),
                     lists:foreach(fun(V) -> io:format("~p~n",[V]) end, Vals),
		     case E of
			ok -> ok;
			_ -> io:format("Error: ~p~n",[E])
		     end;
    skip -> quickcheck(M,Size+1,Hist,N,Limit-1,P)
  end.

hist(Vals) -> showhist(sorthist(makehist(Vals))).

makehist([]) -> [];
makehist([V|Vs]) -> add(V,makehist(Vs)).

add(V,[]) -> [{V,1}];
add(V,[{V,N}|Vs]) -> [{V,N+1}|Vs];
add(V,[E|Vs]) -> [E|add(V,Vs)].

sorthist(H) -> lists:sort(fun({_,M},{_,N}) -> M>N end, H).

showhist([{[],_}]) -> ok;
showhist(H) -> lists:foreach(fun({V,N}) -> io:format("~w%",[N]), 
					   write(V),
					   io:format("~n") end, H).

write([]) -> ok;
write([V|Vs]) -> io:format(" ~p",[V]), write(Vs).

% The trace generator for concurrent programs.
% trace(Timeout,Fun) generates (all possible?) traces of the concurrent program Fun().
%   Timeout -- if Timeout ms pass with no recorded event, the trace ends
%   Size -- limits the length of the recorded trace.
% Generated traces are lists of events of the form
%   {event,Pid,Event}	-- an event occurred in process Pid
%   {exit,Pid,Reason}	-- a process exited
%   {wait,N}		-- N milliseconds passed with no events recorded
% Reported events may be delayed arbitrarily.
% Processes which report events are monitored for exits, and terminated when trace generation stops.

trace(Timeout,Fun) -> fun(Size) ->
  qc:spawn_link(?MODULE,controller,[self(),Fun,Timeout,Size]),
  receive {qc,controller,Trace} -> Trace end
  end.

controller(Parent,Fun,Timeout,Size) ->
  process_flag(trap_exit,true),
  register(?MODULE,self()),
  register(echoer,spawn_link(?MODULE,echoer,[])),
  Pid = spawn_link(Fun),
  Parent ! {qc,controller,control([],[Pid],Timeout,Size,0,0)}.

control(Events,Pids,Timeout,0,Timing,Waiting) -> kill(Pids), [more];
control(Events,Pids,Timeout,Size,Timing,Waiting) when Timing>0, Events/=[] ->
  control(Events,Pids,Timeout,Size,0,Waiting);
control([],Pids,Timeout,Size,Timing,Waiting) when Timing==Timeout ->
  wait(Waiting, begin kill(Pids), [timeout] end);
control(Events,Pids,Timeout,Size,Timing,Waiting) ->
  receive {event,Pid,Event} -> 
		link(Pid),
		[{event,Pid,Event}
		|control([{event,Pid,Event}|Events],[Pid|lists:subtract(Pids,[Pid])],
			 Timeout,Size-1,Timing,Waiting)];
	  {'EXIT',Pid,Reason} -> 
		wait(Waiting,[{exit,Pid,Reason} | 
			      control(lists:keydelete(Pid,2,Events),
				      lists:subtract(Pids,[Pid]),
				      Timeout,Size-1,Timing,0)])
	  after 0 -> 
		L = length(Events),
		N = random:uniform(lists:max([2*L+2,10])),
		if N>L+1 -> 
			pause(), control(Events,Pids,Timeout,Size,Timing,Waiting);
		   N==1  ->
			receive after 1 -> control(Events,Pids,Timeout,Size,Timing+1,Waiting+1) end;
		   true ->
		     	K = random:uniform(L),
			{event,Pid,Event} = lists:nth(K,Events),
			Pid ! wakeup,
			wait(Waiting, control(lists:subtract(Events,[{event,Pid,Event}]),Pids,
						Timeout,Size,0,0))
		end
  end.

wait(0,Trace) -> Trace;
wait(Waiting,Trace) -> [{wait,Waiting} | Trace].

kill(Pids) ->
  [exit(Pid,kill) || Pid <- Pids],
  receive after 1 -> ok end,	% give processes time to die
  exit(whereis(echoer),kill),
  case whereis(killer) of undefined->ok; _ -> unregister(killer) end,
  unregister(?MODULE).

event(Event) -> 
  ?MODULE ! {event, self(), Event},
  receive wakeup -> Event end.

% pause gives up control to the scheduler, allowing other processes to run.

pause() -> echoer ! self(), receive echoer -> ok end.

echoer() -> receive Pid -> Pid ! echoer, echoer() end.

%tracetest() -> sample(trace(3,fun()->erlang:spawn(fun()->event(b)end),event(a)end)).
%tracetest() -> sample_traces(fun()->erlang:spawn(fun()->event(b)end),event(a)end).

tracetest() -> sample_traces(fun() -> Pid=erlang:spawn(fun()->event(spawned),event(ok)end), 
				      event(spawn),
				      exit(Pid,kill), event(kill) end).



sample_traces(Fun) -> sample_traces(10,4,Fun).

sample_traces(Size,Timeout,Fun) -> 
  quickcheck(resize(Size,forall(trace(Timeout,Fun),fun(Trace)->
				  collect(nowaits(rename_pids(Trace)),true) end))).

rename_pids(X) -> 
  Pids = collect_pids(X),
  rename(Pids,X).

collect_pids(Pid) when is_pid(Pid) -> [Pid];
collect_pids([H|T]) -> 
  HPids=collect_pids(H),
  HPids++lists:subtract(collect_pids(T),HPids);
collect_pids(T) when is_tuple(T) -> collect_pids(tuple_to_list(T));
collect_pids(X) -> [].

rename(Pids,Pid) when is_pid(Pid) -> {pid,position(Pid,Pids)};
rename(Pids,[H|T]) -> [rename(Pids,H)|rename(Pids,T)];
rename(Pids,T) when is_tuple(T) -> list_to_tuple(rename(Pids,tuple_to_list(T)));
rename(Pids,X) -> X.

position(X,[X|_]) -> 1;
position(X,[_|Xs]) -> 1 + position(X,Xs).

nowaits(Trace) -> [E || E <- Trace, not is_wait(E)].
is_wait({wait,_}) -> true;
is_wait(E) -> false.

% Here are combinators for expressing properties of traces.

next(P) ->
  fun ([]) -> false;
      ([Ev|Trace]) -> P(Trace)
  end.

empty() -> fun(Trace)->Trace==[]end.

always(P) -> fun(Trace)-> (Trace==[]) or 
			  (P(Trace) and (next(always(P)))(Trace)) end.

always(0,P) -> fun(Trace)->true end;
always(N,P) when N>0 -> fun(Trace)->
	(Trace==[]) or
	(P(Trace) and (next(always(N-1,P)))(Trace)) end.

eventually(P) -> tnot(always(tnot(P))).
eventually(N,P) -> tnot(always(N,tnot(P))).

until(Q,P) -> fun(Trace) -> Q(Trace) or (Trace==[]) or
			    (P(Trace) and (next(until(Q,P)))(Trace)) end.

tnot(P) -> fun(Trace) -> not P(Trace) end.
tand(P,Q) -> fun(Trace) -> P(Trace) and Q(Trace) end.
tor(P,Q) -> fun(Trace) -> P(Trace) or Q(Trace) end.
timplies(P,Q) -> fun(Trace) -> (not P(Trace)) or Q(Trace) end.

matches(Fun)-> fun([]) -> false;
		  ([Event|_]) -> case catch Fun(Event) of
				   true -> true;
				   _ -> false
				 end
	       end.

tafter(Fun) -> fun ([]) -> true;
		  ([Event|Trace]) -> case catch {ok,Fun(Event)} of
					{ok,P} -> P(Trace);
					_ -> true
				     end
	      end.

satisfies(Trace,P) -> P(Trace).

% Processes call mortal() to indicate that they may die.
% Thereafter the killer process kills processes with a 
% given probability.

mortal() -> killer ! self().

killer(Pause,Wait,Kill) -> killer(Pause,Wait,Kill,10000).

killer(Pause,Wait,Kill,MaxKills) -> 
  register(killer,spawn_link(fun()->event(killer_started),
			            killer(Pause,Wait,Kill,MaxKills,[]) 
			     end)).

killer(Pause,Wait,Kill,MaxKills,Pids) ->
  receive Pid -> killer(Pause,Wait,Kill,[Pid|lists:subtract(Pids,[Pid])])
	  after 0 -> N = random:uniform(Pause+Wait+Kill),
  		     if N=<Pause -> pause(), 
				    killer(Pause,Wait,Kill,MaxKills,Pids);
			N=<Pause+Wait -> 
				    receive after 1 -> 
					killer(Pause,Wait,Kill,MaxKills,Pids) 
				    end;
			(Pids==[]) or (MaxKills==0) -> 
				    killer(Pause,Wait,Kill,MaxKills,Pids);
			true -> Pid = lists:nth(random:uniform(length(Pids)),Pids),
				event({kill,Pid}),
				exit(Pid,kill),
				killer(Pause,Wait,Kill,MaxKills-1,
					lists:subtract(Pids,[Pid]))
		     end
  end.