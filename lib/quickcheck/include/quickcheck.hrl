-define(LET(X,G1,G2),qc:bind(G1,fun(X)->G2 end)).
-define(IMPLIES(B,T),qc:implies(B,fun(Size)->qc:test(T,Size) end)).
-define(FORALL(X,G,T),qc:forall((G),fun(X)->T end)).
-define(SIZED(S,G),qc:sized(fun(S)->G end)).
-define(TRACE(T,P),qc:trace(T,fun()->P end)).
-import(qc,[two/1,int/0,nat/0,bool/0,list/1,function/1,oneof/1,frequency/1,return/1,resize/2,collect/2,within/2,sample/1,eval/1,sample_traces/1,sample_traces/3,trace/2,event/1,pause/0,rename_pids/1,nowaits/1,next/1,empty/0,always/1,always/2,eventually/1,eventually/2,until/2,tnot/1,tand/2,tor/2,timplies/2,matches/1,satisfies/2,mortal/0,killer/3,killer/4]).

% macros for trace properties
-define(MATCHES(P),qc:matches(fun(P)->true end)).
-define(NOW(Pat,P),qc:matches(fun(Pat)->P end)).
-define(TIMPLIES(P,Q),fun(Trace)->case P(Trace) of true -> Q(Trace); _ -> true end end).
-define(TAND(P,Q),fun(Trace)->case P(Trace) of true->Q(Trace); _ -> false end end).
-define(TOR(P,Q),fun(Trace)->case P(Trace) of true->true; _ -> Q(Trace) end end).
-define(TNOT(P),fun(Trace)->not P(Trace) end).
-define(AFTER(Pat,P),qc:tafter(fun(Pat)->P end)).