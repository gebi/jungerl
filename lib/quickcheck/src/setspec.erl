-module(setspec).
-compile(export_all).
-include_lib("quickcheck/include/quickcheck.hrl").

% sets are built by new(), from_list(List), add_element(E,S), del_element(E,S),
% union(S1,S2), intersection(S1,S2), subtract(S1,S2), filter(P,S).

set() -> ?SIZED(Size,set(trunc(math:sqrt(Size))+1)).

set(0) -> return({'@',sets,new,[]});
set(Size) ->
  frequency([return({'@',sets,new,[]}), 
	     {6,?LET(L,list(int()),return({'@',sets,from_list,[L]}))},
	     {6,?LET(S,set(Size-1),?LET(E,int(),return({'@',sets,add_element,[E,S]})))},
	     ?LET(S,set(Size-1),?LET(E,int(),return({'@',sets,del_element,[E,S]}))),
	     ?LET({S1,S2},two(set(Size div 3)),return({'@',sets,union,[S1,S2]})),
	     ?LET({S1,S2},two(set(Size div 3)),return({'@',sets,intersection,[S1,S2]})),
	     ?LET({S1,S2},two(set(Size div 3)),return({'@',sets,subtract,[S1,S2]})),
	     ?LET(S,set(Size-1),?LET(P,function(bool()),return({'@',sets,filter,[P,S]})))]).

test() -> qc:sample(?LET(S,set(),return(sets:size(eval(S))))).

evalset({sets,F,Xs}) when is_list(Xs) -> apply(sets,F,[evalset(X) || X<-Xs]);
evalset({sets,F,X}) -> apply(sets,F,[X]);
evalset(X) -> X.

prop_seteq() -> 
  ?FORALL(E,set(), E==norm(E)).

equal(S1,S2) -> lists:sort(sets:to_list(S1)) == lists:sort(sets:to_list(S2)).
norm(S) -> sets:from_list(lists:sort(sets:to_list(S))).

prop_setequal() ->
  ?FORALL(S,set(),collect(sets:size(S),equal(S,norm(S)))).

nodups([X,X|Xs]) -> nodups([X|Xs]);
nodups([X|Xs]) -> [X|nodups(Xs)];
nodups([]) -> ([]).

prop_fromlist() -> 
  ?FORALL(Xs,list(int()),
    equal(sets:from_list(Xs),sets:from_list(nodups(lists:sort(Xs))))).

prop_union_commutes() -> ?SIZED(N,resize(5*N,
  ?FORALL(X,set(),
  ?FORALL(Y,set(),
  collect(sets:size(sets:union(X,Y)),
  equal(sets:union(X,Y),sets:union(Y,X))))))).
