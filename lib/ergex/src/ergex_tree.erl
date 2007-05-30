-module(ergex_tree).
-export([fail/0, empty/0, any/0, element/1,
         seq/2, alt/2, optional/1, star/1, plus/1]).
-export([all_matches/2, match/2]).

% A regex G is a pair {Nullable, R}
% where the bool Nullable is true iff the empty string is to be matched,
% and other matching is determined by R, one of:
%   zero		never matches anything
%   one                 matches the empty string
%   any			matches any one character
%   {element, C}	matches one character, C
%   {seq, R1, R2}	matches R1's language followed by R2's
%   {alt, R1, R2}	matches the union of R1 and R2's languages
%   {plus, R}		matches 1 or more R's
% where the R's *must not* match the empty string, except that one
% subexpression of a seq may when the other does not.
% Use only the following constructors, to enforce this invariant.
% (Termination of the matcher depends on it.)
% (Many of the cases below are optimizations not needed for correctness --
% e.g. the first two seq/2 cases.)

fail()                        -> {false, zero}.

empty()                       -> {true, zero}.

any()                         -> {false, any}.

element(C)                    -> {false, {element, C}}.

seq({N, zero}, G)             -> if N -> G; true -> {false, zero} end;
seq(G, {N, zero})             -> if N -> G; true -> {false, zero} end;
seq({true,  R1}, {N2,    R2}) -> {N2,    {seq, either(one, R1), R2}};
seq({false, R1}, {true,  R2}) -> {false, {seq, R1, either(one, R2)}};
seq({false, R1}, {false, R2}) -> {false, {seq, R1, R2}}.

alt({N1, R1}, {N2, R2})       -> {N1 or N2, either(R1, R2)}.

optional({_, R})              -> {true, R}.

star({_, R})                  -> {false, S} = plus({false, R}),
                                 {true, S}.

plus({N, zero})               -> {N, zero};
plus({N, {plus, R}})          -> {N, {plus, R}};
plus({N, R})                  -> {N, {plus, R}}.

either(zero, R2) -> R2;
either(R1, zero) -> R1;
either(R, R)     -> R;
either(R1, R2)   -> {alt, R1, R2}.


% all_matches(regex(), list(C)) -> list(list(C))
%  returns the tails of the input after all successful matches.

all_matches(G, S) -> ll_to_list(match(G, S)).


% match(regex(), list(C)) -> lazylist(list(C))

match({true,  R}, S) -> m(either(one, R), S);
match({false, R}, S) -> m(R, S).

m(zero, _)             -> [];
m(one, S)              -> singleton(S);
m(any, [_|S])          -> singleton(S);
m(any, [])             -> [];
m({element, C}, [C|S]) -> singleton(S);
m({element, _}, _)     -> [];
m({seq, R1, R2}, S)    -> ll_flatmap(fun (S1) -> m(R2, S1) end,
                                     m(R1, S));
m({alt, R1, R2}, S)    -> ll_append(m(R1, S),
                                    fun () -> m(R2, S) end);
m({plus, R}, S)        -> ll_flatmap(fun (S1) ->
                                         [S1 | fun () -> m({plus, R}, S1) end]
                                     end,
                                     m(R, S)).


% The type lazylist(X) is [] | [X | fun() -> lazylist(X) end].

ll_to_list([]) -> [];
ll_to_list([H|TF]) -> [H | ll_to_list(TF()) ].

singleton(X) -> [X | fun () -> [] end].

ll_append([],     F) -> F();
ll_append([H|TF], F) -> [H | fun () -> ll_append(TF(), F) end].

ll_flatmap(_, [])     -> [];
ll_flatmap(F, [H|TF]) -> ll_append(F(H),
                                   fun () -> ll_flatmap(F, TF()) end).
