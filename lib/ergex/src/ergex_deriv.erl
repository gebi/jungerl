-module(ergex_deriv).
-export([fail/0, empty/0, any/0, element/1, seq/2, alt/2, star/1]).
-export([optional/1, plus/1]).
-export([matches/2, matching/2, matches_null/1, deriv/2]).


%% Regular-expression constructors.
%% The rewrites towards a canonical form are crucial to keep the
%% derivatives from blowing up in size. I don't know where to stop
%% adding optimizations, though, offhand.

fail()                    -> fail.

empty()                   -> empty.

any()                     -> any.

element(C)                -> {element, C}.

alt(fail, G2)                    -> G2;
alt(G1, fail)                    -> G1;
alt(G, G)                        -> G;
alt({alt, G1, G2}, G3)           -> alt(G1, alt(G2, G3));
alt(G,             {alt, G, G1}) -> {alt, G, G1};
alt(G,             {alt, G1, G}) -> {alt, G1, G};
alt({seq, G,  G2}, {seq, G, G4}) -> seq(G, alt(G2, G4));
alt({seq, G1, G},  {seq, G3, G}) -> seq(alt(G1, G3), G);
alt(G,             {seq, G, G1}) -> seq(alt(empty, G), G1);
alt({seq, G, G1},  G)            -> seq(alt(empty, G), G1);
alt(G,             {seq, G1, G}) -> seq(alt(empty, G1), G);
alt({seq, G1, G},  G)            -> seq(alt(empty, G1), G);
alt(G1, G2) when G1 < G2         -> {alt, G1, G2};
alt(G1, G2)                      -> {alt, G2, G1}.

seq(fail, _)              -> fail;
seq(_, fail)              -> fail;
seq(empty, G2)            -> G2;
seq(G1, empty)            -> G1;
seq({seq, G1, G2}, G3)    -> seq(G1, seq(G2, G3));
seq(G1, G2)               -> {seq, G1, G2}.

star(fail)                -> fail;
star(empty)               -> empty;
star({star, G})           -> {star, G};
star(G)                   -> {star, G}.


%% Convenience constructors.

optional(G) -> alt(empty(), G).

plus(G)     -> seq(G, star(G)).


%% Matching

matches(G, S)      -> matches_null(matching(G, S)).

matches_null(G)    -> delta(G) =:= empty.

matching(G, [])    -> G;
matching(G, [C|S]) -> matching(deriv(G, C), S).


%% Derivatives

deriv(fail,          _) -> fail();
deriv(empty,         _) -> fail();
deriv(any,           _) -> empty();
deriv({element, C},  C) -> empty();
deriv({element, _},  _) -> fail();
deriv({alt, G1, G2}, C) -> alt(deriv(G1, C), deriv(G2, C));
deriv({seq, G1, G2}, C) -> alt(seq(deriv(G1, C), G2),
                               seq(delta(G1), deriv(G2, C)));
deriv(G={star, G1},  C) -> seq(deriv(G1, C), G).

delta(fail)          -> fail;
delta(empty)         -> empty;
delta(any)           -> fail;
delta({element, _})  -> fail;
delta({alt, G1, G2}) -> case delta(G1) of
			    fail  -> delta(G2);
			    empty -> empty
			end;
delta({seq, G1, G2}) -> case delta(G1) of
			    fail  -> fail;
			    empty -> delta(G2)
			end;
delta({star, _})     -> empty.
