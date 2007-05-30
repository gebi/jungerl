-module(ergex_parse).
-export([parse/2]).

% N.B. no proper syntax error messages yet -- we just crash.

parse(Rg, S) -> {G, ""} = expr(Rg, S),
                G.

expr(Rg, S) -> case term(Rg, S) of
                   {G1, "|"++S1} -> {G2, S2} = expr(Rg, S1),
                                    {Rg:alt(G1, G2), S2};
                   {G1, S1} -> {G1, S1}
               end.

term(Rg, S) -> {G1, S1} = postfix(Rg, factor(Rg, S)),
               case S1 of
                   [C|_] when C /= $", C /= $), C /= $], C /= $| ->
                       {G2, S2} = term(Rg, S1),
                       {Rg:seq(G1, G2), S2};
                   _ -> {G1, S1}
               end.

postfix(Rg, {G, "*"++S}) -> postfix(Rg, {Rg:star(G), S});
postfix(Rg, {G, "+"++S}) -> postfix(Rg, {Rg:plus(G), S});
postfix(Rg, {G, "?"++S}) -> postfix(Rg, {Rg:optional(G), S});
postfix(_,  {G, S})      -> {G, S}.

factor(Rg, "")     -> {Rg:empty(), ""};
factor(Rg, "."++S) -> {Rg:any(), S};
factor(Rg, "("++S) -> {G1, ")"++S1} = expr(Rg, S),
                      {G1, S1};
factor(Rg, "["++S) -> {Chars, S1} = charset(S),
                      {make_charset(Rg, Chars), S1};
factor(Rg, [C|S])  -> case lists:member(C, "*+?|()[]") of
                          true  -> {Rg:empty(), [C|S]};
                          false -> {Rg:element(C), S}
                      end.

charset("]"++S) -> {[], S};
charset([C|S])  -> {Chars, S1} = charset(S),
                   {[C|Chars], S1}.

make_charset(Rg, [])     -> Rg:fail();
make_charset(Rg, [C|Cs]) -> Rg:alt(Rg:element(C), make_charset(Rg, Cs)).