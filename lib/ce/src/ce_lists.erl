%%% BEGIN ce_lists.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2002 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc List manipulation library.
%%
%% @end

-module(ce_lists).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([find_first/2, trunc/1, delete/2, replace/3, between/2]).
-export([split/2, partition/2, replace_all/3]).
-export([zip/2, zipn/1]).
-export([pairsearch/3, pairsearch/4]).
-export([grep/2]). % perl-like functions
-export([integer_to_big_endian/2, integer_to_little_endian/2]).
-export([big_endian_to_integer/1, little_endian_to_integer/1]).
-export([big_endian_to_integer/3, little_endian_to_integer/3]).
-export([factor/1, diff/2]).

%%% BEGIN ce_lists.erl %%%

%% @spec find_first(term(), [term()]) -> {ok, integer()} | {error, not_found}
%% @doc Finds the first occurance of the given term in the given list
%% and returns the 1-based index of where it was found.

find_first(X, L) -> find_first(X, L, 1).
find_first(X, [], Acc) -> {error, not_found};
find_first(X, [X | T], Acc) -> {ok, Acc};
find_first(X, [H | T], Acc) -> find_first(X, T, Acc+1).

%% @spec trunc([term()]) -> [term()]
%% @doc Returns a list of all but the last element of a given list.

trunc([]) -> [];
trunc([X]) -> [];
trunc([X, Y]) -> [X];
trunc([H | T]) -> [H] ++ ce_lists:trunc(T).

%% @spec delete(integer(), [term()]) -> [term()]
%% @doc Returns a list of all but the nth element of a given list.

delete(X, []) -> [];
delete(1, [H | T]) -> T;
delete(N, L) -> lists:sublist(L, N-1) ++ lists:nthtail(N, L).

%% @spec replace(integer(), [term()], term()) -> [term()]
%% @doc Returns a list with the nth element replaced by a new term.

replace(X, [], G) -> [];
replace(1, [H | T], G) -> [G | T];
replace(N, L, G) -> lists:sublist(L, N-1) ++ [G] ++ lists:nthtail(N, L).

%% @spec replace_all(term(), term(), [term()]) -> [term()]
%% @doc Returns a list with the all elements equal to the first argument
%% replaced with the second argument.

replace_all(A, B, L) -> replace_all(A, B, L, []).
replace_all(A, B, [], Acc) -> lists:reverse(Acc);
replace_all(A, B, [A | T], Acc) ->
  replace_all(A, B, T, [B | Acc]);
replace_all(A, B, [H | T], Acc) ->
  replace_all(A, B, T, [H | Acc]).

%% @spec grep(regexp(), list()) -> string()
%% @doc Searches through a list for an element matching the given regexp.

grep(P, L) -> grep(P, L, []).
grep(P, [], A) -> A;
grep(P, [H | T], A) ->
  case regexp:match(H, P) of
    nomatch -> grep(P, T, A);
          X -> grep(P, T, A ++ [H])
  end.

%% @spec between(term(), [term()]) -> [term()]
%% @doc Extracts elements between delimeter elements,
%% using an empty list when two delimeters are adjacent.
%% Can be used for parsing comma-seperated text where two
%% adjacent commas indicates a significant null value in-between.

between(D, L) -> lists:reverse(between(D, L, [])).
between(D, [], Acc) -> Acc;
between(D, [D], Acc) -> between(D, [], [[] | Acc]);
between(D, [Q], Acc) -> between(D, [], [Q | Acc]);
between(D, [D | R], Acc) -> between(D, R, [[] | Acc]);
between(D, [Q, D | R], Acc) -> between(D, R, [Q | Acc]).

%% @spec pairsearch(term(), integer(), [term()], term()) -> term()
%% @doc Searches for a key in a list of {Key, Value} tuples.
%% Returns the value if found, or the Else argument if not found.

pairsearch(Term, Pos, List, Else) ->
  case lists:keysearch(Term, Pos, List) of
    {value, {_, X}} ->
      X;
    false -> 
      Else
  end.

%% @spec pairsearch(term(), [term()], term()) -> term()
%% @equiv pairsearch(term(), 1, [term()], term())

pairsearch(Term, List, Else) ->
  pairsearch(Term, 1, List, Else).

%% @spec zip([term()], [term()]) -> [tuple()]
%% @doc Creates a list of pairs from corresponding elements of two lists.
%% Thanks to Heinz Eriksson for this code.

zip(A, B) -> lists:reverse(zip(A, B, [])).
zip([H1|L1], [H2|L2], Z) -> zip(L1, L2, [{H1, H2} | Z]);
zip([], L2, Z) -> Z;
zip(L1, [], Z) -> Z.

%% @spec zipn([[term()]]) -> [tuple()]
%% @doc Creates a list of tuples from corresponding elements of a list of lists.
%% Thanks to Vladimir Sekissov for this code.

zipn(Ls) ->
  [list_to_tuple(L) || L <- listn(Ls)].

listn(Ls) ->
  [lists:reverse(L)
   || L <- foldn(fun (A, Acc) -> [A|Acc] end, [], Ls)].

foldn(_, _, []) ->
  [];
foldn(Fun, Acc0, Ls) ->
  foldn(Fun, Acc0, Ls, []).

foldn(_, _, [[]|_], Ret) ->
  lists:reverse(Ret);
foldn(Fun, Acc0, Ls, Ret) ->
  foldn(Fun, Acc0,
	[tl(L) || L <- Ls],
	[lists:foldl(Fun, Acc0, [hd(L) || L <- Ls])|Ret]
       ).

%% @spec split(function(), [term()]) -> {[term()], [term()]}
%% @doc Splits a list into two based on a predicate.  The first list
%% returned consists of all the elements for which the predicate returns
%% true, the second, all for which it returns false.  The returned lists
%% are both in the same order as the given one.

split(Fun, List) -> split(Fun, List, [], []).
split(Fun, [], TA, FA) -> {lists:reverse(TA), lists:reverse(FA)};
split(Fun, [H | T], TA, FA) ->
  case Fun(H) of
    true ->
      split(Fun, T, [H | TA], FA);
    false ->
      split(Fun, T, TA, [H | FA])
  end.

%% @spec partition([term()], integer()) -> {[term()], [term()]}
%% @doc Splits a list into two sublists at a given point.

partition(List, N) ->
  {lists:sublist(List, N), lists:nthtail(N, List)}.

%% @spec little_endian_to_integer([integer()]) -> integer()
%% @doc Converts a little-endian list to an integer.

little_endian_to_integer(List) ->
  r_list_to_integer(lists:reverse(List), 0).

%% @spec little_endian_to_integer([integer()], integer(), integer()) -> integer()
%% @doc Converts a little-endian segment of a list to an integer.

little_endian_to_integer(List, Start, Length) ->
  little_endian_to_integer(lists:sublist(List, Start, Length)).

%% @spec big_endian_to_integer([integer()]) -> integer()
%% @doc Converts a big-endian list to an integer.

big_endian_to_integer(List) ->
  r_list_to_integer(List, 0).

%% @spec big_endian_to_integer([integer()], integer(), integer()) -> integer()
%% @doc Converts a big-endian segment of a list to an integer.

big_endian_to_integer(List, Start, Length) ->
  big_endian_to_integer(lists:sublist(List, Start, Length)).

r_list_to_integer([], Acc) ->
  Acc;
r_list_to_integer([H | T], Acc) ->
  r_list_to_integer(T, Acc * 256 + H).

%% @spec integer_to_big_endian(integer(), integer()) -> [integer()]
%% @doc Converts an integer to a big-endian list.

integer_to_big_endian(Integer, Size) ->
  r_integer_to_list(Integer, Size, []).

%% @spec integer_to_little_endian(integer(), integer()) -> [integer()]
%% @doc Converts an integer to a little-endian list.

integer_to_little_endian(Integer, Size) ->
  lists:reverse(r_integer_to_list(Integer, Size, [])).

r_integer_to_list(Integer, 0, Acc) -> Acc;
r_integer_to_list(Integer, Size, Acc) ->
  r_integer_to_list(Integer div 256, Size-1, [Integer rem 256 | Acc]).

%% @spec factor([term() | [term()]]) -> [[term()]]
%% @doc Factors a list, returning all combinations of sublists.
%% e.g. <code>factor([[a,b],c,[d,e]) ->
%% [[a,c,d],[a,c,e],[b,c,d],[b,c,e]]</code>

factor(L) -> deflate(factor(L, []), []).

factor([], Acc) -> Acc;
factor([H | T], Acc) when list(H) ->
  lists:foldl(fun(Q, A) ->
                A ++ [factor(T, Acc ++ [Q])]
              end, [], H);
factor([H | T], Acc) ->
  factor(T, Acc ++ [H]).

deflate([], Acc) -> Acc;
deflate([H | T], Acc) when list(H) ->
  case H of
    [H2 | T2] when list(H2) ->
      deflate(T, Acc ++ deflate(H, []));
    _ ->
      deflate(T, Acc ++ [H])
  end;
deflate([H | T], Acc)  ->
  deflate(T, Acc ++ [H]).

%% @spec diff([term()], [term()]) -> [diff()]
%%         diff() = {action(), integer(), [term()]}
%%         action() = insert | delete
%% @doc Calculates the differences between two ordered lists.
%% The differences are expresses in terms of a list of tags
%% which indicate which elements were inserted or deleted at
%% what positions within the first list. Note that this by no
%% means claims to provide a minimal set of differences!

diff(List1, List2) ->
  lists:reverse(diff(List2, List1, 1, [])).
diff([], [], Pos, Acc) ->
  Acc;
diff([], List2, Pos, Acc) ->
  [{delete, Pos, List2} | Acc];
diff(List1, [], Pos, Acc) ->
  [{insert, Pos, List1} | Acc];
diff([Head1 | Tail1], [Head1 | Tail2], Pos, Acc) ->
  diff(Tail1, Tail2, Pos + 1, Acc);
diff([Head1 | Tail1], List2=[Head2 | Tail2], Pos, Acc) ->
  diff(Tail1, List2, Pos + 1, [{insert, Pos, [Head1]} | Acc]).

%%% END of ce_lists.erl %%%
