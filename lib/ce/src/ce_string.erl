%%% BEGIN ce_string.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2003 Cat's Eye Technologies.  All rights reserved.
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

%% @doc String library.
%%
%% @end

-module(ce_string).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([extract/2, extract/3, pad/2, pad/3, keyvalue/1, keyvalue/2]).
-export([join/2, join/3, uc/1, lc/1, chomp/1, split/2]). % perl-like functions
-export([caps/1, caps/2]).
-export([fields/3, columns/2, quote_regexp/1]).
-export([remove_eol/1, strip_quotes/1, strip_quotes/2]).

%% @spec extract(string(), integer()) -> string()
%% @doc Extracts a field at a given location from a string.  Returns the field
%% with spaces stripped off.

extract(String, Start) ->
  string:strip(string:substr(String, Start), both).

%% @spec extract(string(), integer(), integer()) -> string()
%% @doc Extracts a field at a given location with a given width.
%% Returns the field with spaces stripped off.

extract(String, Start, Length) ->
  string:strip(string:substr(String, Start, Length), both).

%% @spec pad(string(), integer()) -> string()
%% @doc Pads a string to a given width in characters.

pad(String, Width) -> pad(String, Width, left).

%% @spec pad(string(), integer(), left | right | centre) -> string()
%% @doc Pads a string to a given width in characters.

pad(String, Width, left) when Width > length(String) ->
  string:left(String, Width);
pad(String, Width, right) when Width > length(String) ->
  string:right(String, Width);
pad(String, Width, centre) when Width > length(String) ->
  string:centre(String, Width).

%% @spec chomp(string()) -> string()
%% @doc Removes all newlines from the end of a string.
%% Should work on both 'nix and MS-DOS newlines.

chomp([]) -> [];
chomp(List) when list(List) ->
  lists:reverse(chomp0(lists:reverse(List))).
chomp0([]) -> [];
chomp0([H | T]) when H == 10; H == 13 -> chomp0(T);
chomp0([H | T]=L) -> L.

%% @spec remove_eol(string()) -> string()
%% @doc Removes a single newline from the end of a string.
%% Should work on both 'nix and MS-DOS newlines.

remove_eol("") -> "";
remove_eol(S) ->
  case lists:last(S) of
    10 ->
      S0 = ce_lists:trunc(S),
      case S0 of
        "" -> "";
	_ ->
          case lists:last(S0) of
            13 ->
              ce_lists:trunc(S0);
            _ ->
	      S0
          end
      end;
    _ ->
      S
  end.

%% @spec join(string(), [string()]) -> string()
%% @doc Joins strings with a delimeter between each part.

join(P, L)           -> join0(P, L, []).
join0(P, [], O)      -> O;
join0(P, [X], O)     -> O ++ X;
join0(P, [X | T], O) -> join0(P, T, O ++ X ++ P).

%% @spec join(string(), string(), [string()]) -> string()
%% @doc Joins strings with a delimeter between each part
%% and a second delimeter between the last two parts.
%% e.g. <code>join(", ", ", and ", ["A","B","C"]) -> "A, B, and C"</code>

join(P, Q, L)          -> join0(P, Q, L, []).
join0(P, Q, [], O)     -> O;
join0(P, Q, [X], O)    -> O ++ X;
join0(P, Q, [X, Y], O) -> join0(P, Q, [Y], O ++ X ++ Q);
join0(P, Q, [X | T], O)-> join0(P, Q,   T, O ++ X ++ P).

%% @spec uc(string()) -> string()
%% @doc Translates a string to uppercase. Also flattens the list.

uc(L) -> uc(L, []).
uc([], A) -> A;
uc([H|T], A) -> uc(T, A ++ [uc(H)]);
uc(L, A) -> toupper(L).

%% @spec lc(string()) -> string()
%% @doc Translates a string to lowercase. Also flattens the list.

lc(L) -> lc(L, []).
lc([], A) -> A;
lc([H|T], A) -> lc(T, A ++ [lc(H)]);
lc(L, A) -> tolower(L).

%% @spec caps(string() | atom()) -> string()
%% @equiv caps(String, {$ , $ })

caps(S) -> caps(S, {$ , $ }).

%% @spec caps(string() | atom(), {char(), char()}) -> string()
%% @doc Capitalizes each letter that is not preceded by a letter.
%% Also translates one character to another, typically for turning
%% underscores into hyphens or spaces for human-readability.

caps(S, {From, To}) when atom(S) -> caps(atom_to_list(S), {From, To});
caps(S, {From, To}) when list(S) -> caps(S, {From, To}, [], $ ).

caps([], {From, To}, Acc, Last) ->
  lists:reverse(Acc);
caps([From | T], {From, To}, Acc, Last) ->
  caps(T, {From, To}, [To | Acc], To);
caps([H | T], {From, To}, Acc, Last)
 when Last >= $a, Last =< $z; Last >= $A, Last =< $Z ->
  caps(T, {From, To}, [tolower(H) | Acc], H);
caps([H | T], {From, To}, Acc, Last) ->
  caps(T, {From, To}, [toupper(H) | Acc], H).

toupper(X) when X >= $a, X =< $z -> X + ($A - $a);
toupper(X)                       -> X.

tolower(X) when X >= $A, X =< $Z -> X + ($a - $A);
tolower(X)                       -> X.

%% @spec split(char(), string()) -> [string()]
%% @doc Splits a string on each occurance of a delimeter.
%% e.g. split($+, "a+b++c") -> ["a", "b", "", "c"]

split(D, L) -> split(D, L, [], []).
split(D, [], A, A0) -> lists:reverse([A0] ++ A);
split(D, [D | T], A, A0) ->
  split(D, T, [lists:reverse(A0) | A], []);
split(D, [H | T], A, A0) ->
  split(D, T, A, [H | A0]).

%% @spec keyvalue(string()) -> {string(), string()}
%% @equiv keyvalue(string(), ":")

keyvalue(String) -> keyvalue(String, ":").

%% @spec keyvalue(string(), string()) -> {string(), string()}
%% @doc Gets the key and value from a line of text.
%% Can be used to parse simple mail headers, http headers, etc.

keyvalue(String, Delim) ->
  case string:tokens(String, Delim) of
    [Key | Data] ->
      Data0 = ce_string:join(Delim, Data),
      Data1 = string:strip(Data0, both),
      Key0 = string:strip(Key, both),
      {Key0, Data1};
    [] ->
      {"", ""}
  end.
	
%% @spec fields(string(), string(), string()) -> [string()]
%% @doc Translates string into list of strings by fields.
%% e.g. <code>fields("a,'b,c',d", ",", "'") -> ["a","b,c","d"]</code>

fields(String, Delim, Quote) ->
  DelimPat = quote_regexp(Delim),
  QuotePat = quote_regexp(Quote) ++ ".*" ++ quote_regexp(Quote),
  TokenSpecs =
  [
    {whitespace,  " ", whitespace},
    {delim,       DelimPat, literal},
    {quoted,      QuotePat, literal},
    {atom,        "[^" ++ DelimPat ++ "]+", literal}
  ],
  case ce_parser:scan(String, TokenSpecs) of
    {ok, C} ->
      C0 = lists:foldl(fun({TokenType, String}, A) when list(String) ->
                            [String | A];
                          (_, A) ->
                            A
                       end, [], C),
      C1 = ce_lists:between(Delim, lists:reverse(C0)),
      C2 = lists:map(fun(X) ->
	                 case regexp:match(X, QuotePat) of
			   {match, Start, Length} ->
			     tl(ce_lists:trunc(X));
			   _ ->
			     X
			 end
	               end, C1),
      C2;
    Other ->
      Other
  end.

%% @spec columns(string(), schema()) -> [string()]
%%         schema() = [{column(), width()}]
%% @doc Translates string into list of strings by columns.
%% e.g. <code>columns("1234567890", [{2,3},{8,2}]) -> ["234", "89"]</code>

columns(String, Schema) -> lists:reverse(columns(String, Schema, [])).
columns(String, [], Acc) -> Acc;
columns(String, [{Column, Width} | T], Acc) ->
  Field = ce_string:extract(String, Column, Width),
  columns(String, T, [Field | Acc]).

%% @spec quote_regexp(string()) -> string()
%% @doc Inserts escape chars into string to make it a literal regexp.
%% This way, characters which have special meaning in regular expressions
%% can be used within a regular expression.
%% Similar to Perl's <code>quotemeta</code> function.

quote_regexp(String) -> lists:reverse(quote_regexp(String, [])).
quote_regexp([], Acc) -> Acc;
quote_regexp([H | T], Acc) when H >= $a, H =< $z ->
  quote_regexp(T, [H | Acc]);
quote_regexp([H | T], Acc) when H >= $A, H =< $Z ->
  quote_regexp(T, [H | Acc]);
quote_regexp([H | T], Acc) when H >= $0, H =< $9 ->
  quote_regexp(T, [H | Acc]);
quote_regexp([H | T], Acc) ->
  quote_regexp(T, [H, $\\ | Acc]).

%% @spec strip_quotes(string()) -> string()
%% @equiv strip_quotes(string(), $")

strip_quotes(S) -> strip_quotes(S, $").

%% @spec strip_quotes(string(), char()) -> string()
%% @doc Strips leading and trailing quotes from a string.

strip_quotes("", Q) -> "";
strip_quotes([C], Q) -> C;
strip_quotes(S=[Q | T], Q) ->
  case lists:last(T) of
    Q -> ce_lists:trunc(T);
    _ -> S
  end;
strip_quotes(S, Q) -> S.

%% @spec unescape(string()) -> string()
%% @doc Unescapes backslash-escaped sequences in a string.

unescape(S) -> unescape(S, []).
unescape([], A) -> lists:reverse(A);
unescape([92, 92 | T], A) -> unescape(T, [A | "\\"]);
unescape([92, $b | T], A) -> unescape(T, [A | "\b"]);
unescape([92, $t | T], A) -> unescape(T, [A | "\t"]);
unescape([92, $n | T], A) -> unescape(T, [A | "\n"]);
unescape([92, $r | T], A) -> unescape(T, [A | "\r"]);
% todo: any others...
unescape([H | T], A) -> unescape(T, [A | H]).

%%% END of ce_string.erl %%%
