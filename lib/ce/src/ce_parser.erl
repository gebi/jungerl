%%% BEGIN ce_parser.erl %%%
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

%% @doc Simple scanner and recursive descent parser.
%%
%% <p>These functions implement a simple scanner and parser which,
%% though inefficient, are flexible and may be suitable for rapid
%% prototyping.</p>
%%
%% @end

-module(ce_parser).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([scan/2, parse/3]).
-export([test/0, eval/1]).

%% @spec scan(string(), [token_spec()]) -> {ok, [token()]} | {error, Reason}
%%         token_spec() = {token_name(), regexp(), token_type()}
%%         token_name() = atom()
%%         token_type() = whitespace | delimiter | literal
%%         token() = {token_name(), string() | delimiter}
%% @doc Scans a string for tokens.

scan(String, TokenSpecs) -> scan(String, TokenSpecs, []).
scan("", TokenSpecs, Acc) -> {ok, lists:reverse(Acc)};
scan(String, TokenSpecs, Acc) ->
  case scan_match(String, TokenSpecs) of
    {ok, {String0, Token}} ->
      scan(String0, TokenSpecs, [Token | Acc]);
    {ok, String0} when is_list(String0) ->
      scan(String0, TokenSpecs, Acc);
    Else ->
      Else
  end.

scan_match(String, []) -> {error, unexpected_character};
scan_match(String, [{TokenName, TokenRegExp, TokenType} | Tail]) ->
  case regexp:match(String, [$^ | TokenRegExp]) of
   {match, 1, Length} ->
     Token = string:substr(String, 1, Length),
     Remainder = lists:nthtail(Length, String),
     case TokenType of
       whitespace ->
         {ok, Remainder};
       delimiter ->
         {ok, {Remainder, {TokenName, delimiter}}};
       literal ->
         {ok, {Remainder, {TokenName, Token}}}
     end;
   _ ->
     scan_match(String, Tail)
  end.

%% @spec parse([token()], [production()], production_name()) -> {ok, {[token()], parse_tree()}} | {error, Reason}
%%         production() = {production_name(), [token_name()], production_body()}
%%         production_body() = [token_name() | production_name()]
%%         production_name() = atom()
%%         parse_tree() = {production_name(), [token()], [parse_tree()]}
%% @doc Parses a list of tokens into a parse tree.

parse(Tokens, Productions, Name) ->
  parse(Tokens, Productions, Name, nil).

parse([], Productions, Name, ParseTree) ->
  {ok, {[], ParseTree}};
parse(Tokens=[Token|_], Productions, Name, ParseTree) ->
  ProdBody = find_production_body(Token, Productions, Name),
  case ProdBody of
    [] ->
      {ok, {Tokens, nil}};
    _ ->
      {NewTokens, NewParseTree, LocalLiterals} =
        lists:foldl(
          fun(ProdElem, {[], PT, LPT}) ->
            {[] , PT, LPT};
             (ProdElem, {TheseTokens=[HeadToken | TokenTail], PT, LPT}) ->
            {TokenType, TokenValue} = HeadToken,
            case {ProdElem, TokenValue} of
              {TokenType, delimiter} ->
                {TokenTail, PT, LPT};
              {TokenType, _} ->
                {TokenTail, PT, [HeadToken | LPT]};
              {ProdName, _} ->
                case parse(TheseTokens, Productions, ProdName, PT) of
                  {ok, {T0, PT0}} ->
                    {T0, PT0, LPT ++ [PT0]};
                  Else ->
                    Else
                end
            end
          end, {Tokens, ParseTree, []}, ProdBody),
      G = list_to_tuple([Name | LocalLiterals]),
      {ok, {NewTokens, G}}
  end.

%% @doc Returns the most likely production from the given list of productions
%% based on the name and the tokens.

find_production_body({TokenName, TokenValue}, Productions, Name) ->
  D = lists:foldl(fun({ProdName,ProdTokens,ProdBody}, A) ->
                case ProdName of
                  Name ->
		    case lists:member(TokenName, ProdTokens)
                         or (ProdTokens == []) of
                      true ->
                        ProdBody;
	              _ ->
		        A
		    end;
		  _ ->
		    A
                end
              end, [], Productions).

  
test() ->
  String = "b(d(9,10),c(11,12))",
  io:fwrite("String: ~p~n", [String]),
  TokenSpecs =
  [
    {whitespace,  " ", whitespace},
    {open_paren,  "\\(", delimiter},
    {close_paren, "\\)", delimiter},
    {comma,       "\\,", delimiter},
    {atom,        "[a-z]+", literal},
    {integer,     "[0-9]+", literal}
  ],
  {ok, Tokens} = scan(String, TokenSpecs),
  
  % function  ::= atom "(" list ")".
  % list      ::= term list_tail.
  % list_tail ::= "," list | nil.
  % Term      ::= integer | function.
  
  Productions = 
  [
    {function,  [atom],          [atom, open_paren, list, close_paren]},
    {list,      [integer, atom], [term, list_tail]},
    {list_tail, [],              []},
    {list_tail, [comma],         [comma, list]},
    {term,      [integer],       [integer]},
    {term,      [atom],          [function]}
  ],
  {ok, {Tokens0, ParseTree}} = parse(Tokens, Productions, function),
  io:fwrite("Tokens0: ~p~n", [Tokens0]),
  io:fwrite("ParseTree: ~p~n", [ParseTree]),
  ok.

eval([String]) when list(String) -> eval(String);
eval(String) ->
  TokenSpecs =
  [
    {whitespace,  " ", whitespace},
    {open_paren,  "\\(", delimiter},
    {close_paren, "\\)", delimiter},
    {plus,        "\\+", literal},
    {minus,       "\\-", literal},
    {times,       "\\*", literal},
    {divide,      "\\/", literal},
    {integer,     "[0-9]+", literal}
  ],
  {ok, Tokens} = scan(String, TokenSpecs),
  
  % expr1      ::= expr2 expr1_tail.
  % expr1_tail ::= "+" expr1 | "-" expr1 | nil.
  % expr2      ::= expr3 expr2_tail.
  % expr2_tail ::= "*" expr1 | "/" expr1 | nil.
  % expr3      ::= integer | "(" expr1 ")".
  
  Productions = 
  [
    {expr1,      [integer, open_paren], [expr2, expr1_tail]},
    {expr1_tail, [],                    []},
    {expr1_tail, [plus,minus],          [oper, expr1]},
    {expr2,      [integer, open_paren], [expr3, expr2_tail]},
    {expr2_tail, [],                    []},
    {expr2_tail, [times, divide],       [oper, expr2]},
    {expr3,      [open_paren],          [open_paren, expr1, close_paren]},
    {expr3,      [integer],             [integer]},
    {oper,       [plus],                [plus]},
    {oper,       [minus],               [minus]},
    {oper,       [times],               [times]},
    {oper,       [divide],              [divide]}
  ],
  {ok, {Tokens0, ParseTree}} = parse(Tokens, Productions, expr1),
  io:fwrite("Expr: ~p~n", [ParseTree]),
  W = walk(ParseTree),
  io:fwrite("Walk: ~p~n", [W]).

walk({expr1, LHS}) ->
  walk(LHS);
walk({expr1, LHS, {expr1_tail, {oper, {Oper, String}}, RHS}}) ->
  op(walk(LHS), Oper, walk(RHS));
walk({expr1, LHS, nil}) ->
  walk(LHS);
walk({expr2, LHS}) ->
  walk(LHS);
walk({expr2, LHS, {expr2_tail, {oper, {Oper, String}}, RHS}}) ->
  op(walk(LHS), Oper, walk(RHS));
walk({expr2, LHS, nil}) ->
  walk(LHS);
walk({expr3, Value}) ->
  walk(Value);
walk({integer, X}) ->
  list_to_integer(X);
walk(nil) ->
  nil.

op(A, plus, B) -> A + B;
op(A, minus, B) -> A - B;
op(A, times, B) -> A * B;
op(A, divide, B) -> A div B.

%%% END of ce_parser.erl %%%

