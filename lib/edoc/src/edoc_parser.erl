-module(edoc_parser).
-define(THIS_MODULE, edoc_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-export([parse_spec/1, parse_typedef/1, format/1]).

%% Multiple entry point hack:

parse_spec(Ts) -> parse([{start_spec, 0} | Ts]).

parse_typedef(Ts) -> parse([{start_typedef, 0} | Ts]).

format({spec, none, T, Ds}) ->
    format(T) ++ format_defs(Ds);
format({spec, {name, F}, T, Ds}) ->
    atom_to_list(F) ++ format(T) ++ format_defs(Ds);
format({typedef, T, Ds}) ->
    format(T) ++ format_defs(Ds);
format({'fun', As, T}) ->
    "(" ++ format_seq(As) ++ ") -> " ++ format(T);
format({par, none, T}) ->
    format(T);
format({par, {name, N}, T}) ->
    atom_to_list(N) ++ "::" ++ format(T);
format({def, V, T}) ->
    format(V) ++ " = " ++ format(T);
format({union, As}) ->
    format_union(As);
format({type, N, As}) ->
    atom_to_list(N) ++ "(" ++ format_seq(As) ++ ")";
format({type, M, N, As}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(N)
	++ "(" ++ format_seq(As) ++ ")";
format({tuple, As}) ->
    "{" ++ format_seq(As) ++ "}";
format({list, T}) ->
    "[" ++ format(T) ++ "]";
format({var, V}) ->
    atom_to_list(V);
format(nil) ->
    "[]";
format({atom, V}) ->
    atom_to_list(V);
format({integer, V}) ->
    integer_to_list(V);
format({float, V}) ->
    float_to_list(V).

format_union(Ts) -> format_seq(Ts, "| ").

format_seq(Ts) -> format_seq(Ts, ", ").

format_seq([T], S) -> format(T);
format_seq([T | Ts], S) -> format(T) ++ S ++ format_seq(Ts, S);
format_seq([], _) -> "".

format_defs([D | Ds]) -> "\n\t" ++ format(D) ++ format_defs(Ds);
format_defs([]) -> "".

%% Utility functions:

tok_val(T) -> element(3, T).

%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    case catch yeccpars1(Tokens, false, 0, [], []) of
	error ->
	    Errorline =
		if Tokens == [] -> 0; true -> element(2, hd(Tokens)) end,
	    {error,
	     {Errorline, ?THIS_MODULE, "syntax error at or after this line."}};
	Other ->
	    Other
    end.

parse_and_scan({Mod, Fun, Args}) ->
    case apply(Mod, Fun, Args) of
	{eof, _} ->
	    {ok, eof};
	{error, Descriptor, _} ->
	    {error, Descriptor};
	{ok, Tokens, _} ->
	    yeccpars1(Tokens, {Mod, Fun, Args}, 0, [], [])
    end.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser toplevel.
% Doesn't have to be exported!
return_error(Line, Message) ->
    throw({error, {Line, ?THIS_MODULE, Message}}).


% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {M, F, A}, State, States, Vstack) ->
    case catch apply(M, F, A) of
        {eof, Endline} ->
            {error, {Endline, ?THIS_MODULE, "end_of_file"}};
        {error, Descriptor, Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?THIS_MODULE, Reason}};
        {ok, Tokens, Endline} ->
	    case catch yeccpars1(Tokens, {M, F, A}, State, States, Vstack) of
		error ->
		    Errorline = element(2, hd(Tokens)),
		    {error, {Errorline, ?THIS_MODULE,
			     "syntax error at or after this line."}};
		Other ->
		    Other
	    end
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?THIS_MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({Cat, _, Val}) -> io_lib:format('~w', [Val]);

yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


yeccpars2(0, start_typedef, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, start_spec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  none,
 yeccpars2(65, __Cat, [2 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(3, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(start, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(6, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(9, __Cat, [7 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(8, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {typedef,{type,tok_val(__1),__2},__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typedef, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(10, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__2|__1],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(defs, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(12, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(13, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {atom,tok_val(__1)},
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {float,tok_val(__1)},
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {integer,tok_val(__1)},
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(19, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(20, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(ptypes, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(21, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {union,lists:reverse(__1)},
 yeccpars2(yeccgoto(utype, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {def,{var,tok_val(__1)},__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(def, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {tuple,__1},
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {var,tok_val(__1)},
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(25, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(utypes, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(27, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(28, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(utype_tuple, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(29, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  lists:reverse(__2),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(utype_tuple, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__3|__1],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(utypes, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(32, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(33, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__3|__1],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(ptypes, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(35, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__3|__1],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(ptypes, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(36, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'fun',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(fun_type, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(38, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(39, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {type,tok_val(__1),__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(41, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {type,tok_val(__1),tok_val(__3),__4},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(utype_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(44, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  lists:reverse(__2),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(utype_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(46, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  nil,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(47, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(48, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {list,__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(49, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(50, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(51, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {def,{type,tok_val(__1),__2},__4},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(def, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(52, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(53, __Cat, [52 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(53, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {typedef,{def,{type,tok_val(__1),__2},__4},__5},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(typedef, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(54, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(parameter_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(parameters, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(56, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(57, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {par,none,__1},
 yeccpars2(yeccgoto(parameter, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(58, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {var,tok_val(__1)},
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(59, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(60, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {par,{name,tok_val(__1)},__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(parameter, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(61, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  lists:reverse(__2),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(parameter_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(62, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(63, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__3|__1],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(parameters, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(64, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {name,tok_val(__1)},
 yeccpars2(yeccgoto(function_name, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(65, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(start, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(68, __Cat, [67 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(68, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {spec,__1,__2,lists:reverse(__3)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(spec, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 exit({parser, __Other, missing_state_in_action_table}).

yeccgoto(def, 9) ->
 11;
yeccgoto(def, 53) ->
 11;
yeccgoto(def, 68) ->
 11;
yeccgoto(defs, 7) ->
 9;
yeccgoto(defs, 52) ->
 53;
yeccgoto(defs, 67) ->
 68;
yeccgoto(fun_type, 6) ->
 17;
yeccgoto(fun_type, 8) ->
 17;
yeccgoto(fun_type, 13) ->
 17;
yeccgoto(fun_type, 14) ->
 17;
yeccgoto(fun_type, 25) ->
 17;
yeccgoto(fun_type, 29) ->
 17;
yeccgoto(fun_type, 32) ->
 17;
yeccgoto(fun_type, 33) ->
 17;
yeccgoto(fun_type, 36) ->
 17;
yeccgoto(fun_type, 38) ->
 17;
yeccgoto(fun_type, 50) ->
 17;
yeccgoto(fun_type, 59) ->
 17;
yeccgoto(fun_type, 62) ->
 17;
yeccgoto(fun_type, 65) ->
 67;
yeccgoto(function_name, 2) ->
 65;
yeccgoto(parameter, 6) ->
 55;
yeccgoto(parameter, 62) ->
 63;
yeccgoto(parameter_list, 4) ->
 7;
yeccgoto(parameter_list, 6) ->
 19;
yeccgoto(parameter_list, 8) ->
 19;
yeccgoto(parameter_list, 10) ->
 49;
yeccgoto(parameter_list, 13) ->
 19;
yeccgoto(parameter_list, 14) ->
 19;
yeccgoto(parameter_list, 25) ->
 19;
yeccgoto(parameter_list, 29) ->
 19;
yeccgoto(parameter_list, 32) ->
 19;
yeccgoto(parameter_list, 33) ->
 19;
yeccgoto(parameter_list, 36) ->
 19;
yeccgoto(parameter_list, 38) ->
 19;
yeccgoto(parameter_list, 50) ->
 19;
yeccgoto(parameter_list, 59) ->
 19;
yeccgoto(parameter_list, 62) ->
 19;
yeccgoto(parameter_list, 65) ->
 19;
yeccgoto(parameters, 6) ->
 56;
yeccgoto(ptype, 6) ->
 20;
yeccgoto(ptype, 8) ->
 20;
yeccgoto(ptype, 13) ->
 20;
yeccgoto(ptype, 14) ->
 20;
yeccgoto(ptype, 25) ->
 20;
yeccgoto(ptype, 29) ->
 20;
yeccgoto(ptype, 32) ->
 35;
yeccgoto(ptype, 33) ->
 34;
yeccgoto(ptype, 36) ->
 20;
yeccgoto(ptype, 38) ->
 20;
yeccgoto(ptype, 50) ->
 20;
yeccgoto(ptype, 59) ->
 20;
yeccgoto(ptype, 62) ->
 20;
yeccgoto(ptypes, 6) ->
 21;
yeccgoto(ptypes, 8) ->
 21;
yeccgoto(ptypes, 13) ->
 21;
yeccgoto(ptypes, 14) ->
 21;
yeccgoto(ptypes, 25) ->
 21;
yeccgoto(ptypes, 29) ->
 21;
yeccgoto(ptypes, 36) ->
 21;
yeccgoto(ptypes, 38) ->
 21;
yeccgoto(ptypes, 50) ->
 21;
yeccgoto(ptypes, 59) ->
 21;
yeccgoto(ptypes, 62) ->
 21;
yeccgoto(spec, 2) ->
 66;
yeccgoto(start, 0) ->
 1;
yeccgoto(typedef, 3) ->
 5;
yeccgoto(utype, 6) ->
 57;
yeccgoto(utype, 8) ->
 52;
yeccgoto(utype, 13) ->
 22;
yeccgoto(utype, 14) ->
 47;
yeccgoto(utype, 25) ->
 26;
yeccgoto(utype, 29) ->
 31;
yeccgoto(utype, 36) ->
 37;
yeccgoto(utype, 38) ->
 26;
yeccgoto(utype, 50) ->
 51;
yeccgoto(utype, 59) ->
 60;
yeccgoto(utype, 62) ->
 57;
yeccgoto(utype_list, 15) ->
 40;
yeccgoto(utype_list, 41) ->
 42;
yeccgoto(utype_tuple, 6) ->
 23;
yeccgoto(utype_tuple, 8) ->
 23;
yeccgoto(utype_tuple, 13) ->
 23;
yeccgoto(utype_tuple, 14) ->
 23;
yeccgoto(utype_tuple, 25) ->
 23;
yeccgoto(utype_tuple, 29) ->
 23;
yeccgoto(utype_tuple, 32) ->
 23;
yeccgoto(utype_tuple, 33) ->
 23;
yeccgoto(utype_tuple, 36) ->
 23;
yeccgoto(utype_tuple, 38) ->
 23;
yeccgoto(utype_tuple, 50) ->
 23;
yeccgoto(utype_tuple, 59) ->
 23;
yeccgoto(utype_tuple, 62) ->
 23;
yeccgoto(utypes, 25) ->
 27;
yeccgoto(utypes, 38) ->
 44;
yeccgoto(__Symbol, __State) ->
 exit({__Symbol, __State, missing_in_goto_table}).


