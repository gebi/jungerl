-module(xmerl_xpath_parse).
-define(THIS_MODULE, xmerl_xpath_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-export([return_error/2]). %% to avoid warning

% token({Token, _Line}) ->
% 	Token;
% token({Token, _Line, _Value}) ->
% 	Token.

value({Token, _Line}) ->
	Token;
value({_Token, _Line, Value}) ->
	Value.

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
        {error, Descriptor, _Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?THIS_MODULE, Reason}};
        {ok, Tokens, _Endline} ->
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
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);

yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


yeccpars2(0, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('AbbreviatedStep', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(4, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('AbbreviatedStep', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(5, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  '/',
 yeccpars2(yeccgoto('AbsoluteLocationPath', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(6, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('RelativeLocationPath', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('RelativeLocationPath', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {abbrev_step,__1},
 yeccpars2(yeccgoto('Step', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(11, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {path,abs,__1},
 yeccpars2(yeccgoto('LocationPath', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(12, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('RelationalExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(13, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('OrExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(14, '!=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('AndExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(15, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('PathExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('PathExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(19, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, 'mod', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('AdditiveExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('NodeTest', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(21, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {step,{child,__1,[]}},
 yeccpars2(yeccgoto('Step', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(22, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('Expr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('UnionExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('FilterExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(25, '>=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '<=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('EqualityExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(26, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {path,rel,__1},
 yeccpars2(yeccgoto('LocationPath', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('RelativeLocationPath', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(28, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('MultiplicativeExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(29, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('UnaryExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(30, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(31, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(32, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {literal,value(__1)},
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(33, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {name,value(__1)},
 yeccpars2(yeccgoto('NameTest', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(34, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(35, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {number,value(__1)},
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {prefix_test,value(__1)},
 yeccpars2(yeccgoto('NameTest', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(37, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {variable_reference,value(__1)},
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {wildcard,value(__1)},
 yeccpars2(yeccgoto('NameTest', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(40, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {processing_instruction,value(__3)},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('NodeTest', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(43, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(44, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {node_type,value(__1)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('NodeTest', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(45, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {function_call,value(__1),[]},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('FunctionCall', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(47, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(48, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  lists:reverse(__1),
 yeccpars2(yeccgoto('<ArgumentList>', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(49, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto('<ArgumentMember>', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(50, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('Argument', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(51, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__3|__1],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<ArgumentMember>', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(53, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {function_call,value(__1),__3},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('FunctionCall', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(54, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(55, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {step,{value(__1),__3,[]}},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {step,{value(__1),__3,__4}},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(57, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  lists:reverse(__1),
 yeccpars2(yeccgoto('<PredicateList>', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto('<PredicateMember>', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(59, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(60, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto('PredicateExpr', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(61, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(62, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {pred,__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Predicate', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(63, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__2|__1],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<PredicateMember>', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(64, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {path,union,{__1,__3}},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('UnionExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(66, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(67, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(68, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {__1,'//',__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AbbreviatedRelativeLocationPath', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(69, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {refine,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelativeLocationPath', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(70, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(71, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(72, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {comp,'>=',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelationalExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(75, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'mod', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {arith,'-',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AdditiveExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(78, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  '*',
 yeccpars2(yeccgoto('MultiplyOperator', hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(79, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(82, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {arith,mod,__1,__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('MultiplicativeExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(83, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {arith,'div',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('MultiplicativeExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(84, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {arith,__2,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('MultiplicativeExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(85, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'mod', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {arith,'+',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AdditiveExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(86, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {comp,'>',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelationalExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(87, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {comp,'<=',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelationalExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(88, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {comp,'<',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelationalExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(89, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(90, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {bool,'or',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('OrExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(91, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(92, '!=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {bool,'and',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AndExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(93, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(94, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'function_name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'number', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'literal', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'var_reference', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(95, '>=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, '<=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {comp,'=',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EqualityExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(96, '>=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '<=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {comp,'!=',__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EqualityExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(97, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {step,{child,__1,__2}},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(98, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(99, 'name', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'prefix_test', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'wildcard', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'node_type', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, 'axis', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(100, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {path,filter,{__1,__2}},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('FilterExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(101, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {__1,'//',__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('PathExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(102, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {refine,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('PathExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(103, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {step,{attribute,__2,[]}},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(104, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {step,{value(__1),__2,__3}},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(105, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'//',__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('AbbreviatedAbsoluteLocationPath', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(106, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('AbsoluteLocationPath', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(107, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {negative,__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('UnaryExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(108, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(109, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 exit({parser, __Other, missing_state_in_action_table}).

yeccgoto('<ArgumentList>', 45) ->
 47;
yeccgoto('<ArgumentMember>', 45) ->
 48;
yeccgoto('<PredicateList>', 21) ->
 97;
yeccgoto('<PredicateList>', 55) ->
 56;
yeccgoto('<PredicateList>', 103) ->
 104;
yeccgoto('<PredicateMember>', 21) ->
 57;
yeccgoto('<PredicateMember>', 55) ->
 57;
yeccgoto('<PredicateMember>', 103) ->
 57;
yeccgoto('AbbreviatedAbsoluteLocationPath', 0) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 1) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 2) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 5) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 6) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 45) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 51) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 59) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 64) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 70) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 71) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 72) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 73) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 75) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 76) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 79) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 80) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 81) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 89) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 91) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 93) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 94) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 98) ->
 8;
yeccgoto('AbbreviatedAbsoluteLocationPath', 99) ->
 8;
yeccgoto('AbbreviatedRelativeLocationPath', 0) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 1) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 2) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 5) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 6) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 45) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 51) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 59) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 64) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 70) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 71) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 72) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 73) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 75) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 76) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 79) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 80) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 81) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 89) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 91) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 93) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 94) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 98) ->
 9;
yeccgoto('AbbreviatedRelativeLocationPath', 99) ->
 9;
yeccgoto('AbbreviatedStep', 0) ->
 10;
yeccgoto('AbbreviatedStep', 1) ->
 10;
yeccgoto('AbbreviatedStep', 2) ->
 10;
yeccgoto('AbbreviatedStep', 5) ->
 10;
yeccgoto('AbbreviatedStep', 6) ->
 10;
yeccgoto('AbbreviatedStep', 45) ->
 10;
yeccgoto('AbbreviatedStep', 51) ->
 10;
yeccgoto('AbbreviatedStep', 59) ->
 10;
yeccgoto('AbbreviatedStep', 64) ->
 10;
yeccgoto('AbbreviatedStep', 66) ->
 10;
yeccgoto('AbbreviatedStep', 67) ->
 10;
yeccgoto('AbbreviatedStep', 70) ->
 10;
yeccgoto('AbbreviatedStep', 71) ->
 10;
yeccgoto('AbbreviatedStep', 72) ->
 10;
yeccgoto('AbbreviatedStep', 73) ->
 10;
yeccgoto('AbbreviatedStep', 75) ->
 10;
yeccgoto('AbbreviatedStep', 76) ->
 10;
yeccgoto('AbbreviatedStep', 79) ->
 10;
yeccgoto('AbbreviatedStep', 80) ->
 10;
yeccgoto('AbbreviatedStep', 81) ->
 10;
yeccgoto('AbbreviatedStep', 89) ->
 10;
yeccgoto('AbbreviatedStep', 91) ->
 10;
yeccgoto('AbbreviatedStep', 93) ->
 10;
yeccgoto('AbbreviatedStep', 94) ->
 10;
yeccgoto('AbbreviatedStep', 98) ->
 10;
yeccgoto('AbbreviatedStep', 99) ->
 10;
yeccgoto('AbsoluteLocationPath', 0) ->
 11;
yeccgoto('AbsoluteLocationPath', 1) ->
 11;
yeccgoto('AbsoluteLocationPath', 2) ->
 11;
yeccgoto('AbsoluteLocationPath', 45) ->
 11;
yeccgoto('AbsoluteLocationPath', 51) ->
 11;
yeccgoto('AbsoluteLocationPath', 59) ->
 11;
yeccgoto('AbsoluteLocationPath', 64) ->
 11;
yeccgoto('AbsoluteLocationPath', 70) ->
 11;
yeccgoto('AbsoluteLocationPath', 71) ->
 11;
yeccgoto('AbsoluteLocationPath', 72) ->
 11;
yeccgoto('AbsoluteLocationPath', 73) ->
 11;
yeccgoto('AbsoluteLocationPath', 75) ->
 11;
yeccgoto('AbsoluteLocationPath', 76) ->
 11;
yeccgoto('AbsoluteLocationPath', 79) ->
 11;
yeccgoto('AbsoluteLocationPath', 80) ->
 11;
yeccgoto('AbsoluteLocationPath', 81) ->
 11;
yeccgoto('AbsoluteLocationPath', 89) ->
 11;
yeccgoto('AbsoluteLocationPath', 91) ->
 11;
yeccgoto('AbsoluteLocationPath', 93) ->
 11;
yeccgoto('AbsoluteLocationPath', 94) ->
 11;
yeccgoto('AdditiveExpr', 0) ->
 12;
yeccgoto('AdditiveExpr', 1) ->
 12;
yeccgoto('AdditiveExpr', 45) ->
 12;
yeccgoto('AdditiveExpr', 51) ->
 12;
yeccgoto('AdditiveExpr', 59) ->
 12;
yeccgoto('AdditiveExpr', 70) ->
 88;
yeccgoto('AdditiveExpr', 71) ->
 87;
yeccgoto('AdditiveExpr', 72) ->
 86;
yeccgoto('AdditiveExpr', 73) ->
 74;
yeccgoto('AdditiveExpr', 89) ->
 12;
yeccgoto('AdditiveExpr', 91) ->
 12;
yeccgoto('AdditiveExpr', 93) ->
 12;
yeccgoto('AdditiveExpr', 94) ->
 12;
yeccgoto('AndExpr', 0) ->
 13;
yeccgoto('AndExpr', 1) ->
 13;
yeccgoto('AndExpr', 45) ->
 13;
yeccgoto('AndExpr', 51) ->
 13;
yeccgoto('AndExpr', 59) ->
 13;
yeccgoto('AndExpr', 89) ->
 90;
yeccgoto('Argument', 45) ->
 49;
yeccgoto('Argument', 51) ->
 52;
yeccgoto('EqualityExpr', 0) ->
 14;
yeccgoto('EqualityExpr', 1) ->
 14;
yeccgoto('EqualityExpr', 45) ->
 14;
yeccgoto('EqualityExpr', 51) ->
 14;
yeccgoto('EqualityExpr', 59) ->
 14;
yeccgoto('EqualityExpr', 89) ->
 14;
yeccgoto('EqualityExpr', 91) ->
 92;
yeccgoto('Expr', 0) ->
 15;
yeccgoto('Expr', 1) ->
 108;
yeccgoto('Expr', 45) ->
 50;
yeccgoto('Expr', 51) ->
 50;
yeccgoto('Expr', 59) ->
 60;
yeccgoto('FilterExpr', 0) ->
 16;
yeccgoto('FilterExpr', 1) ->
 16;
yeccgoto('FilterExpr', 2) ->
 16;
yeccgoto('FilterExpr', 45) ->
 16;
yeccgoto('FilterExpr', 51) ->
 16;
yeccgoto('FilterExpr', 59) ->
 16;
yeccgoto('FilterExpr', 64) ->
 16;
yeccgoto('FilterExpr', 70) ->
 16;
yeccgoto('FilterExpr', 71) ->
 16;
yeccgoto('FilterExpr', 72) ->
 16;
yeccgoto('FilterExpr', 73) ->
 16;
yeccgoto('FilterExpr', 75) ->
 16;
yeccgoto('FilterExpr', 76) ->
 16;
yeccgoto('FilterExpr', 79) ->
 16;
yeccgoto('FilterExpr', 80) ->
 16;
yeccgoto('FilterExpr', 81) ->
 16;
yeccgoto('FilterExpr', 89) ->
 16;
yeccgoto('FilterExpr', 91) ->
 16;
yeccgoto('FilterExpr', 93) ->
 16;
yeccgoto('FilterExpr', 94) ->
 16;
yeccgoto('FunctionCall', 0) ->
 17;
yeccgoto('FunctionCall', 1) ->
 17;
yeccgoto('FunctionCall', 2) ->
 17;
yeccgoto('FunctionCall', 45) ->
 17;
yeccgoto('FunctionCall', 51) ->
 17;
yeccgoto('FunctionCall', 59) ->
 17;
yeccgoto('FunctionCall', 64) ->
 17;
yeccgoto('FunctionCall', 70) ->
 17;
yeccgoto('FunctionCall', 71) ->
 17;
yeccgoto('FunctionCall', 72) ->
 17;
yeccgoto('FunctionCall', 73) ->
 17;
yeccgoto('FunctionCall', 75) ->
 17;
yeccgoto('FunctionCall', 76) ->
 17;
yeccgoto('FunctionCall', 79) ->
 17;
yeccgoto('FunctionCall', 80) ->
 17;
yeccgoto('FunctionCall', 81) ->
 17;
yeccgoto('FunctionCall', 89) ->
 17;
yeccgoto('FunctionCall', 91) ->
 17;
yeccgoto('FunctionCall', 93) ->
 17;
yeccgoto('FunctionCall', 94) ->
 17;
yeccgoto('LocationPath', 0) ->
 18;
yeccgoto('LocationPath', 1) ->
 18;
yeccgoto('LocationPath', 2) ->
 18;
yeccgoto('LocationPath', 45) ->
 18;
yeccgoto('LocationPath', 51) ->
 18;
yeccgoto('LocationPath', 59) ->
 18;
yeccgoto('LocationPath', 64) ->
 18;
yeccgoto('LocationPath', 70) ->
 18;
yeccgoto('LocationPath', 71) ->
 18;
yeccgoto('LocationPath', 72) ->
 18;
yeccgoto('LocationPath', 73) ->
 18;
yeccgoto('LocationPath', 75) ->
 18;
yeccgoto('LocationPath', 76) ->
 18;
yeccgoto('LocationPath', 79) ->
 18;
yeccgoto('LocationPath', 80) ->
 18;
yeccgoto('LocationPath', 81) ->
 18;
yeccgoto('LocationPath', 89) ->
 18;
yeccgoto('LocationPath', 91) ->
 18;
yeccgoto('LocationPath', 93) ->
 18;
yeccgoto('LocationPath', 94) ->
 18;
yeccgoto('MultiplicativeExpr', 0) ->
 19;
yeccgoto('MultiplicativeExpr', 1) ->
 19;
yeccgoto('MultiplicativeExpr', 45) ->
 19;
yeccgoto('MultiplicativeExpr', 51) ->
 19;
yeccgoto('MultiplicativeExpr', 59) ->
 19;
yeccgoto('MultiplicativeExpr', 70) ->
 19;
yeccgoto('MultiplicativeExpr', 71) ->
 19;
yeccgoto('MultiplicativeExpr', 72) ->
 19;
yeccgoto('MultiplicativeExpr', 73) ->
 19;
yeccgoto('MultiplicativeExpr', 75) ->
 85;
yeccgoto('MultiplicativeExpr', 76) ->
 77;
yeccgoto('MultiplicativeExpr', 89) ->
 19;
yeccgoto('MultiplicativeExpr', 91) ->
 19;
yeccgoto('MultiplicativeExpr', 93) ->
 19;
yeccgoto('MultiplicativeExpr', 94) ->
 19;
yeccgoto('MultiplyOperator', 19) ->
 79;
yeccgoto('MultiplyOperator', 77) ->
 79;
yeccgoto('MultiplyOperator', 85) ->
 79;
yeccgoto('NameTest', 0) ->
 20;
yeccgoto('NameTest', 1) ->
 20;
yeccgoto('NameTest', 2) ->
 20;
yeccgoto('NameTest', 5) ->
 20;
yeccgoto('NameTest', 6) ->
 20;
yeccgoto('NameTest', 45) ->
 20;
yeccgoto('NameTest', 51) ->
 20;
yeccgoto('NameTest', 54) ->
 20;
yeccgoto('NameTest', 59) ->
 20;
yeccgoto('NameTest', 64) ->
 20;
yeccgoto('NameTest', 66) ->
 20;
yeccgoto('NameTest', 67) ->
 20;
yeccgoto('NameTest', 70) ->
 20;
yeccgoto('NameTest', 71) ->
 20;
yeccgoto('NameTest', 72) ->
 20;
yeccgoto('NameTest', 73) ->
 20;
yeccgoto('NameTest', 75) ->
 20;
yeccgoto('NameTest', 76) ->
 20;
yeccgoto('NameTest', 79) ->
 20;
yeccgoto('NameTest', 80) ->
 20;
yeccgoto('NameTest', 81) ->
 20;
yeccgoto('NameTest', 89) ->
 20;
yeccgoto('NameTest', 91) ->
 20;
yeccgoto('NameTest', 93) ->
 20;
yeccgoto('NameTest', 94) ->
 20;
yeccgoto('NameTest', 98) ->
 20;
yeccgoto('NameTest', 99) ->
 20;
yeccgoto('NodeTest', 0) ->
 21;
yeccgoto('NodeTest', 1) ->
 21;
yeccgoto('NodeTest', 2) ->
 21;
yeccgoto('NodeTest', 5) ->
 21;
yeccgoto('NodeTest', 6) ->
 21;
yeccgoto('NodeTest', 45) ->
 21;
yeccgoto('NodeTest', 51) ->
 21;
yeccgoto('NodeTest', 54) ->
 55;
yeccgoto('NodeTest', 59) ->
 21;
yeccgoto('NodeTest', 64) ->
 21;
yeccgoto('NodeTest', 66) ->
 21;
yeccgoto('NodeTest', 67) ->
 21;
yeccgoto('NodeTest', 70) ->
 21;
yeccgoto('NodeTest', 71) ->
 21;
yeccgoto('NodeTest', 72) ->
 21;
yeccgoto('NodeTest', 73) ->
 21;
yeccgoto('NodeTest', 75) ->
 21;
yeccgoto('NodeTest', 76) ->
 21;
yeccgoto('NodeTest', 79) ->
 21;
yeccgoto('NodeTest', 80) ->
 21;
yeccgoto('NodeTest', 81) ->
 21;
yeccgoto('NodeTest', 89) ->
 21;
yeccgoto('NodeTest', 91) ->
 21;
yeccgoto('NodeTest', 93) ->
 21;
yeccgoto('NodeTest', 94) ->
 21;
yeccgoto('NodeTest', 98) ->
 21;
yeccgoto('NodeTest', 99) ->
 21;
yeccgoto('OrExpr', 0) ->
 22;
yeccgoto('OrExpr', 1) ->
 22;
yeccgoto('OrExpr', 45) ->
 22;
yeccgoto('OrExpr', 51) ->
 22;
yeccgoto('OrExpr', 59) ->
 22;
yeccgoto('PathExpr', 0) ->
 23;
yeccgoto('PathExpr', 1) ->
 23;
yeccgoto('PathExpr', 2) ->
 23;
yeccgoto('PathExpr', 45) ->
 23;
yeccgoto('PathExpr', 51) ->
 23;
yeccgoto('PathExpr', 59) ->
 23;
yeccgoto('PathExpr', 64) ->
 65;
yeccgoto('PathExpr', 70) ->
 23;
yeccgoto('PathExpr', 71) ->
 23;
yeccgoto('PathExpr', 72) ->
 23;
yeccgoto('PathExpr', 73) ->
 23;
yeccgoto('PathExpr', 75) ->
 23;
yeccgoto('PathExpr', 76) ->
 23;
yeccgoto('PathExpr', 79) ->
 23;
yeccgoto('PathExpr', 80) ->
 23;
yeccgoto('PathExpr', 81) ->
 23;
yeccgoto('PathExpr', 89) ->
 23;
yeccgoto('PathExpr', 91) ->
 23;
yeccgoto('PathExpr', 93) ->
 23;
yeccgoto('PathExpr', 94) ->
 23;
yeccgoto('Predicate', 16) ->
 100;
yeccgoto('Predicate', 21) ->
 58;
yeccgoto('Predicate', 55) ->
 58;
yeccgoto('Predicate', 57) ->
 63;
yeccgoto('Predicate', 103) ->
 58;
yeccgoto('PredicateExpr', 59) ->
 61;
yeccgoto('PrimaryExpr', 0) ->
 24;
yeccgoto('PrimaryExpr', 1) ->
 24;
yeccgoto('PrimaryExpr', 2) ->
 24;
yeccgoto('PrimaryExpr', 45) ->
 24;
yeccgoto('PrimaryExpr', 51) ->
 24;
yeccgoto('PrimaryExpr', 59) ->
 24;
yeccgoto('PrimaryExpr', 64) ->
 24;
yeccgoto('PrimaryExpr', 70) ->
 24;
yeccgoto('PrimaryExpr', 71) ->
 24;
yeccgoto('PrimaryExpr', 72) ->
 24;
yeccgoto('PrimaryExpr', 73) ->
 24;
yeccgoto('PrimaryExpr', 75) ->
 24;
yeccgoto('PrimaryExpr', 76) ->
 24;
yeccgoto('PrimaryExpr', 79) ->
 24;
yeccgoto('PrimaryExpr', 80) ->
 24;
yeccgoto('PrimaryExpr', 81) ->
 24;
yeccgoto('PrimaryExpr', 89) ->
 24;
yeccgoto('PrimaryExpr', 91) ->
 24;
yeccgoto('PrimaryExpr', 93) ->
 24;
yeccgoto('PrimaryExpr', 94) ->
 24;
yeccgoto('RelationalExpr', 0) ->
 25;
yeccgoto('RelationalExpr', 1) ->
 25;
yeccgoto('RelationalExpr', 45) ->
 25;
yeccgoto('RelationalExpr', 51) ->
 25;
yeccgoto('RelationalExpr', 59) ->
 25;
yeccgoto('RelationalExpr', 89) ->
 25;
yeccgoto('RelationalExpr', 91) ->
 25;
yeccgoto('RelationalExpr', 93) ->
 96;
yeccgoto('RelationalExpr', 94) ->
 95;
yeccgoto('RelativeLocationPath', 0) ->
 26;
yeccgoto('RelativeLocationPath', 1) ->
 26;
yeccgoto('RelativeLocationPath', 2) ->
 26;
yeccgoto('RelativeLocationPath', 5) ->
 106;
yeccgoto('RelativeLocationPath', 6) ->
 105;
yeccgoto('RelativeLocationPath', 45) ->
 26;
yeccgoto('RelativeLocationPath', 51) ->
 26;
yeccgoto('RelativeLocationPath', 59) ->
 26;
yeccgoto('RelativeLocationPath', 64) ->
 26;
yeccgoto('RelativeLocationPath', 70) ->
 26;
yeccgoto('RelativeLocationPath', 71) ->
 26;
yeccgoto('RelativeLocationPath', 72) ->
 26;
yeccgoto('RelativeLocationPath', 73) ->
 26;
yeccgoto('RelativeLocationPath', 75) ->
 26;
yeccgoto('RelativeLocationPath', 76) ->
 26;
yeccgoto('RelativeLocationPath', 79) ->
 26;
yeccgoto('RelativeLocationPath', 80) ->
 26;
yeccgoto('RelativeLocationPath', 81) ->
 26;
yeccgoto('RelativeLocationPath', 89) ->
 26;
yeccgoto('RelativeLocationPath', 91) ->
 26;
yeccgoto('RelativeLocationPath', 93) ->
 26;
yeccgoto('RelativeLocationPath', 94) ->
 26;
yeccgoto('RelativeLocationPath', 98) ->
 102;
yeccgoto('RelativeLocationPath', 99) ->
 101;
yeccgoto('Step', 0) ->
 27;
yeccgoto('Step', 1) ->
 27;
yeccgoto('Step', 2) ->
 27;
yeccgoto('Step', 5) ->
 27;
yeccgoto('Step', 6) ->
 27;
yeccgoto('Step', 45) ->
 27;
yeccgoto('Step', 51) ->
 27;
yeccgoto('Step', 59) ->
 27;
yeccgoto('Step', 64) ->
 27;
yeccgoto('Step', 66) ->
 69;
yeccgoto('Step', 67) ->
 68;
yeccgoto('Step', 70) ->
 27;
yeccgoto('Step', 71) ->
 27;
yeccgoto('Step', 72) ->
 27;
yeccgoto('Step', 73) ->
 27;
yeccgoto('Step', 75) ->
 27;
yeccgoto('Step', 76) ->
 27;
yeccgoto('Step', 79) ->
 27;
yeccgoto('Step', 80) ->
 27;
yeccgoto('Step', 81) ->
 27;
yeccgoto('Step', 89) ->
 27;
yeccgoto('Step', 91) ->
 27;
yeccgoto('Step', 93) ->
 27;
yeccgoto('Step', 94) ->
 27;
yeccgoto('Step', 98) ->
 27;
yeccgoto('Step', 99) ->
 27;
yeccgoto('UnaryExpr', 0) ->
 28;
yeccgoto('UnaryExpr', 1) ->
 28;
yeccgoto('UnaryExpr', 2) ->
 107;
yeccgoto('UnaryExpr', 45) ->
 28;
yeccgoto('UnaryExpr', 51) ->
 28;
yeccgoto('UnaryExpr', 59) ->
 28;
yeccgoto('UnaryExpr', 70) ->
 28;
yeccgoto('UnaryExpr', 71) ->
 28;
yeccgoto('UnaryExpr', 72) ->
 28;
yeccgoto('UnaryExpr', 73) ->
 28;
yeccgoto('UnaryExpr', 75) ->
 28;
yeccgoto('UnaryExpr', 76) ->
 28;
yeccgoto('UnaryExpr', 79) ->
 84;
yeccgoto('UnaryExpr', 80) ->
 83;
yeccgoto('UnaryExpr', 81) ->
 82;
yeccgoto('UnaryExpr', 89) ->
 28;
yeccgoto('UnaryExpr', 91) ->
 28;
yeccgoto('UnaryExpr', 93) ->
 28;
yeccgoto('UnaryExpr', 94) ->
 28;
yeccgoto('UnionExpr', 0) ->
 29;
yeccgoto('UnionExpr', 1) ->
 29;
yeccgoto('UnionExpr', 2) ->
 29;
yeccgoto('UnionExpr', 45) ->
 29;
yeccgoto('UnionExpr', 51) ->
 29;
yeccgoto('UnionExpr', 59) ->
 29;
yeccgoto('UnionExpr', 70) ->
 29;
yeccgoto('UnionExpr', 71) ->
 29;
yeccgoto('UnionExpr', 72) ->
 29;
yeccgoto('UnionExpr', 73) ->
 29;
yeccgoto('UnionExpr', 75) ->
 29;
yeccgoto('UnionExpr', 76) ->
 29;
yeccgoto('UnionExpr', 79) ->
 29;
yeccgoto('UnionExpr', 80) ->
 29;
yeccgoto('UnionExpr', 81) ->
 29;
yeccgoto('UnionExpr', 89) ->
 29;
yeccgoto('UnionExpr', 91) ->
 29;
yeccgoto('UnionExpr', 93) ->
 29;
yeccgoto('UnionExpr', 94) ->
 29;
yeccgoto(__Symbol, __State) ->
 exit({__Symbol, __State, missing_in_goto_table}).


