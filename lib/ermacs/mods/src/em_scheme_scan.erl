%% THIS IS A PRE-RELEASE OF LEEX - RELEASED ONLY BECAUSE MANY PEOPLE
%% WANTED IT - THE OFFICIAL RELEASE WILL PROVIDE A DIFFERENT INCOMPATIBLE
%% AND BETTER INTERFACE - BE WARNED
%% PLEASE REPORT ALL BUGS TO THE AUTHOR.

-module('em_scheme_scan').

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% luke
-export([yystate/0, yystate/6, yyaction/4]).

%% User code. This is placed here to allow extra attributes.

format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%%    {ok,Tokens,Line} | {error,ErrorInfo,Line}.

string([], L, [], Ts) ->			%No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
	{A,Alen,Ics1,L1} ->			%Accepting end state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{A,Alen,Ics1,L1,S1} ->		%After an accepting state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{reject,Alen,Tlen,Ics1,L1,S1} ->
	    {error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
	{A,Alen,Tlen,Ics1,L1,S1} ->
	    string_cont(yysuf(Tcs, Alen), L1, yyaction(A, Alen, Tcs, L1), Ts)
    end.

%% string_cont(RestChars, Line, Token, Tokens)
%%  Test for and remove the end token wrapper.

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(Rest, Line, {error,S}, Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(Chars, Line, yystate(), Chars, 0, reject, 0);
token({Line,State,Tcs,Tlen,Action,Alen}, Chars, _) ->
    token(Chars, Line, State, Tcs ++ Chars, Tlen, Action, Alen).

%% token(InChars, Line, State, TokenChars, TokenLen, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

token(Ics0, L0, S0, Tcs, Tlen0, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{L1,S1,Tcs,Alen1,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{L1,S1,Tcs,Tlen1,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,{eof,L1},[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    {done,{error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},L1},Ics1};
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    token_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1))
    end.

%% tokens_cont(RestChars, Line, Token)
%%  Test if we have detected the end token, if so return done else continue.

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, skip_token) ->
    token(Rest, Line, yystate(), Rest, 0, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(Chars, Line, yystate(), Chars, 0, [], reject, 0);
tokens({tokens,Line,State,Tcs,Tlen,Ts,Action,Alen}, Chars, _) ->
    tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Ts, Action, Alen);
tokens({skip_tokens,Line,State,Tcs,Tlen,Error,Action,Alen}, Chars, _) ->
    skip_tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Error, Action, Alen).

%% tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(Ics0, L0, S0, Tcs, Tlen0, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{tokens,L1,S1,Tcs,Alen1,Ts,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{tokens,L1,S1,Tcs,Tlen1,Ts,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,if Ts == [] -> {eof,L1};
		     true -> {ok,yyrev(Ts),L1} end,[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1+1), L1,
			{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}});
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    tokens_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Ts)
    end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%%  Test if we have detected the end token, if so return done else continue.

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%% token_skip(InChars, Line, Error) -> {done,ReturnVal,RestChars}.
%%  Skip tokens until an end token, junk everything and return the error.

%%skip_tokens(Ics, Line, Error) -> {done,{error,Error,Line},Ics}.

skip_tokens(Ics, Line, Error) ->
    skip_tokens(Ics, Line, yystate(), Ics, 0, Error, reject, 0).

%% skip_tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(Ics0, L0, S0, Tcs, Tlen0, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Alen1,Error,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Tlen1,Error,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,{error,Error,L1},[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1), L1, Error);
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Error)
    end.

%% skip_cont(RestChars, Line, Token, Error)
%%  Test if we have detected the end token, if so return done else continue.

skip_cont(Rest, Line, {token,T}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, {end_token,T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {error,S}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0).

yyrev(L) -> yyrev(L, []).

yyrev([H|T], Acc) -> yyrev(T, [H|Acc]);
yyrev([], Acc) -> Acc.

yypre([H|T], N) when N > 0 -> [H|yypre(T, N-1)];
yypre(L, N) -> [].

yysuf([H|T], N) when N > 0 -> yysuf(T, N-1);
yysuf(L, 0) -> L.

yysplit(L, N) -> yysplit(L, N, []).
yysplit([H|T], N, Acc) when N > 0 -> yysplit(T, N-1, [H|Acc]);
yysplit(L, 0, Acc)                -> {lists:reverse(Acc), L}.

%% yystate() -> InitialState.
%% yystate(State, InChars, Line, Token,  ) ->
%%      {Action, AcceptLength, RestChars, Line} |         Accepting end state
%%      {Action, AcceptLength, RestChars, Line, State} |  Accepting state
%%      {Action, AcceptLength, TokLength, RestChars, Line, State} |
%%      {reject, AcceptLength, TokLength, RestChars, Line, State}.
%% Generated state transition functions.

yystate() -> 15.

yystate(18, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 3, Tlen);
yystate(18, Ics, Line, Tlen, Action, Alen) ->
    {3,Tlen,Ics,Line,18};
yystate(17, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(17, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(17, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,17};
yystate(16, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(16, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(16, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, 5, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $d ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $f, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(16, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,16};
yystate(15, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line+1, Tlen+1, Action, Alen);
yystate(15, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [$\r|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [$\s|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [$(|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [$)|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [$d|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [$l|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $c ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $e, C =< $k ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $m, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(15, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,15};
yystate(14, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [$a|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(9, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $` ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $b, C =< $d ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $f, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(14, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,14};
yystate(13, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, Ics, Line, Tlen, Action, Alen) ->
    {4,Tlen,Ics,Line,13};
yystate(12, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(12, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(12, [$n|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, 5, Tlen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $m ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $o, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(12, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,12};
yystate(11, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line};
yystate(10, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(10, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(10, [$m|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, 5, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $l ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $n, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(10, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,10};
yystate(9, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(9, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(9, [$t|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line, Tlen+1, 5, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $s ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $u, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,9};
yystate(8, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(8, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(8, [$i|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 5, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $h ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $j, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(8, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,8};
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {0,Tlen,Ics,Line};
yystate(6, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(6, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(6, [$b|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, 5, Tlen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $a ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $c, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(6, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,6};
yystate(5, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, Ics, Line, Tlen, Action, Alen) ->
    {2,Tlen,Ics,Line,5};
yystate(4, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(4, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(4, [$f|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, 5, Tlen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $e ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $g, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(4, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,4};
yystate(3, Ics, Line, Tlen, Action, Alen) ->
    {1,Tlen,Ics,Line};
yystate(2, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [$d|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $c ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $e, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,2};
yystate(1, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(1, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(1, [$a|Ics], Line, Tlen, Action, Alen) ->
    yystate(5, Ics, Line, Tlen+1, 5, Tlen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $` ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $b, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(1, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,1};
yystate(0, [$\v|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(0, [$\f|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(0, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, 5, Tlen);
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $\016, C =< $\037 ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $!, C =< $' ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $*, C =< $d ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $f, C =< $ÿ ->
    yystate(17, Ics, Line, Tlen+1, 5, Tlen);
yystate(0, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.


%% yyaction(Action, TokenLength, TokenChars, Line) ->
%%        {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, YYlen, YYtcs, YYline) ->
    {token,{'(',YYline}};
yyaction(1, YYlen, YYtcs, YYline) ->
    {token,{')',YYline}};
yyaction(2, YYlen, YYtcs, YYline) ->
    {token,{special,YYline}};
yyaction(3, YYlen, YYtcs, YYline) ->
    {token,{special,YYline}};
yyaction(4, YYlen, YYtcs, YYline) ->
    {token,{special,YYline}};
yyaction(5, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{atom,YYline,YYtext}};
yyaction(6, YYlen, YYtcs, YYline) ->
    skip_token;
yyaction(_, _, _, _) -> error.
