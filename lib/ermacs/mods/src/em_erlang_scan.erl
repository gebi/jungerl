%% THIS IS A PRE-RELEASE OF LEEX - RELEASED ONLY BECAUSE MANY PEOPLE
%% WANTED IT - THE OFFICIAL RELEASE WILL PROVIDE A DIFFERENT INCOMPATIBLE
%% AND BETTER INTERFACE - BE WARNED
%% PLEASE REPORT ALL BUGS TO THE AUTHOR.

-module('em_erlang_scan').

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% luke
-export([yystate/0, yystate/6, yyaction/4]).

%% User code. This is placed here to allow extra attributes.

-author('rv@cslab.ericsson.se').
-copyright('Copyright (c) 1996 Ericsson Telecommunications AB').

-export([reserved_word/1]).

reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('catch') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('query') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word(_) -> false.

base(L, Cs) ->
    H = string:chr(Cs, $#),
    case list_to_integer(string:substr(Cs, 1, H-1)) of
        B when B > 16 -> {error,"illegal base"};
        B ->
            case base(string:substr(Cs, H+1), B, 0) of
                error -> {error,"illegal based number"};
                N -> {token,{integer,L,N}}
            end
    end.

base([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $A, C =< $F, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) -> error;
base([], Base, N) -> N.

cc_convert([$$,$\\|Cs]) ->
    hd(string_escape(Cs));
cc_convert([$$,C]) -> C.

string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= 0, C =< $  ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n;                         %\n = LF
escape_char($r) -> $\r;                         %\r = CR
escape_char($t) -> $\t;                         %\t = TAB
escape_char($v) -> $\v;                         %\v = VT
escape_char($b) -> $\b;                         %\b = BS
escape_char($f) -> $\f;                         %\f = FF
escape_char($e) -> $\e;                         %\e = ESC
escape_char($s) -> $ ;                          %\s = SPC
escape_char($d) -> $\d;                         %\d = DEL
escape_char(C) -> C.


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

yystate() -> 49.

yystate(52, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(52, Ics, Line, Tlen+1, 21, Tlen);
yystate(52, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $ÿ ->
    yystate(52, Ics, Line, Tlen+1, 21, Tlen);
yystate(52, Ics, Line, Tlen, Action, Alen) ->
    {21,Tlen,Ics,Line,52};
yystate(51, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line};
yystate(50, Ics, Line, Tlen, Action, Alen) ->
    {20,Tlen,Ics,Line};
yystate(49, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(45, Ics, Line+1, Tlen+1, Action, Alen);
yystate(49, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$#|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$%|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$,|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(46, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(48, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$;|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$<|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(35, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$?|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$[|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $(, C =< $* ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(40, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(43, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= ${, C =< $} ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(49, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,49};
yystate(48, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 19, Tlen);
yystate(48, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line,48};
yystate(47, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, 3, Tlen);
yystate(47, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(47, Ics, Line, Tlen+1, 3, Tlen);
yystate(47, [C|Ics], Line, Tlen, Action, Alen) when C >= $@, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 3, Tlen);
yystate(47, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(47, Ics, Line, Tlen+1, 3, Tlen);
yystate(47, Ics, Line, Tlen, Action, Alen) ->
    {3,Tlen,Ics,Line,47};
yystate(46, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(50, Ics, Line+1, Tlen+1, 19, Tlen);
yystate(46, [$%|Ics], Line, Tlen, Action, Alen) ->
    yystate(52, Ics, Line, Tlen+1, 19, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(50, Ics, Line, Tlen+1, 19, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(50, Ics, Line, Tlen+1, 19, Tlen);
yystate(46, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line,46};
yystate(45, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(45, Ics, Line+1, Tlen+1, 22, Tlen);
yystate(45, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(45, Ics, Line, Tlen+1, 22, Tlen);
yystate(45, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(45, Ics, Line, Tlen+1, 22, Tlen);
yystate(45, Ics, Line, Tlen, Action, Alen) ->
    {22,Tlen,Ics,Line,45};
yystate(44, Ics, Line, Tlen, Action, Alen) ->
    {13,Tlen,Ics,Line};
yystate(43, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 4, Tlen);
yystate(43, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(43, Ics, Line, Tlen+1, 4, Tlen);
yystate(43, [C|Ics], Line, Tlen, Action, Alen) when C >= $@, C =< $Z ->
    yystate(43, Ics, Line, Tlen+1, 4, Tlen);
yystate(43, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(43, Ics, Line, Tlen+1, 4, Tlen);
yystate(43, Ics, Line, Tlen, Action, Alen) ->
    {4,Tlen,Ics,Line,43};
yystate(42, Ics, Line, Tlen, Action, Alen) ->
    {8,Tlen,Ics,Line};
yystate(41, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line+1, Tlen+1, Action, Alen);
yystate(41, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, Action, Alen);
yystate(41, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(41, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(41, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(41, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $[ ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(41, [C|Ics], Line, Tlen, Action, Alen) when C >= $], C =< $ÿ ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(41, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,41};
yystate(40, [$#|Ics], Line, Tlen, Action, Alen) ->
    yystate(36, Ics, Line, Tlen+1, 2, Tlen);
yystate(40, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, 2, Tlen);
yystate(40, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(40, Ics, Line, Tlen+1, 2, Tlen);
yystate(40, Ics, Line, Tlen, Action, Alen) ->
    {2,Tlen,Ics,Line,40};
yystate(39, Ics, Line, Tlen, Action, Alen) ->
    {14,Tlen,Ics,Line};
yystate(38, Ics, Line, Tlen, Action, Alen) ->
    {18,Tlen,Ics,Line};
yystate(37, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line};
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(32, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $F ->
    yystate(32, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $f ->
    yystate(32, Ics, Line, Tlen+1, Action, Alen);
yystate(36, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,36};
yystate(35, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(39, Ics, Line, Tlen+1, 19, Tlen);
yystate(35, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line,35};
yystate(34, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, 19, Tlen);
yystate(34, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(42, Ics, Line, Tlen+1, 19, Tlen);
yystate(34, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line,34};
yystate(33, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line+1, Tlen+1, Action, Alen);
yystate(33, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [$^|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $[ ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $_, C =< $ÿ ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(33, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,33};
yystate(32, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(32, Ics, Line, Tlen+1, 1, Tlen);
yystate(32, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $F ->
    yystate(32, Ics, Line, Tlen+1, 1, Tlen);
yystate(32, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $f ->
    yystate(32, Ics, Line, Tlen+1, 1, Tlen);
yystate(32, Ics, Line, Tlen, Action, Alen) ->
    {1,Tlen,Ics,Line,32};
yystate(31, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line};
yystate(30, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line};
yystate(29, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line+1, Tlen+1, 5, Tlen);
yystate(29, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, 5, Tlen);
yystate(29, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(33, Ics, Line, Tlen+1, 5, Tlen);
yystate(29, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(41, Ics, Line, Tlen+1, 5, Tlen);
yystate(29, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(41, Ics, Line, Tlen+1, 5, Tlen);
yystate(29, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $[ ->
    yystate(41, Ics, Line, Tlen+1, 5, Tlen);
yystate(29, [C|Ics], Line, Tlen, Action, Alen) when C >= $], C =< $ÿ ->
    yystate(41, Ics, Line, Tlen+1, 5, Tlen);
yystate(29, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,29};
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(28, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,28};
yystate(27, Ics, Line, Tlen, Action, Alen) ->
    {15,Tlen,Ics,Line};
yystate(26, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(30, Ics, Line, Tlen+1, 19, Tlen);
yystate(26, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line,26};
yystate(25, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line+1, Tlen+1, Action, Alen);
yystate(25, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $[ ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $], C =< $ÿ ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(25, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,25};
yystate(24, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 0, Tlen);
yystate(24, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 0, Tlen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(24, Ics, Line, Tlen+1, 0, Tlen);
yystate(24, Ics, Line, Tlen, Action, Alen) ->
    {0,Tlen,Ics,Line,24};
yystate(23, Ics, Line, Tlen, Action, Alen) ->
    {12,Tlen,Ics,Line};
yystate(22, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line+1, Tlen+1, Action, Alen);
yystate(22, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $& ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= $(, C =< $[ ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= $], C =< $ÿ ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(22, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,22};
yystate(21, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $[ ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= $], C =< $ÿ ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(21, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,21};
yystate(20, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(20, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,20};
yystate(19, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(19, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,19};
yystate(18, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line+1, Tlen+1, 6, Tlen);
yystate(18, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(6, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $& ->
    yystate(6, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $(, C =< $[ ->
    yystate(6, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $], C =< $ÿ ->
    yystate(6, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,18};
yystate(17, [$^|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, 7, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(5, Ics, Line, Tlen+1, 7, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $/ ->
    yystate(5, Ics, Line, Tlen+1, 7, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $7 ->
    yystate(13, Ics, Line, Tlen+1, 7, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $8, C =< $] ->
    yystate(5, Ics, Line, Tlen+1, 7, Tlen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $_, C =< $ÿ ->
    yystate(5, Ics, Line, Tlen+1, 7, Tlen);
yystate(17, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line,17};
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(16, Ics, Line, Tlen+1, 0, Tlen);
yystate(16, Ics, Line, Tlen, Action, Alen) ->
    {0,Tlen,Ics,Line,16};
yystate(15, Ics, Line, Tlen, Action, Alen) ->
    {10,Tlen,Ics,Line};
yystate(14, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line+1, Tlen+1, Action, Alen);
yystate(14, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [$^|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $& ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $(, C =< $[ ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $_, C =< $ÿ ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(14, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,14};
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $7 ->
    yystate(9, Ics, Line, Tlen+1, 7, Tlen);
yystate(13, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line,13};
yystate(12, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(12, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,12};
yystate(11, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(11, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,11};
yystate(10, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line};
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $7 ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,9};
yystate(8, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, 19, Tlen);
yystate(8, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line,8};
yystate(7, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, 19, Tlen);
yystate(7, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, 19, Tlen);
yystate(7, [$<|Ics], Line, Tlen, Action, Alen) ->
    yystate(27, Ics, Line, Tlen+1, 19, Tlen);
yystate(7, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(31, Ics, Line, Tlen+1, 19, Tlen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line,7};
yystate(6, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line+1, Tlen+1, Action, Alen);
yystate(6, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(6, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $& ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $(, C =< $[ ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $], C =< $ÿ ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(6, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,6};
yystate(5, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line};
yystate(4, Ics, Line, Tlen, Action, Alen) ->
    {9,Tlen,Ics,Line};
yystate(3, Ics, Line, Tlen, Action, Alen) ->
    {16,Tlen,Ics,Line};
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(2, Ics, Line, Tlen+1, 23, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $ÿ ->
    yystate(2, Ics, Line, Tlen+1, 23, Tlen);
yystate(2, Ics, Line, Tlen, Action, Alen) ->
    {23,Tlen,Ics,Line,2};
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(5, Ics, Line, Tlen+1, 7, Tlen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $ÿ ->
    yystate(5, Ics, Line, Tlen+1, 7, Tlen);
yystate(1, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line,1};
yystate(0, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, 19, Tlen);
yystate(0, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.


%% yyaction(Action, TokenLength, TokenChars, Line) ->
%%        {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{float,YYline,list_to_float(YYtext)}};
yyaction(1, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    base(YYline,YYtext);
yyaction(2, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{integer,YYline,list_to_integer(YYtext)}};
yyaction(3, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    Atom = list_to_atom(YYtext),
    {token,case reserved_word(Atom) of
         true ->
             {Atom,YYline};
         false ->
             {atom,YYline,Atom}
     end};
yyaction(4, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{var,YYline,list_to_atom(YYtext)}};
yyaction(5, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    S = lists:sublist(YYtext,2,length(YYtext) - 2),
    {token,{string,YYline,string_gen(S)}};
yyaction(6, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    S = lists:sublist(YYtext,2,length(YYtext) - 2),
    {token,{atom,YYline,list_to_atom(string_gen(S))}};
yyaction(7, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{integer,YYline,cc_convert(YYtext)}};
yyaction(8, YYlen, YYtcs, YYline) ->
    {token,{'->',YYline}};
yyaction(9, YYlen, YYtcs, YYline) ->
    {token,{':-',YYline}};
yyaction(10, YYlen, YYtcs, YYline) ->
    {token,{'=/=',YYline}};
yyaction(11, YYlen, YYtcs, YYline) ->
    {token,{'==',YYline}};
yyaction(12, YYlen, YYtcs, YYline) ->
    {token,{'=:=',YYline}};
yyaction(13, YYlen, YYtcs, YYline) ->
    {token,{'/=',YYline}};
yyaction(14, YYlen, YYtcs, YYline) ->
    {token,{'>=',YYline}};
yyaction(15, YYlen, YYtcs, YYline) ->
    {token,{'=<',YYline}};
yyaction(16, YYlen, YYtcs, YYline) ->
    {token,{'<=',YYline}};
yyaction(17, YYlen, YYtcs, YYline) ->
    {token,{'++',YYline}};
yyaction(18, YYlen, YYtcs, YYline) ->
    {token,{'--',YYline}};
yyaction(19, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{list_to_atom(YYtext),YYline}};
yyaction(20, YYlen, YYtcs, YYline) ->
    {end_token,{dot,YYline}};
yyaction(21, YYlen, YYtcs, YYline) ->
    {end_token,{dot,YYline}};
yyaction(22, YYlen, YYtcs, YYline) -> skip_token;
yyaction(23, YYlen, YYtcs, YYline) ->
    skip_token;
yyaction(_, _, _, _) -> error.
