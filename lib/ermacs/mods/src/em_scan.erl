%%%----------------------------------------------------------------------
%%% File    : em_scan.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Scanner for leex-style DFAs.
%%% Created :  1 May 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(em_scan).
-author('luke@bluetail.com').

-record(scanner, {initial_state, dfa_fn, action_fn}).

%% Token record. The actual lexeme, if returned by the scanner, is
%% discarded - all that's taken from the user-written "actions" is the
%% token type. Instead we just keep the start/finish positions in a
%% cord.
%%
%% Not retaining line/column information. Hopefully that's convenient
%% to derive later.
-record(token,
	{
	  %% type chosen by scanner. There is also a special type:
	  %% 'em_skipped', when the scanner returns 'skip_token' but
	  %% we record it anyway.
	  type,
	  %% start position in cord
	  start,
	  %% finish position in cord (point *after* the last character)
	  finish,
	  %% number of characters beyond the end of the lexeme that
	  %% the scanner state machine examined, recorded to track
	  %% dependency
	  lookahead,
	  %% for book keeping, saying whether the token is known to
	  %% need re-scanning
	  dirty=false
	 }).

-compile(export_all).
%%-export([Function/Arity, ...]).

%% Bogus line number to pass to leex/yecc, because I don't want to
%% track lines at this low level. It's not safe to use an atom, but
%% hopefully -42 looks deliberate enough to prompt a grep :-)

-define(line, -42). %% Confusing number appearing where line # should be :-)

make_scanner(State0, DFA, Action) ->
    #scanner{initial_state=State0,
	     dfa_fn=DFA,
	     action_fn=Action}.

make_scheme_scanner() -> make_leex_scanner(em_scheme_scan).
make_erlang_scanner() -> make_leex_scanner(em_erlang_scan).
make_test_scanner() -> make_leex_scanner(em_test_scan).

make_leex_scanner(Mod) ->
    make_scanner(Mod:yystate(),
		 {Mod, yystate},
		 {Mod, yyaction}).

scan_annotation(Scanner, _S0, Cord, RText, Start, End) ->
    Walker = cord:walker(Cord),
    case edit_scan(Scanner, Walker) of
	{ok, Toks} ->
	    {ok, Toks};
	{error, Reason} ->
	    {ok, bad_scan}
    end.

test_test(Str) ->
    case test_string(Str) of
	{ok, Toks} ->
	    {ok, lists:map(fun(T) -> element(1, T) end,
			   Toks)};
	X ->
	    X
    end.

erlang_test(Str) ->
    case erlang_string(Str) of
	{ok, Toks} ->
	    {ok, lists:map(fun(T) -> element(1, T) end,
			   Toks)};
	X ->
	    X
    end.

scheme_test(Str) ->
    case scheme_string(Str) of
	{ok, Toks} ->
	    {ok, lists:map(fun(T) -> element(1, T) end,
			   Toks)};
	X ->
	    X
    end.

test_string(Str) ->
    Cord = list_to_binary(Str),
    Scan = make_test_scanner(),
    edit_scan(Scan, cord:walker(Cord)).

scheme_string(Str) ->
    Cord = list_to_binary(Str),
    Scan = make_scheme_scanner(),
    edit_scan(Scan, cord:walker(Cord)).

erlang_string(Str) ->
    Cord = list_to_binary(Str),
    Scan = make_erlang_scanner(),
    edit_scan(Scan, cord:walker(Cord)).

%% Returns: {ok, [#token]} | {error, Reason}
edit_scan(Scn, Walker) ->
    edit_scan(Scn, Walker, 1, []).

edit_scan(Scn, Walker, Pos) ->
    edit_scan(Scn, Walker, 1, []).

edit_scan(Scn, W0, Pos, Acc) ->
    case token(Scn, W0) of
	{done, eof} ->
	    {ok, lists:reverse(Acc)};
	{done, Result, W1} ->
	    Token = make_token(Pos, Result),
	    edit_scan(Scn, W1, Token#token.finish, [Token|Acc]);
	{error, Reason} ->
	    {error, Reason}
    end.

make_skipped_token(Pos, Len, LookAhead) ->
    #token{type=em_skipped,
	   start=Pos,
	   finish=Pos+Len,
	   lookahead=LookAhead}.

% tokens(Scn, Walker) ->
%     tokens(Scn, Walker, []).

% tokens(Scn, W0, Acc) ->
%     case token(Scn, W0) of
% 	{done, {ok, T, Acs}, W1} ->
% 	    tokens(Scn, W1, [T|Acc]);
% 	{done, {skip_token, Acs}, W1} ->
% 	    tokens(Scn, W1, Acc);
% 	{done, eof} ->
% 	    {ok, lists:reverse(Acc)};
% 	{error, Reason} ->
% 	    {error, Reason}
%     end.

token(Scn, Walker) ->
    State0 = Scn#scanner.initial_state,
    token(Scn, Walker, State0, [], 0, reject, 0).

token(Scn, W0, S0, Tcs0, Tlen0, A0, Alen0) ->
    ActionF = Scn#scanner.action_fn,
    DFA_F  = Scn#scanner.dfa_fn,
    {Ics, Tcs1, W1} = case cord:walker_next(W0) of
			     {done, WEOF} ->
				 {eof, Tcs0, WEOF};
			     {Ch, Wnext} ->
				 {[Ch], [Ch|Tcs0], Wnext}
			 end,
    case DFA_F(S0, Ics, ?line, Tlen0, A0, Alen0) of
	{A1,Alen1,[],L1} ->		% accepting end state
	    TcsFwd = lists:reverse(Tcs1),
	    token_cont(Scn,TcsFwd,W1,[],ActionF(A1,Alen1,TcsFwd,?line));
 	{A1,Alen1,[],L1,S1} ->		% after accepting state
	    token(Scn,W1,S1,Tcs1,Alen1,A1,Alen1);
	{A1,Alen1,Ics1,L1,S1} ->	% accepting state with leftover
                                        % chars.. sounds like a time
                                        % to act.
	    TcsFwd = lists:reverse(Tcs1),
	    Acs = yypre(TcsFwd, Alen1),
	    token_cont(Scn,Acs,W1,Ics1,ActionF(A1,Alen1,TcsFwd,?line));
	{A1,Alen1,Tlen1,[],L1,S1} ->	% after a non-accepting state
	    token(Scn,W1,S1,Tcs1,Tlen1,A1,Alen1);
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,eof};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    {done,
	     {error,{illegal,yypre(Tcs1, Tlen1+1)}},
	     Ics1};
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    TcsFwd = lists:reverse(Tcs1),
	    {Acs, Rest} = yysplit(TcsFwd, Alen1),
	    token_cont(Scn,Acs,W1,Rest,ActionF(A1,Alen1,TcsFwd,?line))
    end.

token_cont(Scn, Acs, W, Rest, {token, T}) ->
    {done, {ok, T, Acs, length_of(Rest)}, pushback(W, Rest)};
token_cont(Scn, Acs, W, Rest, {end_token, T}) ->
    {done, {ok, T, Acs, length_of(Rest)}, pushback(W, Rest)};
token_cont(Scn, Acs, W, Rest, SkipToken) ->
    {done, {skip_token, Acs, length_of(Rest)}, pushback(W, Rest)};
token_cont(Scn, Acs, W, Rest, {error, S}) ->
    {done, {error, {user,S}}, pushback(W, Rest)}.

adjust_pos(C, L, [$\n|T]) -> adjust_pos(0, L+1, T);
adjust_pos(C, L, [_|T])   -> adjust_pos(C+1, L, T);
adjust_pos(C, L, [])      -> {C, L}.

length_of(eof)  -> 0;
length_of(List) -> length(List).

pushback(W, eof) ->
    W;
pushback(W, Chars) ->
    lists:foldr(fun(C, Wn) -> cord:walker_push(C, Wn) end,
		W,
		Chars).

yyrev([H|T], Acc) -> yyrev(T, [H|Acc]);
yyrev([], Acc) -> Acc.

yypre([H|T], N) when N > 0 -> [H|yypre(T, N-1)];
yypre(L, N) -> [].

yysuf([H|T], N) when N > 0 -> yysuf(T, N-1);
yysuf(L, 0) -> L.

yysplit(L, N) -> yysplit(L, N, []).
yysplit([H|T], N, Acc) when N > 0 -> yysplit(T, N-1, [H|Acc]);
yysplit(L, 0, Acc)                -> {lists:reverse(Acc), L}.

%%%%%
%% Experimentation with incremental parsing (working)

str0() -> "-bar+ +foo-".
toks0() ->
    {ok, Toks} = test_string(str0()),
    Toks.

%% Hand-hacked version of Toks0 with the space character deleted and
%% its token marked as dirty. When properly re-scanned, this should be
%% the same as toks1 (basis of the test case)
dirty_toks() -> [OK_A,OK_B,OK_C,Changed0|Following0] = toks0(),
		 Changed1 = Changed0#token{dirty=true},
		 Following1 = lists:map(fun(T) ->
						Start = T#token.start,
						Finish = T#token.finish,
						T#token{start=Start-1,
							finish=Finish-1}
					end,
					Following0),
		 [OK_A,OK_B,OK_C,Changed1|Following1].

%% for: "bar++foo"
%% The space has been changed (now length 0) and is marked dirty.
str1() -> str0() -- " ".
toks1() ->
    {ok, Toks} = test_string(str1()),
    Toks.

rescan_test() ->
    Scn = make_test_scanner(),
    Dirty = mark_dirty(dirty_toks()),
    Cord = cord:new(str1()),
    Result = rescan(Scn, Cord, Dirty),
    {Result == toks1(), Result, Dirty}.

%% What should happen with this simple algorithm and test case:
%% 1. We see that a token is dirty.
%% 2. We scan backwards for dependencies (by seeing what has a look-ahead
%%    that reaches the dirty token, or another token that does, etc)
%% 3. From the earliest dependency, we start re-scanning everything
%%    until we scan a token which leaves the state of the lexer the same as
%%    it was previously (i.e. so the following token is known to be
%%    unchanged)
%%
%% So, how is the state of the lexer defined? In leex it seems to me
%% that the internal scanner state is always the same at the start of
%% each token (barring anything magical done in actions - which for
%% now I ignore). So, I think it's safe to assume that a token will be
%% unchanged if both the chars in its lexeme and the ones reached by
%% its look-ahead are the same. For "tricky stuff" it may be necessary
%% to capture the process dictionary and include (some of) it in the
%% state.
%%
%% So I will terminate when I reach a token beyond the changed text
%% which is starting in the same place.
%%
%% NB: We shouldn't need to go *too* far backwards when marking
%% dependencies in languages I can think of, because of common
%% zero-lookahead tokens like: \n , ; ( ) ...etc and most other
%% tokens will just be 1-lookahead.

re_lex(Toks0) ->
    Toks1 = mark_dirty(Toks0).

mark_dirty(Toks) ->
    mark_dirty(Toks, []).

mark_dirty([H|T], Acc) when H#token.dirty == true ->
    mark_dirty1([H|T], H#token.start, Acc);
mark_dirty([H|T], Acc) ->
    mark_dirty(T, [H|Acc]);
mark_dirty([], Acc) ->
    lists:reverse(Acc).

mark_dirty1(OK, DirtyPos, Toks0) ->
    F = fun(Tok, DP) ->
		case (Tok#token.finish-1) + Tok#token.lookahead of
		    P when P >= DP ->
			{Tok#token{dirty=true},
			 Tok#token.start};
		    _ ->
			{Tok, DP}
		end
	end,
    {Toks1, _} = lists:mapfoldl(F, DirtyPos, Toks0),
    lists:reverse(Toks1) ++ OK.

%% Rescan dirty tokens.
rescan(Scn, Cord, []) ->
    [];
rescan(Scn, Cord, [H|T]) when H#token.dirty == false ->
    [H|rescan(Scn, Cord, T)];
rescan(Scn, Cord, [H|T]) when H#token.dirty == true ->
    Pos = H#token.start,
    {_, Region} = cord:split(Cord, Pos-1),
    Walker = cord:walker(Region),
    rescan_dirty(Scn, Walker, Pos, [H|T]).

%% rescan_dirty(Toks)
%%
%% The first token is dirty. Scan until we get back to a sane state
rescan_dirty(Scn, W0, Pos, [Tok|Toks]) ->
    Start = Tok#token.start,
    io:format("(Pos = ~p) Rescanning ~p~n", [Pos, Tok]),
    case token(Scn, W0) of
	{done, eof} ->
	    [];
	{done, Result, W1} ->
	    Token = make_token(Pos, Result),
	    [Token|rescan_dirty_cont(Scn, W1, Token#token.finish, Toks)];
	{error, Reason} ->
	    %% FIXME: should make an error-token and carry on
	    {error, Reason}
    end.

rescan_dirty_cont(Scn, W, Pos, []) ->
    [];
rescan_dirty_cont(Scn, W, Pos, Rest) ->
    Next = hd(Rest),
    if
	%% This token no longer exists!
	Next#token.finish =< Pos ->
	    io:format("(Pos = ~p) Discaring token: ~p~n", [Pos, Next]),
	    rescan_dirty_cont(Scn, W, Pos, tl(Rest));
	%% We need to carry on if the token is known to be dirty, or
	%% if we aren't at the same place that it was scanned from
	%% before
	Next#token.dirty == true;
	Next#token.start /= Pos ->
	    rescan_dirty(Scn, W, Pos, Rest);
	true ->
	    Rest
    end.

make_token(Pos, {ok, T, Acs, LookAhead}) ->
    Type = element(1, T),
    Len = length(Acs),
    #token{type=Type,
	   start=Pos,
	   finish=Pos+Len,
	   lookahead=LookAhead};
make_token(Pos, {skip_token, Acs, LookAhead}) ->
    Len = length(Acs),
    #token{type=em_skipped,
	   start=Pos,
	   finish=Pos+Len,
	   lookahead=LookAhead}.

make_error_token(Pos) ->
    #token{type=em_error,
	   start=Pos,
	   finish=Pos+1,
	   lookahead=1}.

