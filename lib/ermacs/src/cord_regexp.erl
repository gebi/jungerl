%%%----------------------------------------------------------------------
%%% File    : cord_regexp.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Regexp ops on cords
%%% Created : 10 Mar 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(cord_regexp).
-author('luke@bluetail.com').

-compile(export_all).
%%-export([Function/Arity, ...]).

first_match(RE, Cord) ->
    first_match(RE, Cord, 1).

first_match(RE, Cord, Pos) ->
    first_match(RE, Cord, Pos, forward).

%% Find the first match of RE in Cord starting from Pos.
first_match(RE, Cord, Pos, Dir) ->
    case first_match_c(RE, Cord, Pos, Dir) of
	{{match, Start, Len}, WalkerOut} ->
	    {match, Start, Len};
	X ->
	    X
    end.

first_match_c(RE, Cord) ->
    first_match_c(RE, Cord, 1).

first_match_c(RE, Cord, Pos) ->
    first_match_c(RE, Cord, Pos, forward).

first_match_c(RE, Cord, Pos, forward) ->
    {_, Region} = cord:split(Cord, Pos-1),
    first_match1(RE, cord:walker(Region, forward), Pos);

first_match_c(RE, Cord, Pos, backward) ->
    case regexp:parse(RE) of
	{ok, REP} ->
	    {Region,_} = cord:split(Cord, Pos),
	    RREP = reverse(REP),
	    case first_match1(RREP, cord:walker(Region, backward), Pos) of
		{{match, S, L}, C} ->
		    {{match, S, L}, C};
		X ->
		    X
	    end;
	{error, Rsn} ->
	    {error, Rsn}
    end.

continue_match(RE, {W, N}) ->
    case first_match1(RE, W, N) of
	{{match, S, L}, C} ->
	    {{match, S, L}, {C, S+L}};
	X ->
	    X
    end.

first_match1(RE, W, Pos) when list(RE) ->
    case regexp:parse(RE) of
	{ok, REP} ->
	    first_match1(optimise(REP), W, Pos);
	X = {error, Rsn} ->
	    X
    end;
first_match1(RE, Walker, Pos) ->
    case cord:walker_at_end(Walker) of
	true ->
	    nomatch;
	false ->
	    Start = case cord:walker_direction(Walker) of
			forward  -> 1;
			backward -> Pos
		    end,
	    case apply_regexp(RE, Start, Walker) of
		nomatch ->
		    %% it would be more efficient to use Walker to get
		    %% a new continuation to recurse with
		    {_, W1} = cord:walker_next(Walker),
		    first_match1(RE, W1, advance(Pos, W1));
		{match, EndPos, WalkerOut} ->
		    Len = case cord:walker_direction(Walker) of
			      forward  -> EndPos;
			      backward -> Pos - EndPos - 1
			  end,
		    {{match, Pos, Len}, WalkerOut}
	    end
    end.

apply_regexp(RE, Pos, W) ->
    {Ch, W1} = cord:walker_next(W),
    re_apply(RE, [], Ch, Pos, W1).

%% re_apply(RE, More, ThisChar, Pos, InputCont) => {match, Len} | nomatch

%% FIXME: Handling of bos (^) and eos ($) need thinking about when it
%% comes to backwards-searching. Right now, ^ will match the end when
%% going backwards.

re_apply(epsilon, More, Ch, P, C) ->
    re_apply_more(More, P, push(Ch, C));
re_apply(eos, More, done, P, C) ->
    case cord:walker_direction(C) of
	forward ->
	    re_apply_more(More, P, C);
	backward ->
	    nomatch
    end;
re_apply(eos, More, $\n, P, C) ->
    re_apply_more(More, P, push($\n, C)); % \n isn't consumed
re_apply(eos, More, done, P, C) ->
    case cord:walker_direction(C) of
	forward  -> re_apply_more(More, P, C);
	backward -> nomatch
    end;
re_apply(bos, More, done, P, C) ->
    case cord:walker_direction(C) of
	forward  -> nomatch;
	backward -> re_apply_more(More, P, C)
    end;
re_apply(eos, _, done, _, _) ->
    true;
re_apply(eos, _, done, _, _) ->
    true;
re_apply({'or', RE1, RE2}, More, Ch, P, C) ->
    re_apply_or({apply, RE1, More, Ch, P, C},
		{apply, RE2, More, Ch, P, C});
re_apply({concat, RE1, RE2}, More, Ch, P, C) ->
    re_apply(RE1, [RE2|More], Ch, P, C);
re_apply({kclosure, CE}, More, Ch, P, C) ->
    re_apply_or({apply_more, More, P, push(Ch, C)},
		{apply, CE, [{kclosure, CE}|More], Ch, P, C});
re_apply({pclosure, CE}, More, Ch, P, C) ->
    re_apply(CE, [{kclosure, CE}|More], Ch, P, C);
re_apply({optional, CE}, More, Ch, P, C) ->
    re_apply_or({apply_more, More, P, push(Ch, C)},
		{apply, CE, More, Ch, P, C});
re_apply(bos, More, Ch, 1, C) ->
    case cord:walker_direction(C) of
	forward  -> re_apply_more(More, 1, push(Ch, C));
	backward -> nomatch
    end;
re_apply(bos, More, $\n, P, C) ->
    re_apply_more(More, 1, push($\n, C)); % \n isn't consumed
re_apply({char_class, Cc}, More, Ch, P, C) ->
    case in_char_class(Ch, Cc) of
	true  -> re_apply_more(More, advance(P, C), C);
	false -> nomatch
    end;
re_apply({comp_class, Cc}, More, Ch, P, C) ->
    case in_char_class(Ch, Cc) of
	true  -> nomatch;
	false -> re_apply_more(More, advance(P, C), C)
    end;
re_apply(Ch, More, Ch, P, C) when integer(Ch) ->
    re_apply_more(More, advance(P, C), C);
re_apply(_, _, _, _, _) ->
    nomatch.

advance(Pos, Walker) ->
    case cord:walker_direction(Walker) of
	forward  -> Pos + 1;
	backward -> Pos - 1
    end.

re_apply_more([H|T], P, C) ->
    {Next, C1} = cord:walker_next(C),
    re_apply(H, T, Next, P, C1);
re_apply_more([], P, C) ->
    %% -1 isn't used in 'regexp' module, not sure what's different..
    {match, P-1, C}.

re_apply_or(A, B) ->
    case re_apply_or_operand(A) of
	nomatch ->
	    re_apply_or_operand(B);
	X ->
	    X
    end.

re_apply_or_operand({apply_more, More, P, C}) ->
    re_apply_more(More, P, C);
re_apply_or_operand({apply, RE, More, Ch, P, C}) ->
    re_apply(RE, More, Ch, P, C).


% re_apply_or({match,P1}, {match,P2}) when P1 >= P2 -> {match,P1};
% re_apply_or({match,P1}, {match,P2}) -> {match,P2};
% re_apply_or(nomatch, R2) -> R2;
% re_apply_or(R1, nomatch) -> R1.

in_char_class(C, [{C1,C2}|Cc]) when C >= C1, C =< C2 -> true;
in_char_class(C, [C|Cc]) -> true;
in_char_class(C, [_|Cc]) -> in_char_class(C, Cc);
in_char_class(C, []) -> false.

push(Ch, Cont) ->
    cord:walker_push(Ch, Cont).

%% reverse(RE)
%% Returns an equivalent regexp which takes its input in reverse order.
%% RE should come from regexp:parse/1
reverse({'or', A, B}) ->
    {'or', reverse(B), reverse(A)};
reverse({concat, A, B}) ->
    {concat, reverse(B), reverse(A)};
reverse({optional, RE}) ->
    {optional, reverse(RE)};
reverse({kclosure, RE}) ->
    {kclosure, reverse(RE)};
reverse({pclosure, RE}) ->
    {pclosure, reverse(RE)};
reverse(Other) ->
    Other.

%% Just trivial stuff for starters.
%%
%% Rearrange concat
optimise({concat, {concat, A, B}, C}) ->
    optimise({concat, A, {concat, B, C}});
%% recurse
optimise({'or', A, B}) ->
    {'or', optimise(A), optimise(B)};
optimise({kclosure, A}) ->
    {kclosure, optimise(A)};
optimise({pclosure, A}) ->
    {pclosure, optimise(A)};
optimise({optional, A}) ->
    {optional, optimise(A)};
optimise(X) ->
    X.

test_exprs() ->
    ["foo",
     "a|b",
     "(foo)|(bar)",
     "a+"].

test_inputs() ->
    ["foo", "a", "b", "c", "foo", "bar", "baz", "aaaaa"].

test() ->
    case [{Inp, RE} || RE <- test_exprs(), Inp <- test_inputs(),
		       not same(Inp, RE)] of
	[] ->
	    ok;
	%% BadOnes is where the match result was different. BUT!
	%% sometimes that is OK. At time of writing, the code seems
	%% correct but has some BadOnes.
	BadOnes ->
	    lists:foreach(fun complain/1, BadOnes)
    end.

same(Inp, REStr) ->
    forwards(Inp, REStr) == backwards(Inp, REStr).

forwards(Inp, RE) ->
    regexp:match(Inp, RE).

backwards(Inp, REStr) ->
    {ok, RE} = regexp:parse(REStr),
    BackwardsResult = regexp:match(lists:reverse(Inp), reverse(RE)).

complain({Inp, REStr}) ->
    {ok, RE} = regexp:parse(REStr),
    ForwardsResult = regexp:match(Inp, RE),
    BackwardsResult = regexp:match(lists:reverse(Inp), reverse(RE)),
    io:format("match(~p, ~p):~n  Fwds = ~p~n  Bwds = ~p~n",
	      [Inp, REStr, ForwardsResult, BackwardsResult]).

%% Compare speed of this module on a cord to the speed of the 'regexp'
%% module on a string.
bench(Regexp, Cord) ->
    List = cord:to_list(Cord),
    {CordSpeed,_} = timer:tc(?MODULE, first_match, [Regexp, Cord]),
    {ListSpeed, _} = timer:tc(regexp, first_match, [List, Regexp]),
    io:format("Cord takes ~p% time of List (~pms vs ~pms).~n",
	      [round(CordSpeed * 100 / ListSpeed),
	       round(CordSpeed/1000),
	       round(ListSpeed/1000)]).

escape([H|T]) ->
    case lists:member(H, specials()) of
        true ->
            [$\\,H|escape(T)];
        false ->
            [H|escape(T)]
    end;
escape([]) ->
    [].

specials() -> "()^$[]*+?.\\|".

