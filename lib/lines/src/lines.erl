%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is lines-1.0.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:       lines.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Efficient array of lines (e.g. for text editor)
%%% Fixes:       : Joe Armstrong fixed bug in replace     
%%% Modules used : lists
%%% 
%%%----------------------------------------------------------------------
%%% Efficient array of lines (e.g. for text editor)
%%% allows for append, as well as insert, replace, delete in any position
%%% with reasonable access times.
%%% Rough benchmarking indicates (on a 440MHz Ultra):
%%%
%%% NoOfLines	Append (uSec)	Read (uSec)	Delete (uSec)
%%% 100		9		7		7
%%% 1,000	14		10		11
%%% 10,000	22		13		15
%%% 100,000	30		16		18
%%%
%%% Comment on the benchmark: The times for Append and Delete are mean
%%% times for "growing file" and "shrinking file", that is, starting from
%%% an empty array and inserting 100,000 lines took ca 3 seconds; deleting
%%% them took ca 1.8 seconds. The Read test involved accessing all lines
%%% in the full array and calculating the mean time.
%%%
%%% The array doesn't care what goes into each position. In other words,
%%% it can be used for any datatype -- not just lines of text.
%%%----------------------------------------------------------------------

-module(lines).
-vsn('1.0').
-date('00-03-13').
-author('ulf.wiger@ericsson.com').

-export([new/0, new/1, new/2,
	 count/1,
	 nth/2,
	 append/2,
	 replace/3,
	 insert/3,
	 insert_after/3,
	 delete/2,
	 convert_to_list/1,
	 convert_from_list/1]).

-define(BREAK, 10).	% how many lines to store in each leaf

-define(dbg(Fmt, Args), ok=io:format("~p: " ++ Fmt, [?LINE|Args])).
%% new() -> line_array()
%%
%% Creates a new line array.
%%
new() ->
    {0, []}.


%% see make_array(N, []).
%%
new(N) ->
    new(N, []).


%% make an array of N lines. Each line will be initialized to DefaultLine.
%% This is _much_ faster and more space efficient than growing an
%% array line by line.
%%
new(N, DefaultLine) when N =< ?BREAK ->
    {N, lists:duplicate(N, DefaultLine)};
new(N, DefaultLine) when N =< 2*?BREAK ->
    Left = {?BREAK, lists:duplicate(?BREAK,DefaultLine)},
    RightN = N - ?BREAK,
    Right = {RightN, lists:duplicate(RightN, DefaultLine)},
    {N, {Left, Right}};
new(N, DefaultLine) ->
    {FullBuckets, RestLeaf, Height} = size_array(N),
    FullBucket = 
	case FullBuckets > 0 of
	    true ->
		{?BREAK, lists:duplicate(?BREAK, DefaultLine)};
	    false ->
		[]
	end,
    RestBucket = {RestLeaf, lists:duplicate(RestLeaf, DefaultLine)},
    {Tree,_,_} = grow_tree(1, Height, FullBuckets, FullBucket, RestBucket),
    Tree.
	    
grow_tree(H, Height, TotB, FullB, RestB) when H < Height ->
    NextH = H+1,
    {{LSz,_}=Left, TotB1, RestB1} =
	grow_tree(NextH, Height, TotB, FullB, RestB),
    {{RSz,_}=Right, TotB2, RestB2} =
	grow_tree(NextH, Height, TotB1, FullB, RestB1),
    {{LSz+RSz, {Left,Right}},TotB2,RestB2};
grow_tree(H, H, 0, _, {0,_}=Empty=_RestB) ->
    {Empty,0,Empty};
grow_tree(H,H,0,FullB,RestB) ->
    {RestB,0,{0,[]}};
grow_tree(H,H,1,FullB,{RestSz,_}=RestB) ->
    {{?BREAK+RestSz, {FullB, RestB}}, 0, {0,[]}};
grow_tree(H,H,TotB,FullB,RestB) when TotB > 1 ->
    {{2*?BREAK, {FullB,FullB}},TotB-2,RestB}.
    

size_array(N) ->
    FullBuckets = N div ?BREAK,
    case N rem ?BREAK of
	0 ->
	    {BMax, Height} = calc_sz(FullBuckets),
	    {FullBuckets, 0, Height};
	RestLeaf ->
	    {BMax, Height} = calc_sz(FullBuckets+1),
	    {FullBuckets, RestLeaf, Height}
    end.

calc_sz(Buckets) ->
    calc_sz(Buckets, Initial=2, Height=1).

calc_sz(N, Sz, Height) when N =< Sz ->
    {Sz, Height};
calc_sz(N, Sz, Height) ->
    calc_sz(N, Sz + (2 bsl Height), Height+1).


%% line_count(line_array()) -> integer()
%%
%% Returns the number of lines stored in the array
%%
count({N, _}) -> 
    N.

%% nth(LineNo : integer(), Array : line_array()) -> line()
%%
%% Returns the line in position LineNo
%%
nth(L, _) when L < 1 ->
    exit({out_of_range, L});
nth(L, {LMax, _}) when L > LMax ->
    exit({out_of_range, L});
nth(L, {LMax, List}) when list(List) ->
    lists:nth(L, List);
nth(L, {LMax, {Left = {LL, _}, Right}}) when L > LL ->
    nth(L-LL, Right);
nth(L, {_, {Left, _}}) ->
    nth(L, Left).

%% append(Line : line(), Array : line_array()) -> line_array().
%%
%% Appends Line to the end of Array.
%% e.g. append(x, [1,2,3,4]) -> [1,2,3,4,x].
%% Returns the modified array.
%%
append(Line, {L, List}) when list(List), L < ?BREAK ->
    {L+1, List ++ [Line]};
append(Line, {L, List}) when list(List) ->
    {L+1, {{L, List}, {1, [Line]}}};
append(Line, {L, {Left = {LL1, L1}, Right}}) ->
    NewRight = append(Line, Right),
    balance_left(L+1, Left, NewRight).

%% replace(LineNo : integer(), Array : line_array(), NewLine : line()) ->
%%	line_array().
%%
%% Replaces the line in position LineNo with NewLine.
%% e.g. replace(3, [1,2,3,4], x) -> [1,2,x,4].
%% Returns the modified array.
%%
replace(Lno, _, _) when Lno < 1 ->
    exit({out_of_range, Lno});
replace(Lno, {L, _}, NewLine) when Lno > L ->
    exit({out_of_range, Lno});
replace(Lno, {L, List}, NewLine) when list(List) ->
    {L, replace_nth(Lno, List, NewLine)};
replace(Lno, {L, {Left={LL1, L1}, Right={LL2, L2}}}, NewLine) when Lno > LL1 ->
    NewRight = replace(Lno-LL1, Right, NewLine),
    {L, {Left, NewRight}};
replace(Lno, {L, {Left={LL1,L1}, Right={LL2,L2}}}, NewLine) ->
    NewLeft = replace(Lno, Left, NewLine),
    {L, {NewLeft, Right}}.

%% insert(LineNo : integer(), Array : line_array(), NewLine) -> line_array().
%%
%% Inserts NewLine *before* the line in position LineNo.
%% e.g. insert(3, [1,2,3,4], x) -> [1,2,x,3,4].
%% Returns the modified array.
%%
insert(Lno, _, _) when Lno < 1 ->
    exit({out_of_range, Lno});
insert(Lno, {L, _}, NewLine) when Lno > L ->
    exit({out_of_range, Lno});
insert(Lno, {L, List}, NewLine) when list(List) ->
    if L < ?BREAK ->
	    {L+1, insert_nth(Lno, List, NewLine)};
       true ->
	    NewList = insert_nth(Lno, List, NewLine),
	    {L1, L2} = split_at(?BREAK, NewList),
	    NewL = L+1,
	    {NewL, {{?BREAK, L1}, {NewL-?BREAK, L2}}}
    end;
insert(Lno, {L, {Left={LL,_}, Right}}, NewLine) when Lno > LL ->
    NewRight = insert(Lno-LL, Right, NewLine),
    balance_left(L+1, Left, NewRight);
insert(Lno, {L, {Left, Right}}, NewLine) ->
    NewLeft = insert(Lno, Left, NewLine),
    balance_right(L+1, NewLeft, Right).

%% insert_after(LineNo : integer(), Array : line_array(), NewLine) -> 
%%	line_array().
%%
%% Inserts NewLine *after* the line in position LineNo.
%% e.g. insert(3, [1,2,3,4], x) -> [1,2,3,x,4].
%% Returns the modified array.
%%
insert_after(Lno, _, _) when Lno < 0 ->
    exit({out_of_range, Lno});
insert_after(Lno, {L, _}, NewLine) when Lno > L ->
    exit({out_of_range, Lno});
insert_after(L, {L,_}=Array, NewLine) ->
    append(NewLine, Array);
insert_after(Lno, {L, List}, NewLine) when list(List) ->
    if L < ?BREAK ->
	    {L+1, insert_after_nth(Lno, List, NewLine)};
       true ->
	    NewList = insert_after_nth(Lno, List, NewLine),
	    {L1, L2} = split_at(?BREAK, NewList),
	    NewL = L+1,
	    {NewL, {{?BREAK, L1}, {NewL-?BREAK, L2}}}
    end;
insert_after(Lno, {L, {Left={LL,_}, Right}}, NewLine) when Lno > LL ->
    NewRight = insert_after(Lno-LL, Right, NewLine),
    balance_left(L+1, Left, NewRight);
insert_after(Lno, {L, {Left, Right}}, NewLine) ->
    NewLeft = insert_after(Lno, Left, NewLine),
    balance_right(L+1, NewLeft, Right).


%% delete(LineNo : integer(), Array : line_array()) -> line_array().
%%
%% Deletes the line in position LineNo.
%% e.g. delete(3, [1,2,3,4]) -> [1,2,4].
%% Returns the modified array.
%%
delete(Lno, _) when Lno < 1 ->
    exit({out_of_range, Lno});
delete(Lno, {N_Tot, _}) when Lno > N_Tot ->
    exit({out_of_range, Lno});
delete(Lno, {N, List}) when list(List) ->
    {N-1, delete_nth(Lno, List)};
delete(Lno, {N, {Left = {N_Left, _}, Right}}) when Lno > N_Left ->
    case delete(Lno-N_Left, Right) of
	{0, _} ->
	    case N-1 of N_Left -> ok end,	% Assert
	    Left;
	NewRight ->
	    balance_right(N-1, Left, NewRight)
    end;
delete(Lno, {N, {Left, Right = {N_Right,_}}}) ->
    case delete(Lno, Left) of
	{0, _} ->
	    case N-1 of N_Right -> ok end,	% Assert
	    Right;
	NewLeft ->
	    balance_left(N-1, NewLeft, Right)
    end.

convert_to_list({_, List}) when list(List) ->
    List;
convert_to_list({L, {Left, Right}}) ->
    convert_to_list(Left) ++ convert_to_list(Right).

convert_from_list(L) when list(L) ->
    lists:foldl(fun(Ln, Lsx) ->
			append(Ln, Lsx)
		end, new(), L).

%%% ===========================================================
%%% internal functions
%%% ===========================================================

replace_nth(1, [H|T], X) ->
    [X|T];
replace_nth(N, [H|T], X) ->
    [H|replace_nth(N-1, T, X)].

insert_nth(1, L, X) ->
    [X|L];
insert_nth(N, [H|T], X) ->
    [H|insert_nth(N-1, T, X)].

insert_after_nth(1, [H|T], X) ->
    [H,X|T];
insert_after_nth(N, [H|T], X) ->
    [H|insert_after_nth(N-1, T, X)].

delete_nth(1, [H|T]) ->
    T;
delete_nth(N, [H|T]) ->
    [H|delete_nth(N-1, T)].

%% split_at(Pos, List) -> {List1, List2}
%%   split List into two after position Pos (List1 includes List[Pos])
%%
split_at(Pos, L) ->
    split_at(Pos, L, []).

split_at(0, L, Acc) ->
    {lists:reverse(Acc), L};
split_at(Pos, [H|T], Acc) ->
    split_at(Pos-1, T, [H|Acc]).


%% Balancing functions
%% Since we know whether we inserted/deleted in the right or left subtree,
%% we have explicit balancing functions for each case.
%% We rebalance if the number of elements in one sub-subtree exceeds the
%% sum of elements in the others.

balance_left(N_Tot, 
	     Left = {N_Left, _}, 
	     Right = {N_Right, {RLeft = {N_RLeft, _}, 
				RRight = {N_RRight, _}}})  ->
    NewN_Left = N_Left + N_RLeft,
    if N_RRight > NewN_Left ->
	    NewLeft = {NewN_Left, {Left, RLeft}},
	    NewRight = RRight,
	    {N_Tot, {NewLeft, NewRight}};
       true ->
	    {N_Tot, {Left, Right}}
    end;
balance_left(N_Tot, Left, Right) ->
    {N_Tot, {Left, Right}}.

balance_right(N_Tot, 
	     Left = {N_Left, {LLeft = {N_LLeft, _},
			      LRight = {N_LRight, _}}}, 
	     Right = {N_Right, _})  ->
    NewN_Right = N_Right + N_LRight,
    if N_LLeft > NewN_Right ->
	    NewLeft = LLeft,
	    NewRight = {NewN_Right, {LRight, Right}},
	    {N_Tot, {NewLeft, NewRight}};
       true ->
	    {N_Tot, {Left, Right}}
    end;
balance_right(N_Tot, Left, Right) ->
    {N_Tot, {Left, Right}}.


