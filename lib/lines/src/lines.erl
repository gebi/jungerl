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
%%% File:   lines.erl
%%% @author Ulf Wiger,  <ulf.wiger@ericsson.com>
%%% @end
%%%----------------------------------------------------------------------

%% @doc Efficient array of lines
%%
%% <p>This module implements an efficient array of lines (e.g. for text
%% editor.)
%% It allows for append, as well as insert, replace, delete in any position
%% with reasonable access times.</p>
%% <p>Rough benchmarking indicates (on a 440MHz Ultra):</p>
%%
%% <table>
%% <tr>
%%   <td><b>NoOfLines</b></td>
%%   <td><b>Append (uSec)</b></td>
%%   <td><b>Read (uSec)</b></td>
%%   <td><b>Delete (uSec)</b></td>
%% </tr>
%% <tr>
%% <td>100</td>	    <td>9</td>	<td>7</td>   <td>7</td>
%% </tr><tr>
%% <td>1,000</td>   <td>14</td>	<td>10</td>  <td>11</td>
%% </tr><tr>
%% <td>10,000</td>  <td>22</td>	<td>13</td>  <td>15</td>
%% </tr><tr>
%% <td>100,000</td> <td>30</td>	<td>16</td>  <td>18</td>
%% </tr>
%% </table>
%%
%% <p>Comment on the benchmark: The times for Append and Delete are mean
%% times for "growing file" and "shrinking file", that is, starting from
%% an empty array and inserting 100,000 lines took ca 3 seconds; deleting
%% them took ca 1.8 seconds. The Read test involved accessing all lines
%% in the full array and calculating the mean time.</p>
%% 
%% <p>There is also a function, new/[1,2] to create an array of a given
%% size with default content. This function is <i>much</i> more efficient
%% than growing the array from scratch. For example, creating an array
%% of 100,000 elements using new(100000, []) took ca 12 ms on the same Ultra.
%% One may compare this to the built-in function erlang:make_tuple/2, with 
%% which the corresponding operation (erlang:make_tuple(100000,[]) took
%% ca 5 ms. Of course, appending an element to a tuple of size 100,000
%% then takes ca 6 ms, whereas append on the lines array takes 30 usec.</p>
%%
%% <p>The array doesn't care what goes into each position. In other words,
%% it can be used for any datatype -- not just lines of text.</p>
%% @end
%%
%% @type line_array() = tuple(). A 10-ary balanced tree.
%% @type line() = term(). Typically a string, but could be anything really.

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
-define(FULL_BUCKET, L1,L2,L3,L4,L5,L6,L7,L8,L9,L10).  % ?BREAK items

-define(dbg(Fmt, Args), ok=io:format("~p: " ++ Fmt, [?LINE|Args])).


%% @spec new() -> line_array()
%%
%% @doc Creates a new line array.
%%
new() ->
    {0, []}.


%% @equiv new(N, [])
%%
new(N) ->
    new(N, []).


%% @spec new(N::integer(), DefaultLine::term()) -> line_array()
%%
%% @doc Make an array of N lines. Each line will be initialized to DefaultLine.
%% <p>This is _much_ faster and more space efficient than growing an
%% array line by line.</p>
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
grow_tree(H,H,0,_FullB,RestB) ->
    {RestB,0,{0,[]}};
grow_tree(H,H,1,FullB,{RestSz,_}=RestB) ->
    {{?BREAK+RestSz, {FullB, RestB}}, 0, {0,[]}};
grow_tree(H,H,TotB,FullB,RestB) when TotB > 1 ->
    {{2*?BREAK, {FullB,FullB}},TotB-2,RestB}.
    

size_array(N) ->
    FullBuckets = N div ?BREAK,
    case N rem ?BREAK of
	0 ->
	    {_BMax, Height} = calc_sz(FullBuckets),
	    {FullBuckets, 0, Height};
	RestLeaf ->
	    {_BMax, Height} = calc_sz(FullBuckets+1),
	    {FullBuckets, RestLeaf, Height}
    end.

calc_sz(Buckets) ->
    calc_sz(Buckets, _Initial=2, _Height=1).

calc_sz(N, Sz, Height) when N =< Sz ->
    {Sz, Height};
calc_sz(N, Sz, Height) ->
    calc_sz(N, Sz + (2 bsl Height), Height+1).


%% @spec count(Array::line_array()) -> integer()
%%
%% @doc Returns the number of lines stored in the array
%%
count({N, _}) ->
    N.

%% @spec nth(LineNo::integer(), Array::line_array()) -> line()
%%
%% @doc Returns the line in position LineNo
%%
nth(L, _) when L < 1 ->
    exit({out_of_range, L});
nth(L, {LMax, _}) when L > LMax ->
    exit({out_of_range, L});
nth(L, {_LMax, List}) when list(List) ->
    lists:nth(L, List);
nth(L, {_LMax, {_Left = {LL, _}, Right}}) when L > LL ->
    nth(L-LL, Right);
nth(L, {_, {Left, _}}) ->
    nth(L, Left).

%% @spec append(Line::line(), Array::line_array()) -> line_array()
%%
%% @doc Appends Line to the end of Array.
%% <p>e.g. <code>append(x, [1,2,3,4]) => [1,2,3,4,x].</code></p>
%% <p>Returns the modified array.</p>
%% @end
append(Line, {L, List}) when list(List), L < ?BREAK ->
    {L+1, List ++ [Line]};
append(Line, {L, List}) when list(List) ->
    {L+1, {{L, List}, {1, [Line]}}};
append(Line, {L, {Left = {_, _}, Right}}) ->
    NewRight = append(Line, Right),
    balance_left(L+1, Left, NewRight).


%% @spec replace(LineNo::integer(), Array::line_array(), NewLine::line()) ->
%%	line_array()
%%
%% @doc Replaces the line in position LineNo with NewLine.
%% <p>e.g. <code>replace(3, [1,2,3,4], x) => [1,2,x,4].</code></p>
%% <p>Returns the modified array.</p>
%% @end
%%
replace(Lno, _, _) when Lno < 1 ->
    exit({out_of_range, Lno});
replace(Lno, {L, _}, _NewLine) when Lno > L ->
    exit({out_of_range, Lno});
replace(Lno, {L, List}, NewLine) when list(List) ->
    {L, replace_nth(Lno, List, NewLine)};
replace(Lno, {L, {Left={LL1, _}, Right}}, NewLine) when Lno > LL1 ->
    NewRight = replace(Lno-LL1, Right, NewLine),
    {L, Left, NewRight};
replace(Lno, {L, {Left, Right}}, NewLine) ->
    NewLeft = replace(Lno, Left, NewLine),
    {L, NewLeft, Right}.

%% @spec insert(LineNo::integer(), Array::line_array(), NewLine::line()) -> line_array()
%%
%% @doc Inserts NewLine *before* the line in position LineNo.
%% <p>e.g. <code>insert(3, [1,2,3,4], x) => [1,2,x,3,4].</code></p>
%% <p>Returns the modified array.</p>
%% @end
%%
insert(Lno, _, _) when Lno < 1 ->
    exit({out_of_range, Lno});
insert(Lno, {L, _}, _NewLine) when Lno > L ->
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

%% @spec insert_after(LineNo::integer(), Array::line_array(), NewLine::line()) -> line_array()
%% 
%% @doc Inserts NewLine *after* the line in position LineNo
%% (<code>LineNo > 0</code>).
%% <p>e.g. <code>insert(3, [1,2,3,4], x) => [1,2,3,x,4].</code></p>
%% <p>Returns the modified array.</p>
%% @end
%%
insert_after(Lno, _, _) when Lno < 0 ->
    exit({out_of_range, Lno});
insert_after(Lno, {L, _}, _NewLine) when Lno > L ->
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


%% @spec delete(LineNo::integer(), Array::line_array()) -> line_array()
%%
%% @doc Deletes the line in position LineNo.
%% <p>e.g. <code>delete(3, [1,2,3,4]) => [1,2,4].</code></p>
%% <p>Returns the modified array.</p>
%% @end
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

%% @spec convert_to_list(line_array()) -> list()
%%
%% @doc Converts an array to a list of lines.
%%
convert_to_list({_, List}) when list(List) ->
    List;
convert_to_list({_, {Left, Right}}) ->
    convert_to_list(Left) ++ convert_to_list(Right).


%% @spec convert_from_list(list()) -> line_array()
%%
%% @doc Converts a list of "lines" to an array.
%% <p>This is done in an efficient manner.</p>
%%
convert_from_list(L) when list(L) ->
    N = length(L),
    {_FullBuckets, _RestLeaf, Height} = size_array(N),
    Buckets = fill_buckets(L),
    {Tree,[]} = l2tree(0, Height, Buckets),
    Tree.
    
l2tree(H, Height,  Buckets) when H < Height ->
    NextH = H+1,
    {{LSz,_}=Left, Buckets1} =
	l2tree(NextH, Height, Buckets),
    {{RSz,_}=Right, Buckets2} =
	l2tree(NextH, Height, Buckets1),
    {{LSz+RSz, {Left,Right}},Buckets2};
l2tree(H, H, []) ->
    {{0,[]}, []};
l2tree(H,H,[LastBucket]) ->
    {LastBucket, []};
l2tree(H,H, [{LSz,_}=B1,{RSz,_}=B2|Rest]) ->
    {{LSz+RSz,{B1,B2}}, Rest}.


fill_buckets([?FULL_BUCKET|Rest]) ->
    [{10, [?FULL_BUCKET]} | fill_buckets(Rest)];
fill_buckets(L) ->
    [{length(L), L}].


%%% ===========================================================
%%% internal functions
%%% ===========================================================

replace_nth(1, [_|T], X) ->
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

delete_nth(1, [_|T]) ->
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
	     Right = {_N_Right, {RLeft = {N_RLeft, _}, 
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
	      Left = {_N_Left, {LLeft = {N_LLeft, _},
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


