%%%----------------------------------------------------------------------
%%% File    : cord.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Data structure for large strings of text
%%% Created : 21 Oct 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------
%%
%% Cords - a scalable data structure for strings of text.
%%
%% Cords are binary trees with erlang binaries as leaves. The trees
%% are kept fairly balanced, and the sizes of the binary objects are
%% kept within acceptable bounds. By using binaries, it should only
%% cost about one byte per character.
%%
%% The main idea is to have a fast insert/replace operation which
%% doesn't change the binaries too much. By keeping the binaries on
%% the leaves fairly small, we generally get to move them about in one
%% piece while we're balancing things after large updates or pulling
%% out large regions. An added advantage of not disturbing the
%% binaries is that it's not too expensive to keep old copies of cords
%% around, since most of the binaries get shared on the heap.
%%
%% API:
%%   new():
%%     Creates a new, empty cord.
%%   new(ListOrBinary):
%%     Creates a new cord from some input characters.
%%   replace(Cord, NewText, Start, Length):
%%     Replaces the `Length' long portion of `Cord' starting at `Start'
%%     with `NewText' (a cord, binary, or iolist).
%%   join(Left, Right)
%%     Join two cords together into a new one.
%%   to_binary(Cord): Convert a cord into a binary
%%   to_list(Cord):   Convert a cord into a list
%%   walk(Cord, Pos, Direction, Fun):
%%     "Walk" character by character along `Cord' in `Direction'. For each
%%     character, we call Fun(Char) which returns either {result, R}, or
%%     {more, NextFun}. If we run out of characters before the fun returns
%%     a result, we call Fun(finish), which is required to return
%%     {result, R}.

-module(cord).
-author('luke@bluetail.com').

-compile(export_all).

-export([new/0, new/1, cord_size/1,
	 replace/4, region/3, region_binary/3, region_list/3,
	 to_binary/1, to_list/1, to_iolist/1,
	 walk/4]).

%% It seems that static values won't do for scaling to gigantic (tens
%% of megabytes) sizes.
-define(MIN_SIZE, 512).
-define(MAX_SIZE, 2048).

%% A cord is either a #cord record or a binary.
-record(cord, {size,			% Combined size
	       left,			% cord()
	       right,			% cord()
	       %% The dirty flag indicates that a cord (or one of its
	       %% children) has been changed since the last time the
	       %% cord was 'fixed'
	       dirty=false
	       }).

-define(assert(X),
	(case X of
	     true ->
		 true;
	     false ->
		 exit(lists:flatten(io_lib:format("Assertion failed at ~p:~p",
						  [?MODULE, ?LINE])))
	 end)).

new() -> <<>>.

new(B) when binary(B) -> fix_cord(B);
new(L) when list(L)   -> new(list_to_binary(L)).

%% More efficient way to create a cord from a file. This
%% implementation is not terribly clever (read small chunks, balance
%% at the end), but at least it isn't a memory hog.
%%
%% Returns: {ok, cord()} | {error, Reason}
new_from_file(Filename) ->
    case file:open(Filename, [raw, binary]) of
	X = {error, Rsn} ->
	    X;
	{ok, F} ->
	    read_chunks(F)
    end.

read_chunks(F) ->
    read_chunks(F, new()).

read_chunks(F, Acc) ->
    case file:read(F, ?MAX_SIZE) of
	eof ->
	    {ok, fix_cord(Acc)};
	{ok, Bin} ->
	    read_chunks(F, make_cord(Acc, Bin));
	X = {error, Reason} ->
	    X
    end.

%% Make a cord.
%% This is a "dirty" operation that doesn't rebalance the tree.
make_cord(Left, Right) ->
    #cord{size=cord_size(Left) + cord_size(Right),
	  left=Left,
	  right=Right,
	  dirty=true}.

cord_size(Cord) when binary(Cord) ->
    size(Cord);
cord_size(Cord) when record(Cord, cord) ->
    Cord#cord.size.

max_depth(Cord) when binary(Cord) ->
    1;
max_depth(Cord) ->
    1 + max(max_depth(Cord#cord.left),
	    max_depth(Cord#cord.right)).

nr_nodes(Cord) when binary(Cord) ->
    1;
nr_nodes(Cord) ->
    1 + nr_nodes(Cord#cord.left) + nr_nodes(Cord#cord.right).

mean_leaf_size(Cord) ->
    Sizes = leaf_sizes(Cord),
    Sum = lists:foldr(fun(X, Acc) -> X + Acc end,
		      0,
		      Sizes),
    round(Sum / length(Sizes)).

leaf_sizes(Cord) when binary(Cord) ->
    [size(Cord)];
leaf_sizes(Cord) ->
    leaf_sizes(Cord#cord.left) ++ leaf_sizes(Cord#cord.right).

max(X, Y) when X > Y -> X;
max(X, Y)            -> Y.

insert(Cord, New, Point) ->
    replace(Cord, New, Point, 0).

delete(Cord, Point, Length) ->
    replace(Cord, [], Point, Length).

%% replace/4: Replace a region of the cord. `New' is the text to
%% replace the region with, and can be either a list, binary, or cord.
replace(Cord, New, Start, Length) when list(New) ->
    replace(Cord, list_to_binary(New), Start, Length);
replace(Cord, New, Start, Length) ->
    %% Replace is done by copying the areas on the left and right of
    %% the region, and joining them together with the new cord in the
    %% middle.
    {A, B} = split(Cord, Start-1),
    {C, D} = split(B, Length),
    fix_cord(make_cord(make_cord(A, New), D)).

split(Cord, 0) when binary(Cord) ->
    {<<>>, Cord};
split(Cord, Pos) when binary(Cord) ->
    ?assert(Pos =< cord_size(Cord)),
    <<Left:Pos/binary, Right/binary>> = Cord,
    {Left, Right};

split(Cord, Pos) when record(Cord, cord) ->
    ?assert(Pos =< cord_size(Cord)),
    LeftSz = cord_size(Cord#cord.left),
    RightSz = cord_size(Cord#cord.right),
    %%io:format("Split - left:~p right:~p~n", [LeftSz, RightSz]),
    if LeftSz == Pos ->
	    {Cord#cord.left, Cord#cord.right};
       LeftSz > Pos ->
	    {SplitLeft, SplitRight} = split(Cord#cord.left, Pos),
	    {SplitLeft, make_cord(SplitRight, Cord#cord.right)};
       LeftSz < Pos ->
	    {SplitLeft, SplitRight} = split(Cord#cord.right, Pos-LeftSz),
	    {make_cord(Cord#cord.left, SplitLeft), SplitRight}
    end.

%% join two cords together and rebalance.
join(Left, Right) when binary(Left) ->
    fix_cord(make_cord(Left, Right)).

%% fix_cord/1
%%
%% "Fix" a cord so that it's reasonably balanced, and it's leaves are
%% reasonable sizes.

%% Leaf (binary) - break it up if it's too big
fix_cord(Bin) when binary(Bin) ->
    if size(Bin) > ?MAX_SIZE ->
	    {Left, Right} = split(Bin, round(size(Bin) / 2)),
	    fix_cord(make_cord(Left, Right));
       true ->
	    Bin
    end;
fix_cord(Cord) when Cord#cord.dirty == false ->
    Cord;
%% Branch (cord) - merge its children if they're too small, balance it
%% if it's too unbalanced.
fix_cord(Cord) when record(Cord, cord) ->
    Sz = cord_size(Cord),
    Left = Cord#cord.left,
    Right = Cord#cord.right,
    LeftSz = cord_size(Left),
    RightSz = cord_size(Right),
    SzDiff = abs(LeftSz - RightSz),
    if Sz < ?MIN_SIZE ->
	    %% Too small - make it into a binary
	    to_binary(Cord);
       SzDiff > (Sz/3) ->
	    %% needs rebalancing
	    if LeftSz > RightSz ->
		    if binary(Left) ->
			    fix_cord(to_binary(Cord));
		       true ->
			    balance_from_left(Cord)
		    end;
	       LeftSz =< RightSz ->
		    if binary(Right) ->
			    fix_cord(to_binary(Cord));
		       true ->
			    balance_from_right(Cord)
		    end
	    end;
       true ->
	    %% this cord is ok, fix the children
	    Cord#cord{left=fix_cord(Left),
		      right=fix_cord(Right),
		      dirty=false}
    end.

%% Balance by taking from the left side.
%% Left must be a #cord, right can be a binary.
balance_from_left(#cord{left=Left, right=Right}) when record(Left, cord) ->
    LLSz = cord_size(Left#cord.left),
    LRSz = cord_size(Left#cord.right),
    if
	LRSz < LLSz ->
	    %% single rotate
	    fix_cord(make_cord(Left#cord.left,
			       make_cord(Left#cord.right,
					 Right)));
	record(Left#cord.right, cord) ->
	    %% double rotate
	    LeftRight = Left#cord.right,
	    fix_cord(make_cord(make_cord(Left#cord.left,
					 LeftRight#cord.left),
			       make_cord(LeftRight#cord.right,
					 Right)));
	true ->
	    fix_cord(to_binary(make_cord(Left, Right)))
    end.

%% oh, pain, duplication. never have been good at taking redundancy
%% out of symmetric functions. -luke
balance_from_right(#cord{left=Left, right=Right}) when record(Right, cord) ->
    RLSz = cord_size(Right#cord.left),
    RRSz = cord_size(Right#cord.right),
    if
	RLSz < RRSz ->
	    %% single rotate
	    fix_cord(make_cord(make_cord(Left,
					 Right#cord.left),
			       Right#cord.right));
	record(Right#cord.left, cord) ->
	    %% double rotate
	    RightLeft = Right#cord.left,
	    fix_cord(make_cord(make_cord(Left,
					 RightLeft#cord.left),
			       make_cord(RightLeft#cord.right,
					 Right#cord.right)));
	true ->
	    fix_cord(to_binary(make_cord(Left, Right)))
    end.

%% Return: cord()
region(Cord, Start, Length) ->
    {A, B} = split(Cord, Start-1),
    {C, D} = split(B, Length),
    C.

%% Return: binary()
region_binary(Cord, Start, Length) ->
    to_binary(region(Cord, Start, Length)).

%% Return: list()
region_list(Cord, Start, Length) ->
    binary_to_list(region_binary(Cord, Start, Length)).

to_binary(Cord) when binary(Cord) ->
    Cord;
to_binary(Cord) ->
    list_to_binary(to_binary1(Cord)).

to_binary1(Cord) when binary(Cord) ->
    Cord;
to_binary1(Cord) when record(Cord, cord) ->
    [to_binary1(Cord#cord.left),to_binary1(Cord#cord.right)].

to_list(Cord) ->
    binary_to_list(to_binary(Cord)).

to_iolist(Cord) when binary(Cord) ->
    [Cord];
to_iolist(Cord) ->
    [to_iolist(Cord#cord.left) | to_iolist(Cord#cord.right)].

%% Walk backwards along a cord, character by character.
%% F = fun(X) -> {more, F2} | {result, R}
%% X = char() | finish
walk(Cord, Pos, Direction, F) ->
    %% Make this simple: extract the region we want to walk along.
    Region = case Direction of
		 backward ->
		     {A, B} = split(Cord, Pos),
		     A;
		 forward ->
		     {A, B} = split(Cord, Pos-1),
		     B
	     end,
    case walk1(Region, Direction, F) of
	{result, R} ->
	    R;
	{more, FNext} ->
	    {result, R} = FNext(finish),
	    R
    end.

walk1(<<>>, Direction, F) ->
    {more, F};
walk1(Bin, Direction, F) when binary(Bin) ->
    {Chunk, Char} = case Direction of
			backward ->
			    Sz = size(Bin) - 1,
			    <<Front:Sz/binary, Back>> = Bin,
			    {Front, Back};
			forward ->
			    <<Front, Back/binary>> = Bin,
			    {Back, Front}
		    end,
    case F(Char) of
	{more, F2} ->
	    walk1(Chunk, Direction, F2);
	{result, R} ->
	    {result, R}
    end;
walk1(Cord, Direction, F) when record(Cord, cord) ->
    {First, Second} = case Direction of
			  backward ->
			      {Cord#cord.right, Cord#cord.left};
			  forward ->
			      {Cord#cord.left, Cord#cord.right}
		      end,
    case walk1(First, Direction, F) of
	{more, F2} ->
	    walk1(Second, Direction, F2);
	{result, R} ->
	    {result, R}
    end.

walker(Cord) ->
    walker(Cord, forward).

walker(Cord, Direction) ->
    walker(Cord, Direction, <<>>).

walker(Cord, Direction, More) when binary(Cord) ->
    {Cord, Direction, More};
walker(Cord, forward, More) ->
    walker(Cord#cord.left, forward, make_cord(Cord#cord.right, More));
walker(Cord, backward, More) ->
    walker(Cord#cord.right, backward, make_cord(More, Cord#cord.left)).

walker_at_end({walked_to_end, _}) ->
    true;
walker_at_end({C,_,Rest}) ->
    cord_size(C) + cord_size(Rest) == 0.

walker_direction({walked_to_end, Direction}) -> Direction;
walker_direction({_, Direction, _})          -> Direction.

walker_next({walked_to_end, Direction}) ->
    {done, {walked_to_end, Direction}};
walker_next({<<>>, Direction, More}) ->
    case cord_size(More) of
	0 ->
	    {done, {walked_to_end, Direction}};
	_ ->
	    walker_next(walker(More, Direction))
    end;
walker_next({<<A, Chunk/binary>>, forward, More}) ->
    {A, {Chunk, forward, More}};
walker_next({Bin, backward, More}) ->
    ChunkSz = size(Bin) - 1,
    <<Chunk:ChunkSz/binary, A>> = Bin,
    {A, {Chunk, backward, More}}.

walker_push(done, Walker) ->
    Walker;
walker_push(X, {walked_to_end, Dir}) ->
    {<<X>>, Dir, <<>>};
walker_push(X, {Bin, forward, More}) ->
    {<<X>>, forward, make_cord(Bin, More)};
walker_push(X, {Bin, backward, More}) ->
    {<<X>>, backward, make_cord(Bin, More)}.

walker_test() ->
    Cord = make_cord(make_cord(<<1,2>>, <<3>>),
		     make_cord(<<4,5,6>>, <<7,8,9>>)),
    W = walker(Cord, forward),
    walker_test_loop(W).

walker_test_loop(W) ->
    case walker_next(W) of
	{done, _} ->
	    [];
	{Ch, W2} ->
	    [Ch|walker_test_loop(W2)]
    end.

test() ->
    %% Test of binary cords
    BinCord = <<1, 2, 3, 4, 5>> ,
    {BinCord, <<>>} = split(BinCord, size(BinCord)),
    {<<>>, BinCord} = split(BinCord, 0),
    {<<1, 2>>, <<3, 4, 5>>} = split(BinCord, 2),
    %% Test of a simple cord
    Cord1 = make_cord(<<1, 2>>, <<3, 4, 5>>),
    {<<>>, <<1, 2, 3, 4, 5>>} = binsplit(Cord1, 0),
    {<<1, 2>>, <<3, 4, 5>>} = binsplit(Cord1, 2),
    {<<1, 2, 3>>, <<4, 5>>} = binsplit(Cord1, 3),
    %% A less trivial cord
    %% (spaces before commas are to workaround an erlang-mode indent problem)
    A = <<1, 2>> ,
    B = <<3, 4>> ,
    C = <<5, 6>> ,
    D = <<7, 8>> ,
    E = <<9>> ,
    Cord2 = make_cord(make_cord(A, B), make_cord(C, make_cord(D, E))),
    {<<>>, <<1, 2, 3, 4, 5, 6, 7, 8, 9>>} = binsplit(Cord2, 0),
    {<<1, 2, 3, 4, 5>>, <<6, 7, 8, 9>>} = binsplit(Cord2, 5),
    %% Joining
    BinCord2 = to_binary(Cord2),
    %% Why does = fail but == work?
    true = <<BinCord/binary, BinCord2/binary>> == binjoin(BinCord, Cord2),
    true = <<BinCord2/binary, BinCord/binary>> == binjoin(Cord2, Cord1),
    true = <<BinCord2/binary, BinCord2/binary>> == binjoin(Cord2, Cord2),
    %% Test "walking"
    [1,2,3,4,5] = walk(Cord2, 5, backward, walk_test([])),
    [9, 8, 7, 6, 5] = walk(Cord2, 5, forward, walk_test([])),
    %% Test some operations
    <<1, 2, 3, 4>> = delete(Cord2, 5, 5),
    <<1, 2, 3,
    1, 2, 3, 4, 5, 6, 7, 8, 9,
    4, 5, 6, 7, 8, 9>> = cord:insert(Cord2, Cord2, 4),
    ok.

walk_test(Acc) ->
    fun(finish) ->
	    {result, Acc};
       (X) ->
	    {more, walk_test([X|Acc])}
    end.

test2() ->
    A = <<1, 2, 3, 4>> ,
    B = <<5, 6>> ,
    C = <<>> ,
    D = <<7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17>> ,
    join(A, join(B, join(C, D))).

binsplit(Cord, Pos) ->
    {Left, Right} = split(Cord, Pos),
    {to_binary(Left), to_binary(Right)}.

binjoin(Left, Right) ->
    to_binary(make_cord(Left, Right)).

benchmark(File) ->
    {ok, Cord} = cord:new_from_file(File),
    Sz = cord_size(Cord),
    random:seed(),
    Randoms = [random:uniform(Sz) || _ <- lists:seq(1, 100)],
    timer:tc(?MODULE, split_with_each, [Cord, Randoms, 10]).

split_with_each(Cord, L, N) ->
    split_with_each(Cord, L, L, N).

split_with_each(Cord, _, _, 0) ->
    ok;
split_with_each(Cord, [H|T], L, N) ->
    split(Cord, H),
    split_with_each(Cord, T, L, N);
split_with_each(Cord, [], L, N) ->
    split_with_each(Cord, L, L, N-1).


