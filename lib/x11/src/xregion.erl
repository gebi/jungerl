%%% File    : xregion.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : region code
%%% Created :  6 Feb 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(xregion).

-compile(export_all).
-import(lists, [map/2]).

-record(region,
	{
	  extent, %% {X1,Y2,X2,Y2}
	  rects   %% [{X1,Y2,X2,Y2}]
	 }).

min(X,Y) when X < Y -> X;
min(X,Y) -> Y.

max(X,Y) when X < Y -> Y;
max(X,Y) -> X.

sortx(R) ->
    Rs = lists:sort(fun({X1,_,_,_},{X2,_,_,_}) ->
			    X1 < X2
		    end, R#region.rects),
    R#region { rects = Rs }.

sorty(R) ->
    Rs = lists:sort(fun({_,Y1,_,_},{_,Y2,_,_}) ->
			    Y1 < Y2
		    end, R#region.rects),
    R#region { rects = Rs }.

sortxy(R) ->
    Rs = lists:sort(fun({X1,Y1,_,_},{X2,Y2,_,_}) ->
			    (X1 < X2) and (Y1 < Y2)
		    end, R#region.rects),
    R#region { rects = Rs }.

%% 
%% 
%%
-define(XOUT,  16#08).
-define(XLEFT, 16#01).
-define(XRIGHT,16#02).
-define(XIN,   16#03).
-define(XCOVER,16#04).

-define(YOUT,  16#80).
-define(YOVER, 16#10).
-define(YUNDER,16#20).
-define(YIN,   16#30).
-define(YCOVER,16#40).

-define(OUT, (?XOUT bor ?YOUT)).

rcode(Ax1,Ay1,Ax2,Ay2,Bx1,By1,Bx2,By2) ->
    if
	Ax2 =< Bx1            -> ?XOUT bor ?XLEFT;
	Ax1 >= Bx2            -> ?XOUT bor ?XRIGHT;
	Ax1 =< Bx1 ->
	    if Ax2 < Bx2     -> ?XLEFT;
	       true          -> ?XCOVER
	    end;
	Ax2 < Bx2             -> ?XIN;
	  true                  -> ?XRIGHT
    end +
	if Ay2 =< By1             -> ?YOUT bor ?YOVER;
	   Ay1 >= By2             -> ?YOUT bor ?YUNDER;
	   Ay1 =< By1 ->
		if  Ay2 < By2     -> ?YOVER;
		    true          -> ?YCOVER
		end;
	   Ay2 < By2              -> ?YIN;
	   true                   -> ?YUNDER
    end.



rcode({Ax1,Ay1,Ax2,Ay2},{Bx1,By1,Bx2,By2}) ->
    rcode(Ax1,Ay1,Ax2,Ay2,Bx1,By1,Bx2,By2).

disjunct(A, B) ->
    ( rcode(A, B) band ?OUT ) =/= 0.

%% Create a new empty region
create() ->
    #region { extent = {0,0,0,0},
	      rects = []
	     }.

offset(R, X, Y) ->
    #region {
	extent = offset_box(R#region.extent,X,Y),
	rects = map(fun(Box) -> offset_box(Box,X,Y) end, R#region.rects)
       }.


empty(R) ->
    R#region.rects == [].

equal(R) ->
    false.

point(R, X, Y) ->
    false.

rect(R, X, Y, Width, Height) ->
    false.
    
%% Intersect to regions
intersect(R1, R2) ->
    ok.

union(R1,R2) ->
    ok.

subtract(R1,R2) ->
    ok.

exlusive(R1,R2) ->
    Ra = subtract(R1, R2),
    Rb = subtract(R2, R1),
    union(Ra,Rb).

offset_box({X1,Y1,X2,Y2}, X, Y) ->
    {X1+X,Y1+Y,X2+X,Y2+Y}.

intersect_box({Ax1,Ay1,Ax2,Ay2},{Bx1,By1,Bx2,By2}) ->
    R =  rcode(Ax1,Ay1,Ax2,Ay2,Bx1,By1,Bx2,By2),
    io:format("R = ~p\n", [R]),
    if R band ?OUT =/= 0 ->
	    [];
       true ->
	    [{max(Ax1,Bx1),max(Ay1,By1),
	      min(Ax2,Bx2),min(Ay2,By2)}]
    end.

union_box(A={Ax1,Ay1,Ax2,Ay2},B={Bx1,By1,Bx2,By2}) ->
    R =  rcode(Ax1,Ay1,Ax2,Ay2,Bx1,By1,Bx2,By2),
    io:format("R = ~p\n", [R]),
    if R band ?OUT =/= 0 ->
	    [A,B];
       %% LEFT: Ax1 <= Bx1 <= Ax2 <= Bx2
       R == ?XLEFT bor ?YOVER ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2),
		  box(Bx1,Ay1,Ax2,By1),
		  B);
       R == ?XLEFT bor ?YIN ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2), B);
       R == ?XLEFT bor ?YCOVER ->
	    boxes(box(Ax2,By1,Bx2,By2), A);
       R == ?XLEFT bor ?YUNDER ->
	    boxes(box(Bx1,By1,Ax2,Ay2),
		  box(Ax1,Ay1,Bx1,Ay2),
		  B);

       %% IN:  Bx1 < Ax1 < Ax2 <= Bx2
       R == ?XIN bor ?YOVER ->
	    boxes(box(Ax1,Ay1,Ax2,By1), B);
       R == ?XIN bor ?YIN ->
	    [B];
       R == ?XIN bor ?YCOVER ->
	    boxes(box(Ax1,By2,Ax2,Ay2),
		  box(Ax1,Ay1,Ax2,By1),
		  B);
       R == ?XIN bor ?YUNDER ->
	    boxes(box(Ax1,By2,Ax2,Ay2), B);

       %% COVER: Ax1 <= Bx1 < Bx2 <= Ax2
       R == ?XCOVER bor ?YOVER ->
	    boxes(box(Bx1,Ay2,Bx2,Bx2),A);

       R == ?XCOVER bor ?YIN ->
	    boxes(box(Bx2,Ay1,Ax2,Ay2),
		  box(Ax1,Ay1,Bx1,Ay2),
		  B);

       R == ?XCOVER bor ?YCOVER ->
	    [A];

       R == ?XCOVER bor ?YUNDER ->
	    boxes(box(Bx1,By1,Bx2,Ay1),A);

       %% RIGHT: Bx1 < Ax1 < Bx2 < Ax2
       R == ?XRIGHT bor ?YOVER ->
	    boxes(box(Bx1,Ay1,Ax2,Ay2),
		  box(Ax1,Ay1,Bx1,By2),
		  B);

       R == ?XRIGHT bor ?YIN ->
	    boxes(box(Bx2,Ay1,Ax2,Ay2), B);

       R == ?XRIGHT bor ?YCOVER ->
	    boxes(box(Bx1,By1,Ax1,By2), A);

       R == ?XRIGHT bor ?YUNDER ->
	    boxes(box(Bx2,Ay1,Ax2,Ay2),
		  box(Ax1,By2,Bx2,Ay2),
		  B)
    end.

%% A - B
subtract_box(A={Ax1,Ay1,Ax2,Ay2},B={Bx1,By1,Bx2,By2}) ->
    R =  rcode(Ax1,Ay1,Ax2,Ay2,Bx1,By1,Bx2,By2),
    io:format("R = ~p\n", [R]),
    if R band ?OUT =/= 0 ->
	    [A];
       %% LEFT: Ax1 <= Bx1 <= Ax2 <= Bx2
       R == ?XLEFT bor ?YOVER ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2),
		  box(Bx1,Ay1,Ax2,By1));
       R == ?XLEFT bor ?YIN ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2));
       R == ?XLEFT bor ?YCOVER ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2),
		  box(Bx1,Ay1,Ax2,By1),
		  box(Bx1,By2,Ax2,Ay2));
       R == ?XLEFT bor ?YUNDER ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2),
		  box(Bx1,By2,Ax2,Ay2));

       %% IN:  Bx1 < Ax1 < Ax2 <= Bx2
       R == ?XIN bor ?YOVER ->
	    boxes(box(Ax1,Ay1,Ax2,By1));
       R == ?XIN bor ?YIN ->
	    [];
       R == ?XIN bor ?YCOVER ->
	    boxes(box(Ax1,Ay1,Ax2,By1),
		  box(Ax1,By2,Ax2,Ay2));

       R == ?XIN bor ?YUNDER ->
	    boxes(box(Ax1,By2,Ax2,Ay2));

       %% COVER: Ax1 <= Bx1 < Bx2 <= Ax2
       R == ?XCOVER bor ?YOVER ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2),
		  box(Bx1,Ay1,Bx2,By1),
		  box(Bx2,Ay1,Ax2,Ay2));
       R == ?XCOVER bor ?YIN ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2),
		  box(Bx2,Ay1,Ax2,Ay2));
       R == ?XCOVER bor ?YCOVER ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2),
		  box(Bx1,Ay1,Bx2,By1),
		  box(Bx1,By2,Bx2,Ay2),
		  box(Ax2,Ay1,Bx2,By2));
       R == ?XCOVER bor ?YUNDER ->
	    boxes(box(Ax1,Ay1,Bx1,Ay2),
		  box(Bx1,By2,Bx2,Ay2),
		  box(Bx2,Ay1,Ax2,By2));

       %% RIGHT: Bx1 < Ax1 < Bx2 < Ax2
       R == ?XRIGHT bor ?YOVER ->
	    boxes(box(Ax1,Ay1,Bx2,By1),
		  box(Bx2,Ay1,Ax2,Ay2));
       R == ?XRIGHT bor ?YIN ->
	    boxes(box(Bx2,Ay1,Ax2,Ay2));

       R == ?XRIGHT bor ?YCOVER ->
	    boxes(box(Ax1,Ay1,Bx2,By1),
		  box(Ax1,By2,Bx2,Ay2),
		  box(Bx2,Ay1,Ax2,Ay2));
       R == ?XRIGHT bor ?YUNDER ->
	    boxes(box(Ax1,By2,Bx2,Ay2),
		  box(Bx2,Ay1,Ax2,Ay2))
    end.


box(X,_,X,_) -> false;
box(_,Y,_,Y) -> false;
box(X1,Y1,X2,Y2) when X1<X2,Y1<Y2 -> {X1,Y1,X2,Y2}.

boxes(false) -> [];
boxes(B) -> [B].

boxes(false,B) -> boxes(B);
boxes(A,B) -> [A|boxes(B)].

boxes(false,B,C) -> boxes(B,C);
boxes(A,B,C) -> [A|boxes(B,C)].

boxes(false,B,C,D) -> boxes(B,C,D);
boxes(A,B,C,D) -> [A|boxes(B,C,D)].

    
    
	    

	    
	    


	    

	     
	


	     
	    



    



