%%% File    : demo.erl
%%% Author  : Christian Sunesson <e98_csu@e.kth.se>
%%% Description : Demo applications.
%%% Created : 13 Jun 2003 by Christian Sunesson <e98_csu@fred.e.kth.se>

-module(demo).
-export([xev/0, xev/1, xev0/1]).
-export([clock/0, clock/1, clock0/1]).

-include("x.hrl").
-include("xproto.gx.hrl").

-define(WIDTH, 200).
-define(HEIGHT, 200).

%%-define(DBG(Format, As), io:format(Format, As)).
-define(DBG(Format, As), ok).

xev() ->
    xev("").
xev(DisplayName) ->
    spawn_link(?MODULE, xev0, [DisplayName]).

%% registering a symbol id when creating windows might make code
%% less cluttered if you do not need to carry window ids around.

xev0(DisplayName) ->
    {ok, D} = x:openDisplay(DisplayName),
    Scr   = x:defaultScreen(D),
    White = x:whitePixel(D, Scr),
    Black = x:blackPixel(D, Scr),
    W  = x:createSimpleWindow(D, 0, 10, 10, ?WIDTH, ?HEIGHT, 4, Black, White),
    x:selectInput(D, W, 
		  ?OwnerGrabButtonMask-1 bor ?OwnerGrabButtonMask),
    x:mapWindow(D, W),
    xev0(D,W).

xev0(D, State) ->
    receive
	{x,Event} -> 
	    io:format("xev: got event ~p\n", [element(1, Event)]),
	    case Event of
		#xDestroyWindowEvent{window=W} ->
		    io:format("~p\n", [Event]),
		    exit(destroyed);
		Else ->
		    io:format("~p\n", [Event]),
		    xev0(D,State)
	    end;
	Other ->
	    io:format("got other: ~p\n", [Other]),
	    xev0(D, State)
    end.

clock() ->
    clock("").
clock(DisplayName) ->
    spawn(?MODULE, clock0, [DisplayName]).

clock0(DisplayName) ->
    {ok, D} = x:openDisplay(DisplayName),
    Scr   = x:defaultScreen(D),
    White = x:whitePixel(D, Scr),
    Black = x:blackPixel(D, Scr),
    Red = color(D, #xColor{red=16#ffff, blue=0, green=0}),
    W  = x:createSimpleWindow(D, 0, 10, 10, ?WIDTH, ?HEIGHT, 4, Black, White),
    BlackGC = x:createGC(D, W, ?GCDefaultValueMask bor ?GCForeground bor
			 ?GCLineWidth,
			 #xgc_values { foreground = Black, line_width=5 }),
    RedGC = x:createGC(D, W, ?GCDefaultValueMask bor ?GCForeground bor
		       ?GCLineWidth,
		       #xgc_values { foreground = Red, line_width=3 }),
    x:selectInput(D, W, 
		  ?ExposureMask bor ?StructureNotifyMask),
    x:mapWindow(D, W),
    clock0(D,{W, BlackGC, RedGC, ?WIDTH, ?HEIGHT}).

clock0(D, State) ->
    receive
	{x,Event} -> 
	    %%io:format("xev: got event ~p\n", [element(1, Event)]),
	    case Event of
		#xExposeEvent{count=0} ->
		    {Win, BGC, RGC, Width, Height} = State,
		    drawClock(D, Win, BGC, RGC, Width, Height),
		    clock0(D, State);
		#xConfigureEvent{width=Width, height=Height} ->
		    %%io:format("configure ~p by ~p\n", [Width, Height]),
		    {Win, BGC, RGC, W, H} = State,
		    drawClock(D, Win, BGC, RGC, Width, Height),
		    NewState = {Win, BGC, RGC, Width, Height},
		    clock0(D, NewState);
		#xDestroyWindowEvent{window=W} ->
		    exit(destroyed);
		Else ->
		    %%io:format("~p\n", [Event]),
		    clock0(D,State)
	    end;
	Other ->
	    %%io:format("got other: ~p\n", [Other]),
	    clock0(D, State)
    after 1000 ->
	    {Win, BGC, RGC, W, H} = State,
	    drawClock(D, Win, BGC, RGC, W, H),
	    clock0(D,State)
    end.

drawClock(D, W, BGC, RGC, Width, Height) ->
    x:clearWindow(D,W),
    {Hour, Min, Sec} = time(),
    x:drawArc(D,W,RGC, 0+10, 0+10, Width-20, Height-20, 
	      90*64, -(Hour rem 12)*30*64),
    x:drawArc(D,W,BGC, 0+20, 0+20, Width-40, Height-40,
	      90*64, -Min*6*64),
    x:drawArc(D,W, RGC, 0+30, 0+30, Width-60, Height-60, 
	      90*64, -Sec*6*64).
    


    

color(D, Color ) when record(Color, xColor) ->
    R = x:allocColor(D, Color),
    R#xColor.pixel.
