%%% File    : run.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : 
%%% Created :  3 Feb 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(run).
-include("x.hrl").
-include("xproto.gx.hrl").

-define(WIDTH, 200).
-define(HEIGHT, 200).

-export([start/0, lines/0, font/0, rectangles/0, keys/0]).

start() ->
    {ok,D} = x:openDisplay(),
    D.

color(D, Color) ->
    R = x:allocColor(D, Color),
    R#xColor.pixel.

gcs(D, W, [Color | Cs]) ->
    Mask = ?GCDefaultValueMask bor ?GCForeground,
    Values = #xgc_values { foreground = Color },
    [ x:createGC(D, W, Mask, Values) | gcs(D, W, Cs)];
gcs(D, W, []) -> 
    [].

gcsf(D, W, Font, [Color | Cs]) ->
    Mask = ?GCDefaultValueMask bor ?GCForeground bor ?GCFont,
    Values = #xgc_values { foreground = Color, font = Font },
    [ x:createGC(D, W, Mask, Values) | gcsf(D, W, Font, Cs)];
gcsf(D, W, Font, []) -> 
    [].

colors(D) ->
    Scr   = x:defaultScreen(D),
    White = x:whitePixel(D, Scr),
    Black = x:blackPixel(D, Scr),
    Red   = color(D, #xColor { red=16#ffff, green=0, blue=0}),
    Green = color(D, #xColor { red=0, green=16#ffff, blue=0}),
    Blue  = color(D, #xColor { red=0, green=0, blue=16#ffff}),
    [White, Black, Red, Green, Blue].

inputs(D, Fid, W, Gcs) ->
    receive
	{x,Event} -> 
	    io:format("inputs: got event ~p\n", [Event]),
	    case element(1,Event) of
		xExposeEvent -> draw(D, Fid, W, Gcs);
		_ -> ok
	    end,
	    inputs(D, Fid, W, Gcs)
    end.

draw(D, Fid, W, Gcs) ->
    Text = "Hello world",
    {_,_,_,Ov} = x:queryTextExtents(D, Fid, Text),
    X = 10,
    Y = (?HEIGHT div 2) + Ov#xCharStruct.ascent,

    x:drawString(D, W, element(2,Gcs), X, Y, Text),
    x:drawRectangle(D, W, element(5, Gcs),
		    X-2, Y-Ov#xCharStruct.ascent-2,
		    Ov#xCharStruct.width+2,
		    Ov#xCharStruct.descent+Ov#xCharStruct.ascent+3).
	    

print_key(Key,State,MinKeyCode, MaxKeyCode, KeyMap, ModMap) ->
    if Key >= MinKeyCode, Key =< MaxKeyCode ->
	    Map = element(Key-MinKeyCode+1, KeyMap),
	    io:format("Map = ~w\n", [Map]),
	    Sym = case Map of
		      [S0|_] when State == 0 -> S0;
		      [_,S1|_] when State == ?ShiftMask -> S1;
		      [_,_,S2|_] when State == ?Mod3Mask -> S2;
		      [_,_,_|S3] when State == ?Mod3Mask bor ?ShiftMask -> S3;
		      _ -> 0
		  end,
	    io:format("Symbol = ~p\n", [[Sym]]);
       true ->
	    io:format("NoMapn",[])
    end.
				          


key_inputs(D, MinKeyCode, MaxKeyCode, KeyMap, ModMap) ->
    receive
	{x,Event} -> 
	    io:format("inputs: got event ~p\n", [Event]),
	    case element(1,Event) of
		xKeyPressedEvent ->
		    Key = Event#xKeyPressedEvent.detail,
		    State = Event#xKeyPressedEvent.state,
		    io:format("KeyPressed ~w state=~w\n", [Key,State]),
		    print_key(Key,State,MinKeyCode,MaxKeyCode,KeyMap,ModMap);
		xKeyReleasedEvent -> 
		    Key = Event#xKeyPressedEvent.detail,
		    State = Event#xKeyPressedEvent.state,
		    io:format("KeyReleased ~w state=~w\n", [Key,State]),
		    print_key(Key,State,MinKeyCode,MaxKeyCode,KeyMap,ModMap);
		_ -> 
		    ok
	    end,
	    key_inputs(D,  MinKeyCode, MaxKeyCode, KeyMap, ModMap);
	Other ->
	    io:format("got other: ~p\n", [Other]),
	    key_inputs(D,  MinKeyCode, MaxKeyCode, KeyMap, ModMap)
    end.

    

keys() ->
    {ok,D} = x:openDisplay(),
    Scr   = x:defaultScreen(D),
    White = x:whitePixel(D, Scr),
    Black = x:blackPixel(D, Scr),
    W  = x:createSimpleWindow(D, 0, 10, 10, ?WIDTH, ?HEIGHT, 4, Black, White),
    x:selectInput(D, W, 
		  ?ButtonPressMask bor ?ButtonReleaseMask bor
		  ?KeyPressMask bor ?KeyReleaseMask bor
		  ?ExposureMask bor
		  ?VisibilityChangeMask bor
		  ?StructureNotifyMask),
    x:mapWindow(D, W),
    {MinKeyCode, MaxKeyCode} = x:displayKeycodes(D),
    KeyMap = x:getKeyboardMapping(D, MinKeyCode, (MaxKeyCode-MinKeyCode)+1),
    ModMap = x:getModifierMapping(D),
    key_inputs(D, MinKeyCode, MaxKeyCode,
	       list_to_tuple(KeyMap), list_to_tuple(ModMap)).

    


font() ->
    {ok,D} = x:openDisplay(),
    Font = "-adobe-times-medium-r-normal--20-140-100-100-p-96-iso8859-1",
    Colors = colors(D),
    Fid   = x:loadFont(D, Font),

    [White,Black|_] = Colors,

    W  = x:createSimpleWindow(D, 0, 10, 10, ?WIDTH, ?HEIGHT, 4, Black, White),
    x:selectInput(D, W, 
		  ?ButtonPressMask bor 
		  ?KeyPressMask bor
		  ?ExposureMask bor
		  ?VisibilityChangeMask bor
		  ?StructureNotifyMask),

    Gcs = list_to_tuple(gcsf(D, W, Fid, Colors)),

    x:mapWindow(D, W),

    inputs(D, Fid, W, Gcs).


rectangles() ->
    {ok,D} = x:openDisplay(),
    Colors = colors(D),
    [White,Black|_] = Colors,
    W  = x:createSimpleWindow(D, 0, 10, 10, ?WIDTH, ?HEIGHT, 4, Black, White),
    Gcs = list_to_tuple(gcs(D, W, Colors)),
    x:mapWindow(D, W),
    random:seed(),
    receive
    after 1000 -> ok
    end,
    draw_rects(D, W, Gcs, ?WIDTH, ?HEIGHT),
    W.

draw_rects(D, W, Gcs, WWidth, WHeight) ->
    X1 = random:uniform(WWidth)-1,
    X2 = random:uniform(WWidth)-1,
    Y1 = random:uniform(WHeight)-1,
    Y2 = random:uniform(WHeight)-1,
    {X,Width} = if X1 < X2 ->
			{X1, X2-X1};
		   true ->
			{X2, X1-X2}
		end,
    {Y,Height} = if Y1 < Y2 ->
			 {Y1, Y2-Y1};
		    true ->
			 {Y2, Y1-Y2}
		 end,
    Gc = element(random:uniform(size(Gcs)), Gcs),
    x:fillRectangle(D, W, Gc, X, Y, Width, Height),
    draw_rects(D, W, Gcs, WWidth, WHeight).



lines() ->
    {ok,D} = x:openDisplay(),
    Colors = colors(D),
    [White,Black|_] = Colors,
    W  = x:createSimpleWindow(D, 0, 10, 10, ?WIDTH, ?HEIGHT, 4, Black, White),
    Gcs = list_to_tuple(gcs(D, W, Colors)),
    x:mapWindow(D, W),
    receive
    after 1000 -> ok
    end,
    draw_lines(D, W, Gcs, 0, ?WIDTH-1),
    W.

draw_lines(D, W, Gcs, N, N) -> 
    receive
    after 100 -> ok
    end,
    x:clearArea(D,W,0,0,?WIDTH,?HEIGHT,?False),
    receive
    after 100 -> ok
    end,
    draw_lines(D, W, Gcs, 0, N);

draw_lines(D, W, Gcs, I, N) ->
    draw_line(D, W, Gcs, 0, ?WIDTH-1, I),
    draw_lines(D, W, Gcs, I+1, N).

draw_line(D, W, Gcs, X1, X2, Y) when X1 =< X2 ->
    Sz = size(Gcs),
    G  = ((X1*?WIDTH+Y) rem Sz) + 1,
    Gc = element(G, Gcs),
    x:drawPoint(D, W, Gc, X1, Y),
    draw_line(D, W, Gcs, X1+1, X2, Y);
draw_line(D, W, Gcs, X1, X2, Y) ->
    ok.

    

    
