-module(rcube).
-author('tobbe@erix.ericsson.se').
%%% --------------------------------------------------------------------
%%% Created:  92-01-30 by Tobbe (tobbe@erix.ericsson.se)
%%%           One of my first programs in Erlang, making use of
%%%           PXW which was an interface to the Athena Widget set.
%%%           I wanted to try out some simple graphics concepts,
%%%           such as how to remove hidden lines before drawing them.
%%%           Since then I have used this program as little test
%%%           on how easy/good the graphics capabilities are for
%%%           various toolkits.
%%%
%%% Modified: 12 Aug 1997 by tobbe@cslab.ericsson.se
%%%           Transformed to use GS.
%%%
%%% Modified: 19 Feb 1999 by tnt@home.se
%%%           Transformed again to use the Erlang X11 binding.
%%%
%%% --------------------------------------------------------------------
-export([start/0, start/1,init/1,kicker/2]).

-import(matrix44,[multiply14/2,mk_rotate_matrix/2,mk_hcord/3,
		  mk_translate_matrix/3]).

-include("ex11.hrl").

-define(KICK_INTERVAL, 10). % Time in (ms) between redraws

-record(rcube,{window,      % The main window
	       draw_gc,     % GC to draw the cube into the pixmap
	       clear_gc,    % GC to clear the pixmap
	       pixmap,      % The pixmap
	       pix_w,       % The pixmap width
	       pix_h        % Te pixmap height
	      }).

start() ->
    start("localhost").

start(Host) ->
    spawn(?MODULE,init,[Host]).

init(Host) ->
    {X,Rcube} = mk_window(Host),
    init_xyz(),
    Fig = figure(),
    Object = draw_cube(X,Rcube,Fig),
    Kicker = start_kicker(?KICK_INTERVAL),
    timer:sleep(1000),
    loop(X,Rcube,Fig,Kicker).

init_xyz() ->
    put(x,2),
    put(y,2),
    put(z,2),
    put(cube_center,{90,70,50,1}).

loop(X,Rcube,Fig,Kicker) ->
    receive
	{Kicker,kick} ->
	    NewFig = redraw(X,Rcube,Fig),
	    loop(X,Rcube,NewFig,Kicker);
	{ex11,Msg} ->
	    dispatch(X,Rcube,Fig,Kicker,Msg);
	reset ->
	    loop(X,Rcube,figure(),Kicker);
	start when Kicker==false ->
	    K = start_kicker(?KICK_INTERVAL),
	    loop(X,Rcube,Fig,K);
	stop when pid(Kicker) ->
	    stop_kicker(Kicker),
	    loop(X,Rcube,Fig,false);
	{x,X} ->
	    put(x,X),
	    loop(X,Rcube,Fig,Kicker);
	{y,Y} ->
	    put(y,Y),
	    loop(X,Rcube,Fig,Kicker);
	{z,Z} ->
	    put(z,Z),
	    loop(X,Rcube,Fig,Kicker);
	quit ->
	    exit(normal);
	geo ->
	    %% Just to test that replies work
	    R = #get_geometry{drawable=Rcube#rcube.window},
	    ex11:req(X,R),
	    loop(X,Rcube,Fig,Kicker);
	XX ->
	    io:format("Got: ~p~n",[XX]),
	    loop(X,Rcube,Fig,Kicker)
    end.

%% -------------------------------------------
%% Print out the events we get (just testing)

dispatch(X,Rcube,Fig,Kicker,E) when ?IS_EXPOSE_EVENT(E) ->
    io:format("Got expose-event: x=~w  y=~w  width=~w  height=~w  count=~w~n",
	      [E#expose.x,E#expose.y,E#expose.width,E#expose.height,E#expose.count]),
    loop(X,Rcube,Fig,Kicker);
dispatch(X,Rcube,Fig,Kicker,E) when ?IS_CONFIGURE_NOTIFY_EVENT(E) ->
    io:format("Got configure_notify-event: x=~w  y=~w  width=~w  height=~w~n",
	      [E#configure_notify.x,E#configure_notify.y,
	       E#configure_notify.width,E#configure_notify.height]),
    loop(X,Rcube,Fig,Kicker);
dispatch(X,Rcube,Fig,Kicker,E) when ?IS_REPARENT_NOTIFY_EVENT(E) ->
    io:format("Got reparent_notify-event: x=~w  y=~w  override_redirect=~w~n",
	      [E#reparent_notify.x,E#reparent_notify.y,E#reparent_notify.override_redirect]),
    loop(X,Rcube,Fig,Kicker);
dispatch(X,Rcube,Fig,Kicker,E) when ?IS_MAP_NOTIFY_EVENT(E) ->
    io:format("Got map_notify-event ~n",[]),
    loop(X,Rcube,Fig,Kicker);
dispatch(X,Rcube,Fig,Kicker,E) ->
        io:format("Got unknown event: ~w~n",[E]),
    loop(X,Rcube,Fig,Kicker).

%% ------------------
%% Redraw ye'ol cube

redraw(X,Rcube,Fig) ->
    NewFig = do_transformations(Fig),
    draw_cube(X,Rcube,NewFig),
    NewFig.

%% ------------------------------------------------
%% Clear the pixmap and draw the cube in to it.
%% Copy the pixmap to the displayed window.
%% Return a list of the poly-line objects created.

draw_cube(X,Rcube,Fig) ->
    clear_pixmap(X,Rcube),
    Object = draw_surfaces(X,Rcube,Fig,surfaces()),
    CopyArea = #copy_area{dst    = Rcube#rcube.window,
			  src    = Rcube#rcube.pixmap,
			  cid    = Rcube#rcube.draw_gc,
			  width  = Rcube#rcube.pix_w,
			  height = Rcube#rcube.pix_h
			 },
    ex11:req(X,CopyArea),
    ex11:flush(X),
    Object.

%% --------------------------------------------------
%% Check for hidden surfaces and draw what's visible

draw_surfaces(_,_,_,0) -> [];
draw_surfaces(X,Rcube,Fig,Num) ->
    Points = surface(Num),
    {P1,P2,P3,P4} = Points,
    case hidden_surface_p(Points,Fig) of
	true ->
	    draw_surfaces(X,Rcube,Fig,Num-1);
	false ->
	    Plist = mk_point_list(Points,Fig),
	    PolyLine = #poly_line{drawable = Rcube#rcube.pixmap,
				  gc       = Rcube#rcube.draw_gc,
				  points   = Plist
				 },
	    ex11:req(X,PolyLine),
	    draw_surfaces(X,Rcube,Fig,Num-1)
    end.

mk_point_list({P1,P2,P3,P4},Fig) ->
    {X1,Y1} = gp(P1,Fig),
    {X2,Y2} = gp(P2,Fig),
    {X3,Y3} = gp(P3,Fig),
    {X4,Y4} = gp(P4,Fig),
    First = #point{x=round(X1),y=round(Y1)}, 
    [%% First line
     First,
     #point{x=round(X2),y=round(Y2)},
     %% Second line
     #point{x=round(X2),y=round(Y2)},
     #point{x=round(X3),y=round(Y3)},
     %% Third line
     #point{x=round(X3),y=round(Y3)},
     #point{x=round(X4),y=round(Y4)},
     %% Fourth line
     #point{x=round(X4),y=round(Y4)},
     First].

%% Get the X and Y coordinate for the specified point
gp(1,[{X,Y,_,_}|_]) -> {X,Y};
gp(N,[_|T])         -> gp(N-1,T).

hidden_surface_p({P1,P2,P3,_},Fig) ->
    {X1,Y1} = gp(P1,Fig),
    {X2,Y2} = gp(P2,Fig),
    {X3,Y3} = gp(P3,Fig),
    C = X1*(Y2-Y3) + X2*(Y3-Y1) + X3*(Y1-Y2),
    if
	C > 0 -> false;
	true  -> true
    end.

%% --------------------------------
%% Do the rotate transformations    

do_transformations(Fig) ->
    {Cx,Cy,Cz,_} = cube_center(),
    ToOrigo = cube_transform(Fig,mk_translate_matrix(-Cx,-Cy,-Cz)),
    Xtransformed = x_transform(ToOrigo),
    Ytransformed = y_transform(Xtransformed),
    Ztransformed = z_transform(Ytransformed),
    Back = cube_transform(Ztransformed,mk_translate_matrix(Cx,Cy,Cz)),
    hcord_to_cart(Back).

x_transform(Cube) -> transform(Cube,x,get(x)).
y_transform(Cube) -> transform(Cube,y,get(y)).
z_transform(Cube) -> transform(Cube,z,get(z)).

%% Only do the transform when Angle > 0
transform(Cube,_,0) -> Cube;
transform(Cube,Axis,Value) ->
    M = mk_rotate_matrix(Axis,Value),
    cube_transform(Cube,M).

hcord_to_cart([]) -> [];
hcord_to_cart([{X,Y,Z,S}|Tail]) when S==0 ->
    [{X,Y,Z,0}|hcord_to_cart(Tail)];
hcord_to_cart([{X,Y,Z,S}|Tail]) ->
    [{X/S,Y/S,Z/S,1}|hcord_to_cart(Tail)].

cube_transform([],_) -> [];
cube_transform([Vector|Tail],Matrix) ->
    [multiply14(Vector,Matrix)|cube_transform(Tail,Matrix)].

cube_center() -> get(cube_center).

% Note that the order of the points is crucial, 
% in order to fit the definition of surfaces.
figure() ->
    P1 = mk_hcord(80,60,40),
    P2 = mk_hcord(100,60,40),
    P3 = mk_hcord(100,80,40),
    P4 = mk_hcord(80,80,40),
    P5 = mk_hcord(80,60,60),
    P6 = mk_hcord(100,60,60),
    P7 = mk_hcord(100,80,60),
    P8 = mk_hcord(80,80,60),
    [P1,P2,P3,P4,P5,P6,P7,P8].
   
surfaces() -> 6.    % Number of surfaces

% Defines the points a surface consists of
surface(1) -> {4,3,2,1};
surface(2) -> {3,7,6,2};
surface(3) -> {8,7,3,4};
surface(4) -> {5,8,4,1};
surface(5) -> {6,7,8,5};
surface(6) -> {1,2,6,5}.

%% ------------------------
%% Create the window stuff

mk_window(Host) ->
    {ok,X} = ex11:start(Host),
    %%
    %% Get background and foreground from the Display
    %%
    {ok,Dpy} = ex11:get_display(X), 
    White = ?WHITE_PIXEL(Dpy),
    Black = ?BLACK_PIXEL(Dpy),
    %%
    %% Create a top-level window
    %%
    Wmask = (?WIN_DEFAULT_VALUEMASK bor
	     ?WIN_VALUEMASK_BG_PIXEL bor
	     ?WIN_VALUEMASK_EVENT_MASK),
    Events = (?EVENT_EXPOSURE bor
	      ?EVENT_STRUCTURE_NOTIFY),
    Wval = #win_values{bg_pixel   = White,
		       event_mask = Events
		      },
    {ok,Window} = ex11:req(X,#create_window{width      = 200,
					   height     = 200,
					   value_mask = Wmask,
					   value_list = Wval
					  }),
io:format("PIX=~w~n", [Window]),
    %%
    %% Create the Pixmap
    %%
    PixW = 120,
    PixH = 120,
    {ok,Pix} = ex11:req(X,#create_pixmap{drawable = Window,
					width    = PixW,
					height   = PixH
				       }),
    %%
    %% Create the GC's
    %%
    LineWidth = 1,
    DrawGC  = mk_draw_gc(X,Pix,LineWidth,White,Black),
    ClearGC = mk_clear_gc(X,Pix,White),
    %%
    %% Create the rcube datastructure
    %%
    Rcube = #rcube{window    = Window,
		   pixmap    = Pix,
		   draw_gc   = DrawGC,
		   clear_gc  = ClearGC,
		   pix_w     = PixW,
		   pix_h     = PixH
		  },
    %%
    %% Clear the pixmap
    %%
    clear_pixmap(X,Rcube),
    %%
    %% Show the window
    %%
    ex11:req(X,#map_window{window=Window}),
    {X,Rcube}.

mk_clear_gc(X,Pix,Bg) ->
    mk_draw_gc(X,Pix,0,Bg,Bg).

mk_draw_gc(X,Pix,LineW,Bg,Fg) ->
    GCmask = (?GC_DEFAULT_VALUEMASK bor
	      ?GC_VALUEMASK_BACKGROUND bor
	      ?GC_VALUEMASK_FOREGROUND),
    GCval = #gc_values{line_width=LineW,
		       join_style=?GC_JOINSTYLE_BEVEL,
		       exposures=?FALSE,
		       background=Bg,
		       foreground=Fg},
    {ok,Cid} = ex11:req(X,#create_gc{drawable=Pix,
				    value_mask=GCmask,
				    value_list=GCval}),
    Cid.
    
clear_pixmap(X,R) ->
    Pix = R#rcube.pixmap,
    ClearGC = R#rcube.clear_gc,
    Rectangle = #rectangle{x=0,y=0,
			   width=R#rcube.pix_w,
			   height=R#rcube.pix_h},
    ex11:req(X,#poly_fill_rectangle{drawable=Pix,
				   cid=ClearGC,
				   rectangles=[Rectangle]}).
					

%% -------------------
%% THE KICKER PROCESS
%% -------------------

start_kicker(T) ->
    spawn_link(?MODULE,kicker,[self(),T]).

kicker(Who,T) ->
    sleep_or_stop(T),
    Who ! {self(),kick},
    kicker(Who,T).

stop_kicker(Kicker) -> Kicker ! stop.

sleep_or_stop(T) ->
    receive
	stop -> exit(stopped)
    after T -> true
    end.
