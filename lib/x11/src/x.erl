%%% File    : x.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : X11 binding
%%%
%%% Created : 27 Jan 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(x).

-include("x.hrl").
-include("xproto.gx.hrl").

-compile(export_all).
-import(lists, [map/2, reverse/1]).


connectionNumber(#display { fd=Fd}) -> 
    Fd.

rootWindow(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.root).

defaultScreen(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.default_screen).

defaultRootWindow(Dpy) ->
    rootWindow(Dpy, defaultScreen(Dpy)).

defaultVisual(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.root_visual).

defaultGC(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.default_gc).

blackPixel(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.black_pixel).

whitePixel(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.white_pixel).

displayWidth(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.width).

displayHeight(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.height).

displayWidthMM(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.mwidth).

displayHeightMM(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.mheight).

displayPlanes(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.root_depth).

displayCells(#display {store=S}, Scr) ->
    Vis = ets:lookup_element(S, [screen|Scr], #xScreen.root_visual),
    ets:lookup_element(S, [visual|Vis], #xVisual.map_entries).

screenCount(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.nscreens).

serverVendor(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.vendor).

protocolVersion(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.proto_major_version).

protocolRevision(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.proto_minor_version).

vendorRelease(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.release).

displayString(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.display_name).
    
%% #define QLength(dpy) 		(((_XPrivDisplay)dpy)->qlen)
defaultDepth(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.root_depth).

defaultColormap(#display {store=S}, Scr) ->
    ets:lookup_element(S, [screen|Scr], #xScreen.cmap).

bitmapUnit(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.bitmap_unit).

bitmapBitOrder(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.bitmap_bit_order).

bitmapPad(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.bitmap_pad).

imageByteOrder(#display {store=S, id=Id}) ->
    ets:lookup_element(S, Id, #xDisplay.byte_order).

displayKeycodes(#display {store=S, id=Id}) ->
    { ets:lookup_element(S, Id, #xDisplay.min_keycode),
      ets:lookup_element(S, Id, #xDisplay.max_keycode)}.
    
    
%% #define NextRequest(dpy)	(((_XPrivDisplay)dpy)->request + 1)
%% #define LastKnownRequestProcessed(dpy)	(((_XPrivDisplay)dpy)->last_request_read)
%% macros for screen oriented applications (toolkit) 
%% #define ScreenOfDisplay(dpy, scr)(&((_XPrivDisplay)dpy)->screens[scr])
%% #define DefaultScreenOfDisplay(dpy) ScreenOfDisplay(dpy,DefaultScreen(dpy))
%% #define DisplayOfScreen(s)	((s)->display)
%% #define RootWindowOfScreen(s)	((s)->root)
%% #define BlackPixelOfScreen(s)	((s)->black_pixel)
%% #define WhitePixelOfScreen(s)	((s)->white_pixel)
%% #define DefaultColormapOfScreen(s)((s)->cmap)
%% #define DefaultDepthOfScreen(s)	((s)->root_depth)
%% #define DefaultGCOfScreen(s)	((s)->default_gc)
%% #define DefaultVisualOfScreen(s)((s)->root_visual)
%% #define WidthOfScreen(s)	((s)->width)
%% #define HeightOfScreen(s)	((s)->height)
%% #define WidthMMOfScreen(s)	((s)->mwidth)
%% #define HeightMMOfScreen(s)	((s)->mheight)
%% #define PlanesOfScreen(s)	((s)->root_depth)
%% #define CellsOfScreen(s)	(DefaultVisualOfScreen((s))->map_entries)
%% #define MinCmapsOfScreen(s)	((s)->min_maps)
%% #define MaxCmapsOfScreen(s)	((s)->max_maps)
%% #define DoesSaveUnders(s)	((s)->save_unders)
%% #define DoesBackingStore(s)	((s)->backing_store)
%% #define EventMaskOfScreen(s)	((s)->root_input_mask)

openDisplay() ->
    openDisplay("").

openDisplay("") ->
    case os:getenv("DISPLAY") of
	false -> {error, einval};
	""    -> {error, einval};
	Display -> openDisplay(Display)
    end;
openDisplay(DisplayName) ->
    {Host,Num} = case string:tokens(DisplayName, ": ") of
		     [H, D] -> {H,D};
		     [D] -> {"localhost", D}
		 end,
    case string:tokens(Num, ".") of
	[Dpy,Scr] ->
	    xcli:open(Host,list_to_integer(Dpy),list_to_integer(Scr));
	[Dpy] ->
	    xcli:open(Host,list_to_integer(Dpy),0);
	[] -> 
	    xcli:open(Host,0,0)
    end.
    
closeDisplay(Display) ->
    ok.

createWindow(Display, Parent, X, Y, Width, Height, BorderWidth, Depth,
	     Class, Visual, ValueMask, Attributes) ->
    Wid = xcli:allocId(Display),
    Depth1 = if Depth == 0; Depth == undefined ->
		     defaultDepth(Display, defaultScreen(Display));
		true ->
		     Depth
	     end,
    Parent1 = if Parent == 0; Parent == undefined ->
		      defaultRootWindow(Display);
		 true -> Parent
	      end,
    xcli:send(Display,
	      xproto:createWindow(Wid,
				  Parent1, X, Y, Width, Height, 
				  BorderWidth, Depth1, 
				  Class, Visual, 
				  ValueMask, Attributes)),
    Wid.


createSimpleWindow(Display, Parent, X, Y, Width, Height, 
		   BorderWidth, Border, Background) ->
    Attributes = #xwindow_attributes { background_pixel = Background,
				       border_pixel = Border },
    ValueMask = ?CWBackPixel bor ?CWBorderPixel,
    createWindow(Display, Parent, X, Y, Width, Height,BorderWidth,
		 0,?CopyFromParent,?CopyFromParent,
		 ValueMask,Attributes).

mapWindow(Display, W) ->
    xcli:send(Display, xproto:mapWindow(W)).

mapSubwindows(Display, W) ->
    xcli:send(Display, xproto:mapSubwindows(W)).

changeWindowAttributes(Display, Window, ValueMask, Attributes) ->
    xcli:send(Display, 
	      xproto:changeWindowAttributes(Window, 
					    ValueMask, Attributes)).

selectInput(Display,Window,Mask) ->
    xcli:send(Display,
	      xproto:changeWindowAttribute(Window,?CWEventMask, Mask)).

setWindowBackground(Display, Window, Pixel) ->
    xcli:send(Display,
	      xproto:changeWindowAttribute(Window, 
					   ?CWBackPixel, Pixel)).

setWindowBackgroundPixmap(Display,Window,Pixmap) ->
    xcli:send(Display,
	      xproto:changeWindowAttribute(Window, ?CWBackPixmap, Pixmap)).

setWindowBorder(Display,Window,Pixel) ->
    xcli:send(Display,
	      xproto:changeWindowAttribute(Window, ?CWBorderPixel, Pixel)).

setWindowBorderPixmap(Display,Window,Pixmap) ->
    xcli:send(Display,
	      xproto:changeWindowAttribute(Window, ?CWBorderPixmap, Pixmap)).

setWindowColormap(Display, Window, Colormap) ->
    xcli:send(Display,
	      xproto:changeWindowAttribute(Window, ?CWColormap, Colormap)).

getWindowAttributes(Display, Window) ->
    case xcli:call(Display, xproto:getGeometry(Window)) of
	Bin when binary(Bin) ->
	    {G,_} = ?xGetGeometryReply_dec(Bin),
	    getWindowAttributes_geom(Display, Window, G);
	Error ->
	    Error
    end.


getWindowAttributes_geom(Display, Window, G) ->
    case xcli:call(Display, xproto:getWindowAttributes(Window)) of
	Bin when binary(Bin) ->
	    {R,_} = ?xGetWindowAttributesReply_dec(Bin),
	    #xWindowAttributes 
	    {  x = G#xGetGeometryReply.x,
	       y = G#xGetGeometryReply.y,
	       width = G#xGetGeometryReply.width,
	       height = G#xGetGeometryReply.height,
	       border_width = G#xGetGeometryReply.borderWidth,
	       depth  = G#xGetGeometryReply.depth, 
	       root   = G#xGetGeometryReply.root,
	       visual = R#xGetWindowAttributesReply.visualID,
	       class  = R#xGetWindowAttributesReply.class,
	       bit_gravity = R#xGetWindowAttributesReply.bitGravity,
	       win_gravity = R#xGetWindowAttributesReply.winGravity,
	       backing_store = R#xGetWindowAttributesReply.backingStore,
	       backing_planes = R#xGetWindowAttributesReply.backingBitPlanes,
	       backing_pixel = R#xGetWindowAttributesReply.backingPixel,
	       save_under = R#xGetWindowAttributesReply.saveUnder,
	       colormap = R#xGetWindowAttributesReply.colormap,
	       map_installed = R#xGetWindowAttributesReply.mapInstalled,
	       map_state = R#xGetWindowAttributesReply.mapState,
	       all_event_masks = R#xGetWindowAttributesReply.allEventMasks,
	       your_event_mask = R#xGetWindowAttributesReply.yourEventMask,
	       do_not_propagate_mask = 
	           R#xGetWindowAttributesReply.doNotPropagateMask,
	       override_redirect = R#xGetWindowAttributesReply.override,
	       screen = fixme
	      };
	Error ->
	    Error
    end.



configureWindow(Display, Window, ValueMask, Values) ->
    xcli:send(Display, 
	      xproto:configureWindow(Window,ValueMask,Values)).

moveWindow(Display, Window, X, Y) ->
    configureWindow(Display, Window,
		    ?CWX bor ?CWY,
		    #xWindowChanges {x = X, y = Y }).

resizeWindow(Display, Window, Width, Height) ->
    configureWindow(Display, Window,
		    ?CWWidth bor ?CWHeight,
		    #xWindowChanges {width = Width, height = Height }).

moveResizeWindow(Display, Window, X, Y, Width, Height) ->
    configureWindow(Display, Window,
		    ?CWX bor ?CWY bor ?CWWidth bor ?CWHeight,
		    #xWindowChanges {x = X, y = Y,
				     width = Width, height = Height }).

setWindowBorderWidth(Display, Window, Width) ->
    configureWindow(Display, Window,
		    ?CWBorderWidth,
		    #xWindowChanges {border_width = Width }).


getGeometry(Display, Window) ->
    case xcli:call(Display, xproto:getGeometry(Window)) of
	Bin when binary(Bin) ->
	    {Rec,_} = ?xGetGeometryReply_dec(Bin),
	    #xGeometry { x = Rec#xGetGeometryReply.x,
			 y = Rec#xGetGeometryReply.y,
			 width = Rec#xGetGeometryReply.width,
			 height = Rec#xGetGeometryReply.height,
			 border_width = Rec#xGetGeometryReply.borderWidth,
			 depth = Rec#xGetGeometryReply.depth,
			 root = Rec#xGetGeometryReply.root };
	Error ->
	    Error
    end.
    

createPixmap(Display, D, Width, Height, Depth) ->
    Pid = xcli:allocId(Display),
    Depth1 = if Depth == undefined ->
		     defaultDepth(Display, defaultScreen(Display));
		 true ->
		     Depth
	     end,
    xcli:send(Display, xproto:createPixmap(Pid,D,Width,Height,Depth1)),
    Pid.


freePixmap(Display, Pixmap) ->
    xcli:send(Display, xproto:freePixmap(Pixmap)).

createGC(Display, D, ValueMask, Values) ->
    Gc = xcli:allocId(Display),
    xcli:send(Display, xproto:createGC(Gc, D, ValueMask, Values)),
    Gc.

copyGC(Display, Src, ValueMask, Dest) ->
    xcli:send(Display, xproto:copyGC(Src, ValueMask, Dest)).

changeGC(Display, Gc, ValueMask, Values) ->
    %% FIXME? updated the shadow gc then use flushGC do the
    %% real change, flushGC must then be called when ever some
    %% drawing is to take place.
    xcli:send(Display, xproto:changeGC(Gc, ValueMask, Values)).

freeGC(Display, Gc) ->
    xcli:send(Display, xproto:freeGC(Gc)).

setLineAttributes(Display, Gc, LineWidth, LineStyle, CapStyle, JoinStyle) ->
    Values = #xgc_values {
	      line_width = LineWidth,
	      line_style = LineStyle,
	      cap_style = CapStyle,
	      join_style = JoinStyle },
    changeGC(Display, Gc, 
	     ?GCLineWidth bor
	     ?GCLineStyle bor
	     ?GCCapStyle bor
	     ?GCJoinStyle, Values).

setFillStyle(Display, Gc, FillStyle) ->
    Values = #xgc_values { fill_style = FillStyle },
    changeGC(Display, Gc, ?GCFillStyle, Values).

setFillRule(Display, Gc, FillRule) ->
    Values = #xgc_values { fill_rule = FillRule },
    changeGC(Display, Gc, ?GCFillStyle, Values).

setArcMode(Display, Gc, ArcMode) ->
    Values = #xgc_values { arc_mode = ArcMode },
    changeGC(Display, Gc, ?GCArcMode, Values).    

setState(Display, Gc, ForeGround, BackGround, Function, PlaneMask) ->
    Values = #xgc_values { function = Function,
			   plane_mask = PlaneMask,
			   foreground = ForeGround,
			   background = BackGround },
    changeGC(Display, Gc, ?GCFunction bor ?GCPlaneMask bor
	     ?GCForeground bor ?GCBackground, Values).

setFunction(Display, Gc, Function) ->
    Values = #xgc_values { function = Function },
    changeGC(Display, Gc, ?GCFunction, Values).

setPlaneMask(Display, Gc, PlaneMask) ->
    Values = #xgc_values { plane_mask = PlaneMask },
    changeGC(Display, Gc, ?GCPlaneMask, Values).    

setForeground(Display, Gc, ForeGround) ->
    Values = #xgc_values { foreground = ForeGround },
    changeGC(Display, Gc, ?GCForeground, Values).

setBackground(Display, Gc, BackGround) ->
    Values = #xgc_values { background = BackGround },
    changeGC(Display, Gc, ?GCBackground, Values).


setClipOrigin(Display, Gc, ClipXOrigin, ClipYOrigin) ->
    Values = #xgc_values { clip_x = ClipXOrigin,
			   clip_y = ClipYOrigin },
    changeGC(Display, Gc, 
	     ?GCClipXOrigin bor
	     ?GCClipYOrigin, Values).


setDashes(Display, Gc, DashOffset, DashList) ->
    xcli:send(Display, xproto:setDashes(Gc, DashOffset, DashList)).

clearArea(Display, Window, X, Y, Width, Height, Exposures) ->
    %% fixme Width=0  => With=width(W)-X
    %%       Height=0 => Height=height(W)-Y
    xcli:send(Display,
	      xproto:clearArea(Window, X, Y, Width, Height, Exposures)).

clearWindow(Display, Window) ->
    clearArea(Display, Window, 0, 0, 0, 0, ?False).

copyArea(Display, Src, Dest, Gc, SrcX, SrcY, Width, Height, DestX,DestY) ->
    cxli:send(Display, 
	      xproto:copyArea(Src, Dest, Gc, SrcX, SrcY, Width, Height, 
			      DestX, DestY)).

copyPlane(Display,Src,Dest,Gc,SrcX,SrcY,Width,Height,DestX,DestY,Plane) ->
    cxli:send(Display, 
	      xproto:copyPlane(Src, Dest, Gc, SrcX, SrcY, Width, Height, 
			       DestX, DestY,Plane)). 

drawPoint(Display, D, Gc, X, Y) ->
    xcli:send(Display, xproto:polyPoint(D,Gc,X,Y)).

drawPoints(Display, D, Gc, Points, Mode) ->
    xcli:send(Display, xproto:polyPoints(D,Gc,Points,Mode)).    

drawLine(Display, D, Gc, X1, Y1, X2, Y2) ->
    xcli:send(Display, xproto:polyLine(D, Gc, X1,Y2,X2,Y2)).

drawLines(Display, D, Gc, Points, Mode) ->
    xcli:send(Display, xproto:polyLines(D, Mode, Gc, Points)).

drawSegments(Display, D, Gc, Segments) ->
    xcli:send(Display, xproto:polySegments(D, Gc, Segments)).

drawRectangle(Display, D, Gc, X, Y, Width, Height) ->
    xcli:send(Display, xproto:polyRectangle(D, Gc, X,Y,Width,Height)).

drawRectangles(Display, D, Gc, Rectangles) ->
    xcli:send(Display, xproto:polyRectangles(D, Gc, Rectangles)).

drawArc(Display, D, Gc, X, Y, Width, Height, Angle1, Angle2) ->
    xcli:send(Display, xproto:polyArc(D, Gc, X, Y, Width, Height, 
				      Angle1, Angle2)). 

drawArcs(Display, D, Gc, Arcs) ->
    xcli:send(Display, xproto:polyArcs(D, Gc, Arcs)).

fillRectangle(Display, D, Gc, X, Y, Width, Height) ->
    xcli:send(Display, xproto:polyFillRectangle(D, Gc, X, Y, Width, Height)).

fillRectangles(Display, D, Gc, Rects) ->
    xcli:send(Display, xproto:polyFillRectangle(D, Gc, Rects)).

fillPolygon(Display, D, Gc, Points, Shape, Mode) ->
    xcli:send(Display, xproto:fillPoly(D,Gc,Shape,Mode,Points)).


fillArc(Display, D, Gc, X, Y, Width, Height, Angle1, Angle2) ->
    xcli:send(Display, xproto:fillArc(D, Gc, X, Y, Width, Height, 
				      Angle1, Angle2)).

fillArcs(Display, D, Gc, Arcs) ->
    xcli:send(Display, xproto:fillArcs(D, Gc, Arcs)).


drawText(Display, D, Gc, X, Y, Items) ->
    xcli:send(Display, xproto:polyText8(D, Gc, X, Y, Items)).

drawText16(Display, D, Gc, X, Y, Items) ->
    xcli:send(Display, xproto:polyText16(D, Gc, X, Y, Items)).

drawImageString(Display, D, Gc, X, Y, String) ->
    xcli:send(Display, xproto:imageText8(D, Gc, X, Y, String)).

drawImageString16(Display, D, Gc, X, Y, String) ->
    xcli:send(Display, xproto:imageText16(D, Gc, X, Y, String)).

drawString(Display, D, Gc, X, Y, String) ->
    drawText(Display, D, Gc, X, Y, make_text_items(String)).

drawString16(Display, D, Gc, X, Y, String) ->
    drawText16(Display, D, Gc, X, Y, make_text_items(String)).


loadFont(Display, Name) ->
    Fid = xcli:allocId(Display),
    xcli:send(Display, xproto:openFont(Fid,Name)),
    Fid.

queryFont(Display, Fid) ->
    case xcli:call(Display, xproto:queryFont(Fid)) of
	Bin when binary(Bin) ->
	    Bin;
	Error -> Error
    end.
	    
loadQueryFont(Display, Name) ->
    Fid = loadFont(Display, Name),
    queryFont(Display, Fid).
    
    
unloadFont(Display, Fid) ->
    xcli:send(Display, xproto:closeFont(Fid)).

listFonts(Display, Pattern, MaxNames) ->
    nyi.


allocColor(Display, Cmap, Color) ->
    case xcli:call(Display, xproto:allocColor(Cmap, 
					      Color#xColor.red,
					      Color#xColor.green,
					      Color#xColor.blue)) of
	Bin when binary(Bin) ->
	    { Rec, _} = ?xAllocColorReply_dec(Bin),
	    #xColor { pixel = Rec#xAllocColorReply.pixel,
		      red   = Rec#xAllocColorReply.red,
		      green = Rec#xAllocColorReply.green,
		      blue  = Rec#xAllocColorReply.blue };
	Error ->
	    Error
    end.

allocColor(Display, Color) ->
    allocColor(Display, defaultColormap(Display, defaultScreen(Display)),
	       Color).


%% return {Dir,Ascent,Descent,#xCharStruct}
queryTextExtents(Display, Fid, Text) ->
    case xcli:call(Display, xproto:queryTextExtents(Fid, Text)) of
	Bin when binary(Bin) ->
	    {Rec,_} = ?xQueryTextExtentsReply_dec(Bin),
	    {Rec#xQueryTextExtentsReply.drawDirection,
	     Rec#xQueryTextExtentsReply.fontAscent,
	     Rec#xQueryTextExtentsReply.fontDescent,
	     #xCharStruct { lbearing = Rec#xQueryTextExtentsReply.overallLeft,
			    rbearing = Rec#xQueryTextExtentsReply.overallRight,
			    width =  Rec#xQueryTextExtentsReply.overallWidth,
			    ascent=  Rec#xQueryTextExtentsReply.overallAscent,
			    descent= Rec#xQueryTextExtentsReply.overallDescent
			    }};
	Error ->
	    Error
    end.


%% return { KeyCodes }
getKeyboardMapping(Display, FirstKeyCode, KeyCodeCount) ->
    case xcli:call(Display, 
		   xproto:getKeyboardMapping(FirstKeyCode, KeyCodeCount)) of
	Bin when binary(Bin) ->
	    {Rec,Bin1} = ?xGetKeyboardMappingReply_dec(Bin),
	    N = Rec#xGetKeyboardMappingReply.keySymsPerKeyCode,
	    decode_keyboard_mapping(Bin1, [], [], 0, N);
	Error ->
	    Error
    end.

getModifierMapping(Display) ->
    case xcli:call(Display, xproto:getModifierMapping()) of
	Bin when binary(Bin) ->
	    {Rec,Bin1} = ?xGetModifierMappingReply_dec(Bin),
	    N = Rec#xGetModifierMappingReply.numKeyPerModifier,
	    decode_modifier_mapping(Bin1, [], [], 0, N);
	Error ->
	    Error
    end.


autoRepeatOn(Display) ->
    changeKeyboardControl(Display, ?KBAutoRepeatMode,
	         #xKeyboardControl { auto_repeat_mode = ?AutoRepeatModeOn }).

autoRepeatOff(Display) ->
    changeKeyboardControl(Display, ?KBAutoRepeatMode,
		 #xKeyboardControl { auto_repeat_mode = ?AutoRepeatModeOff}).

bell(Display, Percent) ->
    xcli:send(Display, xproto:bell(Percent)).

keyClick(Display, Percent) ->
    changeKeyboardControl(Display, ?KBKeyClickPercent,
			  #xKeyboardControl { key_click_percent = Percent}).
    
	      
    
setPointerMapping(Display, Map) ->
    xcli:send(Display, xproto:setPointerMapping(Map)).

    
getPointerMapping(Display) ->
    case xcli:call(Display, xproto:getPointerMapping()) of
	Bin when binary(Bin) ->
	    {Rec,Bin1} = ?xGetPointerMappingReply_dec(Bin),
	    N = Rec#xGetPointerMappingReply.nElts,
	    <<Elems:N/binary, _/binary>> = Bin1,
	    binary_to_list(Elems);
	Error ->
	    Error
    end.

getKeyboardControl(Display) ->
    case xcli:call(Display, xproto:getKeyboardControl()) of
	Bin when binary(Bin) ->
	    {Rec, Bin1} = ?xGetKeyboardControlReply_dec(Bin),
	    #xKeyboardState {
             key_click_percent = Rec#xGetKeyboardControlReply.keyClickPercent,
	     bell_percent = Rec#xGetKeyboardControlReply.bellPercent,
             bell_picth = Rec#xGetKeyboardControlReply.bellPitch,
             bell_duration = Rec#xGetKeyboardControlReply.bellDuration,
             led_mask = Rec#xGetKeyboardControlReply.ledMask,
             global_auto_repeat=Rec#xGetKeyboardControlReply.globalAutoRepeat,
             auto_repeats = Rec#xGetKeyboardControlReply.map
	     };
	Error -> Error
    end.

changeKeyboardControl(Display, ValueMask, Values) ->
    xcli:send(Display, xproto:changeKeyboardControl(ValueMask, Values)).

		   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_keyboard_mapping(Bin, Acc, Map, N, N) ->
    decode_keyboard_mapping(Bin, [], [reverse(Acc)|Map], 0, N);
decode_keyboard_mapping(<<?KEYSYM(Sym),Bin/binary>>, Acc, Map, I, N) ->
    decode_keyboard_mapping(Bin, [Sym|Acc], Map, I+1, N);
decode_keyboard_mapping(<<>>, [], Map, 0, N) ->
    reverse(Map).

decode_modifier_mapping(Bin, Acc, Map, N, N) ->
    decode_modifier_mapping(Bin, [], [reverse(Acc)|Map], 0, N);
decode_modifier_mapping(<<?KEYCODE(Code),Bin/binary>>, Acc, Map, I, N) ->
    decode_modifier_mapping(Bin, [Code|Acc], Map, I+1, N);
decode_modifier_mapping(<<>>, [], Map, 0, N) ->
    reverse(Map).


					  
%% Convert a "long" string into TextItems chunks
make_text_items(String) ->
    map(fun(Chunk) -> 
		#xTextItem { chars = Chunk }
	end, xproto:textSplit(String, 254)).
