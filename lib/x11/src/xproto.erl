%%% File    : xproto.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : xproto.erl xprotocol basic request/reply
%%% Created : 27 Jan 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(xproto).

-include("x.hrl").

-include("xproto.gx.hrl").


-compile(export_all).
-import(lists, [map/2, reverse/1, duplicate/2]).

%% 1
createWindow(Wid, Parent, X, Y, Width, Height, BorderWidth, Depth,
	     Class, Visual, ValueMask, Attributes) ->
    req(?X_CreateWindow, Depth,
	<< ?xCreateWindowReq(Wid,Parent,X,Y,Width,Height,
	 BorderWidth,Class,Visual,ValueMask), 
	 (window_attributes(ValueMask, Attributes))/binary>>).


%% 2
changeWindowAttributes(Window, ValueMask, Attributes) ->
    req(?X_ChangeWindowAttributes, 0,
	<<?xChangeWindowAttributesReq(Window,ValueMask),
	 (window_attributes(ValueMask, Attributes))/binary>>).

changeWindowAttribute(Window, Bit, Attribute) ->
    req(?X_ChangeWindowAttributes, 0,
	<<?xChangeWindowAttributesReq(Window,Bit),
	 Attribute:32>>).

%% 3
getWindowAttributes(Window) ->
    req(?X_GetWindowAttributes, 0, 
	<<?xResourceReq(Window)>>).
    
%% 4
destroyWindow(Window) ->
    req(?X_DestroyWindow, 0,
	<<?xResourceReq(Window)>>).
    
%% 5
destroySubwindows(Window) ->
    req(?X_DestroySubwindows, 0, 
	<<?xResourceReq(Window)>>).

%% 6
changeSaveSet() ->
    nyi.

%% 7
reparentWindow(Window) ->
    nyi.

%% 8
mapWindow(Window) ->
    req(?X_MapWindow, 0, 
	<<?xResourceReq(Window)>>).

%% 9
mapSubWindows(Window) ->
    req(?X_MapSubwindows, 0, 
	<<?xResourceReq(Window)>>).

%% 10
unmapWindow(Window) ->
    req(?X_UnmapWindow, 0, 
	<<?xResourceReq(Window)>>).

%% 11
unmapSubwindows(Window) ->
    req(?X_UnmapSubwindows, 0, 
	<<?xResourceReq(Window)>>).

%% 12
configureWindow(Window, ValueMask, Values) ->
    req(?X_ConfigureWindow, 0,
	<<?xConfigureWindowReq(Window, ValueMask, 0),
	 (window_changes(ValueMask, Values))/binary>>).

%% 13
circulateWindow(Window, Direction) ->
    req(?X_CirculateWindow, Direction, 
	<<?xCirculateWindowReq(Window)>>).

%% 14
getGeometry(Window) ->
    req(?X_GetGeometry, 0, 
	<<?xResourceReq(Window)>>).

%% 16
internAtom(OnlyIfExist, NBytes, Atom) ->
    req(?X_InternAtom, OnlyIfExist,
	<<?xInternAtomReq(NBytes, 0), 
	 (list_to_binary(Atom))/binary>>).

%% 17
getAtomName(N) ->
    req(?X_GetAtomName, 0,
	<<?ATOM(N)>>).

%% 45
openFont(Fid, Name) ->
    NBytes = length(Name),
    req(?X_OpenFont, 0,
	<< ?xOpenFontReq(Fid, NBytes, 0, 0), 
	 (list_to_binary(Name))/binary>>).
    
%% 46
closeFont(Fid) ->
    req(?X_CloseFont, 0, 
	<<?xResourceReq(Fid)>>).

%% 47
queryFont(Fid) ->
    req(?X_QueryFont, 0,
	<<?xResourceReq(Fid)>>).


	
%% 48
queryTextExtents(Fid, Text) ->
    N = length(Text),
    BinText16 = list_to_binary(map(fun(C) -> <<?CARD16(C)>> end, Text)),
    req(?X_QueryTextExtents, N band 1,
	<<?xQueryTextExtentsReq(Fid), BinText16/binary>>).


%% 53
createPixmap(Pid,Drawable,Width,Height,Depth) ->
    req(?X_CreatePixmap, Depth,
	<<?xCreatePixmapReq(Pid,Drawable,Width,Height)>>).

%% 54
freePixmap(Pid) ->
    req(?X_FreePixmap, 0,
	<<?xResourceReq(Pid)>>).

%% 55
createGC(Gc, Drawable, ValueMask, Values) ->
    req(?X_CreateGC, 0,
	<<?xCreateGCReq(Gc, Drawable, ValueMask),
	 (gcvalues(ValueMask, Values))/binary >>).

%% 56 
changeGC(Gc, ValueMask, Values) ->
    req(?X_ChangeGC, 0,
	<<?xChangeGCReq(Gc, ValueMask),
	 (gcvalues(ValueMask, Values))/binary >>).

%% 57
copyGC(SrcGC, ValueMask, DstGC) ->    
    req(?X_CopyGC, 0,
	<<?xCopyGCReq(SrcGC,DstGC,ValueMask)>>).

%% 58 
setDashes(Gc, DashOffset, Dashes) ->
    Bin = list_to_binary(Dashes),
    N = size(Bin),
    req(?X_SetDashes, 0,
	<<?xSetDashesReq(Gc,DashOffset,N),
	 Bin/binary>>).
    
%% 60
freeGC(Gc) ->
    req(?X_FreeGC, 0,
	<<?xResourceReq(Gc)>>).
    
%% 61
clearArea(Window, X, Y, Width, Height, Exposures) ->
    req(?X_ClearArea, Exposures,
	<<?xClearAreaReq(Window, X, Y, Width, Height)>>).

%% 62
copyArea(Src, Dest, Gc, SrcX, SrcY, Width, Height, DestX, DestY) ->
    req(?X_CopyArea, 0,
	<<?xCopyAreaReq(Src,Dest,Gc,SrcX,SrcY,DestX,DestY,Width,Height)>>).

%% 63
copyPlane(Src, Dest, Gc, SrcX, SrcY, Width, Height, DstX, DstY, BitPlane) ->
    req(?X_CopyPlane, 0,
	<<?xCopyPlaneReq(Src,Dest,Gc,SrcX,SrcY,DstX,DstY,
	 Width,Height,BitPlane)>>).

%% 64
polyPoints(Drawable, CoordMode, Gc, Points) ->
    req(?X_PolyPoint, CoordMode, 
	<<?xPolyPointReq(Drawable,Gc),
	 (points(Points))/binary>>) .

polyPoint(Drawable, Gc, X, Y) ->
    req(?X_PolyPoint, 0, 
	<<?xPolyPointReq(Drawable,Gc),
	 ?xPoint(X,Y)>> ).

%% 65
polyLines(Drawable, CoordMode, Gc, Points) ->
    req(?X_PolyLine, CoordMode, 
	<<?xPolyLineReq(Drawable,Gc),
	 (points(Points))/binary>>).

polyLine(Drawable, Gc, X1,Y1,X2,Y2) ->
    req( ?X_PolyLine, 0,
	 <<?xPolyLineReq(Drawable,Gc),
	  ?xPoint(X1,Y1), ?xPoint(X2,Y2)>> ).
    
%% 66
polySegments(Drawable, Gc, Segments) ->
    req(?X_PolySegment, 0,
	<<?xPolySegmentReq(Drawable,Gc),
	 (segments(Segments))/binary>>).    

%% 67
polyRectangles(Drawable, Gc, Rects) ->
    req(?X_PolyRectangle, 0,
	<<?xPolyRectangleReq(Drawable,Gc),
	 (rectangles(Rects))/binary>>).    

polyRectangle(Drawable, Gc, X, Y, Width, Height) ->
    req(?X_PolyRectangle, 0,
	<<?xPolyRectangleReq(Drawable,Gc),
	 ?xRectangle(X,Y,Width,Height) >>).
    
%% 68
polyArcs(Drawable, Gc, Arcs) ->
    req(?X_PolyArc, 0,
	<<?xPolyArcReq(Drawable,Gc),
	 (arcs(Arcs))/binary>>).

polyArc(Drawable, Gc, X, Y, Width, Height, Angle1, Angle2) ->
    req(?X_PolyArc, 0,
	<<?xPolyArcReq(Drawable,Gc),
	 ?xArc(X,Y,Width,Height,Angle1,Angle2) >>).

%% 69
fillPoly(Drawable, Gc, Shape, CoordMode, Points) ->
    req(?X_FillPoly, 0,
	<<?xFillPolyReq(Drawable,Gc,Shape,CoordMode,0),
	 (points(Points))/binary>>).

%% 70
polyFillRectangles(Drawable, Gc, Rects) ->
    req(?X_PolyFillRectangle, 0,
	<<?xPolyFillRectangleReq(Drawable,Gc),
	 (rectangles(Rects))/binary>>). 

polyFillRectangle(Drawable, Gc, X, Y, Width, Height) ->
    req(?X_PolyFillRectangle, 0,
	<<?xPolyFillRectangleReq(Drawable,Gc),
	 ?xRectangle(X,Y,Width,Height)>>).

    
%% 71
polyFillArcs(Drawable, Gc, Arcs) ->
    req(?X_PolyFillArc, 0,
	<<?xPolyFillArcReq(Drawable,Gc),
	 (arcs(Arcs))/binary>>). 

polyFillArc(Drawable, Gc,  X, Y, Width, Height, Angle1, Angle2) ->
    req(?X_PolyFillArc, 0,
	<<?xPolyFillArcReq(Drawable,Gc),
	 ?xArc(X,Y,Width,Height,Angle1,Angle2) >>).

%% 72
putImage(Drawable,Gc,Width,Height,DstX,DstY,LeftPad,Depth,Data) ->
    req(?X_PutImage, 0,
	<< ?xPutImageReq(Drawable,Gc,Width,Height,DstX,DstY,LeftPad,Depth,0),
	 Data/binary>>).

%% 73
getImage(Drawable,X,Y,Width,Height,PlaneMask) ->
    req(?X_GetImage, 0,
	<< ?xGetImageReq(Drawable,X,Y,Width,Height,PlaneMask) >>).

%% 74
polyText8(Drawable, Gc, X, Y, Items) ->
    req(?X_PolyText8,0,
	<<?xPolyText8Req(Drawable,Gc,X,Y),
	 (text8items(Items))/binary>>).

%% 75
polyText16(Drawable, Gc, X, Y, Items) ->
    req(?X_PolyText16,0,
	<<?xPolyText16Req(Drawable,Gc,X,Y),
	 (text16items(Items))/binary>>).

%% 77  (FIXME NChars > 255)
imageText8(Drawable, Gc, X, Y, String) ->
    NChars = length(String),
    Bin = string8(String),
    req(?X_ImageText8,NChars,
	<< ?xImageText8Req(Drawable, Gc, X, Y), Bin/binary>>).

%% 75 (FIXME NChars > 255)
imageText16(Drawable, Gc, X, Y, String) ->
    NChars = length(String),
    Bin = string16(String),
    NChars = size(Bin),
    req(?X_ImageText16,NChars,
	<< ?xImageText16Req(Drawable, Gc, X, Y), Bin/binary>>).

%% 84
allocColor(Cmap, R, G, B) ->
    req(?X_AllocColor, 0,
	<<?xAllocColorReq(Cmap, R, G, B, 0)>>).

%% 85 
allocNamedColor(Cmap, Name) ->
    NBytes = length(Name),
    req(?X_AllocNamedColor, 0,
	<<?xAllocNamedColorReq(Cmap, NBytes, 0, 0),
	 (list_to_binary(Name))/binary>>).

%% 86 
allocColorCells() ->
    nyi.
    
%% 87
allocColorPlanes() ->
    nyi.

%% 88 
freeColors() ->
    nyi.

%% 89
storeColors() ->
    nyi.

%% 90
storeNamedColor() ->
    nyi.

%% 91
queryColors() ->
    nyi.

%% 92
lookupColor() ->
    nyi.

%% 100
changeKeyboardMapping(First, KeySymsPerKeycode, KeySyms, NumCodes) ->
    nyi.

%% 101
getKeyboardMapping(FirstKeyCode, KeyCodeCount) ->
    req(?X_GetKeyboardMapping, 0,
	<<?xGetKeyboardMappingReq(FirstKeyCode,KeyCodeCount,0)>>).

%% 102
changeKeyboardControl(Mask, Control) ->
    req(?X_ChangeKeyboardControl, 0, 
	<<?xChangeKeyboardControlReq(Mask),
	 (keyboard_control(Mask, Control))/binary>>).
    

%% 103
getKeyboardControl() ->
    req(?X_GetKeyboardControl, 0, <<>>).


%% 104
bell(Percent) ->
    req(?X_Bell, Percent, <<>>). 


%% 116
setPointerMapping(Map) ->
    req(?X_SetPointerMapping, 0, list_to_binary(Map)).

%% 117
getPointerMapping() ->
    req(?X_GetPointerMapping, 0, <<>>).

%% 118
setModifierMapping(Map) ->
    nyi.

%% 119
getModifierMapping() ->
    req(?X_GetModifierMapping, 0, <<>>).

%%%%%%%%  EOF REQUESTS %%%%%%%%%%%%%%%%%%%


%% encoding of data structures
points([#xPoint {x=X, y=Y}|T]) ->
    <<?xPoint(X,Y), (points(T))/binary>>;
points([]) -> <<>>.

segments([#xSegment{x1=X1,y1=Y1,x2=X2,y2=Y2}|T]) ->
    <<?xSegment(X1,Y1,X2,Y2), (segments(T))/binary>>;
segments([]) -> <<>>.

rectangles([#xRectangle{x=X,y=Y,width=W,height=H} |T]) ->
    <<?xRectangle(X,Y,W,H),(rectangles(T))/binary>>;
rectangles([]) -> <<>>.

arcs([#xArc{x=X,y=Y,width=W,height=H,angle1=A1,angle2=A2}|T]) ->
    <<?xArc(X,Y,W,H,A1,A2),(arcs(T))/binary>>;
arcs([]) -> <<>>.


text8items(Items) ->
    list_to_binary(text8i(Items)).

text8i([#xTextItem { chars=S, delta=D, font=F} | Es]) ->
    B1 = textFont(F),
    {B2,D1} = textDelta(D),
    B3 = text8chunks(textSplit(S,254), D1),
    [B1, B2, B3 | text8i(Es)];
text8i([]) ->
    [].

text16items(Items) ->
    list_to_binary(text16i(Items)).

text16i([#xTextItem { chars=S, delta=D, font=F} | Es]) ->
    B1 = textFont(F),
    {B2,D1} = textDelta(D),
    B3 = text16chunks(textSplit(S,254), D1),
    [B1, B2, B3 | text16i(Es)];
text16i([]) ->
    [].

textFont(?None) -> <<>>;
textFont(F) -> <<255, ?uint32(F,big)>>.

textDelta(D) when D < 127, D >= -128 -> {[], D};
textDelta(D) when D > 0 ->
    N = D div 127,
    {duplicate(N, <<?xTextElt(0, 127)>>), D rem 127};
textDelta(D) ->
    N = D div -128,
    {duplicate(N, <<?xTextElt(0, -128)>>), D rem -128}.
    

text16chunks([Chunk|Chunks], D) ->
    Len = length(Chunk),
    [ <<?xTextElt(Len, D), (string16(Chunk))/binary>> |
      text16chunks(Chunks, 0)];
text16chunks([], D) when D > 0 ->
    [ <<?xTextElt(0, D)>> ];
text16chunks([], 0) ->
    [].

text8chunks([Chunk|Chunks], D) ->
    Len = length(Chunk),
    [ <<?xTextElt(Len, D), (string8(Chunk))/binary>> |
      text8chunks(Chunks, 0)];
text8chunks([], D) when D > 0 ->
    [ <<?xTextElt(0, D)>> ];
text8chunks([], 0) ->
    [].

    
%% Split a text into chunks of at more "ChunkSize" in length
textSplit(String, ChunkSize) ->
    if length(String) > ChunkSize ->
	    textSplit(String, [], [], 0, ChunkSize);
       true ->
	    [String]
    end.

textSplit(String, Acc, Ls, N, N) ->
    textSplit(String, [], [reverse(Acc)|Ls], 0, N);
textSplit([H|T], Acc, Ls, I, N) ->
    textSplit(T, [H|Acc], Ls, I+1, N);
textSplit([], [], Ls, I, N) ->
    reverse(Ls);
textSplit([], Acc, Ls, I, N) ->
    reverse([reverse(Acc)|Ls]).

string8(String) when length(String) < 255 ->
    list_to_binary(String).

string16(String) when length(String) < 255 ->
    list_to_binary(map(fun(I) -> <<?CARD16(I)>> end, String)).
    

gcvalues(ValueMask, Values) ->
    list_to_binary(
      [
       if ValueMask band ?GCFunction =/= 0 ->
	       [<<?CARD32((Values#xgc_values.function))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCPlaneMask =/= 0 ->
	       [<<?CARD32((Values#xgc_values.plane_mask))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCForeground =/= 0 ->
	       [<<?CARD32((Values#xgc_values.foreground))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCBackground =/= 0 ->
	       [<<?CARD32((Values#xgc_values.background))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCLineWidth =/= 0 ->
	       [<<?CARD32((Values#xgc_values.line_width))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCLineStyle =/= 0 ->
	       [<<?CARD32((Values#xgc_values.line_style))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCCapStyle =/= 0 ->
	       [<<?CARD32((Values#xgc_values.cap_style))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCJoinStyle =/= 0 ->
	       [<<?CARD32((Values#xgc_values.join_style))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCFillStyle =/= 0 ->
	       [<<?CARD32((Values#xgc_values.fill_style))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCFillRule =/= 0 ->
	       [<<?CARD32((Values#xgc_values.fill_rule))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCTile =/= 0 ->
	       [<<?CARD32((Values#xgc_values.tile))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCStipple =/= 0 ->
	       [<<?CARD32((Values#xgc_values.stipple))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCTileStipXOrigin =/= 0 ->
	       [<<?CARD32((Values#xgc_values.tile_stipple_x))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCTileStipYOrigin =/= 0 ->
	       [<<?CARD32((Values#xgc_values.tile_stipple_y))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCFont =/= 0 ->
	       [<<?CARD32((Values#xgc_values.font))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCSubwindowMode =/= 0 ->
	       [<<?CARD32((Values#xgc_values.subwin_mode))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCGraphicsExposures =/= 0 ->
	       [<<?CARD32((Values#xgc_values.exposures))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCClipXOrigin =/= 0 ->
	       [<<?CARD32((Values#xgc_values.clip_x))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCClipYOrigin =/= 0 ->
	       [<<?CARD32((Values#xgc_values.clip_y))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCClipMask =/= 0 ->
	       [<<?CARD32((Values#xgc_values.clip_mask))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCDashOffset =/= 0 ->
	       [<<?CARD32((Values#xgc_values.dash_offset))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCDashList =/= 0 -> %% FIXME
	       [<<?CARD32((Values#xgc_values.dashes))>>];
	  true ->
	       []
       end,
       if ValueMask band ?GCArcMode =/= 0 ->
	       [<<?CARD32((Values#xgc_values.arc_mode))>>];
	  true ->
	       []
       end
      ]).



window_attributes(ValueMask, Attributes) ->
    list_to_binary(
    [ 
      if ValueMask band ?CWBackPixmap =/= 0 ->
	      [<<(Attributes#xwindow_attributes.background_pixmap):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWBackPixel =/= 0 ->
	      [<<(Attributes#xwindow_attributes.background_pixel):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWBorderPixmap =/= 0 ->
	      [<<(Attributes#xwindow_attributes.border_pixmap):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWBorderPixel =/= 0 ->
	      [<<(Attributes#xwindow_attributes.border_pixel):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWBitGravity =/= 0 ->
	      [<<(Attributes#xwindow_attributes.bit_gravity):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWWinGravity =/= 0 ->
	      [<<(Attributes#xwindow_attributes.win_gravity):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWBackingStore =/= 0 ->
	      [<<(Attributes#xwindow_attributes.backing_store):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWBackingPlanes =/= 0 ->
	      [<<(Attributes#xwindow_attributes.backing_planes):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWBackingPixel =/= 0 ->
	      [<<(Attributes#xwindow_attributes.backing_pixel):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWOverrideRedirect =/= 0 ->
	      [<<(Attributes#xwindow_attributes.override_redirect):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWSaveUnder =/= 0 ->
	      [<<(Attributes#xwindow_attributes.save_under):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWEventMask =/= 0 ->
	      [<<(Attributes#xwindow_attributes.event_mask):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWDontPropagate =/= 0 ->
	      [<<(Attributes#xwindow_attributes.dont_propagate):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWColormap =/= 0 ->
	      [<<(Attributes#xwindow_attributes.colormap):32 >> ];
	 true ->
	      []
      end,

      if ValueMask band ?CWCursor =/= 0 ->
	      [<<(Attributes#xwindow_attributes.cursor):32 >> ];
	 true ->
	      []
      end
      ]).

keyboard_control(ValueMask, Control) ->    
    list_to_binary(
      [
       if ValueMask band ?KBKeyClickPercent =/= 0 ->
	       [<<(Control#xKeyboardControl.key_click_percent):32>>];
	  true -> []
       end,
       if ValueMask band ?KBBellPercent =/= 0 ->
	       [<<(Control#xKeyboardControl.bell_percent):32>>];
	  true -> []
       end,
       if ValueMask band ?KBBellPitch =/= 0 ->
	       [<<(Control#xKeyboardControl.bell_pitch):32>>];
	  true -> []
       end,
       if ValueMask band ?KBBellDuration =/= 0 ->
	       [<<(Control#xKeyboardControl.bell_duration):32>>];
	  true -> []
       end,
       if ValueMask band ?KBLed =/= 0 ->
	       [<<(Control#xKeyboardControl.led):32>>];
	  true -> []
       end,
       if ValueMask band ?KBLedMode =/= 0 ->
	       [<<(Control#xKeyboardControl.led_mode):32>>];
	  true -> []
       end,
       if ValueMask band ?KBKey =/= 0 ->
	       [<<(Control#xKeyboardControl.key):32>>];
	  true -> []
       end,
       if ValueMask band ?KBAutoRepeatMode =/= 0 ->
	       [<<(Control#xKeyboardControl.auto_repeat_mode):32>>];
	  true -> []
       end
      ]).


window_changes(ValueMask, Change) ->
    list_to_binary(
      [
       if ValueMask band ?CWX =/= 0 ->
	       [<<(Change#xWindowChanges.x):32>>];
	  true -> []
       end,
       if ValueMask band ?CWY =/= 0 ->
	       [<<(Change#xWindowChanges.y):32>>];
	  true -> []
       end,
       if ValueMask band ?CWWidth =/= 0 ->
	       [<<(Change#xWindowChanges.width):32>>];
	  true -> []
       end,
       if ValueMask band ?CWHeight =/= 0 ->
	       [<<(Change#xWindowChanges.height):32>>];
	  true -> []
       end,
       if ValueMask band ?CWBorderWidth =/= 0 ->
	       [<<(Change#xWindowChanges.border_width):32>>];
	  true -> []
       end,
       if ValueMask band ?CWSibling =/= 0 ->
	       [<<(Change#xWindowChanges.sibling):32>>];
	  true -> []
       end,
       if ValueMask band ?CWStackMode =/= 0 ->
	       [<<(Change#xWindowChanges.stack_mode):32>>];
	  true -> []
       end
      ]).


connClientPrefix(Order, Auth) ->
    AuthString = Auth#xauth.data,
    AuthProto  = Auth#xauth.name,
    SzString = size(AuthString),
    SzProto  = size(AuthProto),
    <<Order:8,
     0,
     ?X_PROTOCOL:16,
     ?X_PROTOCOL_REVISION:16,
     SzProto:16,
     SzString:16,
     0:16,
     AuthProto/binary,   %% auth-proto-name
     (?PAD4(SzProto))/binary,
     AuthString/binary,
     (?PAD4(SzString))/binary>>.

%%
%% Format the request and pad to 32 bit 
%%

req(ReqType, Code, Request) ->
    Sz = size(Request),
    Pad = ?PAD4(Sz),
    Length = ((Sz + size(Pad)) bsr 2)+1,
    <<?CARD8(ReqType), ?CARD8(Code), ?CARD16(Length),
     Request/binary, Pad/binary>>.

