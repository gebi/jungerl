
-ifndef(__X_HRL__).
-define(__X_HRL__, true).

-define(X_PROTOCOL, 11).	 %% current protocol version 
-define(X_PROTOCOL_REVISION, 0). %% current minor version


-define(PAD4_Len(Len), ((4-((Len) band 3)) band 3)).

-define(PAD4(Len),
	case ?PAD4_Len(Len) of
	    0 -> <<>>;
	    1 -> <<0>>;
	    2 -> <<0,0>>;
	    3 -> <<0,0,0>>
     end).

-define(MSB_BYTEORDER, $B).
-define(LSB_BYTEORDER, $l).

-define(True,  1).
-define(False, 0).

-define(None,                0). %% universal null resource or null atom


-define(ParentRelative,      1).	%% background pixmap in CreateWindow
				        %% and ChangeWindowAttributes 

-define(CopyFromParent,      0).	%% border pixmap in CreateWindow
				        %% and ChangeWindowAttributes
				        %% special VisualID and special window
				        %% class passed to CreateWindow 

-define(PointerWindow,       0).	%% destination window in SendEvent 
-define(InputFocus,          1).	%% destination window in SendEvent 

-define(PointerRoot,         1).	%% focus window in SetInputFocus 

-define(AnyPropertyType,     0).	%% special Atom, passed to GetProperty 

-define(AnyKey,              0).	%% special Key Code, passed to GrabKey 

-define(AnyButton,           0).	%% special Button Code, 
                                        %% passed to GrabButton 

-define(AllTemporary,        0).	%% special Resource ID passed to 
                                        %% KillClient 

-define(CurrentTime,         0).	%% special Time 

-define(NoSymbol,            0).	%% special KeySym 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% EVENT DEFINITIONS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Input Event Masks. Used as event-mask window attribute and as arguments
%%  to Grab requests.  Not to be confused with event names.  

-define(NoEventMask,			0).
-define(KeyPressMask,			16#00000001).
-define(KeyReleaseMask,			16#00000002). 
-define(ButtonPressMask,		16#00000004). 
-define(ButtonReleaseMask,		16#00000008). 
-define(EnterWindowMask,		16#00000010). 
-define(LeaveWindowMask,		16#00000020). 
-define(PointerMotionMask,		16#00000040). 
-define(PointerMotionHintMask,		16#00000080).
-define(Button1MotionMask,		16#00000100). 
-define(Button2MotionMask,		16#00000200). 
-define(Button3MotionMask,		16#00000400).
-define(Button4MotionMask,		16#00000800).
-define(Button5MotionMask,		16#00001000).
-define(ButtonMotionMask,		16#00002000).
-define(KeymapStateMask,		16#00004000).
-define(ExposureMask,			16#00008000).
-define(VisibilityChangeMask,		16#00010000).
-define(StructureNotifyMask,		16#00020000).
-define(ResizeRedirectMask,		16#00040000).
-define(SubstructureNotifyMask,		16#00080000).
-define(SubstructureRedirectMask,	16#00100000).
-define(FocusChangeMask,		16#00200000).
-define(PropertyChangeMask,		16#00400000).
-define(ColormapChangeMask,		16#00800000).
-define(OwnerGrabButtonMask,		16#01000000).

%% Event names.  Used in "type" field in XEvent structures.  Not to be
%% confused with event masks above.  They start from 2 because 0 and 1
%% are reserved in the protocol for errors and replies. 

-define(KeyPress,		2).
-define(KeyRelease,		3).
-define(ButtonPress,		4).
-define(ButtonRelease,		5).
-define(MotionNotify,		6).
-define(EnterNotify,		7).
-define(LeaveNotify,		8).
-define(FocusIn,		9).
-define(FocusOut,		10).
-define(KeymapNotify,		11).
-define(Expose,			12).
-define(GraphicsExpose,		13).
-define(NoExpose,		14).
-define(VisibilityNotify,	15).
-define(CreateNotify,		16).
-define(DestroyNotify,		17).
-define(UnmapNotify,		18).
-define(MapNotify,		19).
-define(MapRequest,		20).
-define(ReparentNotify,		21).
-define(ConfigureNotify,	22).
-define(ConfigureRequest,	23).
-define(GravityNotify,		24).
-define(ResizeRequest,		25).
-define(CirculateNotify,	26).
-define(CirculateRequest,	27).
-define(PropertyNotify,		28).
-define(SelectionClear,		29).
-define(SelectionRequest,	30).
-define(SelectionNotify,	31).
-define(ColormapNotify,		32).
-define(ClientMessage,		33).
-define(MappingNotify,		34).
-define(LASTEvent,		35). %% must be bigger than any event # 


%% Key masks. Used as modifiers to GrabButton and GrabKey, 
%% results of QueryPointer,
%% state in various key-, mouse-, and button-related events. 

-define(ShiftMask,		16#0001).
-define(LockMask,		16#0002).
-define(ControlMask,		16#0004).
-define(Mod1Mask,		16#0008).
-define(Mod2Mask,		16#0010).
-define(Mod3Mask,		16#0020).
-define(Mod4Mask,		16#0040).
-define(Mod5Mask,		16#0080).

%% modifier names.  Used to build a SetModifierMapping request or
%% to read a GetModifierMapping request.  These correspond to the
%% masks defined above. 
-define(ShiftMapIndex,		0).
-define(LockMapIndex,		1).
-define(ControlMapIndex,	2).
-define(Mod1MapIndex,		3).
-define(Mod2MapIndex,		4).
-define(Mod3MapIndex,		5).
-define(Mod4MapIndex,		6).
-define(Mod5MapIndex,		7).


%% button masks.  Used in same manner as Key masks above. Not to be confused
%% with button names below. 

-define(Button1Mask,		16#0100).
-define(Button2Mask,		16#0200).
-define(Button3Mask,		16#0400).
-define(Button4Mask,		16#0800).
-define(Button5Mask,		16#1000).

-define(AnyModifier,		16#4000). %% used in GrabButton, GrabKey 


%% button names. Used as arguments to GrabButton and as detail in ButtonPress
%% and ButtonRelease events.  Not to be confused with button masks above.
%%  Note that 0 is already defined above as "AnyButton".  

-define(Button1,		1).
-define(Button2,		2).
-define(Button3,		3).
-define(Button4,		4).
-define(Button5,		5).

%% Notify modes 

-define(NotifyNormal,		0).
-define(NotifyGrab,		1).
-define(NotifyUngrab,		2).
-define(NotifyWhileGrabbed,	3).

-define(NotifyHint,		1).	%% for MotionNotify events 
		       
%% Notify detail 

-define(NotifyAncestor,		0).
-define(NotifyVirtual,		1).
-define(NotifyInferior,		2).
-define(NotifyNonlinear,	3).
-define(NotifyNonlinearVirtual,	4).
-define(NotifyPointer,		5).
-define(NotifyPointerRoot,	6).
-define(NotifyDetailNone,	7).

%% Visibility notify 

-define(VisibilityUnobscured,		0).
-define(VisibilityPartiallyObscured,	1).
-define(VisibilityFullyObscured,	2).

%% Circulation request 

-define(PlaceOnTop,		0).
-define(PlaceOnBottom,		1).

%% protocol families 

-define(FamilyInternet,		0).
-define(FamilyDECnet,		1).
-define(FamilyChaos,		2).

%% Property notification 

-define(PropertyNewValue,	0).
-define(PropertyDelete,		1).

%% Color Map notification 

-define(ColormapUninstalled,	0).
-define(ColormapInstalled,	1).

%% GrabPointer, GrabButton, GrabKeyboard, GrabKey Modes 

-define(GrabModeSync,		0).
-define(GrabModeAsync,		1).

%% GrabPointer, GrabKeyboard reply status 

-define(GrabSuccess,		0).
-define(AlreadyGrabbed,		1).
-define(GrabInvalidTime,	2).
-define(GrabNotViewable,	3).
-define(GrabFrozen,		4).

%% AllowEvents modes 

-define(AsyncPointer,		0).
-define(SyncPointer,		1).
-define(ReplayPointer,		2).
-define(AsyncKeyboard,		3).
-define(SyncKeyboard,		4).
-define(ReplayKeyboard,		5).
-define(AsyncBoth,		6).
-define(SyncBoth,		7).

%% Used in SetInputFocus, GetInputFocus 

-define(RevertToNone,           ?None).
-define(RevertToPointerRoot,	?PointerRoot).
-define(RevertToParent,		2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ERROR CODES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Success		,   0).	%% everything's okay 
-define(BadRequest	,   1).	%% bad request code 
-define(BadValue	,   2).	%% int parameter out of range 
-define(BadWindow	,   3).	%% parameter not a Window 
-define(BadPixmap	,   4).	%% parameter not a Pixmap 
-define(BadAtom		,   5).	%% parameter not an Atom 
-define(BadCursor	,   6).	%% parameter not a Cursor 
-define(BadFont		,   7).	%% parameter not a Font 
-define(BadMatch	,   8).	%% parameter mismatch 
-define(BadDrawable	,   9).	%% parameter not a Pixmap or Window 
-define(BadAccess	,  10).	%% depending on context:
				%% - key/button already grabbed
				%% - attempt to free an illegal 
				%%   cmap entry 
				%% - attempt to store into a read-only 
				%%   color map entry.
 				%%- attempt to modify the access control
				%%   list from other than the local host.
-define(BadAlloc	,  11).	%% insufficient resources 
-define(BadColor	,  12).	%% no such colormap 
-define(BadGC		,  13).	%% parameter not a GC 
-define(BadIDChoice	,  14).	%% choice not in range or already used 
-define(BadName		,  15).	%% font or color name doesn't exist 
-define(BadLength	,  16).	%% Request length incorrect 
-define(BadImplementation, 17).	%% server is defective 

-define(FirstExtensionError,	128).
-define(LastExtensionError,	255).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WINDOW DEFINITIONS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Window classes used by CreateWindow 
%% Note that CopyFromParent is already defined as 0 above 

-define(InputOutput,		1).
-define(InputOnly,		2).

%% Window attributes for CreateWindow and ChangeWindowAttributes 

%% Value-mask values
-define(CWBackPixmap,      16#00000001).
-define(CWBackPixel,       16#00000002).
-define(CWBorderPixmap,    16#00000004).
-define(CWBorderPixel,     16#00000008).
-define(CWBitGravity,      16#00000010).
-define(CWWinGravity,      16#00000020).
-define(CWBackingStore,    16#00000040).
-define(CWBackingPlanes,   16#00000080).
-define(CWBackingPixel,    16#00000100).
-define(CWOverrideRedirect,16#00000200).
-define(CWSaveUnder,       16#00000400).
-define(CWEventMask,       16#00000800).
-define(CWDontPropagate,   16#00001000).
-define(CWColormap,        16#00002000).
-define(CWCursor,          16#00004000).

%% ConfigureWindow structure 
-define(CWX,			16#01).
-define(CWY,			16#02).
-define(CWWidth,		16#04).
-define(CWHeight,		16#08).
-define(CWBorderWidth,		16#10).
-define(CWSibling,		16#20).
-define(CWStackMode,		16#40).


%% Bit Gravity 
-define(ForgetGravity,		0).
-define(NorthWestGravity,	1).
-define(NorthGravity,		2).
-define(NorthEastGravity,	3).
-define(WestGravity,		4).
-define(CenterGravity,		5).
-define(EastGravity,		6).
-define(SouthWestGravity,	7).
-define(SouthGravity,		8).
-define(SouthEastGravity,	9).
-define(StaticGravity,		10).

%% Window gravity + bit gravity above 

-define(UnmapGravity,		0).

%% Used in CreateWindow for backing-store hint 

-define(NotUseful,               0).
-define(WhenMapped,              1).
-define(Always,                  2).

%% Used in GetWindowAttributes reply 

-define(IsUnmapped,		0).
-define(IsUnviewable,		1).
-define(IsViewable,		2).

%% Used in ChangeSaveSet 

-define(SetModeInsert,           0).
-define(SetModeDelete,           1).

%% Used in ChangeCloseDownMode 

-define(DestroyAll,              0).
-define(RetainPermanent,         1).
-define(RetainTemporary,         2).

%% Window stacking method (in configureWindow) 

-define(Above,                   0).
-define(Below,                   1).
-define(TopIf,                   2).
-define(BottomIf,                3).
-define(Opposite,                4).

%% Circulation direction 

-define(RaiseLowest,             0).
-define(LowerHighest,            1).

%% Property modes 

-define(PropModeReplace,         0).
-define(PropModePrepend,         1).
-define(PropModeAppend,          2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRAPHICS DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% graphics functions, as in GC.alu 

-define(GXclear,		16#0).		%% 0
-define(GXand,			16#1).		%% src AND dst
-define(GXandReverse,		16#2).		%% src AND NOT dst
-define(GXcopy,			16#3).		%% src
-define(GXandInverted,		16#4).		%% NOT src AND dst
-define(GXnoop,			16#5).		%% dst
-define(GXxor,			16#6).		%% src XOR dst
-define(GXor,			16#7).		%% src OR dst
-define(GXnor,			16#8).		%% NOT src AND NOT dst
-define(GXequiv,		16#9).		%% NOT src XOR dst
-define(GXinvert,		16#a).		%% NOT dst
-define(GXorReverse,		16#b).		%% src OR NOT dst
-define(GXcopyInverted,		16#c).		%% NOT src
-define(GXorInverted,		16#d).		%% NOT src OR dst 
-define(GXnand,			16#e).		%% NOT src OR NOT dst 
-define(GXset,			16#f).		%% 1 

%% LineStyle 

-define(LineSolid,		0).
-define(LineOnOffDash,		1).
-define(LineDoubleDash,		2).

%% capStyle 

-define(CapNotLast,		0).
-define(CapButt,		1).
-define(CapRound,		2).
-define(CapProjecting,		3).

%% joinStyle 

-define(JoinMiter,		0).
-define(JoinRound,		1).
-define(JoinBevel,		2).

%% fillStyle 

-define(FillSolid,		0).
-define(FillTiled,		1).
-define(FillStippled,		2).
-define(FillOpaqueStippled,	3).

%% fillRule 

-define(EvenOddRule,		0).
-define(WindingRule,		1).

%% subwindow mode 

-define(ClipByChildren,		0).
-define(IncludeInferiors,	1).

%% SetClipRectangles ordering 

-define(Unsorted,		0).
-define(YSorted,		1).
-define(YXSorted,		2).
-define(YXBanded,		3).

%% CoordinateMode for drawing routines 

-define(CoordModeOrigin,	0). %% relative to the origin
-define(CoordModePrevious,      1). %% relative to previous point

%% Polygon shapes 

-define(Complex,		0).  %% paths may intersect 
-define(Nonconvex,		1).  %% no paths intersect, but not convex 
-define(Convex,			2).  %% wholly convex 

%% Arc modes for PolyFillArc 

-define(ArcChord,		0).  %% join endpoints of arc
-define(ArcPieSlice,		1).  %% join endpoints to center of arc 

%% GC components: masks used in CreateGC, CopyGC, ChangeGC, OR'ed into
%% GC.stateChanges 

-define(GCFunction,             16#00000001).
-define(GCPlaneMask,            16#00000002).
-define(GCForeground,           16#00000004).
-define(GCBackground,           16#00000008).
-define(GCLineWidth,            16#00000010).
-define(GCLineStyle,            16#00000020).
-define(GCCapStyle,             16#00000040).
-define(GCJoinStyle,		16#00000080).
-define(GCFillStyle,		16#00000100).
-define(GCFillRule,		16#00000200).
-define(GCTile,			16#00000400).
-define(GCStipple,		16#00000800).
-define(GCTileStipXOrigin,	16#00001000).
-define(GCTileStipYOrigin,	16#00002000).
-define(GCFont, 	        16#00004000).
-define(GCSubwindowMode,	16#00008000).
-define(GCGraphicsExposures,    16#00010000).
-define(GCClipXOrigin,		16#00020000).
-define(GCClipYOrigin,		16#00040000).
-define(GCClipMask,		16#00080000).
-define(GCDashOffset,		16#00100000).
-define(GCDashList,		16#00200000).
-define(GCArcMode,		16#00400000).

-define(GCLastBit,		22).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FONTS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% used in QueryFont -- draw direction 

-define(FontLeftToRight,        0).
-define(FontRightToLeft,	1).

-define(FontChange,		255).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  IMAGING 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ImageFormat -- PutImage, GetImage 

-define(XYBitmap,		0).  %% depth 1, XYFormat 
-define(XYPixmap,		1).  %% depth == drawable depth 
-define(ZPixmap,		2). %% depth == drawable depth 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  COLOR MAP STUFF 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% For CreateColormap 

-define(AllocNone,		0). %% create map with no entries 
-define(AllocAll,		1). %% allocate entire map writeable 


%% Flags used in StoreNamedColor, StoreColors 

-define(DoRed,			16#01).
-define(DoGreen,		16#02).
-define(DoBlue,			16#04).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CURSOR STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% QueryBestSize Class 

-define(CursorShape,		0). %% largest size that can be displayed 
-define(TileShape,		1). %% size tiled fastest 
-define(StippleShape,		2). %% size stippled fastest 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% KEYBOARD/POINTER STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(AutoRepeatModeOff,	0).
-define(AutoRepeatModeOn,	1).
-define(AutoRepeatModeDefault,	2).

-define(LedModeOff,		0).
-define(LedModeOn,		1).

%% masks for ChangeKeyboardControl 

-define(KBKeyClickPercent,	16#01).
-define(KBBellPercent,		16#02).
-define(KBBellPitch,		16#04).
-define(KBBellDuration,		16#08).
-define(KBLed,			16#10).
-define(KBLedMode,		16#20).
-define(KBKey,			16#40).
-define(KBAutoRepeatMode,	16#80).

-define(MappingSuccess,     	0).
-define(MappingBusy,        	1).
-define(MappingFailed,		2).

-define(MappingModifier,	0).
-define(MappingKeyboard,	1).
-define(MappingPointer,		2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SCREEN SAVER STUFF 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DontPreferBlanking,	0).
-define(PreferBlanking,		1).
-define(DefaultBlanking,	2).

-define(DisableScreenSaver,	0).
-define(DisableScreenInterval,	0).

-define(DontAllowExposures,	0).
-define(AllowExposures,		1).
-define(DefaultExposures,	2).

%% for ForceScreenSaver 

-define(ScreenSaverReset,  0).
-define(ScreenSaverActive, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HOSTS AND CONNECTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% for ChangeHosts 

-define(HostInsert,		0).
-define(HostDelete,		1).

%% for ChangeAccessControl 

-define(EnableAccess,		1).
-define(DisableAccess,		0).

%% Display classes  used in opening the connection 
%% Note that the statically allocated ones are even numbered and the
%% dynamically changeable ones are odd numbered 

-define(StaticGray,		0).
-define(GrayScale,		1).
-define(StaticColor,		2).
-define(PseudoColor,		3).
-define(TrueColor,		4).
-define(DirectColor,		5).

%% Byte order  used in imageByteOrder and bitmapBitOrder 

-define(LSBFirst,		0).
-define(MSBFirst,		1).


-define(FAMILY_LOCAL,          256).  % not part of X standard (i.e. X.h) 
-define(FAMILY_WILD,         65535). 
-define(FAMILY_NETNAME,        254).  % not part of X standard 
-define(FAMILY_KRB5_PRINCIPAL, 253).  % Kerberos 5 principal name 
-define(FAMILY_LOCALHOST,      252).  % for local non-net authentication 
-define(FAMILY_IP_ADDRESS,       0).  % ...as it seems...

-record(xauth,
	{ family,
	  address,
	  number,
	  name,
	  data
	 }).


-record(xdepth,
	{
	  depth,
	  nvisuals,
	  visuals
	 }).


-record(xColor,
	{
	  pixel,
	  red,
	  green,
	  blue 
	 }).
	 

-record(xPixmapFormatValues, 
	{
	  id,   %% [format|I]
	  depth,
	  bitsPerPixel,
	  scanLinePad
	 }).

%% display structure in ets table id=0
-record(xDisplay,
	{
	  id,
	  fd,
	  proto_major_version,
	  proto_minor_version,
	  vendor,
	  byte_order,
	  bitmap_unit,
	  bitmap_pad,
	  bitmap_bit_order,
	  nformats,
	  release,
	  max_request_size,
	  display_name,
	  nscreens,
	  motion_buffer,
	  min_keycode,
	  max_keycode,
	  %% resource allocation
	  resource_base,
	  resource_mask,
	  resource_shift,	  
	  resource_imask,
	  %% Other stuff
	  default_screen
	 }).

%% screen structure in ets table id=[sceen|I]
-record(xScreen,
	{ 
	  id,            %% id on form [screen|I]
	  root,          %% root window Id
	  width,         %% width of screen in pixel
	  height,        %% height of sceen in pixel
	  mwidth,        %%  width of screen in mm
	  mheight,       %%  height of screen in mm
	  ndepths,       %% number of depths possible
	  depths,        %% [#xDepths]
	  root_depth,    %% bits per pixel
	  root_visual,   %% visual of root 
	  cmap,          %% Default colormap
	  white_pixel,
	  black_pixel,
	  max_maps,      %% max colormaps
	  min_maps,      %% min colormaps
	  backing_store, %% Never, WhenMapped,Always 
	  save_unders,
	  root_input_mask,
	  default_gc     %% GC for the root root visual
	 }).

%% visual structure in ets table id=[visual|I]
-record(xVisual, 
	{
	  id,      %% id on form [visual|I]
	  class,
	  red_mask,
	  green_mask,
	  blue_mask,
	  bits_per_rgb,
	  map_entries
	 }).

%% key value data
-record(xPrivate,
	{
	  id,     %% uniq id in store
	  value   %% current value (counter etc)
	 }).

%% Mapping from id to onwer
-record(xResource,
	{
	  id,     %% uniq id in store
	  owner   %% owner pid of resource
	 }).

-record(xFontStruct, {
	  fid,               %% Font id for this font
	  data,	             %% hook for extension to hang data 
	  direction,         %% hint about direction the font is painted 
	  min_char_or_byte2, %% first character 
	  max_char_or_byte2, %% last character 
	  min_byte1,         %% first row that exists 
	  max_byte1,         %% last row that exists */
	  all_chars_exist,   %% flag if all characters have non-zero size
	  default_char,      %% char to print for undefined character 
	  n_properties,      %% how many properties there are 
	  properties,        %% pointer to array of additional properties
	  min_bounds,  %% XCharStruct minimum bounds over all existing char
	  max_bounds,  %% XCharStruct maximum bounds over all existing char
	  per_char,    %% XCharStruct, first_char to last_char information 
	  ascent,      %% log. extent above baseline for spacing 
	  descent      %% log. descent below baseline for spacing 
	 }).

%% Shadow GC structure in store
-record(xGc, {
	  id,
	  update_mask = 0,  %% mark items needing update

	  function = ?GXcopy,
	  plane_mask,
	  foreground,
	  background,
	  line_width = 1,
	  line_style = ?LineSolid,
	  cap_style = ?CapNotLast,
	  join_style = ?JoinMiter,
	  fill_style = ?FillSolid,
	  fill_rule = ?EvenOddRule,
	  tile,
	  stipple,
	  tile_stipple_x,
	  tile_stipple_y,
	  font,
	  subwin_mode = ?ClipByChildren,
	  exposures = ?True,
	  clip_x,
	  clip_y,
	  clip_mask = ?None,
	  dash_offset,
	  dashes,
	  arc_mode = ?ArcChord
	 }).

-record(xWindow, {
	  id,

	  parent,     %% parent id
	  x,
	  y,
	  width,
	  height,
	  

	  background_pixmap = ?None,
	  background_pixel,
	  border_pixmap     = ?CopyFromParent,
	  border_pixel,
	  bit_gravity,
	  win_gravity,
	  backing_store,
	  backing_planes,
	  backing_pixel,
	  save_under,
	  event_mask,
	  dont_propagate,
	  override_redirect,
	  colormap,
	  cursor
	 }).
	  

-record(xCharStruct,
	{
	  lbearing,
	  rbearing,
	  width,
	  ascent,
	  descent,
	  attributes
	 }).


-record(xTextItem,
	{
	  chars = "",
	  delta = 0,
	  font  = ?None
	 }).


-record(display,
	{
	  store,   %% storage for data
	  owner,   %% xcli pid
	  id,      %% id of xDisplay structure in store
	  fd       %% descriptor to x server
	 }).

-record(request,
	{
	  ref,     %% reference value for reply
	  pid      %% requestor's pid
	 }).

-record(xKeyboardControl,
	{
	  key_click_percent,
	  bell_percent,
	  bell_pitch,
	  bell_duration,
	  led,
	  led_mode,
	  key,
	  auto_repeat_mode
	 }).

-record(xKeyboardState,
	{
	  key_click_percent,
	  bell_percent,
	  bell_picth,
	  bell_duration,
	  led_mask,
	  global_auto_repeat,
	  auto_repeats
	 }).


-define(AllPlanes, 16#ffffffff).

-define(GCDefaultValueMask,
	(?GCFunction bor
	 ?GCLineWidth bor
	 ?GCLineStyle bor
	 ?GCCapStyle bor
	 ?GCJoinStyle bor
	 ?GCFillStyle bor
	 ?GCFillRule bor
	 ?GCSubwindowMode bor
	 ?GCGraphicsExposures bor
	 ?GCClipMask bor
	 ?GCArcMode)).

-record(xwindow_attributes,
	{
	  background_pixmap = ?None,
	  background_pixel,
	  border_pixmap     = ?CopyFromParent,
	  border_pixel,
	  bit_gravity,
	  win_gravity,
	  backing_store,
	  backing_planes,
	  backing_pixel,
	  save_under,
	  event_mask,
	  dont_propagate,
	  override_redirect,
	  colormap,
	  cursor
	 }).


-record(xgc_values,
	{
	  function = ?GXcopy,
	  plane_mask,
	  foreground,
	  background,
	  line_width = 1,
	  line_style = ?LineSolid,
	  cap_style = ?CapNotLast,
	  join_style = ?JoinMiter,
	  fill_style = ?FillSolid,
	  fill_rule = ?EvenOddRule,
	  tile,
	  stipple,
	  tile_stipple_x,
	  tile_stipple_y,
	  font,
	  subwin_mode = ?ClipByChildren,
	  exposures = ?True,
	  clip_x,
	  clip_y,
	  clip_mask = ?None,
	  dash_offset,
	  dashes,
	  arc_mode = ?ArcChord
	 }).

-record(xWindowChanges, 
	{
	  x,
	  y,
	  width,
	  height,
	  border_width,
	  sibling,
	  stack_mode
	 }).

%% return value of GetGeometry 
-record(xGeometry, {
	  x,
	  y,
	  width,
	  height,
	  border_width,
	  depth,
	  root
	 }).


-record(xWindowAttributes, {
	  x,
	  y,
	  width,
	  height,
	  border_width,
	  depth,
	  visual,
	  root,
	  class,
	  bit_gravity,
	  win_gravity,
	  backing_store,
	  backing_planes,
	  backing_pixel,
	  save_under,
	  colormap,
	  map_installed,
	  map_state,
	  all_event_masks,
	  your_event_mask,
	  do_not_propagate_mask,
	  override_redirect,
	  screen
	 }).


-record(xImage,
	{
	  width,       %% Image width
	  height,      %% Image height
	  xoffset,     %% Number of pixels offset in X direction
	  format,      %% XYBitmap, XYPixmap, ZPixmap
	  data,        %% Image data
	  byte_order,  %% LSBFirst, MSBFirst
	  bitmap_unit,  %% quant of scanline 8,16,32
	  bitmap_bit_order,  %% LSBFirst, MSBFirst
	  bitmap_pad,        %% 8, 16, 32 either XY or ZPixmap
	  depth,             %% depth of Image
	  bytes_per_line,    %% Number of bytes per line
	  bits_per_pixel,    %% bits per pixel (ZPixmap)
	  red_mask,
	  green_mask,
	  blue_mask
	 }).
	  

-endif.
