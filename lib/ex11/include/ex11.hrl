-ifndef(_EX11_H).
-define(_EX11_H, 1).

%%% ---------------------------------------------------------------------
%%% Created:  15 Feb 1999 by Tobbe, tobbe@cslab.ericsson.se
%%% Function: Include file, with basic X data.
%%% ====================================================================
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.0, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is x11-0-1
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1999, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): Vlad Dumitrescu, vlad_dumitrescu@hotmail.com.
%%%
%%% ---------------------------------------------------------------------

%% --------------------------------------------------
%% Client byte order.
%% (This Erlang implementation of X11 is always running
%%  as a MSB client. The bit manipulating primitives in
%%  Erlang will isolate us from the underlying machine.)

-define(MSB_BYTEORDER, $B).
-define(LSB_BYTEORDER, $l).

%% ------------
%% Misc Values

-define(COPY_FROM_PARENT,   0).
-define(NONE,               0).
-define(FALSE,              0).
-define(TRUE,               1).
-define(PARENT_RELATIVE,    1).

%% --------------
%% Set of Events

-define(EVENT_KEY_PRESS,             16#00000001).
-define(EVENT_KEY_RELEASE,           16#00000002).
-define(EVENT_BUTTON_PRESS,          16#00000004).
-define(EVENT_BUTTON_RELEASE,        16#00000008).
-define(EVENT_ENTER_WINDOW,          16#00000010).
-define(EVENT_LEAVE_WINDOW,          16#00000020).
-define(EVENT_POINTER_MOTION,        16#00000040).
-define(EVENT_POINTER_MOTION_HINT,   16#00000080).
-define(EVENT_BUTTON1_MOTION,        16#00000100).
-define(EVENT_BUTTON2_MOTION,        16#00000200).
-define(EVENT_BUTTON3_MOTION,        16#00000400).
-define(EVENT_BUTTON4_MOTION,        16#00000800).
-define(EVENT_BUTTON5_MOTION,        16#00001000).
-define(EVENT_BUTTON_MOTION,         16#00002000).
-define(EVENT_KEYMAP_STATE,          16#00004000).
-define(EVENT_EXPOSURE,              16#00008000).
-define(EVENT_VISIBILITY_CHANGE,     16#00010000).
-define(EVENT_STRUCTURE_NOTIFY,      16#00020000).
-define(EVENT_RESIZE_REDIRECT,       16#00040000).
-define(EVENT_SUBSTRUCTURE_NOTIFY,   16#00080000).
-define(EVENT_SUBSTRUCTURE_REDIRECT, 16#00100000).
-define(EVENT_FOCUS_CHANGE,          16#00200000).
-define(EVENT_PROPERTY_CHANGE,       16#00400000).
-define(EVENT_COLORMAP_CHANGE,       16#00800000).
-define(EVENT_OWNER_GRAB_BUTTON,     16#01000000).

%% ---------------
-define(BIT_GRAVITY_FORGET, 0).
-define(BIT_GRAVITY_NORTHWEST, 1).
-define(BIT_GRAVITY_NORTH, 2).
-define(BIT_GRAVITY_NORTHEAST, 3).
-define(BIT_GRAVITY_WEST, 4).
-define(BIT_GRAVITY_CENTER, 5).
-define(BIT_GRAVITY_EAST, 6).
-define(BIT_GRAVITY_SOUTHWEST, 7).
-define(BIT_GRAVITY_SOUTH, 8).
-define(BIT_GRAVITY_SOUTHEAST, 9).
-define(BIT_GRAVITY_STATIC, 10).

-define(WIN_GRAVITY_UNMAP, 0).
-define(WIN_GRAVITY_NORTHWEST, 1).
-define(WIN_GRAVITY_NORTH, 2).
-define(WIN_GRAVITY_NORTHEAST, 3).
-define(WIN_GRAVITY_WEST, 4).
-define(WIN_GRAVITY_CENTER, 5).
-define(WIN_GRAVITY_EAST, 6).
-define(WIN_GRAVITY_SOUTHWEST, 7).
-define(WIN_GRAVITY_SOUTH, 8).
-define(WIN_GRAVITY_SOUTHEAST, 9).
-define(WIN_GRAVITY_STATIC, 10).

-define(KEYBUT_MASK_SHIFT, 16#0001).
-define(KEYBUT_MASK_LOCK, 16#0002).
-define(KEYBUT_MASK_CONTROL, 16#0004).
-define(KEYBUT_MASK_MOD1, 16#0008).
-define(KEYBUT_MASK_MOD2, 16#0010).
-define(KEYBUT_MASK_MOD3, 16#0020).
-define(KEYBUT_MASK_MOD4, 16#0040).
-define(KEYBUT_MASK_MOD5, 16#0080).
-define(KEYBUT_MASK_BUTTON1, 16#0100).
-define(KEYBUT_MASK_BUTTON2, 16#0200).
-define(KEYBUT_MASK_BUTTON3, 16#0400).
-define(KEYBUT_MASK_BUTTON4, 16#0800).
-define(KEYBUT_MASK_BUTTON5, 16#1000).


-define(ATOM_PRIMARY, 1).
-define(ATOM_SECONDARY, 2).
-define(ATOM_ARC, 3).
-define(ATOM_ATOM, 4).
-define(ATOM_BITMAP, 5).
-define(ATOM_CARDINAL, 6).
-define(ATOM_COLORMAP, 7).

%% ----------------------
%% MAJOR DATA STRUCTURES
%% ----------------------

%% ----------------------------------------
%% Handle to Xlib that is given to a user. 

-record(xlib,{pid,   % Pid to the Xlib client process.
	      user,  % Pid to the holder of this handle (the user)
	      db}).  % Corresonding ets database.

%% Guard test
-define(IS_XLIB_HANDLE(X), record(X,xlib)).

%% -----------------------------------------------------
%% Used to create a continuation, when not enough bytes
%% was received, to be able to decode a message.
%% Also used as a test on the result returned from 
%% the decode routine.

-record(more, {cont}).

%% Represents a non-continuation
-define(NO_CONT, no_cont).  

%% Guard tests
-define(NEED_MORE(M), record(M,more)).
-define(IS_CONT(M),   record(M,more)).

%% Call continuation
-define(CONTINUE(Cont,Data), ((Cont#more.cont)(Data)) ).

%% ------------
%% Visual Info

-record(visual,
	{visualid,        % visual id of this visual 
	 class,           % class of screen (monochrome, etc.) 
	 red_mask,        % mask values
	 green_mask, 
	 blue_mask,  
         bits_per_rgb,    % log base 2 of distinct color values 
         map_entries      % color map entries 
	}).

-define(PRINT_VISUAL(V),
	io:format("VISUAL:~n"
		  "  visualid = ~p~n  class = ~p~n"
		  "  red_mask = ~p~n  green_mask = ~p~n"
		  "  blue_mask = ~p~n  bits_per_rgb = ~p~n"
		  "  map_entries = ~p~n",
		  [V#visual.visualid, V#visual.class,
		   V#visual.red_mask, V#visual.green_mask,
		   V#visual.blue_mask, V#visual.bits_per_rgb,
		   V#visual.map_entries])).

%% ----------
%% Depth Info

-record(depth,
        {depth,           % this depth (Z) of the depth 
	 nvisuals,        % number of Visual types at this depth 
	 visuals          % list of visuals possible at this depth
	}).

-define(PRINT_DEPTH(D),
	begin
	io:format("DEPTH:~n"
		  "  depth    = ~p~n  nvisuals = ~p~n",
		  [D#depth.depth, D#depth.nvisuals]),
	    lists:foreach(
	      fun(V) -> ?PRINT_VISUAL(V) end,
	      D#depth.visuals)
	end).

%% ------------
%% Screen Info

-record(screen,
	{root,            % Root window id. 
	 width, height,   % width and height of screen 
	 mwidth, mheight, % width and height of  in millimeters 
	 ndepths,         % number of depths possible 
	 white_pixel,     % White and Black pixel values 
	 black_pixel,      
	 root_depth,      % bits per pixel 
	 cmap,            % default color map 
         backing_store,   % Never, WhenMapped, Always 
         save_unders,
	 max_maps,        % max and min color maps 
	 min_maps, 
         root_input_mask, % initial root input mask
	 depths,          % list of allowable depths on the screen 
	 root_visual,     % root visual 
         default_gc       % GC for the root root visual 
	}).

-define(PRINT_SCREEN(S),
	begin
	io:format("SCREEN:~n"
		  "  root            = ~p~n  width           = ~p~n"
		  "  height          = ~p~n  mwidth          = ~p~n"
		  "  mheight         = ~p~n  ndepths         = ~p~n"
		  "  white_pixel     = ~p~n  black_pixel     = ~p~n"
		  "  root_depth      = ~p~n  cmap            = ~p~n"
		  "  backing_store   = ~p~n  save_unders     = ~p~n"
		  "  max_maps        = ~p~n  min_maps        = ~p~n"
		  "  root_input_mask = ~p~n  root_visual     = ~p~n"
		  "  default_gc      = ~p~n  Ndepths         = ~p~n", 
		  [S#screen.root, S#screen.width, 
		   S#screen.height, S#screen.mwidth, 
		   S#screen.mheight, S#screen.ndepths,        
		   S#screen.white_pixel, S#screen.black_pixel,      
		   S#screen.root_depth, S#screen.cmap,           
		   S#screen.backing_store, S#screen.save_unders,
		   S#screen.max_maps, S#screen.min_maps, 
		   S#screen.root_input_mask, S#screen.root_visual,  
		   S#screen.default_gc, length(S#screen.depths)]),
	    lists:foreach(
	      fun(D) -> ?PRINT_DEPTH(D) end,
	      S#screen.depths)
	end).


%% ------------
%% Format Info

-record(format,
	{depth,
	 bpp,
	 scanline_pad}).

%% -------------
%% Display Info

-record(display,
        {fd,                  % Network socket. 
	 resource_id = 0,
	 resource_max = 0,
	 resource_mask = 0,
	 resource_shift = 0,
	 resource_base = 0,
	 proto_major_version, % major version of server's X protocol
	 proto_minor_version, % minor version of servers X protocol 
	 vendor,              % vendor of the server hardware 
	 byte_order,          % screen byte order, LSBFirst, MSBFirst 
	 bitmap_unit,         % padding and data requirements 
	 bitmap_pad,          % padding requirements on bitmaps 
	 bitmap_bit_order,    % LeastSignificant or MostSignificant
	 nformats,            % number of pixmap formats in list 
	 pixmap_formats,      % pixmap format list
	 release,             % release of the server
	 last_request_read,   % seq number of last event read 
	 request,             % sequence number of last request.
	 max_request_size,    % maximum number 32 bit words in request
	 display_name,        % "host:display" string used on this connect
	 default_screen,      % default screen for operations 
	 nscreens,            % number of screens on this server
         screens,             % list of screens 
	 motion_buffer,       % size of motion buffer
	 min_keycode,         % minimum defined keycode
	 max_keycode          % maximum defined keycode 
	}).

%% Guard test
-define(IS_DISPLAY(D), record(D,display)).

%% Access macros
-define(ROOT_DEPTH(D),   ((hd(D#display.screens))#screen.root_depth) ).
-define(ROOT_ID(D),      ((hd(D#display.screens))#screen.root) ).
-define(WHITE_PIXEL(D),  ((hd(D#display.screens))#screen.white_pixel) ).
-define(BLACK_PIXEL(D),  ((hd(D#display.screens))#screen.black_pixel) ).


%% For debugging
-define(PRINT_DISPLAY(D), 
	begin
	io:format("DISPLAY:~n"
		  "  fd                  = ~p~n  resource_id         = ~p~n"
		  "  resource_max        = ~p~n  resource_mask       = ~p~n"
		  "  resource_shift      = ~p~n  resource_base       = ~p~n"
		  "  proto_major_version = ~p~n  proto_minor_version = ~p~n"
		  "  vendor              = ~p~n  byte_order          = ~p~n"
		  "  bitmap_unit         = ~p~n  bitmap_pad          = ~p~n"
		  "  bitmap_bit_order    = ~p~n  nformats            = ~p~n"
		  "  pixmap_formats      = ~p~n  release             = ~p~n"
		  "  last_request_read   = ~p~n  request             = ~p~n"
		  "  max_request_size    = ~p~n  display_name        = ~p~n"
		  "  default_screen      = ~p~n  nscreens            = ~p~n"
		  "  motion_buffer       = ~p~n  min_keycode         = ~p~n"
		  "  max_keycode         = ~p~n",
		  [D#display.fd, D#display.resource_id,
		   D#display.resource_max, D#display.resource_mask,
		   D#display.resource_shift, D#display.resource_base,
		   D#display.proto_major_version, D#display.proto_minor_version,
		   D#display.vendor, D#display.byte_order,
		   D#display.bitmap_unit, D#display.bitmap_pad,
		   D#display.bitmap_bit_order, D#display.nformats,
		   D#display.pixmap_formats, D#display.release,
		   D#display.last_request_read, D#display.request,
		   D#display.max_request_size, D#display.display_name,
		   D#display.default_screen, D#display.nscreens,
		   D#display.motion_buffer, D#display.min_keycode,
		   D#display.max_keycode]),
	    lists:foreach(
	      fun(S) -> ?PRINT_SCREEN(S) end,
	      D#display.screens)
	end).


%% ----------------
%% Xauthority Info
	 
-record(xauth,
	{family,
	 address,
	 number,
	 name,
	 data}).

-define(PRINT_XAUTH(A),
	io:format("XAUTHORITY:~n"
		  "  family  = ~p~n  address = ~p~n"
		  "  number  = ~p~n  name    = ~p~n"
		  "  data    = ~w~n",
		  [A#xauth.family, A#xauth.address,
		   A#xauth.number, A#xauth.name,
		   A#xauth.data])).

%% ---------------
%% ERROR MESSAGES		  
%% ---------------

%% NB: Special case. 
%%     Error message returned connection is refused 
-record(refused_connection,
	{maj_proto,          % protocol-major-version
	 min_proto,          % protocol-minor-version
	 reason}).           % reason (a string)

-define(ERROR_REQUEST,          1).
-define(ERROR_VALUE,            2).
-define(ERROR_WINDOW,           3).
-define(ERROR_PIXMAP,           4).
-define(ERROR_ATOM,             5).
-define(ERROR_CURSOR,           6).
-define(ERROR_FONT,             7).
-define(ERROR_MATCH,            8).
-define(ERROR_DRAWABLE,         9).
-define(ERROR_ACCESS,          10).
-define(ERROR_ALLOC,           11).
-define(ERROR_COLORMAP,        12).
-define(ERROR_GCONTEXT,        13).
-define(ERROR_IDCHOICE,        14).
-define(ERROR_NAME,            15).
-define(ERROR_LENGTH,          16). 
-define(ERROR_IMPLEMENTATION,  17).

-record(error, 
	{type,            % For example: request | value | window | ...  
	 seqno,           % Sequence number
	 bad_resource_id, % Bad resource id
	 minor_opcode,    % Minor opcode
	 major_opcode}).  % Major opcode

%% Guard test
-define(ERROR(E), record(E,error)).

-define(PRINT_ERROR(E),
	error_logger:error_msg(
	  "ERROR-MSG: type= ~w  seqno= ~w  minor_opcode =~w  major_opcode= ~w  id= ~w~n",
	  [E#error.type,E#error.seqno,E#error.minor_opcode,E#error.major_opcode,E#error.bad_resource_id])).

%% ---------------
%% EVENT MESSAGES		  
%% ---------------

%% Event codes
-define(EVENT_KEYPRESS, 2).
-define(EVENT_KEYRELEASE, 3).
-define(EVENT_BUTTONPRESS, 4).
-define(EVENT_BUTTONRELEASE, 5).
-define(EVENT_MOTION_NOTIFY, 6).
-define(EVENT_ENTER_NOTIFY, 7).
-define(EVENT_LEAVE_NOTIFY, 8).
-define(EVENT_FOCUS_IN, 9).
-define(EVENT_FOCUS_OUT, 10).
-define(EVENT_KEYMAP_NOTIFY, 11).
-define(EVENT_EXPOSE,            12).
-define(EVENT_GRAPHICS_EXPOSURE, 13).
-define(EVENT_NO_EXPOSURE, 14).
-define(EVENT_VISIBILITY_NOTIFY, 15).
-define(EVENT_CREATE_NOTIFY, 16).
-define(EVENT_DESTROY_NOTIFY, 17).
-define(EVENT_UNMAP_NOTIFY, 18).
-define(EVENT_MAP_NOTIFY,        19).
-define(EVENT_MAP_REQUEST, 20).
-define(EVENT_REPARENT_NOTIFY,   21).
-define(EVENT_CONFIGURE_NOTIFY,  22).
-define(EVENT_CONFIGURE_REQUEST, 23).
-define(EVENT_GRAVITY_NOTIFY, 24).
-define(EVENT_RESIZE_REQUEST, 25).
-define(EVENT_CIRCULATE_NOTIFY, 26).
-define(EVENT_CIRCULATE_REQUEST, 27).
-define(EVENT_PROPERTY_NOTIFY, 28).
-define(EVENT_SELECTION_CLEAR, 29).
-define(EVENT_SELECTION_REQUEST, 30).
-define(EVENT_SELECTION_NOTIFY, 31).
-define(EVENT_COLORMAP_NOTIFY, 32).
-define(EVENT_CLIENT_MESSAGE, 33).
-define(EVENT_MAPPING_NOTIFY, 34).

%% Event records
-record(key_press, {seqno}).

-record(key_release, {seqno}).

-record(expose,	{seqno, window, x, y, width, height, count}).

-record(map_notify, {seqno, event, window, override_redirect}).

-record(reparent_notify, {seqno, event, window, parent, x, y, override_redirect}).

-record(configure_notify, {seqno, event, window, above_sibling, x, y, 
			   width, height, border_width, override_redirect}).

%% Event guards
-define(IS_KEY_PRESS_EVENT(E), record(E, key_press)).
-define(IS_KEY_RELEASE_EVENT(E), record(E, key_release)).
-define(IS_EXPOSE_EVENT(E), record(E, expose)).
-define(IS_MAP_NOTIFY_EVENT(E), record(E, map_notify)).
-define(IS_REPARENT_NOTIFY_EVENT(E), record(E, reparent_notify)).
-define(IS_CONFIGURE_NOTIFY_EVENT(E), record(E, configure_notify)).

%% TEMPORARY: Until all events is implemented
-record(event_nyi, {event}).


%% -----------------
%% REQUEST MESSAGES
%% -----------------

%% ---------
%% ClearArea

-record(clear_area,
	{opcode               = 61,
	 exposures=?TRUE,
	 len                  = 4,
	 window,                  % defined by the user
	 x=0,                     % relative origin
	 y=0,                     % relative origin
	 width,
	 height
	}).

%% Guard test
-define(IS_CLEAR_AREA(R), record(R,clear_area)).

%% ---------
%% CopyArea

-record(copy_area,
	{opcode               = 62,
	 len                  = 7,
	 src,  
	 dst,  
	 cid,  
	 src_x=0,                 % relative src origin
	 src_y=0,                 % relative src origin
	 dst_x=0,                 % relative dst origin
	 dst_y=0,                 % relative dst origin
	 width,
	 height
	}).

%% Guard test
-define(IS_COPY_AREA(R), record(R,copy_area)).

%% --------
%% CreateGC

%% Value-mask values
-define(GC_VALUEMASK_FUNCTION,          16#00000001).
-define(GC_VALUEMASK_PLANE_MASK,        16#00000002).
-define(GC_VALUEMASK_FOREGROUND,        16#00000004).
-define(GC_VALUEMASK_BACKGROUND,        16#00000008).
-define(GC_VALUEMASK_LINE_WIDTH,        16#00000010).
-define(GC_VALUEMASK_LINE_STYLE,        16#00000021).
-define(GC_VALUEMASK_CAP_STYLE,         16#00000041).
-define(GC_VALUEMASK_JOIN_STYLE,        16#00000080).
-define(GC_VALUEMASK_FILL_STYLE,        16#00000100).
-define(GC_VALUEMASK_FILL_RULE,         16#00000200).
-define(GC_VALUEMASK_TILE,              16#00000400).
-define(GC_VALUEMASK_STIPPLE,           16#00000800).
-define(GC_VALUEMASK_TILE_STIPPLE_X,    16#00001000).
-define(GC_VALUEMASK_TILE_STIPPLE_Y,    16#00002000).
-define(GC_VALUEMASK_FONT,              16#00004000).
-define(GC_VALUEMASK_SUBWIN_MODE,       16#00008000).
-define(GC_VALUEMASK_EXPOSURES,         16#00010000).
-define(GC_VALUEMASK_CLIP_X,            16#00020000).
-define(GC_VALUEMASK_CLIP_Y,            16#00040000).
-define(GC_VALUEMASK_CLIP_MASK,         16#00080000).
-define(GC_VALUEMASK_DASH_OFFSET,       16#00100000).
-define(GC_VALUEMASK_DASHES,            16#00200000).
-define(GC_VALUEMASK_ARC_MODE,          16#00400000).

-define(IS_SET(Mask,Value), ((Mask band Value) =/= 0) ).
-define(CLR(Mask,Value), (Mask band (bnot Value)) ).

%% Function Values
-define(GC_FUNCTION_CLEAR,                 0).
-define(GC_FUNCTION_AND,                   1).
-define(GC_FUNCTION_AND_REVERSE,           2).
-define(GC_FUNCTION_COPY,                  3).
-define(GC_FUNCTION_AND_INVERTED,          4).
-define(GC_FUNCTION_NO_OP,                 5).
-define(GC_FUNCTION_XOR,                   6).
-define(GC_FUNCTION_OR,                    7).
-define(GC_FUNCTION_NOR,                   8).
-define(GC_FUNCTION_EQUIV,                 9).
-define(GC_FUNCTION_INVERT,               10).
-define(GC_FUNCTION_OR_REVERSE,           11).
-define(GC_FUNCTION_COPY_INVERTED,        12). 
-define(GC_FUNCTION_OR_INVERTED,          13). 
-define(GC_FUNCTION_NAND,                 14).
-define(GC_FUNCTION_SET,                  15).
%% Line-style values
-define(GC_LINESTYLE_SOLID,                0).
-define(GC_LINESTYLE_ON_OFF_DASH,          1).
-define(GC_LINESTYLE_DOUBLE_DASH,          2).
%% Cap-style values
-define(GC_CAPSTYLE_NOT_LAST,              0).
-define(GC_CAPSTYLE_BUTT,                  1).
-define(GC_CAPSTYLE_ROUND,                 2).
-define(GC_CAPSTYLE_PROJECTING,            3).
%% Join-style values
-define(GC_JOINSTYLE_MITER,                0).
-define(GC_JOINSTYLE_ROUND,                1). 
-define(GC_JOINSTYLE_BEVEL,                2).
%% Fill-style values
-define(GC_FILLSTYLE_SOLID,                0).
-define(GC_FILLSTYLE_TILED,                1).
-define(GC_FILLSTYLE_STIPPLED,             2).
-define(GC_FILLSTYLE_OPAQUE_STIPPLED,      3).
%% Fill-rule values
-define(GC_FILLRULE_EVEN_ODD,              0).
-define(GC_FILLRULE_WINDING,               1).
%% Sub-window mode values
-define(GC_SUBWINMODE_CLIP_BY_CHILDREN,    0).
-define(GC_SUBWINMODE_INCLUDE_INFERIORS,   1).
%% Clip-mask values
-define(GC_CLIPMASK_NONE,                  0).
%% Arc-mode values
-define(GC_ARCMODE_CHORD,                  0).
-define(GC_ARCMODE_PIE_SLICE,              1).

-define(GC_DEFAULT_VALUEMASK,
	(?GC_VALUEMASK_FUNCTION bor
	 ?GC_VALUEMASK_LINE_WIDTH bor
	 ?GC_VALUEMASK_LINE_STYLE bor
	 ?GC_VALUEMASK_CAP_STYLE bor
	 ?GC_VALUEMASK_JOIN_STYLE bor
	 ?GC_VALUEMASK_FILL_STYLE bor
	 ?GC_VALUEMASK_FILL_RULE bor
	 ?GC_VALUEMASK_SUBWIN_MODE bor
	 ?GC_VALUEMASK_EXPOSURES bor
	 ?GC_VALUEMASK_CLIP_MASK bor
	 ?GC_VALUEMASK_ARC_MODE)).

-record(gc_values,
	{function = ?GC_FUNCTION_COPY,
	 plane_mask,
	 foreground,
	 background,
	 line_width = 1,
	 line_style = ?GC_LINESTYLE_SOLID,
	 cap_style = ?GC_CAPSTYLE_NOT_LAST,
	 join_style = ?GC_JOINSTYLE_MITER,
	 fill_style = ?GC_FILLSTYLE_SOLID,
	 fill_rule = ?GC_FILLRULE_EVEN_ODD,
	 tile,
	 stipple,
	 tile_stipple_x,
	 tile_stipple_y,
	 font,
	 subwin_mode = ?GC_SUBWINMODE_CLIP_BY_CHILDREN,
	 exposures = ?TRUE,
	 clip_x,
	 clip_y,
	 clip_mask = ?GC_CLIPMASK_NONE,
	 dash_offset,
	 dashes,
	 arc_mode = ?GC_ARCMODE_CHORD
	}).

%% Guard test
-define(IS_GC_VALUES(R), record(R,gc_values)).

-record(create_gc,
	{opcode               = 55,
	 len                  = 4,   % with an empty value-list
	 cid,                        % created by Encode if undefined
	 drawable,                   % have to be defined by the user
	 value_mask           = ?GC_DEFAULT_VALUEMASK,
	 value_list           = #gc_values{}
	}).

%% Guard test
-define(IS_CREATE_GC(R), record(R,create_gc)).

%% ---------
%% ChangeGC

-record(change_gc,
	{opcode               = 56,
	 len                  = 3,   % with an empty value-list
	 cid,                        % have to be defined by the user
	 value_mask           = ?GC_DEFAULT_VALUEMASK,
	 value_list           = #gc_values{}
	}).

%% Guard test
-define(IS_CHANGE_GC(R), record(R,change_gc)).

%% -------------
%% CreatePixmap

-record(create_pixmap,
	{opcode               = 53,
	 depth,                      % defaults to root depth
	 len                  = 4,
	 pid,                        % created by Encode if undefined
	 drawable,                   % have to be defined by the user
	 width,
	 height
	}).

%% Guard test
-define(IS_CREATE_PIXMAP(R), record(R,create_pixmap)).

%% ------------
%% CreateWindow

%% Value-mask values
-define(WIN_VALUEMASK_BG_PIXMAP,        16#00000001).
-define(WIN_VALUEMASK_BG_PIXEL,         16#00000002).
-define(WIN_VALUEMASK_BD_PIXMAP,        16#00000004).
-define(WIN_VALUEMASK_BD_PIXEL,         16#00000008).
-define(WIN_VALUEMASK_BIT_GRAVITY,      16#00000010).
-define(WIN_VALUEMASK_WIN_GRAVITY,      16#00000020).
-define(WIN_VALUEMASK_BACKING_STORE,    16#00000040).
-define(WIN_VALUEMASK_BACKING_PLANES,   16#00000080).
-define(WIN_VALUEMASK_BACKING_PIXEL,    16#00000100).
-define(WIN_VALUEMASK_OVERRIDE_REDIRECT,16#00000200).
-define(WIN_VALUEMASK_SAVE_UNDER,       16#00000400).
-define(WIN_VALUEMASK_EVENT_MASK,       16#00000800).
-define(WIN_VALUEMASK_DO_NOT_PROPAGATE, 16#00001000).
-define(WIN_VALUEMASK_COLORMAP,         16#00002000).
-define(WIN_VALUEMASK_CURSOR,           16#00004000).

%% Background-pixmap values
-define(WIN_BGPIXMAP_NONE,                 0).
-define(WIN_BGPIXMAP_PARENT_RELATIVE,      1).
%% Border-pixmap values
-define(WIN_BDPIXMAP_COPY_FROM_PARENT,     0).
%% Backing-store values
-define(WIN_BACKINGSTORE_NOT_USEFUL,       0).
-define(WIN_BACKINGSTORE_WHEN_MAPPED,      1).
-define(WIN_BACKINGSTORE_ALWAYS,           2).
%% Colormap values     
-define(WIN_COLORMAP_COPY_FROM_PARENT,     0).
%% Cursor values     
-define(WIN_CURSOR_NONE,                   0).

-define(WIN_DEFAULT_VALUEMASK,
	(?WIN_VALUEMASK_BG_PIXMAP bor
	 ?WIN_VALUEMASK_BD_PIXMAP)).

-record(win_values,
	{bg_pixmap            = ?WIN_BGPIXMAP_NONE,
	 bg_pixel,
	 bd_pixmap            = ?WIN_BDPIXMAP_COPY_FROM_PARENT,
	 bd_pixel,
	 bit_gravity,
	 win_gravity,
	 backing_store,
	 backing_planes,
	 backing_pixel,
	 override_redirect,
	 save_under,
	 event_mask,
	 do_not_propagate,
	 colormap,
	 cursor
	}).

%% Guards test
-define(IS_WIN_VALUES(R), record(R,win_values)).

-record(create_window,
	{opcode               = 1,
	 depth,
	 len                  = 8,   % with an empty value-list
	 window,                     % created by Encode if undefined
	 parent,                     % Root if undefined (window isa top-level)
	 x                    = 0,
	 y                    = 0,
	 width                = 200,
	 height               = 200,
	 border_width         = 1,
	 class                = ?COPY_FROM_PARENT,
	 visual               = ?COPY_FROM_PARENT,
	 value_mask           = ?WIN_DEFAULT_VALUEMASK,
	 value_list           = #win_values{}
	}).

%% Guard test
-define(IS_CREATE_WINDOW(R), record(R,create_window)).

%% -------------
%% GetGeometry

-record(get_geometry,
	{opcode               = 14,
	 len                  = 2,
	 drawable
	}).

-record(get_geometry_reply,
	{depth, 
	 len                  = 0,
	 root,
	 x,
	 y,
	 width,
	 height,
	 border_width
	}).

%% Guard test
-define(IS_GET_GEOMETRY(R), record(R,get_geometry)).
-define(IS_GET_GEOMETRY_REPLY(R), record(R,get_geometry_reply)).

%% ----------
%% MapWindow

-record(map_window,
	{opcode               = 8,
	 len                  = 2,
	 window
	}).

%% Guard test
-define(IS_MAP_WINDOW(R), record(R,map_window)).

%% -----------------
%% PolyFillRectangle

-record(rectangle, {x, y, width, height}).

%% Guard test
-define(IS_RECTANGLE(R), record(R,rectangle)).

-record(poly_fill_rectangle,
	{opcode               = 70,
	 len                  = 3,
	 drawable,
	 cid,
	 rectangles           = []
	}).

%% Guard test
-define(IS_POLY_FILL_RECTANGLE(R), record(R,poly_fill_rectangle)).


%% ---------
%% PolyLine

%% Coordinate-mode values
-define(POLY_COORDMODE_ORIGIN,    0).
-define(POLY_COORDMODE_PREVIOUS,  1).

-record(point, {x, y}).

%% Guard test
-define(IS_POINT(R), record(R,point)).

-record(poly_line,
	{opcode               = 65,
	 coord_mode           = ?POLY_COORDMODE_ORIGIN,
	 len                  = 3,
	 drawable,
	 gc,
	 points               % list of point records
	}).
	 
%% Guard test
-define(IS_POLY_LINE(R), record(R,poly_line)).

%% ------------
%% PolySegment

-record(segment, {x1, y1, x2, y2}).

%% Guard test
-define(IS_SEGMENT(R), record(R,segment)).

-record(poly_segment,
	{opcode               = 66,
	 len                  = 3,
	 drawable,
	 gc,
	 segments             % list of segment records
	}).
	 
%% Guard test
-define(IS_POLY_SEGMENT(R), record(R,poly_segment)).


-endif.

