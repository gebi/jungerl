-module(ex11_proto).
-author('tnt@home.se').
%%%---------------------------------------------------------------------
%%% Created : 14 Feb 1999 by tnt@home.se
%%% Function: The X11 protocol requests and events.
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
%%% Contributor(s): ______________________________________.
%%%
%%%
%%% Modified: 23 Feb 1998 by tony@cslab.ericsson.se
%%%           To make it work under Windows.
%%%
%%%---------------------------------------------------------------------
-vc('$Id$ ').
-export([map_window/2,create_window/2,decode_error/1,
	 setup_connection/1,decode_refused_connection/1,
	 decode/2,decode_connect_reply/2,encode/2]).

-import(ex11_utils,[i16/1,i16/2,i32/1,i32/4,split_list/2]).
-import(error_logger,[error_msg/2]).

-include("ex11.hrl").

%% -----------------------------------------------------
%% Setup Connection.
%% NB: The connect phase is a special case, and doesn't
%%     follow the regularity of X11 protocol.
%%     Only one authentication scheme is implemented.
%% -----------------------------------------------------

setup_connection([]) ->
    <<?MSB_BYTEORDER:8,         % Erlang byte-order
     0:8,                      % unused
     11:16,                % proto-major-ver
     0:16,                    % proto-minor-ver
     0:16,                 % proto-name-len
     0:16,                 % proto-data-len
     0,0>>;                   % unused (pad)
setup_connection(Cookie) ->
    Len = length(Cookie),
    Pad = add_pad(Cookie),
    <<?MSB_BYTEORDER:8,         % Erlang byte-order
     0:8,                      % unused
     11:16,                % proto-major-ver
     0:16,                    % proto-minor-ver
     18:16,                % proto-name-len
     Len:16,    % proto-data-len
     0:16,                    % unused
     "MIT-MAGIC-COOKIE-1",   % auth-proto-name
     0:16,                    % pad
     Cookie,                 % auth-proto-data
     Pad>>.       % pad

decode_refused_connection(<<Len,ProtoMaj:16,ProtoMin:16,_TotLen:16,D/binary>>) ->
    <<ReasonB:Len/binary, _D1/binary>> = D,
    Reason = binary_to_list(ReasonB),
    #refused_connection{maj_proto=ProtoMaj,
			min_proto=ProtoMin,
			reason=Reason}.
    
decode_connect_reply(Fd,Msg) ->
    <<_:32, _Len:16, RelNo:32, ResBase:32, ResMask0:32,
     MbufSz:32, VendorLen:16, MaxReqLen:16,
     Screens, Formats,
     ImOrder, BmapOrder, BmapScanU, BmapScanP,
     MinKCode, MaxKCode, _:32,
     B/binary>> = list_to_binary(Msg),
    VendorPad = bpad(VendorLen),
    PmapFormLen = 8*Formats,
    <<VendorB:VendorLen/binary,
     _:VendorPad,
     PmapForm:PmapFormLen/binary,
     PmapScreen/binary>> = B,

    {ResMask,ResShift} = resource_calc(ResMask0),
    Vendor = binary_to_list(VendorB),
    Format = decode_format(Formats,PmapForm),
    Screen = decode_screen(Screens,PmapScreen),

    Display = #display{
      resource_mask=ResMask,
      resource_base=ResBase,
      resource_shift=ResShift,
      release=RelNo,
      motion_buffer=MbufSz,
      max_request_size=MaxReqLen,
      bitmap_bit_order=BmapOrder,
      bitmap_unit=BmapScanU,
      bitmap_pad=BmapScanP,
      byte_order=ImOrder,
      vendor=Vendor,
      min_keycode=MinKCode,
      max_keycode=MaxKCode,
      nscreens=Screens,
      screens=Screen,
      nformats=Formats,
      pixmap_formats=Format,
      fd=Fd},
    {ok,Display}.

%% Eh...what a hell am I doing here ? See OpenDis.c l.374
resource_calc(ResMask0) ->
    resource_calc(ResMask0,ResMask0,0).

resource_calc(ResMask,Mask,ResShift) when (Mask bor 1) =/= 0 ->
    {(ResMask bsr ResShift) - 5, % ResMask
     ResShift};
resource_calc(ResMask,Mask,ResShift) ->
    resource_calc(ResMask,Mask bsr 1,ResShift+1).

decode_format(0, _) -> [];
decode_format(N, <<Depth,
		  Bpp,
		  ScanlinePad,
		  _:40,
		  T/binary>>) when N>0 ->
    Format = #format{depth=Depth,
		     bpp=Bpp,
		     scanline_pad=ScanlinePad
	    },
    [Format | decode_format(N-1,T)].

decode_screen(0, _) -> [];
decode_screen(N, <<Window:32,
		  Colormap:32,
		  WhitePixel:32, BlackPixel:32,
		  CinpMask:32,
		  WidthInPixel:16, HeightInPixel:16,
		  WidthInMm:16, HeightInMm:16,
		  MinInstMaps:16, MaxInstMaps:16,
		  RootVisual:32,
		  Bs, Su, Rd, Nd,
		  D/binary>>) when N>0 ->

    {Depths, Rest} = decode_depth(Nd,D),
    Screen = #screen{root=Window,
		     cmap=Colormap,
		     white_pixel=WhitePixel,
		     black_pixel=BlackPixel,
		     root_input_mask=CinpMask,
		     width=WidthInPixel,
		     height=HeightInPixel,
		     mwidth=WidthInMm,
		     mheight=HeightInMm,
		     max_maps=MaxInstMaps,
		     min_maps=MinInstMaps,
		     root_visual=RootVisual,
		     backing_store=Bs,
		     save_unders=Su,
		     depths=Depths,
		     root_depth=Rd},
    [Screen | decode_screen(N-1,Rest)].
    
decode_depth(Nd,D) -> decode_depth(Nd,D,[]).

decode_depth(0,Rest,Acc) -> {lists:reverse(Acc),Rest};
decode_depth(Nd,<<Depth,_,NoVisuals:16,_:32,T/binary>>,Acc) when Nd>0 ->
    NV = 24*NoVisuals,
    <<Vis:NV/binary,Rest/binary>> = T,
    Visuals = decode_visuals(NoVisuals, Vis),
    D = #depth{depth=Depth,
	       nvisuals=NoVisuals,
	       visuals=Visuals},
    decode_depth(Nd-1, Rest, [D|Acc]).

decode_visuals(0,_) -> [];
decode_visuals(Nv,<<VisualId:32,Class,BpRGB,ColMapEnt:16,
		   RedMask:32, GreenMask:32, BlueMask:32,
		   _:32, Rest/binary>>) when Nv>0 ->
    Visual = #visual{visualid=VisualId,
		     class=Class,
		     bits_per_rgb=BpRGB,
		     map_entries=ColMapEnt,
		     red_mask=RedMask,
		     green_mask=GreenMask,
		     blue_mask=BlueMask},
    [Visual | decode_visuals(Nv-1,Rest)].

%% ----------------------------------------------------
%% ENCODING REQUEST MESSAGES
%%
%% The encode routine is assumed to be called
%% from the ex11 module, which in its turn 
%% encapsulates the encoding for the user.
%%
%% There is two valid return values from the
%% encode routines: {ok,Msg} and {ok,Msg,Result}.
%%
%% 'Msg' is the encoded message, which is sent to 
%% the X11 client for further transport to the
%% X-server. 
%%
%%'Result' shall be returned to the user since
%% it may contain important data which has been
%% created during the encoding (e.g Window id's).
%% ----------------------------------------------------

encode(X,Req) when ?IS_XLIB_HANDLE(X) -> enc(X,Req).

enc(X,Req) when ?IS_CLEAR_AREA(Req)          -> clear_area(X,Req);
enc(X,Req) when ?IS_COPY_AREA(Req)           -> copy_area(X,Req);
enc(X,Req) when ?IS_CHANGE_GC(Req)           -> change_gc(X,Req);
enc(X,Req) when ?IS_CREATE_GC(Req)           -> create_gc(X,Req);
enc(X,Req) when ?IS_CREATE_PIXMAP(Req)       -> create_pixmap(X,Req);
enc(X,Req) when ?IS_CREATE_WINDOW(Req)       -> create_window(X,Req);
enc(X,Req) when ?IS_GET_GEOMETRY(Req)        -> get_geometry(X,Req);
enc(X,Req) when ?IS_MAP_WINDOW(Req)          -> map_window(X,Req);
enc(X,Req) when ?IS_POLY_FILL_RECTANGLE(Req) -> poly_fill_rectangle(X,Req);
enc(X,Req) when ?IS_POLY_LINE(Req)           -> poly_line(X,Req);
enc(X,Req) when ?IS_POLY_SEGMENT(Req)        -> poly_segment(X,Req);
enc(_,_)                                     -> {error,unknown_request}.


%% -----------------------
%% The Change GC request

change_gc(X,#change_gc{opcode=Opcode, cid=Cid, value_mask=ValueMask}=R) 
  when Cid =/= undefined ->
    ValueList = encode_gc_valuelist(ValueMask, R#change_gc.value_list),
    Len = 3 + length(ValueList),
    {ok,
     <<Opcode, 
      0, % unused
      Len:16,
      Cid:32,
      ValueMask:32,
      ValueList>>}.

%% -----------------------
%% The clear area request

clear_area(X,#clear_area{}=R) when R#clear_area.window =/= undefined,
				   R#clear_area.width =/= undefined,
				   R#clear_area.height =/= undefined ->
    {ok,
     <<(R#clear_area.opcode),
      (R#clear_area.exposures),
      4:16,
      (R#clear_area.window):32,
      (R#clear_area.x):16,
      (R#clear_area.y):16,
      (R#clear_area.width):16,
      (R#clear_area.height):16>>}.
      
%% ----------------------
%% The copy area request

copy_area(X,R) when R#copy_area.src =/= undefined,
		    R#copy_area.dst =/= undefined,
		    R#copy_area.cid =/= undefined,
		    R#copy_area.src_x =/= undefined,
		    R#copy_area.src_y =/= undefined,
		    R#copy_area.dst_x =/= undefined,
		    R#copy_area.dst_y =/= undefined,
		    R#copy_area.width =/= undefined,
		    R#copy_area.height =/= undefined ->
    {ok,
     <<(R#copy_area.opcode),
      0, % unused
      7:16,
      (R#copy_area.src):32,
      (R#copy_area.dst):32,
      (R#copy_area.cid):32,
      (R#copy_area.src_x):16,
      (R#copy_area.src_y):16,
      (R#copy_area.dst_x):16,
      (R#copy_area.dst_y):16,
      (R#copy_area.width):16,
      (R#copy_area.height):16>>}.
      
%% ---------------------
%% The Create GC request

create_gc(X,R) when R#create_gc.drawable =/= undefined ->
    Cid = case R#create_gc.cid of
	      undefined ->
		  ex11:lock_display(X),
		  {ok,Dpy0} = ex11:get_display(X),
		  {Id,Dpy1} = ex11_utils:xalloc_id(Dpy0),
		  ex11:set_display(X,Dpy1),
		  ex11:unlock_display(X),
		  ex11_client:new_gc(X,Id),
		  Id;
	      Cid0 -> Cid0
	  end,
    ValueMask = R#create_gc.value_mask,
    ValueList = encode_gc_valuelist(ValueMask,R#create_gc.value_list),
    {ok,
     [(R#create_gc.opcode),
      0, % unused
      i16(4 + length(ValueList)),
      i32(Cid),
      i32(R#create_gc.drawable),
      i32(ValueMask),
      ValueList],
     {ok,Cid}}.


encode_gc_valuelist(ValueMask,R) when ?IS_GC_VALUES(R) -> 
    enc_gc_vl(ValueMask,R).
    
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FUNCTION) ->
    [i32(R#gc_values.function)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FUNCTION),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_PLANE_MASK) ->
    [i32(R#gc_values.plane_mask)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_PLANE_MASK),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FOREGROUND) ->
    [i32(R#gc_values.foreground)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FOREGROUND),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_BACKGROUND) ->
    [i32(R#gc_values.background)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_BACKGROUND),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_LINE_WIDTH) ->
    [i32(R#gc_values.line_width)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_LINE_WIDTH),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_LINE_STYLE) ->
    [i32(R#gc_values.line_style)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_LINE_STYLE),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_CAP_STYLE) ->
    [i32(R#gc_values.cap_style)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_CAP_STYLE),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_JOIN_STYLE) ->
    [i32(R#gc_values.join_style)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_JOIN_STYLE),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FILL_STYLE) ->
    [i32(R#gc_values.fill_style)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FILL_STYLE),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FILL_RULE) ->
    [i32(R#gc_values.fill_rule)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FILL_RULE),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_TILE) ->
    [i32(R#gc_values.tile)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_TILE),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_STIPPLE) ->
    [i32(R#gc_values.stipple)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_STIPPLE),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_TILE_STIPPLE_X) ->
    [i32(R#gc_values.tile_stipple_x)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_TILE_STIPPLE_X),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_TILE_STIPPLE_Y) ->
    [i32(R#gc_values.tile_stipple_y)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_TILE_STIPPLE_Y),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FONT) ->
    [i32(R#gc_values.font)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FONT),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_SUBWIN_MODE) ->
    [i32(R#gc_values.subwin_mode)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_SUBWIN_MODE),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_EXPOSURES) ->
    [i32(R#gc_values.exposures)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_EXPOSURES),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_CLIP_X) ->
    [i32(R#gc_values.clip_x)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_CLIP_X),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_CLIP_Y) ->
    [i32(R#gc_values.clip_y)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_CLIP_Y),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_CLIP_MASK) ->
    [i32(R#gc_values.clip_mask)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_CLIP_MASK),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_DASH_OFFSET) ->
    [i32(R#gc_values.dash_offset)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_DASH_OFFSET),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_DASHES) ->
    [i32(R#gc_values.dashes)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_DASHES),R)];
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_ARC_MODE) ->
    [i32(R#gc_values.arc_mode)|enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_ARC_MODE),R)];
enc_gc_vl(_,_) -> [].

%% --------------------------
%% The Create Pixmap request

create_pixmap(X,R) when R#create_pixmap.drawable =/= undefined,
			R#create_pixmap.width =/= undefined,
			R#create_pixmap.height =/= undefined ->
    ex11:lock_display(X),
    {ok,Dpy0} = ex11:get_display(X),
    {Pid,Dpy} = case R#create_pixmap.pid of
		    undefined ->
			{Id,Dpy1} = ex11_utils:xalloc_id(Dpy0),
			ex11:set_display(X,Dpy1),
			ex11_client:new_pixmap(X,Id),
			{Id,Dpy1};
		    Pid0 -> {Pid0,Dpy0}
		end,
    ex11:unlock_display(X),
    Depth = case R#create_pixmap.depth of
		undefined -> ?ROOT_DEPTH(Dpy);
		Depth0    -> Depth0
	    end,
    {ok,
     [R#create_pixmap.opcode,
      Depth,
      i16(4),
      i32(Pid),
      i32(R#create_pixmap.drawable),
      i16(R#create_pixmap.width),
      i16(R#create_pixmap.height)],
     {ok,Pid}}.

%% --------------------------
%% The Create Window request

create_window(X,R) ->
    ex11:lock_display(X),
    {ok,Dpy0} = ex11:get_display(X),
    {Win,Dpy} = case R#create_window.window of
		    undefined ->
			{Id,Dpy1} = ex11_utils:xalloc_id(Dpy0),
			ex11:set_display(X,Dpy1),
			ex11_client:new_window(X,Id),
			{Id,Dpy1};
		    Window0 -> {Window0,Dpy0}
		end,
    ex11:unlock_display(X),
    Depth = case R#create_window.depth of
		undefined -> ?ROOT_DEPTH(Dpy);
		Depth0    -> Depth0
	    end,
    %% If Parent is undefined then make
    %% it a window a top-level window.
    Parent = case R#create_window.parent of
		 undefined -> ?ROOT_ID(Dpy); 
		 Parent0   -> Parent0
	     end,
    ValueMask = R#create_window.value_mask,
    ValueList = encode_win_valuelist(ValueMask,
				     R#create_window.value_list),
    {ok,
     [R#create_window.opcode,      
      Depth,      
      i16(8+length(ValueList)),        
      i32(Win), 
      i32(Parent),                 
      i16(R#create_window.x),
      i16(R#create_window.y),
      i16(R#create_window.width),
      i16(R#create_window.height),
      i16(R#create_window.border_width),
      i16(R#create_window.class),
      i32(R#create_window.visual),
      i32(R#create_window.value_mask),
      ValueList],
     {ok,Win}}.

encode_win_valuelist(ValueMask,R) when ?IS_WIN_VALUES(R) ->
    enc_win_vl(ValueMask,R).

enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BG_PIXMAP) ->
    [i32(R#win_values.bg_pixmap)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BG_PIXMAP),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BG_PIXEL) ->
    [i32(R#win_values.bg_pixel)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BG_PIXEL),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BD_PIXMAP) ->
    [i32(R#win_values.bd_pixmap)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BD_PIXMAP),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BD_PIXEL) ->
    [i32(R#win_values.bd_pixel)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BD_PIXEL),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BIT_GRAVITY) ->
    [i32(R#win_values.bit_gravity)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BIT_GRAVITY),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_WIN_GRAVITY) ->
    [i32(R#win_values.win_gravity)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_WIN_GRAVITY),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BACKING_STORE) ->
    [i32(R#win_values.backing_store)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BACKING_STORE),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BACKING_PLANES) ->
    [i32(R#win_values.backing_planes)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BACKING_PLANES),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BACKING_PIXEL) ->
    [i32(R#win_values.backing_pixel)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BACKING_PIXEL),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_OVERRIDE_REDIRECT) ->
    [i32(R#win_values.override_redirect)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_OVERRIDE_REDIRECT),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_SAVE_UNDER) ->
    [i32(R#win_values.save_under)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_SAVE_UNDER),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_EVENT_MASK) ->
    [i32(R#win_values.event_mask)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_EVENT_MASK),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_DO_NOT_PROPAGATE) ->
    [i32(R#win_values.do_not_propagate)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_DO_NOT_PROPAGATE),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_COLORMAP) ->
    [i32(R#win_values.colormap)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_COLORMAP),R)];
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_CURSOR) ->
    [i32(R#win_values.cursor)|enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_CURSOR),R)];
enc_win_vl(_,_) -> [].

%% --------------------------
%% The Get Geometry request.

get_geometry(X,R) when R#get_geometry.drawable =/= undefined ->
    {reply,
     [R#get_geometry.opcode,
      0, % unused
      i16(R#get_geometry.len),
      i32(R#get_geometry.drawable)],
     get_geometry_reply}.  % Reply type

%% ------------------------
%% The Map Window request.

map_window(X,R) ->
    {ok,[R#map_window.opcode,
	 0, % unused
	 i16(R#map_window.len),
	 i32(R#map_window.window)]}.

%% --------------------------------
%% The Poly Fill Rectangle request

poly_fill_rectangle(X,R) when R#poly_fill_rectangle.drawable =/= undefined,
			      R#poly_fill_rectangle.cid =/= undefined,
			      list(R#poly_fill_rectangle.rectangles) ->
    RectangleList = encode_rectangles(R#poly_fill_rectangle.rectangles),
    {ok,
     [R#poly_fill_rectangle.opcode,
      0, % unused
      i16(3 + length(RectangleList)),
      i32(R#poly_fill_rectangle.drawable),
      i32(R#poly_fill_rectangle.cid),
      RectangleList]}.

encode_rectangles([H|T]) when ?IS_RECTANGLE(H) ->
    [i16(H#rectangle.x) ++ i16(H#rectangle.y),
     i16(H#rectangle.width) ++ i16(H#rectangle.height)
     |encode_rectangles(T)];
encode_rectangles([]) -> [].
    
%% ----------------------
%% The Poly Line request

poly_line(X,R) when R#poly_line.drawable =/= undefined,
		    R#poly_line.gc =/= undefined,
		    list(R#poly_line.points) ->
    PointsList = encode_points(R#poly_line.points),
    {ok,
     [R#poly_line.opcode,
      R#poly_line.coord_mode, 
      i16(3 + length(PointsList)),
      i32(R#poly_line.drawable),
      i32(R#poly_line.gc),
      PointsList]}.

encode_points([H|T]) when ?IS_POINT(H) ->
    [i16(H#point.x) ++ i16(H#point.y)|encode_points(T)];
encode_points([]) -> [].
    
%% ----------------------
%% The Poly Segment request

poly_segment(X,R) when R#poly_segment.drawable =/= undefined,
		       R#poly_segment.gc =/= undefined,
		       list(R#poly_segment.segments) ->
    SegmList = encode_segments(R#poly_segment.segments),
    {ok,
     [R#poly_segment.opcode,
      i16(3 + length(SegmList)),
      i32(R#poly_segment.drawable),
      i32(R#poly_segment.gc),
      SegmList]}.

encode_segments([H|T]) when ?IS_SEGMENT(H) ->
    [i16(H#segment.x1) ++ i16(H#segment.y1),
     i16(H#segment.x2) ++ i16(H#segment.y2)
     |encode_segments(T)];
encode_segments([]) -> [].
    
%% ------------------
%% DECODING MESSAGES
%% ------------------

decode(Db,[])        -> decode_cont(Db,[]);
decode(_,[0|Emsg])   -> decode_error(Emsg);
decode(Db,[1|Reply]) -> decode_reply(Db,Reply);
decode(_,Event)      -> decode_event(Event).

decode_cont(X,Msg) ->
    #more{cont=fun({Db,More}) -> decode(Db,Msg ++ More) end}.

%% ------------------------
%% Decoding reply messages

decode_reply(Db,[B,S1,S0|D]) ->
    SeqNo = i16(S1,S0),
    case ex11_db:get_reply(Db,SeqNo) of
	{ok,{Pid,ReplyType}} when pid(Pid) -> 
	    decode_reply_type(ReplyType,Pid,[B,S1,S0|D]);
	_ -> 
	    error_msg("unable to decode reply message: ~w~n",[[B,S1,S0|D]])
    end.

decode_reply_type(get_geometry_reply,Pid,[D,_,_,_,_,_,_,R3,R2,R1,R0,X1,X0,
					  Y1,Y0,W1,W0,H1,H0,B1,B0|T]) ->
    {_,Rest} = split_list(10,T), % 10 unused bytes
    Reply = #get_geometry_reply{depth=D,
				root=i32(R3,R2,R1,R0),
				x=i16(X1,X0),
				y=i16(Y1,Y0),
				width=i16(W1,W0),
				height=i16(H1,H0),
				border_width=i16(B1,B0)},
    {reply,{Pid,Reply},Rest}.
    

%% ------------------------
%% Decoding error messages

decode_error(Emsg) when length(Emsg)<32 -> 
    error_cont(Emsg);
decode_error([?ERROR_IDCHOICE,S1,S0,R3,R2,R1,R0,Mi1,Mi0,Ma|D]) ->
    {_,Rest} = split_list(21,D), % 21 unused bytes
    E = #error{type=idchoice,
	       seqno=i16(S1,S0),
	       bad_resource_id=i32(R3,R2,R1,R0),
	       minor_opcode=i16(Mi1,Mi0),
	       major_opcode=Ma},
    {error,E,Rest};
decode_error([?ERROR_LENGTH,S1,S0,_,_,_,_,Mi1,Mi0,Ma|D]) ->
    {_,Rest} = split_list(21,D), % 21 unused bytes
    E = #error{type=idchoice,
	       seqno=i16(S1,S0),
	       minor_opcode=i16(Mi1,Mi0),
	       major_opcode=Ma},
    {error,E,Rest};
decode_error([?ERROR_REQUEST,S1,S0,_,_,_,_,Mi1,Mi0,Ma|D]) ->
    {_,Rest} = split_list(21,D), % 21 unused bytes
    E = #error{type=request,
	       seqno=i16(S1,S0),
	       minor_opcode=i16(Mi1,Mi0),
	       major_opcode=Ma},
    {error,E,Rest};
decode_error([Error|D]) ->
    io:format("decode_error(NYI): ~w , ~w ~n",[Error,D]),
    {error,#error{type=Error},[]}.

error_cont(Emsg) ->
    #more{cont=fun(More) -> decode_error(Emsg ++ More) end}.

%% ------------------------
%% Decoding event messages

decode_event(Event) when length(Event)<32 -> 
    event_cont(Event);
decode_event([?EVENT_EXPOSE,_,S1,S0,W3,W2,W1,W0,
	      X1,X0,Y1,Y0,Wi1,Wi0,H1,H0,C1,C0|D]) ->
    {_,Rest} = split_list(14,D), % 14 unused bytes
    E = #expose{seqno=i16(S1,S0),
		window=i32(W3,W2,W1,W0),
		x=i16(X1,X0),
		y=i16(Y1,Y0),
		width=i16(Wi1,Wi0),
		height=i16(H1,H0),
		count=i16(C1,C0)},
    {event,E,Rest};
decode_event([?EVENT_MAP_NOTIFY,_,S1,S0,E3,E2,E1,E0,W3,W2,W1,W0,V|D]) ->
    {_,Rest} = split_list(19,D), % 19 unused bytes
    E = #map_notify{seqno=i16(S1,S0),
		    event=i32(W3,W2,W1,W0),
		    window=i32(W3,W2,W1,W0),
		    override_redirect=V},
    {event,E,Rest};
decode_event([?EVENT_REPARENT_NOTIFY,_,S1,S0,E3,E2,E1,E0,W3,W2,W1,W0,
	      P3,P2,P1,P0,X1,X0,Y1,Y0,V|D]) ->
    {_,Rest} = split_list(11,D), % 11 unused bytes
    E = #reparent_notify{seqno=i16(S1,S0),
			 event=i32(E3,E2,E1,E0),
			 window=i32(W3,W2,W1,W0),
			 parent=i32(P3,P2,P1,P0),
			 x=i16(X1,X0),
			 y=i16(Y1,Y0),
			 override_redirect=V},
    {event,E,Rest};
decode_event([?EVENT_CONFIGURE_NOTIFY,_,S1,S0,E3,E2,E1,E0,W3,W2,W1,W0,
	      A3,A2,A1,A0,X1,X0,Y1,Y0,Wi1,Wi0,H1,H0,B1,B0,V|D]) ->
    {_,Rest} = split_list(5,D), % 5 unused bytes
    E = #configure_notify{seqno=i16(S1,S0),
			  event=i32(E3,E2,E1,E0),
			  window=i32(W3,W2,W1,W0),
			  above_sibling=i32(A3,A2,A1,A0),
			  x=i16(X1,X0),
			  y=i16(Y1,Y0),
			  width=i16(Wi1,Wi0),
			  height=i16(H1,H0),
			  border_width=i16(B1,B0),
			  override_redirect=V},
    {event,E,Rest};
decode_event([Event|D]) ->
    io:format("decode_event(NYI): ~w , ~w~n",[Event,D]),
    {event,#event_nyi{event=Event},[]}.

event_cont(Event) ->
    #more{cont=fun(More) -> decode_event(Event ++ More) end}.



%% ---------------
%% Misc routines
%% ---------------

pad(E) -> (4 - (E rem 4)) rem 4.
bpad(E) -> pad(E)*8.

rm_pad(0,L)         -> L;
rm_pad(1,[_|L])     -> L;
rm_pad(2,[_,_|L])   -> L;
rm_pad(3,[_,_,_|L]) -> L;
rm_pad(1,<<_:8, L>>)     -> L;
rm_pad(2,<<_:16, L>>)     -> L;
rm_pad(3,<<_:24, L>>)     -> L.

add_pad(Data) -> add_pad_0(pad(length(Data))).

add_pad_0(0) -> [];
add_pad_0(1) -> [0];
add_pad_0(2) -> [0,0];
add_pad_0(3) -> [0,0,0].

