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
%%% Contributor(s): Vlad Dumitrescu, vlad_dumitrescu@hotmail.com.
%%%
%%%
%%% Modified: 23 Feb 1998 by tony@cslab.ericsson.se
%%%             To make it work under Windows.
%%%           25 Feb 2003 by Vlad Dumitrescu
%%%             Converting to binary format
%%%---------------------------------------------------------------------
-export([map_window/2,create_window/2,decode_error/1,
	 setup_connection/1,decode_refused_connection/1,
	 decode/2,decode_connect_reply/2,encode/2]).

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
     0,                      % unused
     11:16,                % proto-major-ver
     0:16,                    % proto-minor-ver
     0:16,                 % proto-name-len
     0:16,                 % proto-data-len
     0,0>>;                   % unused (pad)
setup_connection(Cookie) ->
    Len = length(Cookie),
    Pad = add_pad(Cookie),
    <<?MSB_BYTEORDER:8,         % Erlang byte-order
     0,                      % unused
     11:16,                % proto-major-ver
     0:16,                    % proto-minor-ver
     18:16,                % proto-name-len
     Len:16,    % proto-data-len
     0:16,                    % unused
     "MIT-MAGIC-COOKIE-1",   % auth-proto-name
     0:16,                    % pad
     (list_to_binary(Cookie))/binary,                 % auth-proto-data
     Pad/binary>>.       % pad

decode_refused_connection(<<Len,ProtoMaj:16,ProtoMin:16,_TotLen:16,D/binary>>) ->
    <<ReasonB:Len/binary, _/binary>> = D,
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
    B/binary>> = Msg,
    VendorPad = pad(VendorLen),
    PmapFormLen = 8*Formats,
    <<VendorB:VendorLen/binary,
    _:VendorPad/binary,
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

change_gc(_X, #change_gc{opcode=Opcode, cid=Cid, value_mask=ValueMask}=R) 
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

clear_area(_X, #clear_area{}=R) when R#clear_area.window =/= undefined,
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

copy_area(_X, R) when R#copy_area.src =/= undefined,
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

create_gc(X, R) when R#create_gc.drawable =/= undefined ->
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

    B = <<(R#create_gc.opcode),
	 0, % unused
	 (4 + size(ValueList) div 4):16,
	 (Cid):32,
	 (R#create_gc.drawable):32,
	 (ValueMask):32,
	 ValueList/binary>>,

    {ok, B, {ok,Cid}}.


encode_gc_valuelist(ValueMask,R) when ?IS_GC_VALUES(R) -> 
    enc_gc_vl(ValueMask,R).
    
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FUNCTION) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FUNCTION),R),
    <<(R#gc_values.function):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_PLANE_MASK) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_PLANE_MASK),R),
    <<(R#gc_values.plane_mask):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FOREGROUND) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FOREGROUND),R),
    <<(R#gc_values.foreground):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_BACKGROUND) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_BACKGROUND),R),
    <<(R#gc_values.background):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_LINE_WIDTH) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_LINE_WIDTH),R),
    <<(R#gc_values.line_width):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_LINE_STYLE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_LINE_STYLE),R),
    <<(R#gc_values.line_style):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_CAP_STYLE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_CAP_STYLE),R),
    <<(R#gc_values.cap_style):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_JOIN_STYLE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_JOIN_STYLE),R),
    <<(R#gc_values.join_style):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FILL_STYLE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FILL_STYLE),R),
    <<(R#gc_values.fill_style):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FILL_RULE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FILL_RULE),R),
    <<(R#gc_values.fill_rule):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_TILE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_TILE),R),
    <<(R#gc_values.tile):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_STIPPLE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_STIPPLE),R),
    <<(R#gc_values.stipple):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_TILE_STIPPLE_X) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_TILE_STIPPLE_X),R),
    <<(R#gc_values.tile_stipple_x):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_TILE_STIPPLE_Y) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_TILE_STIPPLE_Y),R),
    <<(R#gc_values.tile_stipple_y):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_FONT) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_FONT),R),
    <<(R#gc_values.font):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_SUBWIN_MODE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_SUBWIN_MODE),R),
    <<(R#gc_values.subwin_mode):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_EXPOSURES) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_EXPOSURES),R),
    <<(R#gc_values.exposures):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_CLIP_X) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_CLIP_X),R),
    <<(R#gc_values.clip_x):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_CLIP_Y) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_CLIP_Y),R),
    <<(R#gc_values.clip_y):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_CLIP_MASK) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_CLIP_MASK),R),
    <<(R#gc_values.clip_mask):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_DASH_OFFSET) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_DASH_OFFSET),R),
    <<(R#gc_values.dash_offset):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_DASHES) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_DASHES),R),
    <<(R#gc_values.dashes):32, Rest/binary>>;
enc_gc_vl(Vm,R) when ?IS_SET(Vm,?GC_VALUEMASK_ARC_MODE) ->
    Rest = enc_gc_vl(?CLR(Vm,?GC_VALUEMASK_ARC_MODE),R),
    <<(R#gc_values.arc_mode):32, Rest/binary>>;
enc_gc_vl(_,_) -> 
    <<>>.

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
    B = <<(R#create_pixmap.opcode),
	 Depth,
	 4:16,
	 (Pid):32,
	 (R#create_pixmap.drawable):32,
	 (R#create_pixmap.width):16,
	 (R#create_pixmap.height):16>>,

    {ok, B, {ok,Pid}}.

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
    B = <<(R#create_window.opcode),      
	 Depth,      
	 (8+size(ValueList) div 4):16,        
	 (Win):32, 
	 (Parent):32,                 
	 (R#create_window.x):16,
	 (R#create_window.y):16,
	 (R#create_window.width):16,
	 (R#create_window.height):16,
	 (R#create_window.border_width):16,
	 (R#create_window.class):16,
	 (R#create_window.visual):32,
	 (R#create_window.value_mask):32,
	 ValueList/binary>>,

    {ok, B, {ok,Win}}.

encode_win_valuelist(ValueMask,R) when ?IS_WIN_VALUES(R) ->
    enc_win_vl(ValueMask,R).

enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BG_PIXMAP) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BG_PIXMAP),R),
    <<(R#win_values.bg_pixmap):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BG_PIXEL) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BG_PIXEL),R),
    <<(R#win_values.bg_pixel):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BD_PIXMAP) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BD_PIXMAP),R),
    <<(R#win_values.bd_pixmap):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BD_PIXEL) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BD_PIXEL),R),
    <<(R#win_values.bd_pixel):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BIT_GRAVITY) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BIT_GRAVITY),R),
    <<(R#win_values.bit_gravity):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_WIN_GRAVITY) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_WIN_GRAVITY),R),
    <<(R#win_values.win_gravity):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BACKING_STORE) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BACKING_STORE),R),
    <<(R#win_values.backing_store):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BACKING_PLANES) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BACKING_PLANES),R),
    <<(R#win_values.backing_planes):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_BACKING_PIXEL) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_BACKING_PIXEL),R),
    <<(R#win_values.backing_pixel):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_OVERRIDE_REDIRECT) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_OVERRIDE_REDIRECT),R),
    <<(R#win_values.override_redirect):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_SAVE_UNDER) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_SAVE_UNDER),R),
    <<(R#win_values.save_under):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_EVENT_MASK) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_EVENT_MASK),R),
    <<(R#win_values.event_mask):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_DO_NOT_PROPAGATE) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_DO_NOT_PROPAGATE),R),
    <<(R#win_values.do_not_propagate):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_COLORMAP) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_COLORMAP),R),
    <<(R#win_values.colormap):32, Rest/binary>>;
enc_win_vl(Vm,R) when ?IS_SET(Vm,?WIN_VALUEMASK_CURSOR) ->
    Rest = enc_gc_vl(?CLR(Vm,?WIN_VALUEMASK_CURSOR),R),
    <<(R#win_values.cursor):32, Rest/binary>>;
enc_win_vl(_,_) -> 
    <<>>.

%% --------------------------
%% The Get Geometry request.

get_geometry(_X,R) when R#get_geometry.drawable =/= undefined ->
    {reply,
     <<(R#get_geometry.opcode),
      0, % unused
      (R#get_geometry.len):16,
      (R#get_geometry.drawable):32>>,
     get_geometry_reply}.  % Reply type

%% ------------------------
%% The Map Window request.

map_window(_X,R) ->
    {ok,<<(R#map_window.opcode),
	 0, % unused
	 (R#map_window.len):16,
	 (R#map_window.window):32>>}.

%% --------------------------------
%% The Poly Fill Rectangle request

poly_fill_rectangle(_X,R) when R#poly_fill_rectangle.drawable =/= undefined,
			       R#poly_fill_rectangle.cid =/= undefined,
			       list(R#poly_fill_rectangle.rectangles) ->
    Rectangles = encode_rectangles(R#poly_fill_rectangle.rectangles),
    {ok,
     <<(R#poly_fill_rectangle.opcode),
      0, % unused
      (3 + size(Rectangles) div 4):16,
      (R#poly_fill_rectangle.drawable):32,
      (R#poly_fill_rectangle.cid):32,
      Rectangles/binary>>}.

encode_rectangles([H|T]) when ?IS_RECTANGLE(H) ->
    <<(H#rectangle.x):16, (H#rectangle.y):16,
     (H#rectangle.width):16, (H#rectangle.height):16,
     (encode_rectangles(T))/binary>>;
encode_rectangles([]) -> <<>>.
    
%% ----------------------
%% The Poly Line request

poly_line(_X,R) when R#poly_line.drawable =/= undefined,
		     R#poly_line.gc =/= undefined,
		     list(R#poly_line.points) ->
    PointsList = encode_points(R#poly_line.points),
    {ok,
     <<(R#poly_line.opcode),
      (R#poly_line.coord_mode), 
      (3 + size(PointsList) div 4):16,
      (R#poly_line.drawable):32,
      (R#poly_line.gc):32,
      PointsList/binary>>}.

encode_points([H|T]) when ?IS_POINT(H) ->
    <<(H#point.x):16, (H#point.y):16, (encode_points(T))/binary>>;
encode_points([]) -> <<>>.
    
%% ----------------------
%% The Poly Segment request

poly_segment(_X,R) when R#poly_segment.drawable =/= undefined,
			R#poly_segment.gc =/= undefined,
			list(R#poly_segment.segments) ->
    SegmList = encode_segments(R#poly_segment.segments),
    {ok,
     <<(R#poly_segment.opcode),
      (3 + size(SegmList) div 4):16,
      (R#poly_segment.drawable):32,
      (R#poly_segment.gc):32,
      SegmList/binary>>}.

encode_segments([H|T]) when ?IS_SEGMENT(H) ->
    <<(H#segment.x1):16, (H#segment.y1):16,
     (H#segment.x2):16, (H#segment.y2):16,
     (encode_segments(T))/binary>>;
encode_segments([]) -> 
    <<>>.
    
%% ------------------
%% DECODING MESSAGES
%% ------------------

decode(Db,<<>>) -> 
decode_cont(Db,<<>>);
decode(_,<<0, Emsg/binary>>) -> 
    decode_error(Emsg);
decode(Db,<<1, Reply/binary>>) -> 
    decode_reply(Db,Reply);
decode(_,Event)      -> 
    decode_event(Event).

decode_cont(_X, Msg) ->
    #more{cont = fun({Db,More}) -> 
			 decode(Db, <<Msg/binary, More/binary>>) 
		 end}.

%% ------------------------
%% Decoding reply messages

decode_reply(Db,<<_B, SeqNo:16, _D/binary>>=R) ->
    case ex11_db:get_reply(Db,SeqNo) of
	{ok,{Pid,ReplyType}} when pid(Pid) -> 
	    decode_reply_type(ReplyType,Pid,R);
	_ -> 
	    error_msg("unable to decode reply message: ~w~n",[R])
    end.

decode_reply_type(get_geometry_reply,Pid,<<Depth,_:48,Root:32,X:16,
					  Y:16,W:16,H:16,Border:16, 
					  _:80, Rest/binary>>) ->
    Reply = #get_geometry_reply{depth=Depth,
				root=Root,
				x=X,
				y=Y,
				width=W,
				height=H,
				border_width=Border},
    {reply,{Pid,Reply},Rest}.
    

%% ------------------------
%% Decoding error messages

decode_error(Emsg) when size(Emsg)<32 -> 
    error_cont(Emsg);
decode_error(<<Error,SeqNo:16,BadRes:32,Mi:16,Ma, _:168, Rest/binary>>) ->
    E = #error{type=err_type(Error),
	       seqno=SeqNo,
	       bad_resource_id=BadRes,
	       minor_opcode=Mi,
	       major_opcode=Ma},
    {error,E,Rest}.

error_cont(Emsg) ->
    #more{cont=fun(More) -> 
		       decode_error(<<Emsg/binary, More/binary>>) 
	       end}.

err_type(?ERROR_REQUEST) ->
    request;
err_type(?ERROR_VALUE) ->
    value;
err_type(?ERROR_WINDOW) ->
    window;
err_type(?ERROR_PIXMAP) ->
    pixmap;
err_type(?ERROR_ATOM) ->
    atom;
err_type(?ERROR_CURSOR) ->
    cursor;
err_type(?ERROR_FONT) ->
    font;
err_type(?ERROR_MATCH) ->
    match;
err_type(?ERROR_DRAWABLE) ->
    drawable;
err_type(?ERROR_ACCESS) ->
    access;
err_type(?ERROR_ALLOC) ->
    alloc;
err_type(?ERROR_COLORMAP) ->
    colormap;
err_type(?ERROR_GCONTEXT) ->
    gcontext;
err_type(?ERROR_IDCHOICE) ->
    idchoice;
err_type(?ERROR_NAME) ->
    name;
err_type(?ERROR_LENGTH) ->
    length;
err_type(?ERROR_IMPLEMENTATION) ->
    implementation;
err_type(_) ->
    undefined.

%% ------------------------
%% Decoding event messages

decode_event(Event) when size(Event)<32 -> 
    event_cont(Event);
decode_event(<<?EVENT_EXPOSE, _, SeqNo:16,
	     Win:32,
	     X:16, Y:16, W:16, H:16, C:16,
	     _:112, Rest/binary>>) ->
    E = #expose{seqno=SeqNo,
		window=Win,
		x=X,
		y=Y,
		width=W,
		height=H,
		count=C},
    {event,E,Rest};
decode_event(<<?EVENT_MAP_NOTIFY, _, SeqNo:16,
	     Event:32, Win:32, V, 
	     _:152, Rest/binary>>) ->
    E = #map_notify{seqno=SeqNo,
		    event=Event,
		    window=Win,
		    override_redirect=V},
    {event,E,Rest};
decode_event(<<?EVENT_REPARENT_NOTIFY, _, SeqNo:16,
	     Event:32, Win:32, Parent:32, 
	     X:16, Y:16, V, 
	     _:88, Rest/binary>>) ->
    E = #reparent_notify{seqno=SeqNo,
			 event=Event,
			 window=Win,
			 parent=Parent,
			 x=X,
			 y=Y,
			 override_redirect=V},
    {event,E,Rest};
decode_event(<<?EVENT_CONFIGURE_NOTIFY, _, SeqNo:16, 
	     Event:32, Win:32, Above:32,
	     X:16, Y:16, W:16, H:16, Border:16, V,
	     _:40, Rest/binary>>) ->
    E = #configure_notify{seqno=SeqNo,
			  event=Event,
			  window=Win,
			  above_sibling=Above,
			  x=X,
			  y=Y,
			  width=W,
			  height=H,
			  border_width=Border,
			  override_redirect=V},
    {event,E,Rest};
decode_event(<<Event, D/binary>>) ->
    io:format("decode_event(NYI): ~w , ~w~n",[Event,D]),
    {event,#event_nyi{event=Event},<<>>}.

event_cont(Event) ->
    #more{cont=fun(More) -> 
		       decode_event(<<Event/binary, More/binary>>) 
	       end}.

%% ---------------
%% Misc routines
%% ---------------

pad(E) -> (4 - (E rem 4)) rem 4.

rm_pad(0,L)         -> L;
rm_pad(1,<<_:8, L>>)     -> L;
rm_pad(2,<<_:16, L>>)     -> L;
rm_pad(3,<<_:24, L>>)     -> L.

add_pad(Data) when binary(Data) -> 
    add_pad_0(pad(size(Data)));
add_pad(Data) when list(Data) -> 
    add_pad_0(pad(length(Data))).

add_pad_0(0) -> <<>>;
add_pad_0(1) -> <<0>>;
add_pad_0(2) -> <<0,0>>;
add_pad_0(3) -> <<0,0,0>>.


