%%% File    : xcli.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : Functions for dispatching events and database lookups
%%% Created : 27 Jan 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(xcli).

-include("x.hrl").

-include("xproto.gx.hrl").

-include_lib("kernel/include/inet.hrl").

-compile(export_all).

-import(lists, [reverse/1]).


-define(XCONNECT_TMO, 10000).
-define(XRECV_TMO,    5000).
-define(XCALL_TMO,    5000).

%%-define(DBG(Format, As), io:format(Format, As)).
-define(DBG(Format, As), ok).

open(Host, DpyN, DefScr) ->
    Pid = spawn_link(?MODULE, init, [self(),Host,DpyN, DefScr]),
    receive
	{Pid,Result} ->
	    Result
    end.

init(Creator, Host, DpyN, DefScr) ->
    ?DBG("init: ~p ~p ~p\n", [Host, DpyN, DefScr]),
    Home = os:getenv("HOME"),
    case xauth:parse(filename:join(Home,".Xauthority")) of
	{error,Err} ->
	    Creator ! {self(), {error,Err}};
	Xauth ->
	    case xauth:lookup(Host, Xauth) of
		{value,Auth} ->
		    auth(Creator,Host,Auth,DpyN,DefScr);
		false ->
		    Creator ! {self(), {error,bad_auth}}
	    end
    end.
		    

auth(Creator,Host,Auth,DpyN,DefScr) ->
    ?DBG("auth: ~p ~p ~p ~p\n", [Host, DpyN, DefScr,Auth]),
    case gen_tcp:connect(Host, ?X_TCP_PORT+DpyN, 
			 [{mode,binary},{packet,0},{active,false}], 
			 ?XCONNECT_TMO) of
	{ok, Socket} ->
	    connect(Creator,Host,Socket,Auth,DpyN,DefScr);
	Error ->
	    Creator ! {self(),Error}
    end.

connect(Creator,Host,Socket,Auth,DpyN,DefScr) ->
    ?DBG("connect: ~p ~p ~p ~p\n", [Host, DpyN, DefScr,Auth]),
    gen_tcp:send(Socket, xproto:connClientPrefix(?MSB_BYTEORDER, Auth)),
    case gen_tcp:recv(Socket, 8, ?XRECV_TMO) of
	{ok, <<?xConnSetupPrefix(Success,LengthReason,
	      MajorVersion, MinorVersion, Length) >> } ->
	    case Success of
		?X_Reply ->
		    ?DBG("xcli: connected to ~p display ~p X~w.~w len=~w\n",
			[Host,DpyN,MajorVersion,MinorVersion,Length]),
		    setup(Creator,Host,Socket,DpyN,DefScr,Length,
			  MajorVersion, MinorVersion);
		_ ->
		    case gen_tcp:recv(Socket, LengthReason, ?XRECV_TMO) of
			{ok, Reason} ->
			    Creator ! {self(),{error,
					       {Success,
						binary_to_list(Reason)}}};
			Error ->
			    Creator ! {self(), {error, Error}}
		    end
	    end;
	Error ->
	    Creator ! {self(), Error}
    end.

setup(Creator,Host,Socket,DpyN,DefScr,Length32,MajorVersion, MinorVersion) ->
    ?DBG("setup: ~p ~p ~p\n", [Host, DpyN, DefScr]),
    Store = ets:new(xdpy, [public,set,{keypos,2}]),
    Id = 0,
    Display = #display { owner = self(), store=Store, id = Id, fd = Socket},
    case gen_tcp:recv(Socket, Length32*4, ?XRECV_TMO) of
	{ok, << ?xConnSetup(Release,RidBase,RidMask,
	      MotionBufferSize, NBytesVendor, MaxRequestSize,
	      NumRoots, NumFormats, ImageByteOrder, BitmapBitOrder,
	      BitMapScanLineUnit, BitMapScanLinePad,MinKeyCode,
	      MaxKeyCode,_), Data0/binary >> } ->
	    {Vendor,Data1} = get_vendor(Display, NBytesVendor, Data0),
	    RidShift = rid_shift(RidMask),

	    D = #xDisplay { id = Id,
			    fd = Socket,
			    proto_major_version = MajorVersion,
			    proto_minor_version = MinorVersion,
			    vendor = Vendor,
			    byte_order = ImageByteOrder,
			    bitmap_unit = BitMapScanLineUnit,
			    bitmap_pad = BitMapScanLinePad,
			    bitmap_bit_order = BitmapBitOrder,
			    nformats = NumFormats,
			    release = Release,
			    max_request_size = MaxRequestSize,
			    display_name = ":0.0", %% FIXME
			    nscreens = NumRoots,
			    motion_buffer = MotionBufferSize,
			    min_keycode = MinKeyCode,
			    max_keycode = MaxKeyCode,
			    %% resource allocation
			    resource_base = RidBase,
			    resource_mask = RidMask,
			    %% Computed
			    resource_shift = RidShift,
			    resource_imask = RidMask bsr RidShift,
			    %% 
			    default_screen = DefScr
			   },
	    store(Display, D),
	    store(Display, #xPrivate { id = resource_id, value = 0}),
	    store(Display, #xPrivate { id = request_id, value = 0}),
	    Data2 = setup_formats(Display, 0, NumFormats, Data1),
	    Data3 = setup_screens(Display, 0, NumRoots, Data2),
	    Creator ! {self(), {ok, Display}},
	    inet:setopts(Socket, [{active,once},{buffer,32}]),
	    main(Display,Socket, queue:new());
	{ok, Bin} ->
	    ?DBG("got length = ~p expected\n", [size(Bin),Length32*4]),
	    Creator ! {self(), {error,bad_length}};
	Error ->
	    Creator ! {self(), Error}
    end.


main(Display,Socket,Queue) ->
    receive 
	{tcp,Socket,Bin= <<?BYTE(Type),_/binary>>} ->
	    ?DBG("main: got ~p bytes\n", [size(Bin)]),
	    case Type of
		?X_Reply -> 
		    Queue1 = reply(Socket, Bin, Queue),
		    inet:setopts(Socket,[{active,once}]),
		    main(Display,Socket,Queue1);
		?X_Error -> 
		    Queue1 = error(Bin, Queue),
		    inet:setopts(Socket,[{active,once}]),
		    main(Display,Socket,Queue1);
		_ -> 
		    event(Type,Display#display.store,Bin),
		    inet:setopts(Socket,[{active,once}]),
		    main(Display,Socket,Queue)
	    end;
	{call,From,Request} ->
	    gen_tcp:send(Socket, Request),
	    main(Display, Socket, queue:in(From,Queue));
	{tcp_closed,Socket} ->
	    io:format("main: X server close connection\n"),
	    {error, closed};
	{tcp_error,Socket,Error} ->
	    io:format("main: tcp_error ~p\n", [Error]),
	    Error;
	Other ->
	    io:format("main: unknown input ~p\n", [Other]),
	    main(Display, Socket, Queue)
    end.


reply(Socket, Bin0 = <<_:32, ?CARD32(Length), _/binary>>, Queue) ->
    if Length == 0 ->
	    reply(Bin0, Queue);
       Length > 0 ->
	    ?DBG("dispatch_reply: read more ~p bytes\n", [Length*4]),
	    case gen_tcp:recv(Socket, Length*4) of
		{ok,Bin1} ->
		    reply(<<Bin0/binary,Bin1/binary>>, Queue);
		Error ->
		    io:format("main: recv: error ~p\n", [Error]),
		    error
	    end
    end.

error_string(?BadRequest) -> "bad request code";
error_string(?BadValue) -> "int parameter out of range";
error_string(?BadWindow) -> "parameter not a window";
error_string(?BadPixmap) -> "parameter not a Pixmap";
error_string(?BadAtom) ->  "parameter not an Atom";
error_string(?BadCursor) -> "paramter not a Cursor";
error_string(?BadFont) -> "parameter not a Font";
error_string(?BadMatch) -> "parameter mismatch";
error_string(?BadDrawable) -> "parameter not a Pixmap or Window";
error_string(?BadAccess) -> "illegal access, ...";
error_string(?BadAlloc) -> "insufficent resourcse";
error_string(?BadColor) -> "no such colormap";
error_string(?BadGC) -> "parameter not a GC";
error_string(?BadIDChoice) -> "choice not in range or already used";
error_string(?BadName) -> "font or color name doesn't exist";
error_string(?BadLength) -> "request length incorrect";
error_string(?BadImplementation) -> "server is defective";
error_string(_) -> "unknown".

    
error(Bin, Queue) ->
    { Error, _} = ?xError_dec(Bin),
    io:format("xcli: error: ~s ~p\n", 
	      [error_string(Error#xError.errorCode),
	       Error]),
    reply(Error, Queue).


event(Event,Store,B0) ->
    ?DBG("event number ~p\n", [Event]),
    {Ev,B1} = decode_event(Event,B0),
    deliver(Ev, Store).


reply(Reply, Queue) ->
    case queue:out(Queue) of
	{{value,[Pid|Ref]}, Queue1} ->
	    Pid ! {Ref, Reply},
	    Queue1;
	{empty,Queue1} ->
	    io:format("xcli: queue was empty\n"),
	    Queue1
    end.

deliver(Ev, Store) ->
    case event_window(Ev) of
	root ->
	    io:format("xcli: got root event ~p\n", [Ev]),
	    ok;  %% ourselfs;
	none -> 
	    io:format("xcli: got event to none ~p\n", [Ev]),
	    ok;  %% no one to send to
	Wid ->
	    case ets:lookup(Store, Wid) of
		[Res] ->
		    Res#xResource.owner ! {x,Ev};
		[] ->
		    io:format("xcli: event window does not have an owner~p\n", 
			      [Ev]),
		    ok
	    end
    end.

event_window(Ev) ->
    case element(1, Ev) of
	xKeyPressedEvent -> Ev#xKeyPressedEvent.event;
	xKeyReleasedEvent -> Ev#xKeyReleasedEvent.event;
	xButtonPressedEvent -> Ev#xButtonPressedEvent.event;
	xButtonReleasedEvent -> Ev#xButtonReleasedEvent.event;
	xEnterWindowEvent -> Ev#xEnterWindowEvent.event;
	xLeaveWindowEvent -> Ev#xLeaveWindowEvent.event;
	xFocusInEvent -> Ev#xFocusInEvent.window;
	xFocusOutEvent -> Ev#xFocusOutEvent.window;
	xExposeEvent -> Ev#xExposeEvent.window;
	xGraphicsExposeEvent -> Ev#xGraphicsExposeEvent.drawable;
	xNoExposeEvent -> Ev#xNoExposeEvent.drawable;
	xVisibilityEvent -> Ev#xVisibilityEvent.window;
	xCreateWindowEvent -> Ev#xCreateWindowEvent.window;
	xDestroyWindowEvent -> Ev#xDestroyWindowEvent.window;
	xUnmapEvent -> Ev#xUnmapEvent.window;
	xMapEvent -> Ev#xMapEvent.window;
	xMapRequestEvent -> Ev#xMapRequestEvent.window;
	xReparentEvent -> Ev#xReparentEvent.window;
	xConfigureEvent -> Ev#xConfigureEvent.window;
	xConfigureRequestEvent -> Ev#xConfigureRequestEvent.window;
	xGravityEvent -> Ev#xGravityEvent.window;
	xResizeRequestEvent -> Ev#xResizeRequestEvent.window;
	xCirculateEvent -> Ev#xCirculateEvent.window;
	xPropertyEvent -> Ev#xPropertyEvent.window;
	xSelectionClearEvent -> Ev#xSelectionClearEvent.window;
	xSelectionRequestEvent -> Ev#xSelectionRequestEvent.owner;
	xSelectionEvent -> Ev#xSelectionEvent.requestor;
	xColormapEvent -> Ev#xColormapEvent.window;
	xMappingEvent -> none;
	xClientMessageEvent -> Ev#xClientMessageEvent.window;
	_ -> none
    end.
	    
decode_event(?KeyPress,B0) -> ?xKeyPressedEvent_dec(B0);
decode_event(?KeyRelease,B0) -> ?xKeyReleasedEvent_dec(B0);
decode_event(?ButtonPress,B0) -> ?xButtonPressedEvent_dec(B0);
decode_event(?ButtonRelease,B0) -> ?xButtonReleasedEvent_dec(B0);
decode_event(?MotionNotify,B0) -> ?xEvent_dec(B0);
decode_event(?EnterNotify,B0) -> ?xEnterWindowEvent_dec(B0);
decode_event(?LeaveNotify,B0) -> ?xLeaveWindowEvent_dec(B0);
decode_event(?FocusIn,B0) -> ?xFocusInEvent_dec(B0);
decode_event(?FocusOut,B0) -> ?xFocusOutEvent_dec(B0);
decode_event(?KeymapNotify,B0) -> ?xKeymapEvent_dec(B0);
decode_event(?Expose,B0)       -> ?xExposeEvent_dec(B0);
decode_event(?GraphicsExpose,B0) -> ?xGraphicsExposeEvent_dec(B0);
decode_event(?NoExpose,B0) -> ?xNoExposeEvent_dec(B0);
decode_event(?VisibilityNotify,B0) -> ?xVisibilityEvent_dec(B0);
decode_event(?CreateNotify,B0) -> ?xCreateWindowEvent_dec(B0);
decode_event(?DestroyNotify,B0) -> ?xDestroyWindowEvent_dec(B0);
decode_event(?UnmapNotify,B0) -> ?xUnmapEvent_dec(B0);
decode_event(?MapNotify,B0) -> ?xMapEvent_dec(B0);
decode_event(?MapRequest,B0) -> ?xMapRequestEvent_dec(B0);
decode_event(?ReparentNotify,B0) -> ?xReparentEvent_dec(B0);
decode_event(?ConfigureNotify,B0) -> ?xConfigureEvent_dec(B0);
decode_event(?ConfigureRequest,B0) -> ?xConfigureRequestEvent_dec(B0);
decode_event(?GravityNotify,B0) -> ?xGravityEvent_dec(B0);
decode_event(?ResizeRequest,B0) -> ?xResizeRequestEvent_dec(B0);
decode_event(?CirculateNotify,B0) -> ?xCirculateEvent_dec(B0);
decode_event(?CirculateRequest,B0) -> ?xEvent_dec(B0);
decode_event(?PropertyNotify,B0) -> ?xPropertyEvent_dec(B0);
decode_event(?SelectionClear,B0) -> ?xSelectionClearEvent_dec(B0);
decode_event(?SelectionRequest,B0) -> ?xSelectionRequestEvent_dec(B0);
decode_event(?SelectionNotify,B0) -> ?xSelectionEvent_dec(B0);
decode_event(?ColormapNotify,B0) -> ?xColormapEvent_dec(B0);
decode_event(?ClientMessage,B0) -> ?xClientMessageEvent_dec(B0);
decode_event(?MappingNotify,B0) -> ?xMappingEvent_dec(B0);
decode_event(_, B0) -> ?xEvent_dec(B0).


get_vendor(Display, Len, Data0) ->
    P = ?PAD4_Len(Len),
    <<Vendor:Len/binary, _:P,Data1/binary>> = Data0,
    {binary_to_list(Vendor), Data1}.

setup_formats(Display, N, N, Data) -> Data;
setup_formats(Display, I, N,
	      <<?xPixmapFormat(Depth,BitsPerPixel,ScanLinePad,_,_),

	       Data/binary>>) ->
    Id = [format|I],
    Fmt = #xPixmapFormatValues {id = Id,
				depth=Depth,
				bitsPerPixel=BitsPerPixel,
				scanLinePad=ScanLinePad},
    store(Display, Fmt),
    setup_formats(Display,I+1,N,Data).


setup_screens(Display, I, I, Data) -> Data;
setup_screens(Display, I, Num, 
	      <<?xWindowRoot(WindowId,DefaultColormap,WhitePixel,BlackPixel,
	       CurrentInputMask,PixWidth,PixHeight,
	       MmWidth,MmHeight,MinInstalledMaps,MaxInstalledMaps,
	       RootVisualID,BackingStore,SaveUnders,RootDepth,
	       NDepths), Data/binary>>) ->
    {Depths,Data1} = setup_depths(Display,0, NDepths, Data, []),
    Scr = #xScreen { id = [screen|I],
		     root       = WindowId,
		     width      = PixWidth,
		     height     = PixHeight,
		     mwidth     = MmWidth,
		     mheight    = MmHeight,
		     ndepths    = NDepths,
		     depths     = Depths,
		     root_depth = RootDepth,
		     root_visual = RootVisualID,
		     default_gc = fixme,
		     cmap = DefaultColormap,
		     white_pixel = WhitePixel,
		     black_pixel = BlackPixel,
		     max_maps = MaxInstalledMaps,
		     min_maps = MinInstalledMaps,
		     backing_store = BackingStore,
		     save_unders = SaveUnders,
		     root_input_mask = CurrentInputMask
		    },
    store(Display, Scr),
    setup_screens(Display, I+1, Num, Data1).

setup_depths(Display,N,N,Data,Depths) -> 
    {reverse(Depths),Data};
setup_depths(Display,I,N,Data,Depths) ->
    <<?xDepth(Depth,_,NVisuals,_), Data1/binary>> = Data,
    ?DBG("get_depth: ~w nvis=~w\n", [Depth,NVisuals]),
    {Visuals, Data2} = setup_visuals(Display,NVisuals,Data1,[]),
    setup_depths(Display,I+1,N,Data2,
		 [#xdepth { depth = Depth, 
			    nvisuals = NVisuals,
			    visuals = Visuals} | Depths]).


setup_visuals(Display,0,Data,Visuals) -> 
    {reverse(Visuals), Data};
setup_visuals(Display,I,Data,Visuals) -> 
    <<?xVisualType(VisualID,Class,BitsPerRGB,ColormapEntries,
     RedMask,GreenMask,BlueMask,Pad),Data1/binary>> = Data,
    Id = [visual|VisualID],
    Visual = #xVisual { id           = Id,
			class        = Class,
			red_mask     = RedMask,
			green_mask   = GreenMask,
			blue_mask    = BlueMask,
			bits_per_rgb = BitsPerRGB,
			map_entries  = ColormapEntries },
    store(Display, Visual),
    setup_visuals(Display,I-1,Data1,[Id|Visuals]).



store(#display{store=Store}, Record) ->
    ?DBG("X11 store: ~p\n", [Record]),
    ets:insert(Store, Record).

%%
%% Send a sync request
%% 1. send a request number to display owner in form { Ref, Pid }
%% 2. send the request to the display socket
%%
%% FIXME: race condition
call(Display, Request) ->
    call(Display, Request, ?XCALL_TMO).

call(#display { owner =  Pid}, Request, Timeout) ->
    Ref = make_ref(),
    Pid ! {call, [self()|Ref], Request},
    receive
	{Ref, Reply} ->  Reply
    after Timeout ->
	    {error, timeout}
    end.

send(#display { fd = Socket}, Request) ->
    gen_tcp:send(Socket, Request).

%% Since we get the range of resources id's we can allocate from
%% as a bitmask we count how many bit we must shift left to 
%% reach the mask
rid_shift(0)            -> exit(bad_x_setup);
rid_shift(M) when M < 0 -> rid_shift(M band 16#ffffffff, 0);
rid_shift(M)            -> rid_shift(M, 0).

rid_shift(M,S) when (M band 1) == 0 ->
    rid_shift(M bsr 1, S+1);
rid_shift(_,S) -> S.

allocId(#display { store=Store, id=Dpy}) ->
    ResId    = ets:update_counter(Store, resource_id, 1),
    ResShift = ets:lookup_element(Store, Dpy, #xDisplay.resource_shift),
    ResBase  = ets:lookup_element(Store, Dpy, #xDisplay.resource_base),
    ResImask = ets:lookup_element(Store, Dpy, #xDisplay.resource_imask),
    RId = if ResId > (ResImask - 5) ->
		  allocId_lookup(Store,ResId,ResBase,ResShift,
				 ResImask,ResImask);
	     true ->
		  (ResId bsl ResShift) + ResBase
	 end,
    ets:insert(Store, #xResource { id = RId, owner = self() }),
    RId.

destroyId(#display { store=Store }, RId) ->
    ets:delete(Store, RId).


%% Fixme race conditions :-)
allocId_lookup(Store, ResId, ResBase, ResShift, ResIMask, 0) ->
    io:format("Xcli: resource ID allocation space exhausted!\n"),
    exit(x_no_resources);
allocId_lookup(Store, ResId, ResBase, ResShift, ResIMask,I) ->
    XId = ((ResId band ResIMask) bsl ResShift) + ResBase,
    case ets:lookup(Store, XId) of
	[] -> XId;
	_ ->
	    ResId1 = ets:update_counter(Store, resource_id, 1),
	    allocId_lookup(Store, ResId1, ResBase, ResShift, ResIMask,I-1)
    end.
