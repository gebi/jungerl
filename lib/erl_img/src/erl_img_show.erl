%%% File    : erl_img_show.erl
%%% Author  : Tony Rogvall <tony@bit.hemma.se>
%%% Description : Sample function for displaying images
%%% Created : 31 Mar 2003 by Tony Rogvall <tony@bit.hemma.se>

-module(erl_img_show).

-compile(export_all).

-include_lib("erlgtk/include/gtk.hrl").
-include_lib("erlgtk/include/gdk.hrl").
-include("erl_img.hrl").

-import(lists, [foldl/3, reverse/1, foreach/2, map/2]).


file(File) ->
    case erl_img:load(File) of
	{ok,Image} ->
	    io:format("pixmaps = ~p\n", [length(Image#erl_image.pixmaps)]),
	    image(Image);
	Error ->
	    Error
    end.


image(Image) ->
    gtk:start(),
    Window = gtk:window_new ('GTK_WINDOW_TOPLEVEL'),
    %% GdkWindow  = gtk:widget_get_window(Window),
    {IList,Visual,Colormap} = make_gdk_image(Image),

    gtk:window_set_title (?GTK_WINDOW (Window), "View"),
    gtk:widget_set_usize(?GTK_WIDGET(Window), 
                         Image#erl_image.width,
                         Image#erl_image.height),
    gtk:widget_set_colormap(Window,Colormap),
    gtk:widget_set_visual(Window, Visual),

    gtk:signal_connect (?GTK_OBJECT (Window), 'destroy',
                        fun() -> gtk:main_quit() end, []),
    Drawing_area = gtk:drawing_area_new (),

    gtk:widget_set_usize (Drawing_area, 
                          Image#erl_image.width,
                          Image#erl_image.height),
    gtk:signal_connect (?GTK_OBJECT (Drawing_area), 'expose_event',
                        fun expose_event/3, ?NULL),
    gtk:signal_connect (?GTK_OBJECT(Drawing_area), 'configure_event',
                        fun configure_event/3, ?NULL),
    gtk:widget_set_events (Drawing_area, ?GDK_EXPOSURE_MASK),
    gtk:container_add (?GTK_CONTAINER(Window), Drawing_area),
    gtk:widget_show (Drawing_area),
    gtk:widget_show  (Window),

    Area = #gdk_rectangle { x=0, y=0, 
			    width=Image#erl_image.width, 
			    height = Image#erl_image.height
			   },
    set_image_list(IList, Drawing_area, Area),
    gtk:main().


set_image_next([Widget,Area]) ->
    case get(image_list) of
	[P1,P2|Ps] ->
	    set_image_list([P2|Ps]++[P1],Widget,Area);
	[P1] ->
	    set_image_list([P1],Widget,Area);
	[] ->
	    ok
    end,
    gtk:widget_draw(Widget, Area),
    false.
    

set_image_list(IList, Widget, Area) ->
    put(image_list, IList),
    case IList of
	[{Tmo,_}|Ps] when Ps =/= [], Tmo > 0-> 
	    gtk:timeout_add(Tmo*10, fun set_image_next/1, [Widget,Area]);
	_ ->
	    false
    end.

    

configure_event(Widget, Event, _) ->
    true.

expose_event(Widget, Event, _) ->
    Area = Event#gdk_event_expose.area,
    GdkWindow  = gtk:widget_get_window(Widget),
    Style = gtk:widget_get_style(Widget),
    State = ?GTK_WIDGET_STATE(Widget),
    Gc = gtk:style_get_fg_gc(Style, State),
    [{_,Image}|_] = get(image_list),
    gdk:draw_image(GdkWindow, Gc,
                   Image,
                   Area#gdk_rectangle.x, Area#gdk_rectangle.y,
                   Area#gdk_rectangle.x, Area#gdk_rectangle.y,
                   Area#gdk_rectangle.width,
                   Area#gdk_rectangle.height),    
    true.

%%
%% generate a list of images
%%
make_gdk_image(Image) ->
    Visual = gdk:visual_get_system(),
    Colormap = gdk:colormap_get_system(),
    %% Visual   = gdk:window_get_visual(Window), 
    %% Colormap = gdk:window_get_colormap(Window),
    Map0 = map_colors(Image#erl_image.palette, Colormap),
    IList = 
	map(
	  fun(Pixmap) ->
		  GdkImage = gdk:image_new('GDK_IMAGE_FASTEST',
					   Visual,
					   Image#erl_image.width,
					   Image#erl_image.height),
		  if Pixmap#erl_pixmap.format == palette8;
		     Pixmap#erl_pixmap.format == palette4 ->
			  Pal = Pixmap#erl_pixmap.palette,
			  Map = if Pal == undefined ->
					Map0;
				   true ->
					map_colors(Pal, Colormap)
				end,
			  indexed_colors(Pixmap,GdkImage,Visual,Colormap,
					 Image#erl_image.order, Map);
		     true ->
			  direct_colors(Pixmap,GdkImage,Visual,Colormap,
					Image#erl_image.order)
		  end,
		  As = Pixmap#erl_pixmap.attributes,
		  case lists:keysearch('DelayTime',1,As) of
		      {value,{_, Tmo}} when Tmo > 0 ->
			  {Tmo, GdkImage};
		      _ ->
			  {0, GdkImage}
		  end
	  end, Image#erl_image.pixmaps),
    {IList, Visual, Colormap}.



%% create a color lookup  table
map_colors(undefined, Colormap) ->
    {};
map_colors(Colors, Colormap) ->
    map_colors(Colors, Colormap, []).
    
map_colors([{R,G,B}| Cs], Colormap, Acc) ->
    C0 = #gdk_color { red = R,green = G, blue = B },
    Pixel = case gdk:colormap_alloc_color(Colormap,C0,false,true) of
                {true,C1} ->
                    C1#gdk_color.pixel;
                _ ->
                    0
            end,
    map_colors(Cs, Colormap, [Pixel|Acc]);
map_colors([Spec | Cs], Colormap, Acc) ->
    Pixel = case gdk:color_parse(Spec) of
		{true,C1} ->
		    case gdk:colormap_alloc_color(Colormap,C1,false,true) of
			{true,C2} ->
			    C2#gdk_color.pixel;
			_ ->
			    0
		    end;
		{false,_} ->
		    0
	    end,
    map_colors(Cs, Colormap, [Pixel|Acc]);
map_colors([], Colormap, Acc) ->
    list_to_tuple(reverse(Acc)).

%%
%%
%%
%%
indexed_colors(Pixmap, GdkImg, Visual, Colormap,Order,Map) ->
    #erl_pixmap { top = Y0, left = X0, width = W, height = H } = Pixmap,
    XList =
	case Order of
	    left_to_right -> lists:seq(0, W-1);
	    right_to_left -> reverse(lists:seq(0, W-1))
	end,
    Transparent =
	case lists:keysearch('Transparent', 1, Pixmap#erl_pixmap.attributes) of
	    false -> undefined;
	    {value,{_,T}} -> T
	end,
    Background = 
	case lists:keysearch('Background', 1, Pixmap#erl_pixmap.attributes) of
	    false -> 0;
	    {value,{_,B}} -> B
	end,
    foreach(
      fun({Y,Row}) ->
	      foreach(
		fun(X) ->
			IX =
			    case Pixmap#erl_pixmap.format of
				palette4 ->
				    Offs = X*4,
				    Pad  = (X band 1)*4,
				    <<_:Offs,I:4,_:Pad,_/binary>> = Row,
				    I;
				palette8 ->
				    Offs = X,
				    <<_:Offs/binary,I,_/binary>> = Row,
				    I
			    end,
			IX1 = if IX == Transparent ->
				      B;
				 true ->
				      IX
			      end,
			   true ->
				gdk:image_put_pixel(GdkImg, X+X0, Y+Y0, 
						    element(IX1+1, Map))
			end
                end, XList)
      end, Pixmap#erl_pixmap.pixels),
    GdkImg.



direct_colors(Pixmap, GdkImg, Visual, Colormap, Order) ->
    #erl_pixmap { top = Y0, left = X0, width = W, height = H } = Pixmap,
    XList =
	case Order of
	    left_to_right -> lists:seq(0, W-1);
	    right_to_left -> reverse(lists:seq(0, W-1))
	end,
    foreach(
      fun({Y,Row}) ->
	      foreach(
		fun(X) ->
			C0 =
			    case Pixmap#erl_pixmap.format of
				gray4 ->
				    Offs = X*4,
				    Pad  = (X band 1)*4,
				    <<_:Offs,G:4,_:Pad,_/binary>> = Row,
				    #gdk_color { red   = G*16*255,
						 green = G*16*255,
						 blue  = G*16*255 };
				gray8 ->
				    Offs = X,
				    <<_:Offs/binary,G,_/binary>> = Row,
				    #gdk_color { red   = G*255,
						 green = G*255,
						 blue  = G*255 };
				b8g8r8 ->
				    Offs = 3*X,
				    <<_:Offs/binary,B,G,R,_/binary>> = Row,
				    #gdk_color { red = R*255,
						 green = G*255,
						 blue = B*255 };
				b8g8r8a8 ->
				    Offs = 4*X,
				    <<_:Offs/binary,B,G,R,A,_/binary>> = Row,
				    #gdk_color { red = R*255,
						 green = G*255,
						 blue = B*255 };
				r8g8b8 ->
				    Offs = 3*X,
				    <<_:Offs/binary,R,G,B,_/binary>> = Row,
				    #gdk_color { red = R*255,
						 green = G*255,
						 blue = B*255 };
				r8g8b8a8 ->
				    Offs = 4*X,
				    <<_:Offs/binary,R,G,B,A,_/binary>> = Row,
				    #gdk_color { red = R*255,
						 green = G*255,
						 blue = B*255 }
			    end,
                        case gdk:colormap_alloc_color(Colormap,C0,false,true)of
                            {true,C1} ->
                                gdk:image_put_pixel(GdkImg,X+X0,Y+Y0,
						    C1#gdk_color.pixel);
                            {false,_} ->
                                gdk:image_put_pixel(GdkImg, X+X0, Y+Y0, 0)
                        end
		end, XList)
      end, Pixmap#erl_pixmap.pixels),
    GdkImg.
