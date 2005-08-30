%%%-------------------------------------------------------------------
%%% File    : points.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 30 Aug 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(points).

-export([start/0,stop/0]).
-import(filename,[join/1,dirname/1]).
-import(random,[uniform/1]).

-define(SIZE,300).
-record(ld, {name,win,gc,pixmap}).

start() -> 
    case whereis(?MODULE) of
	undefined -> spawn(fun init/0);
	_ -> already_started
    end.

stop() -> ?MODULE ! quit.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    gtkNode:start(?MODULE),
    GladeFile = join([dirname(code:which(?MODULE)),?MODULE])++".glade",
    g([],'GN_glade_init',[GladeFile]),
    loop(init_gui()).

loop(LD) ->
    receive
	{?MODULE,{signal,{button_red,_}}} ->   loop(point(LD,red));
	{?MODULE,{signal,{button_green,_}}} -> loop(point(LD,green));
	{?MODULE,{signal,{button_blue,_}}} ->  loop(point(LD,blue));
	{?MODULE,{signal,{drawingarea,'GDK_EXPOSE'}}} -> loop(redraw(LD));
	{?MODULE,{signal,{button_quit,_}}} ->quit();
	{?MODULE,{signal,{window,'GDK_DELETE'}}} -> quit();
	quit -> quit();
	X -> io:fwrite("got ~p~n",[X]),loop(LD)
    end.

quit() -> gtkNode:stop(?MODULE).

redraw(LD = #ld{win=Win,pixmap=Pixmap,gc=GC}) ->
    g(Win,'Gdk_draw_drawable',[GC,Pixmap,0,0,0,0,-1,-1]),
    LD.

point(LD = #ld{pixmap=Pixmap,gc=GC},Color) ->
    g(GC,'Gdk_gc_set_foreground',[Color]),
    g(Pixmap,'Gdk_draw_point',[GC,uniform(?SIZE),uniform(?SIZE)]),
    redraw(LD).

init_gui() -> %% init
    g(drawingarea, 'Gtk_widget_set_size_request',[?SIZE,?SIZE]),
    Win = g(drawingarea,'GN_widget_get_attr',[window]),
    Pixmap = g(Win,'Gdk_pixmap_new',[?SIZE,?SIZE,-1]),
    GC = g(Win,'Gdk_gc_new',[]),
    alloc_colors(GC,[red,green,blue,black,white,grey]),
    g(Win,'Gdk_window_clear',[]),
    g(GC,'Gdk_gc_set_foreground',[black]),
    g(Pixmap,'Gdk_draw_rectangle', [GC, true, 0, 0, -1, -1]),
    redraw(#ld{win=Win,gc=GC,pixmap=Pixmap}),
    #ld{win=Win,gc=GC,pixmap=Pixmap}.
alloc_colors(GC,Cols) ->
    ColorMap = g(GC,'Gdk_gc_get_colormap',[]),
    lists:foreach(fun(Col) -> alloc_color(ColorMap,Col) end, Cols).
alloc_color(ColorMap, Tag) ->
    g([],'Gdk_color_parse',[atom_to_list(Tag),Tag]),
    g(ColorMap,'Gdk_colormap_alloc_color',[Tag,false,true]).

g([],Cmd,Args) -> snd_rec(Cmd,Args);
g(Wid,Cmd,Args) -> snd_rec(Cmd,[Wid|Args]).

snd_rec(Cmd, Args) ->
    ?MODULE ! {self(),[{Cmd,Args}]},
    receive
	{?MODULE,{reply,[{ok,Rep}]}} -> Rep;
	{?MODULE,{reply,[{error,Rep}]}} -> erlang:fault({Cmd,Args,Rep})
    end.
