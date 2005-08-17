%%%-------------------------------------------------------------------
%%% File    : simple.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created :  9 Aug 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(simple).

-export([start/0,stop/0]).

-import(filename,[join/1,dirname/1]).

-record(st, {statusbar_ctxt,treeview}).
-record(treeview,{name,store,store_type,selection,cols=[]}).
-record(col,{title,attr,data_col,ctype,gval_name}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    ssnd([],'GN_glade_init',[GladeFile]),
    loop(init_gui()).

init_gui() ->
    treeview_init(state_init(#st{})).

state_init(St) ->
    Id = ssnd(statusbar1,'Gtk_statusbar_get_context_id',["state"]),
    ssnd(statusbar1,'Gtk_statusbar_push',[Id,"connected"]),
    state_disc(St#st{statusbar_ctxt = Id}).

treeview_init(St) ->
    Cols = [#col{title="Proc",attr="text",data_col=0,ctype=gchararray},
	    #col{title="Size",attr="text",data_col=1,ctype=gint},
	    #col{title="Msgq",attr="text",data_col=2,ctype=gint},
	    #col{title="Reds",attr="text",data_col=3,ctype=gint}],
    St#st{treeview=treeview(treeview1, Cols)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(St) ->
    receive
	{?MODULE,{signal,{window1,'GDK_DELETE'}}} -> quit();
	{?MODULE,{signal,{quit1,'GDK_BUTTON_RELEASE'}}} -> quit();
	{?MODULE,{signal,{connect,'GDK_BUTTON_RELEASE'}}} -> loop(conn(St));
	{?MODULE,{signal,{disconnect,'GDK_BUTTON_RELEASE'}}} -> loop(disc(St));
	{?MODULE,{signal,{about1,'GDK_BUTTON_RELEASE'}}} ->loop(show_about(St));
	{?MODULE,{signal,{dialogb,'GDK_BUTTON_RELEASE'}}}->loop(hide_about(St));
	{?MODULE,{signal,{dialog1,'GDK_DELETE'}}} ->       loop(hide_about(St));
	{data,Data} -> loop(update(St,Data));
	quit -> quit();
	X -> io:fwrite("got ~p~n",[X]),loop(St)
    end.

quit() -> gtkNode:stop(?MODULE).
conn(St) -> do_connect(),state_conn(St).
disc(St) -> do_disconnect(),state_disc(St).
hide_about(St) -> ssnd(dialog1,'Gtk_widget_hide',[]),St.
show_about(St) -> ssnd(dialog1,'Gtk_widget_show',[]),St.
update(St,Data) -> 
    clear(St#st.treeview),
    populate(St#st.treeview,Data),
    St.
state_disc(St) ->
    ssnd(statusbar1,'Gtk_statusbar_push',[St#st.statusbar_ctxt,"disconnected"]),
    ssnd(connect,'Gtk_widget_set_sensitive',[true]),
    ssnd(disconnect,'Gtk_widget_set_sensitive',[false]),
    St.
state_conn(St) ->
    ssnd(statusbar1,'Gtk_statusbar_pop',[St#st.statusbar_ctxt]),
    ssnd(connect,'Gtk_widget_set_sensitive',[false]),
    ssnd(disconnect,'Gtk_widget_set_sensitive',[true]),
    St.

do_connect() -> simple_top:assert(self()).
do_disconnect() -> simple_top:stop().
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear(#treeview{store_type=list,store=LS}) -> 
    ssnd(LS,'Gtk_list_store_clear',[]).

populate(_TV,[]) -> ok;
populate(TV=#treeview{store_type=list,store=LS,cols=Cols},[RowData|Data]) ->
    ssnd(LS,'Gtk_list_store_append',[gtkTreeIter]),
    populate_list_row(LS,Cols,RowData),
    populate(TV,Data).

populate_list_row(_LS,[],[]) -> ok;
populate_list_row(LS,[Col|Cols],[Data|Datas]) ->
    Dcol = Col#col.data_col,
    ssnd(LS,'Gtk_list_store_set_value',[gtkTreeIter,Dcol,gval(Col,Data)]),
    populate_list_row(LS,Cols,Datas).

gval(#col{ctype=gint,gval_name=Gval},Int) when integer(Int) ->
    ssnd(Gval,'G_value_set_int',[Int]), Gval;
gval(#col{ctype=gchararray,gval_name=Gval},Str) when list(Str) ->
    ssnd(Gval,'G_value_set_string',[Str]), Gval.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

treeview(Name, Cols) ->
    lists:foreach(fun(C) -> treeview_column(Name,C) end, Cols),
    {LS,NCols} = liststore(Name,Cols),
    ssnd(Name,'Gtk_tree_view_set_model',[LS]),
    #treeview{name = Name,
	      cols = NCols,
	      store_type = list,
	      store = LS,
	      selection = ssnd(Name,'Gtk_tree_view_get_selection',[])}.

liststore(Name,Cols) -> 
    {ssnd([],'Gtk_list_store_newv',[length(Cols),[C#col.ctype||C<-Cols]]),
     [C#col{gval_name=make_gval(Name,C)} || C <- Cols]}.

make_gval(Name,C) ->
    Gval_name = list_to_atom(atom_to_list(Name)++
			     integer_to_list(C#col.data_col)),
    ssnd([],'GN_new_gvalue',[Gval_name,C#col.ctype]),
    Gval_name.

treeview_column(Treeview, #col{title=Title,attr=Attr,data_col=Col}) ->
    TreeViewCol = ssnd([],'Gtk_tree_view_column_new',[]),
    TextRend = ssnd([],'Gtk_cell_renderer_text_new',[]),
    ssnd(TreeViewCol,'Gtk_tree_view_column_pack_start',[TextRend,false]),
    ssnd(TreeViewCol,'Gtk_tree_view_column_set_title',[Title]),
    ssnd(TreeViewCol,'Gtk_tree_view_column_add_attribute',[TextRend,Attr,Col]),
    ssnd(TreeViewCol,'Gtk_tree_view_column_set_resizable',[true]),
    ssnd(Treeview,'Gtk_tree_view_append_column',[TreeViewCol]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ssnd([],Cmd,Args) -> snd(Cmd,Args);
ssnd(Wid,Cmd,Args) -> snd(Cmd,[Wid|Args]).

snd(Cmd, Args) ->
    ?MODULE ! {self(),[{Cmd,Args}]},
    receive
	{?MODULE,{reply,[{ok,Rep}]}} -> Rep;
	{?MODULE,{reply,[{error,Rep}]}} -> erlang:fault({Cmd,Args,Rep})
    end.
