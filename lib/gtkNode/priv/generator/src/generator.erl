%%%-------------------------------------------------------------------
%%% File    : generate.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description : generates C code that gtkNode_cnode can call.
%%%               each function marshals args and calls one gtk function.
%%%               reads the .defs files from h2def.py
%%%
%%% Created : 27 Oct 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(generator).

-export([go/0,go/1]).
-import(filename, [join/1,dirname/1]).
-import(io, [put_chars/2, get_line/2]).
-import(lists,[foreach/2, map/2,foldl/3,reverse/1, sort/1]).

-record(func, {black=no,cname,pref,ret,const=no,paras=[]}).
-record(type, {what,cname,gtype,etype,cast}).

go() -> go(dirname(dirname(code:which(?MODULE)))).
go(Dir) ->
    Vsn = vsn(),
    ets_new(types),
    ets_new(funcs),
    ets_new(bogus,[{keypos,1}]),
    FNs = [gname(Dir,F,"",Vsn,"defs") || F<-["gtk","gdk","g"]],
    foreach(fun do_types/1, FNs),	    %populate  the types table
    lists(Dir),
    structs(Dir),
    basic_types(),
    R = [gen(F,Dir,Vsn) || F<-["gtk","gdk","g"]],
    io:fwrite("~w ~p~n~n", [?MODULE, R]).

gen(Pref, Dir, Vsn) ->
    do_funcs(gname(Dir,Pref,"_white",Vsn,"defs")), %populate the funcs table
    do_black(join([Dir,src,Pref++"_black.txt"])), %de-populate the funcs table
    {ok,FDc} = file:open(gname(Dir,Pref,"_generated",Vsn,"h"),[write]),
    {ok,FDok} = file:open(gname(Dir,Pref,"_funcs",Vsn,"txt"),[write]),
    {ok,FDcrap} = file:open(gname(Dir,Pref,"_crap_funcs",Vsn,"txt"),[write]),
    {ok,FDtypes} = file:open(gname(Dir,Pref,"_crap_types",Vsn,"txt"),[write]),
    vsn(Vsn,[FDc,FDok,FDcrap,FDtypes]),
    FRWAs = ets:match(funcs,{func,no,'$1',Pref,'$2','$3','$4'}), 
    foreach(fun(FRWA)-> gen_one(FDc, FDcrap, FDok,Pref,FRWA) end, FRWAs),
    log_types(FDtypes),
    {Pref, length(FRWAs), ets:lookup(bogus,good),ets:lookup(bogus,bad)}.

vsn() ->
    case os:cmd("pkg-config --modversion gtk+-2.0") of
	"sh:"++_ -> exit({not_found, 'pkg-config'});
	"Package "++_ -> exit({not_found,'gtk+-2.0'});
	Vsn -> reverse(tl(reverse(Vsn)))
    end.

vsn(_, []) -> ok;
vsn(Vsn, [FD|FDs]) -> 
    io:fwrite(FD, "/* GTK version: ~s */~n", [Vsn]),
    vsn(Vsn,FDs).

gen_one(FDc, FDcrap, FDok, Pref, [Func,Ret,Const,Args]) ->
    unstick(),
    emit_head(Pref, Func),
    emit_argdefs(Args),
    emit_retdef(Ret,Const),
    emit_ari_chk(Args),
    emit_arg_chk(Args),
    emit_call(Func, Ret, Args),
    emit_free(Args),
    emit_return(Ret),
    emit_final(),
    unstick(FDc,FDcrap,FDok).

emit_head(Pref, Func) ->
    estick({UFunc = upcase_1st(Func), rem_pref(Func, Pref)}),
    stick("/*******************************/~n"
	  "gboolean ~s(int ARI, ei_x_buff *XBUF, char *B, int *I){~n~n", [UFunc]).

emit_argdefs([]) -> stick("~n",[]);
emit_argdefs([{Typ,Name,_Doc}|Args]) ->
    stick("  ~s ~s;~n", [Typ,Name]),
    emit_argdefs(Args).

emit_retdef(Ret,Const) ->
    case what_group(Ret) of
	object -> stick("  GObject* R;~n~n",[]);
	struct -> stick("  ~s R;~n~n", [Ret]); 
	none -> stick("  /* no return value */~n~n",[]);
	crap -> stick("  ~s R;~n~n", ["CRAP_"++Ret]);
	list -> 
	    mark_as_crap("list_"++Ret),
	    stick("  ~s R;~n~n", ["CRAP_list_"++Ret]);
	_ -> 
	    case Const of
		yes -> stick("  const ~s R; /* return value */~n~n", [Ret]);
		no -> stick("  ~s R; /* return value */~n~n", [Ret])
	    end
    end.

emit_ari_chk(Args) ->
    stick("  if ( ! gn_check_arity(XBUF, ~p, ARI) )"
		  " return FALSE;~n", [length(Args)]).

emit_arg_chk([]) -> ok;
emit_arg_chk([{Typ,Name,_Doc}|Args]) ->
    estick(Name),
    case Kind = what_group(Typ) of
	crap -> 
	    stick("  if ( ! gn_get_arg_CRAP(XBUF, B, I, ~p, &~s) )"
		  " return FALSE;~n",
		  [Typ,Name]);
	list ->
	    stick("  if ( ! gn_get_arg_~s(XBUF, B, I, ~p, ~s&~s) )"
		  " return FALSE;~n",
		  [Kind,unstar(Typ),"(void**)",Name]);
	struct -> 
	    stick("  if ( ! gn_get_arg_~s(XBUF, B, I, ~p, ~s&~s) )"
		  " return FALSE;~n",
		  [Kind,unstar(Typ),"(void**)",Name]);
	basic ->
	    stick("  if ( ! gn_get_arg_~s(XBUF, B, I, &~s) )"
		  " return FALSE;~n",
		  [unstar(Typ),Name]);
	flags ->
	    stick("  if ( ! gn_get_arg_~s(XBUF, B, I, ~p, ~s&~s) )"
		  " return FALSE;~n", 
		  [Kind,unstar(Typ),"(gint*)",Name]);
	enum ->
	    stick("  if ( ! gn_get_arg_~s(XBUF, B, I, ~p, ~s&~s) )"
		  " return FALSE;~n", 
		  [Kind,unstar(Typ),"(gint*)",Name]);
	object ->
	    stick("  if ( ! gn_get_arg_~w(XBUF, B, I, ~s, ~s&~s) )"
		  " return FALSE;~n", 
		  [Kind,what_gtype(unstar(Typ)),"(GObject**)",Name])
    end,
    emit_arg_chk(Args).

emit_call(Func, Ret, Args) ->
    case what_group(Ret) of
	none -> stick("  ~s(",[Func]);
	object -> stick("  R = (GObject*)~s(",[Func]);
	list -> stick("  R = (~s)~s(", ["CRAP_"++Ret,Func]);
	crap -> stick("  R = (~s)~s(", ["CRAP_"++Ret,Func]);
	_ -> stick("  R = ~s(", [Func])
    end,
    emit_call_arg(Args).

emit_call_arg([{_,Name,_}|As]) -> stick("~s", [Name]),emit_call_args(As);
emit_call_arg(A) -> emit_call_args(A).

emit_call_args([]) -> stick(");~n",[]);
emit_call_args([{_,Name,_}|Args]) ->
    stick(", ~s", [Name]),
    emit_call_args(Args).

emit_free([]) -> ok;
emit_free([{Type,Name,_}|Args]) ->
    case {Type,what_group(Type)} of
	{_,list} -> stick("  g_free(~s);~n", [Name]);
	{"gchar*",_}-> stick("  free(~s);~n", [Name]);
	{"char*",_}-> stick("  free(~s);~n", [Name]);
	_ -> ok
    end,
    emit_free(Args).

emit_return(Ret) ->
    case what_group(Ret) of
	basic ->
	    {ET,Cast} = what_etype(Ret),
	    stick("  gn_put_~w(XBUF,(~s)R);~n", [ET,Cast]);
	none -> stick("  gn_put_void(XBUF);~n", []);
	object -> stick("  gn_put_object(XBUF,R);~n", []);
	struct -> stick("  gn_put_struct(XBUF,~p,(void*)R);~n", [unstar(Ret)]);
	crap -> stick("  gn_put_CRAP(XBUF,~p,R);~n", [Ret]);
	Kind -> stick("  gn_put_~w(XBUF,~p,R);~n", [Kind, Ret])
    end.

emit_final() ->
    stick("  return TRUE;~n}~n",[]).

what_gtype(This) ->
    [#type{what=object,gtype=Type}] = ets:lookup(types,This),
    Type.

what_etype(Basic) ->
    [#type{what=basic,etype=Type,cast=Cast}] = ets:lookup(types,Basic),
    {Type,Cast}.

what_group([]) -> none;
what_group("none") -> none;
what_group(Typ) ->
    case {ets:lookup(types,Typ),ets:lookup(types,unstar(Typ))} of
	{[#type{what = object}],_} -> object;
	{[],[#type{what = object}]} -> object;
	{[#type{what = basic}],_} -> basic;
	{[#type{what = enum}],_} -> enum;
	{[#type{what = flags}],_} -> flags;
	{[#type{what = list}],_} -> list;
	{_,[#type{what = struct}]} -> struct;
	_ -> mark_as_crap(Typ)
    end.

mark_as_crap(Typ) ->
    put(crap,crap),
    ets_upd(bogus,{type,Typ}),
    crap.

estick(X) ->
    case get(efunc) of
	undefined -> put(efunc, {X,[]});
	{F,As} -> put(efunc,{F,[X|As]})
    end.
	     
stick(Form, Args) ->
    S = io_lib:fwrite(Form, Args),
    case get(cfunc) of
	undefined -> put(cfunc, S);
	SS -> put(cfunc, SS++S)
    end.

unstick() -> erase(cfunc), erase(efunc), erase(crap).
unstick(FD1,FD2,FDok) ->
    case get(crap) of
	crap -> ets_upd(bogus,bad),cunstick(FD2);
	_ -> ets_upd(bogus,good),cunstick(FD1), eunstick(FDok)
    end,
    unstick().

cunstick(FD) -> io:fwrite(FD, "~s", [lists:flatten(get(cfunc))]).

eunstick(FD) ->
    {{Cname,_Ename},As} = get(efunc),
    io:fwrite(FD, "~s(",[Cname]),
    eunstick_args(FD,As),
    io:fwrite(FD, ")~n",[]).

eunstick_args(_FD,[]) -> ok;
eunstick_args(FD,As) ->
    [A1|AT] = reverse(As),
    io:fwrite(FD,"~s",[upcase_1st(A1)]),
    foreach(fun(A)->io:fwrite(FD,",~s",[upcase_1st(A)]) end, AT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_black(FN) ->
    bio:string(FN, fun do_black/2, nil).

do_black(Str,_State) ->
    case ets:lookup(funcs,Str) of
	[] -> ok;
	[Rec] -> ets:insert(funcs, Rec#func{black=yes})
    end.

gname(Dir,Pref,Mid,Vsn,Ext) -> 
    join([Dir,gen,Pref++Mid++"-"++Vsn++"."++Ext]).

do_funcs(FN) ->
    io:fwrite("~p~n", [FN]),
    bio:string(FN, fun do_funcl/2, nil).

do_funcl("",State) ->
    State;
do_funcl(";"++_,State) ->
    State;
do_funcl(")", bogus) ->
    nil;
do_funcl(")", {_,Data}) ->
    ets:insert(funcs,Data),
    nil;
do_funcl("(define-function "++_, nil) ->
    {func,#func{}};
do_funcl("(define-method "++_, nil) ->
    {meth,#func{}};
do_funcl("  (parameters", {Flag,Func}) ->
    {para, {Flag, Func}};
do_funcl(_, bogus) ->
    bogus;
do_funcl("  (varargs #t)", _) ->
    bogus;
do_funcl("  )", {para, {Flag, Func}}) ->
    {Flag, Func};
%%do_funcl("   )", {para, {Flag, Func}}) ->
%%    {Flag, Func};
do_funcl(Str, {para, {Flag, Func}}) ->
    OP = Func#func.paras,
    case string:tokens(Str,"'()\" ") of
	["const-"++Para, Name|_Doc] ->
	    {para, {Flag, Func#func{paras=OP++[{Para,Name,const}]}}};
	[Para, Name|_Doc] ->
	    {para, {Flag, Func#func{paras=OP++[{Para,Name,no}]}}}
    end;
do_funcl("  (c-name \""++C, {Flag,Func}) ->
    {Flag,Func#func{cname=trnc(2,C), pref=pref(C)}};
do_funcl("  (return-type \""++C, {Flag,Func}) ->
    case trnc(2,C) of
	"const-"++R -> {Flag,Func#func{ret=R,const=yes}};
	R -> {Flag,Func#func{ret=R}}
    end;
do_funcl("  (of-object \""++C, {meth,Func}) ->
%%%    {meth,Func#func{this=trnc(2,C)}};
    {meth,Func#func{paras=[{trnc(2,C)++"*","object",no}]}};
do_funcl(_,State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lists(_Dir) ->
    Lists = ["GType*"],
    foreach(fun ins_list/1, Lists).

ins_list(L) -> ets:insert(types,#type{what=list,cname=L,cast=""}).

structs(Dir) ->
io:fwrite("dir is ~p~n",[Dir]),
    FN = join([dirname(dirname(Dir)),src,"gtkNode_structs.c"]),
    Structs = bio:string(FN, fun do_structs/2, []),
    foreach(fun ins_struct/1, Structs).
do_structs(Str, Acc) ->
    case regexp:match(Str,"gn_construct_.*\\(") of
	nomatch -> Acc;
	{match,St,Le} -> [string:substr(Str,St+13,Le-14)|Acc]
    end.
ins_struct(S) -> ets:insert(types,#type{what=struct,cname=S,cast=""}).
    
basic_types() ->
    Basic = [{boolean, "int", ["gboolean","boolean"]},
	     {string, "char*", ["gchar*","char*"]},
	     {double, "double", ["gdouble","gfloat"]},
	     {longlong, "long long", ["gint64","gint","int","size_t"]},
	     {ulonglong, "unsigned long long", 
	      ["guint","guint8","guint16","guint32"]},
	     {void,none,["void"]}],
    foreach(fun ins_basic/1, Basic).
ins_basic({ET,Cast,Bs}) ->
    foreach(fun(B) -> ins_basic(B,ET,Cast) end, Bs).
ins_basic(B,ET,Cast) ->
    ets:insert(types,#type{what=basic,cname=B,etype=ET,cast=Cast}).

do_types(FN) ->
    io:fwrite("~p~n", [FN]),
    bio:string(FN, fun do_typel/2, nil).

do_typel("",State) ->
    State;
do_typel(";"++_,State) ->
    State;
do_typel(")", {_,Data}) ->
    ets:insert(types,Data),
    nil;
do_typel("(define-boxed "++_, nil) ->
    {define_boxed,#type{what=boxed}};
do_typel("(define-enum "++_, nil) ->
    {define_enum,#type{what=enum}};
do_typel("(define-flags "++_, nil) ->
    {define_flags,#type{what=flags}};
do_typel("(define-object "++_, nil) ->
    {define_object,#type{what=object}};
do_typel("(define-pointer "++_, nil) ->
    {define_pointer,#type{what=pointer}};
do_typel("  (c-name \""++C, {Flag,Type}) ->
    {Flag,Type#type{cname=trnc(2,C)}};
do_typel("  (gtype-id \""++C, {Flag,Type}) ->
    {Flag,Type#type{gtype=trnc(2,C),cast=cast(trnc(2,C))}};
do_typel(_,State) ->
    State.

cast(C) ->
    [Pref,"TYPE"|Toks] = string:tokens(C,"_"),
    string_join([Pref|Toks], "_").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
upcase_1st(Str) -> [hd(Str)+$A-$a|tl(Str)].

rem_pref([X|Func], [X|Pref]) -> rem_pref(Func, Pref);
rem_pref([$_|Func], []) -> Func.

pref(C) -> hd(string:tokens(C,"_")).

unstar(Str) ->
    case reverse(Str) of
	"*"++X -> reverse(X);
	_ -> Str
    end.

trnc(N,C) -> lists:sublist(C, length(C)-N).

string_join([Pref|Toks], Sep) ->
    foldl(fun(Tok,O) -> O++Sep++Tok end, Pref, Toks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_types(FD) ->
    F = fun(E) -> io:fwrite(FD, "~4w - ~s~n", E) end,
    foreach(F,reverse(sort(ets:match(bogus,{{type,'$2'},'$1'})))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_new(Tab) ->
    catch ets:delete(Tab),
    ets_new(Tab, [{keypos,3}]).
ets_new(Tab, Opt) ->
    catch ets:delete(Tab),
    ets:new(Tab,[named_table,ordered_set]++Opt).
ets_upd(Tab, Key) ->
    case catch ets:update_counter(Tab, Key, 1) of
	{'EXIT',_} -> ets:insert(Tab, {Key,1});
	_ -> ok
    end.
