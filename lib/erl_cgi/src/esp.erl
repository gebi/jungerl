%%% File    : esp.erl
%%% Author  :  <tony@orre.bluetail.com>
%%% Description : esp utility functions
%%% Created : 10 Apr 2002 by  <tony@orre.bluetail.com>

-module(esp).

-export([load/1, load/2]).
-compile(export_all).

-import(lists, [map/2, foreach/2, flatmap/2, foldl/3, member/2]).

%%
%% parse and evaluate and esp page
%%
load(File) ->
    load(File, erl_eval:new_bindings()).

load(File, Bindings) ->
    case file:open(File, [read]) of
        {ok,Fd} ->
            R = eval_stream(Fd, Bindings),
            file:close(Fd),
            R;
        Error ->
            Error
    end.

eval_stream(Fd, Bs) ->
    case eval_stream(Fd, [], Bs, []) of
	{ok,LastForm} -> {ok,LastForm};
        {error,[Error1 | _]} -> {error,Error1}
    end.

eval_stream(Fd, E, Bs, LF) ->
    eval_stream(io:parse_erl_exprs(Fd, ''), Fd, E, Bs, LF).

eval_stream({ok,Form,EndLine}, Fd, E, Bs0, LF) ->
    case catch erl_eval:exprs(Form, Bs0) of
        {value,V,Bs} ->
            eval_stream(Fd, E, Bs, V);
        {'EXIT',Reason} ->
            eval_stream(Fd, [{EndLine,file,Reason}|E], Bs0, LF);
	Thrown ->
	    {ok,Thrown}
    end;
eval_stream({error,What,EndLine}, Fd, E, Bs, LF) ->
    eval_stream(Fd, [What | E], Bs, LF);
eval_stream({eof,EndLine}, Fd, E, Bs, LF) ->
    case E of 
	[] -> {ok, LF};
	_  -> {error, lists:reverse(E)}
    end.

%%
%% Transform tools
%%
	    
replace(Dict, {var,Var}) ->
    case lists:keysearch(Var,1,Dict) of
	{value, {_,Value}} -> Value;
	false -> ""
    end;
replace(Dict, Esp) when list(Esp) ->
    map(fun(Item) -> replace(Dict,Item) end, Esp);
replace(Dict, Esp) when tuple(Esp) ->
    list_to_tuple(
      map(fun(Item) -> replace(Dict,Item) end, tuple_to_list(Esp)));
replace(Dict, Esp) ->
    Esp.

aname(Name) when atom(Name) -> Name;
aname(Name) when list(Name) -> list_to_atom(Name);
aname(Name) when integer(Name) -> list_to_atom(integer_to_list(Name)).

%% Lookup if Attr name exist and return it's state value
st_value(As, St) ->
    case lists:keysearch(name, 1, As) of    
	{value,{_,Name}} ->
	    case lists:keysearch(aname(Name), 1, St) of
		{value, {_, Value}} ->
		    {value,Value};
		false -> false
	    end;
	false -> false
    end.
    

%% check if the name has the value then add checked as an attribute
checked(As, St) ->
    case st_value(As, St) of
	{value,St_value} ->
	    case lists:keysearch(value,1,As) of
		{value,{_, Str_value}} ->
		    if Str_value == St_value -> [checked | As];
		       true -> As
		    end;
		false -> As
	    end;
	false -> As
    end.

%% search among optgroups and opts after value = Val
selected(Val, [{optgroup,As,OptGroup}|Opts]) ->
    [{optgroup,As,selected(Val,OptGroup)} | selected(Val,Opts)];
selected(St_value, [{option,As,Data}|Opts]) ->
    case lists:keysearch(value, 1, As) of
	{value,{_,Str_value}} ->
	    if Str_value == St_value ->
		    [{option,[selected|As],Data}|Opts];
	       true ->
		    [{option,As,Data}|selected(St_value,Opts)]
	    end;
	false ->
	    [{option,As,Data}|selected(St_value,Opts)]
    end;
selected(St_value, []) ->
    [];
selected(St_value, Other) ->
    Other.

%% search among optgroups and opts after missing value
%% and select that (even if diabled)
selected1([{optgroup,As,OptGroup}|Opts]) ->
    [{optgroup,As,selected1(OptGroup)} | selected1(Opts)];
selected1([{option,As,Data}|Opts]) ->
    case lists:keysearch(value, 1, As) of
	false ->
	    [{option,[selected|As],Data}|Opts];
	{value,{_,Str_value}} ->
	    [{option,As,Data}|selected1(Opts)]
    end;
selected1([]) ->
    [];
selected1(Other) ->
    Other.


value(As, St) ->
    case st_value(As, St) of
	{value,St_value} ->
	    case lists:keysearch(value,1,As) of
		false ->
		    As++[{value,St_value}];
		{value,_} ->
		    lists:keyreplace(value,1,As,{value,St_value})
	    end;
	false -> As
    end.

%% Update inputfileds/textarea/select etc according to State
state(St, {input,As}) -> st_input(St, As);
state(St, {input,As,[]}) -> st_input(St, As);

state(St, {select,As,Opts}) ->
    case st_value(As, St) of
	false ->
	    %% select first option with value
	    {select,As,selected1(Opts)}; 
	{value,St_value} ->
	    {select,As,selected(St_value,Opts)}
    end;
state(St, {textarea,As,Data}) ->
    case st_value(As, St) of
	{value, St_value} ->
	    {textarea,As,{pcdata,St_value}};
	false ->
	    {textarea,As,Data}
    end;
state(St, {form,As,Items}) -> %% update action field sometimes
    case lists:keysearch(form_action, 1, St) of
	{value, {_, Action}} ->
	    As1 = lists:keyreplace(action, 1, As, {action,Action}),
	    {form,As1,state(St,Items)};
	false ->
	    {form,As,state(St,Items)}
    end;
state(St, Esp) when list(Esp) ->
    map(fun(Item) -> state(St,Item) end, Esp);
state(St, Esp) when tuple(Esp) ->
    list_to_tuple(
      map(fun(Item) -> state(St,Item) end, tuple_to_list(Esp)));
state(St, Esp) ->
    Esp.

st_input(St, As) ->
    case lists:keysearch(type,1,As) of
	{value, {_,"radio"}} ->
	    {input, checked(As, St)};
	{value, {_,"checkbox"}} ->
	    {input, checked(As, St)};
	{value, _} ->
	    {input, value(As, St)};
	_ ->
	    {input, As}
    end.

%% Extract column for xls|txt|html format output
col_insert(As, Col) ->
    case lists:keysearch(name,1,As) of
	{value,{_,Name}} ->
	    case lists:keysearch(type,1,As) of
		{value,{_,"hidden"}} ->
		    case Name of %% filter quiz,user,pass from output
			"quiz" -> Col;
			"user" -> Col;
			"pass" -> Col;
			_ -> col_ins(aname(Name), Col)
		    end;
		{value,{_,"submit"}} -> Col;
		_ -> col_ins(aname(Name), Col)
	    end;
	false ->
	    Col
    end.

col_ins(Nm, [Nm|Col]) -> [Nm|Col];
col_ins(Nm, [C|Col]) -> [C|col_ins(Nm,Col)];
col_ins(Nm, []) -> [Nm].

col(Esp) ->
    col(Esp,[]).

col({input,As},Col)         -> col_insert(As,Col);
col({input,As,[]},Col)      -> col_insert(As,Col);
col({select,As,Opts},Col)   -> col_insert(As,Col);
col({textarea,As,Data},Col) -> col_insert(As,Col);
col(Esp,Col) when list(Esp) ->
    foldl(fun(Item,Col1) ->
		  col(Item,Col1)
	  end, Col, Esp);
col(Esp,Col) when tuple(Esp) ->
    foldl(fun(Item,Col1) ->
		  col(Item,Col1)
	  end, Col, tuple_to_list(Esp));
col(Es,Col) -> Col.

%% Given a coloumn and a state extract all state values for the columns
st_col(St, [C|Cs], Strip) ->
    case lists:keysearch(C, 1, St) of
	{value, {_, Val}} ->
	    [{C,strip(Val,Strip)} | st_col(St, Cs,Strip)];
	false ->
	    [{C,""}  | st_col(St, Cs,Strip)]
    end;
st_col(St, [], _) ->
    [].
%%
%% replace characters in Strip with $\s 
%% (several consecutive characters is replaced with one $\s)
%%
strip(Val, "") -> Val;
strip(Val, Strip) when list(Val) ->
    strip0(Val,Strip);
strip(Val, Strip) -> Val.
     

strip0([C|Cs], Strip) ->
    case member(C, Strip) of
	true  -> [$\s|strip1(Cs, Strip)];
	false -> [C|strip0(Cs,Strip)]
    end;
strip0([], _) -> [].

strip1([C|Cs], Strip) ->
    case member(C, Strip) of
	true  -> strip1(Cs, Strip);
	false -> [C|strip0(Cs,Strip)]
    end;
strip1([], _) -> [].



    

    
    
    
    
    


