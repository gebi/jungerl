%%%-------------------------------------------------------------------
%%% Created : 2 Nov 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : ehtml help functions
%%%-------------------------------------------------------------------
-module(ehtml).

-export([text/1, text/2, text/3, hidden/2, submit/1, checkbox/3, 
	 radiobutton/3, select/2, textarea/3, textarea/4, table/1,
	 table/2, checkbox/2, submit/2, checkbox/4, script/1,
	 select/3, text/4, file/2, file/3,
	 label_text/5, label_text/6, 'div'/1, 'div'/2]).

-import(lists, [map/2]).


'div'(Body) ->
    'div'(Body, []).

'div'(Body, Attrs) ->
    {'div', Attrs, Body}.


text(Name)  -> 
    text(Name, "").

text(Name, Text)  -> 
    text(Name, Text, "30").

text(Name, Text, Size)  -> 
    text(Name, Text, Size, []).

text(Name, Text, Size, Extra)  -> 
    {input, [{type, "text"}, {name, Name}, {value, Text}, {size, Size}|Extra]}.


label_text(Label, For, Name, Text, Size) ->
    label_text(Label, For, Name, Text, Size, []).

label_text(Label, For, Name, Text, Size, Extra) ->
    [{label, [{for, For}], Label},
     text(Name, Text, Size, [{id, For}|Extra])].


file(Name, Value)  -> 
    file(Name, Value, []).

file(Name, Value, Extra)  -> 
    {input, [{type, "file"}, {name, Name}, {value, Value}|Extra]}.
    

textarea(Name, Rows, Cols)  -> 
    textarea(Name, Rows, Cols, "").

textarea(Name, Rows, Cols, Text)  -> 
    {textarea, [{type, "textarea"}, {name, Name}, 
		{rows, Rows}, {cols, Cols}],
     [Text]}.
    
hidden(Name, Value)  -> 
    {input, [{type, "hidden"}, {name, Name}, {value, Value}]}.
    
submit(Text)  -> 
    {input, [{type, "submit"}, {value, Text}]}.
    
submit(Text, Extra)  -> 
    {input, [{type, "submit"}, {value, Text} | Extra]}.
    
checkbox(Name, Bool) -> 
    {input, [{type, "checkbox"}, {name, Name} | checked(Bool)]}.

checkbox(Name, Value, Bool) -> 
    {input, [{type, "checkbox"}, {name, Name},
	     {value, Value}|checked(Bool)]}.

checkbox(Name, Value, Bool, Other) when list(Other) -> 
    {input, [{type, "checkbox"}, {name, Name},
	     {value, Value}|checked(Bool)] ++ Other}.

radiobutton(Name, Value, Bool)  -> 
    {input, [{type, "radio"}, {name, Name},
	     {value, Value}|checked(Bool)]}.
    
checked(true)  -> [checked];
checked(false) -> [].


%%% Simplest table possible
table(Rows) ->
    {table, [],
     td_rows(Rows)}.

table(Header, Rows) ->
    {table, [],
     [{tr, [], map(fun(C) -> {th, [], [C]} end, Header)},
      td_rows(Rows)]}.

td_rows(Rows) ->
     lists:map(fun(R) ->
		       {tr, [],
			map(fun(C) -> {td, [], [C]} end, R)}
	       end, Rows).

%%%
%%% select/2 - makes a menu
%%% Options is a list of: {Id,Text,SelectedBool} | {Id,Text}
%%%
select(Name, Options) ->
    select(Name, Options, []).

select(Name, Options, Extra) ->
    [{select, [{name, Name}|Extra],
      map(fun({Id, Text, true}) ->
		  {option, [{value, Id},selected], Text};
	     ({Id, Text, false}) ->
		  {option, [{value, Id}], Text};
	     ({Id, Text}) ->
		  {option, [{value, Id}], Text}
	  end, Options)}].


script(Script) ->
    {script, [{type, "text/javascript"}],
     {pre_html, ["\n<!--\n", Script, "\n-->\n"]}}.



