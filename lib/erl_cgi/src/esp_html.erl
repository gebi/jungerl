%%% File    : esp_html.erl
%%% Author  : root <tony@localhost.localdomain>
%%% Description : HTML generator from esp format
%%% Created : 27 Mar 2002 by root <tony@localhost.localdomain>

-module(esp_html).

-export([format/1, format/2]).
-export([to_value/1]).
-export([pcdata/2, pcdata/1]).
-import(lists, [map/2, reverse/1,member/2]).

%% -define(debug,true).

-ifdef(debug).
-define(dbg(Fmt,Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmt,Args), ok).
-endif.

-record(state,
	{
	  %% default formatting function is just a dummy
	  fmt = fun(Tag,As,Body,Ps) -> "" end, 
	  %% default environment is empty [{Var,Value}]
	  env = []                        
	 }).
	  
			

-define(HTML_4_0, 
	"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">").
-define(HTML_4_0_Frameset,
	"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Frameset//EN\">").

format({document,Cs}) ->
    format({document, Cs}, #state { fmt = fun format_fun/4 });
format({flow,Cs}) ->
    format({flow, Cs}, #state { fmt = fun format_fun/4 }).

format({document, Cs}, St) ->
    case Cs of
	[{html,As1,Cs1}] ->
	    elem(html,As1,Cs1,[html],St);
	[{head,As1,Cs1},{body,As2,Cs2}] ->
	    elem(html,[],[{head,As1,Cs1},{body,As2,Cs2}],[html],St);
	Cs ->
	    [?HTML_4_0 ++ "$\n",
	     repeat(fun(T) -> html:is_flow(T) end, Cs, [body,html],St)]
    end;
format({flow, Cs}, St) ->
    repeat(fun(T) -> html:is_flow(T) end, Cs, [body,html],St).
    



fmt(St,Tag,As,Body,Ps) ->
    case St#state.fmt of
	undefined -> "";
	Fun -> Fun(Tag,As,Body,Ps)
    end.
    

format_fun(Tag,As,Body,Ps) ->
    TAG = upper(atom_to_list(Tag)),
    %% Indent = indent(Ps),
    case tag_type(Tag) of
	break_both ->
	    [$<,TAG,attrlist(Tag,As),$>,$\n,
	     Body,
	     $<,$/,TAG,$>,$\n];
	break_begin ->
	    [$<,TAG,attrlist(Tag,As),$>,$\n,Body,$<,$/,TAG,$>];
	break_end ->
	    [$<,TAG,attrlist(Tag,As),$>, Body,$<,$/,TAG,$>,$\n];	    
	break_none ->
	    [$<,TAG,attrlist(Tag,As),$>, Body,$<,$/,TAG,$>];
	no_end_tag ->
	    [$<,TAG,attrlist(Tag,As),$>]
    end.

indent(Ps) ->
    lists:duplicate(length(Ps)*2,$\s).

tag_type(meta) -> no_end_tag;
tag_type(base) -> no_end_tag;
tag_type(link) -> no_end_tag;
tag_type(param) -> no_end_tag;
tag_type(img) ->   no_end_tag;
tag_type(col) -> no_end_tag;
tag_type(frame) ->  no_end_tag;
tag_type(br) -> no_end_tag;
tag_type(base_font) -> no_end_tag;
tag_type(hr) -> no_end_tag;
tag_type(area) -> no_end_tag;
tag_type(input) -> no_end_tag;
tag_type(tt) -> break_none;
tag_type(i) ->  break_none;
tag_type(b) ->  break_none;
tag_type(u) ->  break_none;
tag_type(s) ->  break_none;
tag_type(strike) ->  break_none;
tag_type(big) ->  break_none;
tag_type(small) ->  break_none;
tag_type(p) -> break_end;
tag_type(option) -> break_end;
tag_type(title) -> break_end;
tag_type(_) -> break_both.


upper([C|Cs]) when C >= $a, C =< $z ->
    [(C-$a)+$A | upper(Cs)];
upper([C|Cs]) ->
    [C|upper(Cs)];
upper([]) -> [].
    

%%
%% EXTENSION <erl>#PCDATA</erl>
%%
elem(erl,As,Exprs,Ps,St) ->
    %% el(erl,As,Ts,fun(T) -> T == pcdata end,Ps,St);
    %% Exprs erlang code
    {value,V,Bs} = erl_eval:exprs(Exprs,[]),
    %% V must be a erlang tagged html format
    ?dbg("erlang value = ~p\n", [V]),
    V;

%%
%% <!ENTITY % html.content "HEAD, BODY">
%% <!ELEMENT HTML O O (%html.content;)    -- document root element -->
%%
elem(html,As,Cs,Ps,St) ->
    {E1,Cs1} = require(head,Cs,Ps,St),
    {E0,E2} = case Cs1 of 
		  [{body,As2,Cs2}] -> 
		      {?HTML_4_0 ++ "\n",
		       elem(body,As2,Cs2,[body,html],St)};
		  [{frameset,As2,Cs2}] ->
		      {?HTML_4_0_Frameset ++ "\n",
		       elem(frameset,As2,Cs2,[frameset,html],St)}
	      end,
    [E0,fmt(St,html,As,[E1,E2],Ps)];

%%
%% <!ELEMENT HEAD O O (%head.content;) +(%head.misc;) -- document head -->
%%
%% FIXME: head.content = TITLE & BASE?
%% i.e one TITLE and optionally one BASE in any order
%% 
elem(head,As,Cs,Ps,St) ->
    el(head,As,Cs,fun(T) -> html:is_head_misc(T) or 
				html:is_head_content(T) end,Ps,St);

%%
%% <!ELEMENT BODY O O (%flow)+ +(INS|DEL) -- document body -->
%%
%% FIXME: INS and DEL is not handled
%%
elem(body,As,Cs,Ps,St) -> el(body,As,Cs, html:flow(),Ps,St);

elem(title, As,Cs,Ps,St) ->
    el(title,As,Cs,fun(T) -> T == pcdata end,Ps,St);
%%    ["<TITLE", attrlist(title,As), ">", pcdata(Title,false), "</TITLE>\n"];
%%
%% <!ELEMENT META - O EMPTY               -- generic metainformation -->
%%
elem(meta, As, Cs,Ps,St) -> el0(meta,As,Cs,Ps,St);

%%
%% <!ELEMENT BASE - O EMPTY               -- document base URI -->
%%
elem(base,As,Cs,Ps,St) -> el0(base,As,Cs,Ps,St);

%%
%% <!ELEMENT LINK - O EMPTY               -- a media-independent link -->
%%
elem(link,As,Cs,Ps,St)  -> el0(link,As,Cs,Ps,St);

elem(noscript, As, Cs,Ps,St) -> el(noscript,As,Cs,html:block(),Ps,St);

elem(object, As, Cs,Ps,St) -> el(object,As,Cs,
			   fun(T) -> html:is_flow(T) or (T == param) end,Ps,St);

elem(param, As, Cs,Ps,St) -> el0(param,As,Cs,Ps,St);

elem(img, As, Cs,Ps,St) -> el0(img,As,Cs,Ps,St);


elem(style,As,Cs,Ps,St) ->
    {Begin,End} = case lists:keysearch(hide, 1, As) of
		      {value,{_,true}} -> 
			  {"<!--\n", " -->\n"};
		      _ -> { "", "" }
		  end,
    ["<STYLE", attrlist(style,As), ">\n", 
     Begin,
     map(fun({Name,Style}) -> [Name, " { ", Style, " }\n"] end, Cs),
     End,
     "</STYLE>\n"];

elem(script,As,Script,Ps,St) ->
    {Begin,End} = case lists:keysearch(hide, 1, As) of
		      {value,{_,true}} -> 
			  case lists:keysearch(type, 1, As) of
			      "text/tcl"        -> 
				  {"<!--\n", "# -->\n"};
			      "text/vbscript"   -> 
				  {"<!--\n", "' -->\n"};
			      "text/javascript" -> 
				  {"<!--\n", "// -->\n"};
			      "text/erlscript"  -> 
				  {"<!--\n", "%  -->\n"};
			      _ ->
				  {"<!--\n", " -->\n"}
			  end;
		      _ -> { "", "" }
		  end,
    [ "<SCRIPT", attrlist(script,As), ">\n",    
      Begin,
      Script, "\n",
      End,
      "</SCRIPT>\n"];

elem(form, As, Cs,Ps,St) -> el(form,As,Cs,
			    fun(T) -> html:is_flow(T) and (T =/= form) end,Ps,St);
%%
%% <!ELEMENT TABLE - -
%%     (CAPTION?, (COL*|COLGROUP*), THEAD?, TFOOT?, TBODY+)>
%%
elem(table, As, Cs0,Ps,St) ->
    {E1,Cs1} = optional(caption,Cs0,Ps,St),
    {E2,Cs2} = repeat0(fun(Tag) -> (Tag == col) or (Tag == colgroup) end,
		      Cs1,Ps,St),
    {E3,Cs3} = optional(thead,Cs2,Ps,St),
    {E4,Cs4} = optional(tfoot,Cs3,Ps,St),    
    {E5,Cs5} = repeat0(fun(Tag) -> Tag==tbody end,Cs4,Ps,St),
    E6 = repeat(fun(T) -> T == tr end,Cs5,Ps,St),
    fmt(St,table,As,[E1,E2,E3,E4,E5,E6],Ps);

%%
%% <!ELEMENT TBODY    O O (TR)+           -- table body -->    
%%
elem(tbody, As, Cs,Ps,St) -> el(tbody,As,Cs,fun(T) -> T == tr end,Ps,St);

%%
%% <!ELEMENT THEAD    - O (TR)+           -- table header -->
%%
elem(thead, As, Cs,Ps,St) -> el(thead,As,Cs,fun(T) -> T == tr end,Ps,St);

%%
%% <!ELEMENT TFOOT    - O (TR)+           -- table footer -->
%%
elem(tfoot, As, Cs,Ps,St) -> el(tfoot,As,Cs,fun(T) -> T == tr end,Ps,St);

%%
%% <!ELEMENT TR       - O (TH|TD)+        -- table row -->
%%
elem(tr,As,Cs,Ps,St) -> el(tr,As,Cs,fun(T) -> (T == th) or (T == td) end,Ps,St);

%%
%% <!ELEMENT (TH|TD)  - O (%flow;)* -- table header cell, table data cell-->
%%
elem(th,As,Cs,Ps,St)  -> el(th,As,Cs,html:flow(),Ps,St);
elem(td,As,Cs,Ps,St) ->  el(td,As,Cs,html:flow(),Ps,St);

%%
%% <!ELEMENT CAPTION  - - (%inline;)*     -- table caption -->
%%
elem(caption,As,Cs,Ps,St)  -> el(caption,As,Cs,html:inline(),Ps,St);
elem(col,As,Cs,Ps,St)      -> el0(col,As,Cs,Ps,St);
elem(colgroup,As,Cs,Ps,St) -> el(colgroup,As,Cs,fun(T) -> T == col end,Ps,St);

%% Frames

%%
%% <!ELEMENT FRAMESET - - ((FRAMESET|FRAME)+ & NOFRAMES?) -- window subdivision-->
%%
elem(frameset,As,Cs,Ps,St) ->
    el(frameset,As,Cs, 
       fun(T) ->
	       (T == frameset) or (T == frame) or (T == noframes)
       end,Ps,St);

elem(frame, As, Cs,Ps,St) -> el0(frame,As,Cs,Ps,St);

elem(iframe, As, Cs,Ps,St) -> el(iframe,As,Cs, html:flow(),Ps,St);

elem(noframes,As,Cs,Ps,St) -> el(noframes,As,Cs, html:flow(),Ps,St);

%%
%% LAYER, ILAYER and NOLAYER
%%
elem(layer,As,Cs,Ps,St) ->   el(layer, As, Cs, html:flow(),Ps,St);

elem(ilayer, As,Cs,Ps,St) -> el(ilayer, As, Cs, html:flow(),Ps,St);

elem(nolayer,As,Cs,Ps,St) -> el(nolayer, As, Cs, html:flow(),Ps,St);

%% Lists
elem(ul, As, Cs,Ps,St) -> el(ul,As,Cs,fun(T) -> T == li end,Ps,St);
elem(ol, As, Cs,Ps,St) -> el(ol,As,Cs,fun(T) -> T == li end,Ps,St);
elem(li, As, Cs,Ps,St) -> el(li,As,Cs,html:flow(),Ps,St);
elem(dl, As, Cs,Ps,St) -> el(dl,As,Cs,fun(T) -> (T == dt) or (T == dd) end,Ps,St);
elem(dt, As, Cs,Ps,St) -> el(dt,As,Cs,html:inline(),Ps,St);
elem(dd, As, Cs,Ps,St) -> el(dd,As,Cs,html:flow(),Ps,St);

%%
%% %fontstyle
%% <!ELEMENT (%fontstyle;|%phrase;) - - (%inline;)*>
%%

elem(tt,As, Cs,Ps,St)     -> el(tt,As,Cs,html:inline(),Ps,St);
elem(i,As, Cs,Ps,St)      -> el(i,As,Cs,html:inline(),Ps,St);
elem(b,As, Cs,Ps,St)      -> el(b,As,Cs,html:inline(),Ps,St);
elem(u,As, Cs,Ps,St)      -> el(u,As,Cs,html:inline(),Ps,St);
elem(s,As, Cs,Ps,St)      -> el(s,As,Cs,html:inline(),Ps,St);
elem(strike,As, Cs,Ps,St) -> el(strike,As,Cs,html:inline(),Ps,St);
elem(big,As, Cs,Ps,St)    -> el(big,As,Cs,html:inline(),Ps,St);
elem(small,As, Cs,Ps,St)  -> el(small,As,Cs,html:inline(),Ps,St);

%%
%% <!ELEMENT (%heading;)  - - (%inline;)* -- heading -->
%% 
elem(h1,As,Cs,Ps,St)      -> el(h1,As,Cs,html:inline(),Ps,St);
elem(h2,As,Cs,Ps,St)      -> el(h2,As,Cs,html:inline(),Ps,St);
elem(h3,As,Cs,Ps,St)      -> el(h3,As,Cs,html:inline(),Ps,St);
elem(h4,As,Cs,Ps,St)      -> el(h4,As,Cs,html:inline(),Ps,St);
elem(h5,As,Cs,Ps,St)      -> el(h5,As,Cs,html:inline(),Ps,St);
elem(h6,As,Cs,Ps,St)      -> el(h6,As,Cs,html:inline(),Ps,St);

%%
%% <!ELEMENT ADDRESS - - (%inline;)* -- information on author -->
%%
elem(address,As,Cs,Ps,St) -> el(address,As,Cs,html:inline(),Ps,St);

%%
%% %phrase
%% <!ELEMENT (%fontstyle;|%phrase;) - - (%inline;)*>
%%

elem(em,As,Cs,Ps,St)      -> el(em,As,Cs,html:inline(),Ps,St);
elem(strong,As,Cs,Ps,St)  -> el(strong,As,Cs,html:inline(),Ps,St);
elem(dfn,As,Cs,Ps,St)     -> el(dfn,As,Cs,html:inline(),Ps,St);
elem(code,As,Cs,Ps,St)    -> el(code,As,Cs,html:inline(),Ps,St);
elem(samp,As,Cs,Ps,St)    -> el(samp,As,Cs,html:inline(),Ps,St);
elem(kbd,As,Cs,Ps,St)     -> el(kbd,As,Cs,html:inline(),Ps,St);
elem(var,As,Cs,Ps,St)     -> el(var,As,Cs,html:inline(),Ps,St);
elem(cite,As,Cs,Ps,St)    -> el(cite,As,Cs,html:inline(),Ps,St);
elem(abbr,As,Cs,Ps,St)    -> el(abbr,As,Cs,html:inline(),Ps,St);
elem(acronym,As,Cs,Ps,St) -> el(acronum,As,Cs,html:inline(),Ps,St);

%%
%% <!ELEMENT BLOCKQUOTE - - (%block;|SCRIPT)+ -- long quotation -->
%%
elem(blockquote,As,Cs,Ps,St) -> 
    el(blockquote,As,Cs, fun(T) -> html:is_block(T) or (T == script) end,Ps,St);
%%
%% <!ELEMENT Q - - (%inline;)*            -- short inline quotation -->
%%
elem(q,As,Cs,Ps,St) ->  el(q,As,Cs,html:inline(),Ps,St);

%%
%% <!ELEMENT (SUB|SUP) - - (%inline;)*    -- subscript, superscript -->
%%
elem(sub,As,Cs,Ps,St) -> el(sub,As,Cs,html:inline(),Ps,St);
elem(sup,As,Cs,Ps,St) -> el(sup,As,Cs,html:inline(),Ps,St);

%%
%% <!ELEMENT P - O (%inline;)*            -- paragraph -->
%%
elem(p,As,Cs,Ps,St) -> el(p,As,Cs,html:inline(),Ps,St);

%%
%% <!ELEMENT BR - O EMPTY                 -- forced line break -->
%%
elem(br,As,Cs,Ps,St) -> el0(br,As,Cs,Ps,St);

%%
%% <!ELEMENT PRE - - (%inline;)* -(%pre.exclusion;) -- preformatted text -->
%%
elem(pre,As,Cs,Ps,St) ->
    el(pre,As,Cs,fun(T) -> html:is_inline(T) and not 
			       html:is_pre_exclusion(T) end,Ps,St);

%%
%% <!ELEMENT DIV - - (%flow;)*         -- generic language/style container -->
%%
elem('div',As,Cs,Ps,St) -> el('div', As, Cs, html:flow(),Ps,St);

%%
%% <!ELEMENT CENTER - - (%flow;)*       -- shorthand for DIV align=center -->
%%
elem(center,As,Cs,Ps,St) -> el(center, As, Cs, html:flow(),Ps,St);

%%
%% <!ELEMENT SPAN - - (%inline;)*      -- generic language/style container -->
%%
elem(span,As,Cs,Ps,St) -> el(span, As, Cs, html:inline(),Ps,St);

%%
%% <!ELEMENT BDO - - (%inline;)*          -- I18N BiDi over-ride -->
%%
elem(bdo,As,Cs,Ps,St) -> el(bdo, As, Cs, html:inline(),Ps,St);

%%
%% <!ELEMENT BASEFONT - O EMPTY           -- base font size -->
%%
elem(basefont,As,Cs,Ps,St) -> el0(basefont, As, Cs,Ps,St);

%%
%% <!ELEMENT FONT - - (%inline;)*         -- local change to font -->
%%
elem(font,As,Cs,Ps,St) -> el(font, As, Cs, html:inline(),Ps,St);

%%
%% <!ELEMENT HR - O EMPTY -- horizontal rule -->
%%
elem(hr, As, Cs,Ps,St) -> el0(hr, As, Cs,Ps,St);

%%
%% <!ELEMENT A - - (%inline;)* -(A)       -- anchor -->
%%
elem(a, As, Cs,Ps,St) -> 
    el(a, As, Cs, fun(T) -> html:is_inline(T) and (T =/= a) end,Ps,St);

%%
%% <!ELEMENT MAP - - ((%block;)+ | AREA+) -- client-side image map -->
%%
elem(map, As,Cs,Ps,St) ->
    el(map, As, Cs, fun(T) -> html:is_block(T) or (T == area) end,Ps,St);

%%
%% <!ELEMENT AREA - O EMPTY               -- client-side image map area -->
%%
elem(area, As,Cs,Ps,St) -> el0(area, As, Cs,Ps,St);

%%
%% <!ELEMENT TEXTAREA - - (#PCDATA)       -- multi-line text field -->
%%
elem(textarea,As,Cs,Ps,St) -> el(textarea,As,Cs,fun(T) -> T == pcdata end,Ps,St);

%%
%% <!ELEMENT FIELDSET - - (#PCDATA,LEGEND,(%flow;)*) -- form control group -->
%%
elem(fieldset,As,Cs,Ps,St) ->
    el(fieldset,As,Cs,
       fun(T) -> (T == pcdata) or (T == legend) or html:is_flow(T) end,Ps,St);
%%
%% <!ELEMENT LEGEND - - (%inline;)*       -- fieldset legend -->
%%
elem(legend,As,Cs,Ps,St) -> el(legend,As,Cs,html:inline(),Ps,St);

%% <!ELEMENT BUTTON - -
%%     (%flow;)* -(A|%formctrl;|FORM|ISINDEX|FIELDSET|IFRAME)
%%     -- push button -->
elem(button,As,Cs,Ps,St) ->
    el(button,As,Cs,
       fun(T) -> html:is_flow(T) and
		     (T =/= a) and (not html:is_formctrl(T)) and
		     (T =/= form) and (T =/= isindex) and
		     (T =/= fieldset) and (T =/= iframe)
       end,Ps,St);
%%
%% <!ELEMENT LABEL - - (%inline;)* -(LABEL) -- form field label text -->
%%
elem(label,As,Cs,Ps,St) ->
    el(label,As,Cs, fun(T) -> html:is_inline(T) and (T =/= label) end,Ps,St);

%%
%% <!ELEMENT INPUT - O EMPTY              -- form control -->
%%
elem(input, As, Cs,Ps,St) -> el0(input, As, Cs,Ps,St);


%%
%% <!ELEMENT SELECT - - (OPTGROUP|OPTION)+ -- option selector -->
%%
elem(select,As,Cs,Ps,St) ->
    el(select,As,Cs,fun(T) -> (T == optgroup) or (T == option) end,Ps,St);

%%
%% <!ELEMENT OPTGROUP - - (OPTION)+ -- option group -->
%%
elem(optgroup,As,Cs,Ps,St) ->
    el(optgroup,As,Cs,fun(T) -> (T == option) end,Ps,St);

%%
%% <!ELEMENT OPTION - O (#PCDATA)         -- selectable choice -->
%%
elem(option,As,Cs,Ps,St) ->
    el(option,As,Cs,fun(T) -> T == pcdata end,Ps,St);

elem(pcdata,Data,[],Ps,St) ->
    pcdata(Data, member(pre,Ps) or member(textarea,Ps)).

%%
%% simple element
%%   <TAG attrlist> ... </TAG>
%%
el(Tag,As,Cs,IsEntity,Ps,St) ->
    Body = repeat(IsEntity,Cs,Ps,St),
    fmt(St,Tag,As,Body,Ps).

%%
%% simple element with no (forbidden) endtag
%%   <TAG attrlist>
%%
el0(Tag,As,[],Ps,St) ->
    fmt(St,Tag,As,"",Ps);
el0(Tag,As,Cs,Ps,St) ->
    exit({bad_tag,Tag,Ps}).

%%
%% esp tags have some a number of forms
%% {Tag,Attr,Elems}
%% {Tag,Attr}
%% Tag
%%
tag(T = {Tag,Attr,Elems}) -> T;
tag({Tag,Attr}) -> {Tag,Attr,[]};
tag(Tag) when atom(Tag) -> {Tag,[],[]}.

tag1([T|Ts]) -> {tag(T),Ts};
tag1(T) -> {tag(T),[]}.

%%
%% repeat element 
%% repeat(BoolFun, Entity, Parents)
%%
repeat(IsEntity,[],Ps,St) ->
    [];
repeat(IsEntity,Ts,Ps,St) ->
    {{Tag,Attr,Elems},Ts1} = tag1(Ts),
    if Tag == erl ->
	    Ts2 = elem(erl,Attr,Elems,[Tag|Ps],St),
	    repeat(IsEntity,Ts2,Ps,St) ++
		repeat(IsEntity,Ts1,Ps,St);
       true ->
	    case IsEntity(Tag) of
		true ->
		    [elem(Tag,Attr,Elems,[Tag|Ps],St)|
		     repeat(IsEntity,Ts1,Ps,St)];
		false ->
		    exit({bad_tag,Tag,Ps})
	    end
    end.

    
%% repeat while IsEntity return 
repeat0(IsEntity,[],Ps,St) ->
    {[],[]};
repeat0(IsEntity,Ts,Ps,St) ->
    {{Tag,Attr,Elems},Ts1} = tag1(Ts),
    case IsEntity(Tag) of
	true -> 
	    Elem = elem(Tag,Attr,Elems,[Tag|Ps],St),
	    {Es,Cs} = repeat0(IsEntity,Ts1,Ps,St),
	    {[Elem|Es],Cs};
	false -> {[],Ts}
    end.

    


%% Do optional FIRST element 	    
optional(Tag,Ts,Ps,St) ->
    case tag1(Ts) of
	{{Tag,Attr,Elems},Cs} ->
	    {elem(Tag,Attr,Elems,[Tag|Ps],St), Cs};
	_ ->
	    {"", Ts}
    end.

%% Do required FIRST element 	    
require(Tag,Ts,Ps,St) ->
    {{Tag,Attr,Elems},Cs} = tag1(Ts),
    {elem(Tag,Attr,Elems,[Tag|Ps],St),Cs}.

%%
%% Emit HTML from 8-bit  iso-8859-1
%%
pcdata(Cs, _) ->
    pcdata(Cs).

pcdata(Cs) ->
    pcdata1(Cs, html:chars()).    

pcdata1([C|Cs], TV) when integer(C) ->
    if
	C == $\n -> [C | pcdata1(Cs,TV)];
	C == $\r -> [C | pcdata1(Cs,TV)];
	C == $\t -> [C | pcdata1(Cs,TV)];
	C == $\s -> [C | pcdata1(Cs,TV)];
	C > 255  -> [amp(C) | pcdata1(Cs,TV)];
	C < 0 -> pcdata1(Cs,TV);
	true -> [element(C+1, TV) | pcdata1(Cs,TV)]
    end;
pcdata1([C|Cs], TV) when binary(C) ->
    [ pcdata1(binary_to_list(C),TV) | pcdata1(Cs,TV)];
pcdata1([Cs1|Cs2], TV) when list(Cs1) ->
    [pcdata1(Cs1,TV) | pcdata1(Cs2,TV)];
pcdata1([], _) ->
    [].

amp(N) when N =< 16#ff     ->  "&#"++hex8(N)++";";
amp(N) when N =< 16#ffff   ->  "&#"++hex16(N)++";";
amp(N) when N =< 16#ffffff ->  "&#"++hex24(N)++";";
amp(N) -> "&#"++hex32(N)++";".

hex16(N) ->
    hex8((N bsr 8) band 16#ff) ++ hex8(N band 16#ff).

hex24(N) ->
    hex8((N bsr 16) band 16#ff) ++ 
	hex8((N bsr 8) band 16#ff) ++ 
	hex8(N band 16#ff).

hex32(N) ->
    hex8((N bsr 24) band 16#ff) ++ 
    hex8((N bsr 16) band 16#ff) ++ 
    hex8((N bsr 8) band 16#ff) ++ 
    hex8(N band 16#ff).


hex8(N) ->
    hex4((N bsr 4) band 16#f)++hex4(N band 16#f).

hex4(N) when N < 10 -> [N+$0];
hex4(N) -> [(N-10)+$A].
    


extract_attr(K, Default, As) ->
    extract_attr(K, Default, As, []).

extract_attr(K, Default, [{K,V}|As], Bs) ->
    {{K,to_value(V)}, As++Bs};
extract_attr(K, Default, [KV|As], Bs) ->
    extract_attr(K, Default, As, [KV|Bs]);
extract_attr(K, Default, [], Bs) -> {Default,Bs}.



attrlist(Tag,As) ->
    map(fun ({K,{erl,Exprs}}) ->
		%% FIXME: add (cgi?) environment
		{value,V,Bs} = erl_eval:exprs(Exprs,[]),
		[" ", atom_to_list(K), "=", to_value(V)];
	    ({K,V}) ->
		[" ", atom_to_list(K), "=", to_value(V)];
	   (K) ->
		[" ", atom_to_list(K)]
	end, As).

unquote([$" | Cs]) -> 
     case reverse(Cs) of
       [$" | Cs1] -> reverse(Cs1);
       _ -> Cs
     end;
unquote(Cs) ->
     Cs.

to_value(X) when integer(X) ->
    integer_to_list(X);
to_value(X) when atom(X) ->
    atom_to_list(X);
to_value(X) when list(X) ->
    [$", X, $"];
to_value({var,V}) ->
    "VAR(" ++ atom_to_list(V) ++ ")".


