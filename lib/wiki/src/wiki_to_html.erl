-module(wiki_to_html).

%% Copyright (C) 2000, Bluetail AB
%% File    : wiki_to_html.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%% Purpose : Convert wiki page tree to HTML

-export([format_wiki/3, format_link/2]).

-import(lists, [member/2, map/2]).

-import(pico_utils, [show/1]).

format_wiki(Page, Wik, Root) ->
    LinkFun = fun(I) -> format_link(I, Page, Root) end,
    pp(Wik, LinkFun).

format_link(Page, Root) ->
    format_link({wikiLink, Page}, [], Root).

format_link({wikiLink, Page}, _, Root) ->
    FullName = Root ++ "/" ++ Page ++ ".wob",
    %% io:format("Testing:~p~n",[FullName]),
    case is_file(FullName) of
	true ->
	    ["<a href=\"/wiki/showPage?node=",Page,"\">",Page,"</a> "];
	false ->
	    [" ",Page,"<a href=\"/wiki/createNewPage?node=",Page,"\">???</a>"]
    end;
format_link({editTag, Tag}, Page, Root) ->
    ["<a href=\"/wiki/editTag?node=",Page,"&tag=",i2s(Tag),"\">",
     "<img border=0 src='/wiki/image/edit.gif'></a> "].

i2s(X) ->
    integer_to_list(X).

pp({wik,L}, F) ->
    map(fun(I) -> pp(I, F) end, L);
pp({txt,_,Str}, F) ->
    wiki_format_txt:format(Str, F);
pp({open,Tag,Str}, F) -> 
    open("#CCFFCC",Tag,F,pp({txt,9999,Str}, F));
pp({write_append,Tag,Str}, F) -> 
    open("#99FFFF",Tag,F,pp({txt,8888,Str}, F));
pp(Other, F) ->
    show({cannot,format,Other}).

open(Color, Tag, F, Stuff) ->
    ["\n<table width=\"90%\" cellpadding=20>\n<tr><td bgcolor=\"",
     Color, "\">\n", Stuff,
     "<p>",F({editTag,Tag}),"</td></tr></table><p>\n"];
open(Color, Args, F, Stuff) ->
    show({bad_open_tag,Args}).

is_file(File) ->
    case file:file_info(File) of
        {ok, _} ->
            true;
        _ ->
            false
    end.












