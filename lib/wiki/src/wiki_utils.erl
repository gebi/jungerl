-module(wiki_utils).

%% Copyright (C) 2000, BLuetail AB
%% File    : wiki_utils.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%% Purpose : Wiki web utilities
%%         : Find zombie pages
%%         : Find all references to a given page

-export([findallrefsto/1, zombies/0]).

-import(lists,  [filter/2, member/2, reverse/1, sort/1, map/2]).
-import(pico_utils, [header/1, h1/1, show/1]).
-import(wiki, [p/1, background/1]).
-import(wiki_templates, [template/4]).

findallrefsto(Page) ->
    All = wiki:ls(),
    Pages = filter(fun(I) ->
			   case wiki:read_page(I) of
			       {ok, Str} ->
				   Links = get_links(Str, []),
				   member(Page, Links);
			       error ->
				   false
			   end
		   end, All),
    Spages = sort(Pages),
    Root = wiki:root(),
    template("References", background("info"), "",
	 ["<p>The following pages contain references to ",
	  wiki_to_html:format_link(Page, Root),".",
	  "<ul>",
	  map(fun(F) -> 
		      [wiki_to_html:format_link(F, Root),"<br>"] end, 
	      Spages),
	  "</ul>"]). 

zombies() -> 
    All = wiki:ls(),
    {Reached, Missing} = gc(["home"], [], []),
    %% Missing = Pages refered to but do not exists at all
    %% This is not an error
    NotReached = sort(All -- Reached),
    Root = wiki:root(),
    template("Zombies", background("info"), "", 
	 [h1("Zombies"),
	  p("These pages have no links to them."),
	  "<ul>",
	  map(fun(F) -> 
		      [wiki_to_html:format_link(F, Root),"<br>"] end, 
	      NotReached),
	  "</ul>"]).

    
gc([H|T], Visited, Missing) ->
    case member(H, Visited) or member(H, Missing) of
	true ->
	    gc(T, Visited, Missing);
	false ->
	    case wiki:read_page(H) of
		{ok, Str} ->
		    Links = get_links(Str, []),
		    gc(Links ++ T, [H|Visited], Missing);
		error ->
		    gc(T, Visited, [H|Missing])
	    end
    end;
gc([], Visited, Missing) ->
    {Visited, Missing}.

get_links([$\\,C|T], L) -> get_links(T, L);
get_links([$~|T], L) ->
    {Link, T1} = wiki_format_txt:collect_wiki_link(T),
    get_links(T1, [Link|L]);
get_links([_|T], L) ->
    get_links(T, L);
get_links([], L) ->
    L.

