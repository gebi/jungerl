-ifndef(_GETTEXT_HRL).
-define(_GETTEXT_HRL, true).

-compile({parse_transform,gettext_compile}).

-define(TXT(S), gettext:key2str(S)).

-define(TXT2(S, Lang), gettext:key2str(S, Lang)).

%%%
%%% In case the string is used in a javascript context,
%%% we need to take care of quotes.
%%%
%%% I guess we must assume that quotes and backslashes
%%% always are represented as in good old ASCII...
%%%
%%% NB (11 May 2005): DO NOT USE SINGLE QUOTES IN JAVASCRIPT STRINGS !!!
%%%                   (some browsers doesn't handle escaped single quotes)
%%%
%%%
-define(JTXT(S), gettext:quotes(gettext:key2str(S))).

-define(JTXT2(S, Lang), gettext:quotes(gettext:key2str(S, Lang))).

-endif.
