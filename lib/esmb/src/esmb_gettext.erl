%%%-------------------------------------------------------------------
%%% Created : 12 Jul 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% File    : esmb_gettext.erl
%%% Desc.   : An example on how to use the jungerl-gettext app.
%%%-------------------------------------------------------------------
-module(esmb_gettext).
-export([start/0]).

-include("../../gettext/include/gettext.hrl").


start() -> 
    ?TXT("Hello World").



