%%%----------------------------------------------------------------------
%%% File    : em_stdlib.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Module for loading the standard library of editor commands.
%%%           This module is loaded/initialised on editor startup.
%%% Created : 29 Apr 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(em_stdlib).
-author('luke@bluetail.com').

-export([mod_init/0]).

mod_init() ->
    lists:foreach(fun(Mod) -> edit_mod:require(Mod) end,
		  mods()).

%% List of modules to be initialised at start-up.
mods() ->
    [em_erlang, em_scheme].

