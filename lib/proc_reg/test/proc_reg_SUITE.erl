%%%-------------------------------------------------------------------
%%% File    : proc_reg_SUITE.erl
%%% Author  : Ulf Wiger <etxuwig@wsb221>
%%% Description : 
%%%
%%% Created :  5 Jul 2004 by Ulf Wiger <etxuwig@wsb221>
%%%-------------------------------------------------------------------
-module(proc_reg_SUITE).
-include("test_server.hrl").

-define(default_timeout, ?t:minutes(1)).
-define(application, proc_reg).


-export([
	 all/1,
	 init_per_testcase/2,
	 fin_per_testcase/2,
	 app_test/1
	]).


init_per_testcase(Case, Config) ->
    ?line code:add_path("/home/etxuwig/work/erlang/lib/proc_reg/ebin"),
    ?line ok = application:load(proc_reg),
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(Case, Config) ->
    ?line Dog = ?config(watchdog, Config),
    ?line test_server:timetrap_cancel(Dog),
    ok.


all(doc) ->
    [];
all(suite) ->
    [app_test].




app_test(suite) ->
    [];
app_test(doc) ->
    ["Application consistency test."];
app_test(Config) when list(Config) ->
   ?line ?t:app_test(proc_reg),
    ok.


