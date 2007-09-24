-module(memcached_tests).
-export([start/0]).

%% You need to make eunit available with something like this in your .erlang file:
%% code:add_pathz("/home/davidw/downloads/eunit/ebin").

-include_lib("eunit/include/eunit.hrl").

start() ->
    test().

get_server_pid() ->
    {ok, Pid} = memcached:start_link('localhost', 11211),
    Pid.

set_test() ->
    memcached:mcset(get_server_pid(), foo, 1).

setstr_test() ->
    memcached:mcset(get_server_pid(), bar, "bar").

setfloat_test() ->
    memcached:mcset(get_server_pid(), pi, 3.14156).

get_test() ->
    {ok, 1} = memcached:mcget(get_server_pid(), foo).

get2_test() ->
    {ok, [1, "bar"]} = memcached:mcget(get_server_pid(), [foo, bar]).

get1str_test() ->
    {ok, [1]} = memcached:mcget(get_server_pid(), "foo").

get2str_test() ->
    {ok, [1, "bar"]} = memcached:mcget(get_server_pid(), ["foo", "bar"]).

getfloat_test() ->
    {ok, 3.14156} = memcached:mcget(get_server_pid(), pi).

delete_test() ->
    Data = "To be, or not to be",
    memcached:mcset(get_server_pid(), todelete, Data),
    {ok, Res1} = memcached:mcget(get_server_pid(), todelete),
    Res2 = memcached:mcdelete(get_server_pid(), todelete),
    {Data, ok} = {Res1, Res2}.

deletefail_test() ->
    not_found = memcached:mcdelete(get_server_pid(), doesntexist).

quit_test() ->
    memcached:mcquit(get_server_pid()).
