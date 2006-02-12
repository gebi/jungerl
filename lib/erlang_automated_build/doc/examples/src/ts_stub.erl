%%%-----------------------------------------------------------------------------
%%% File        : ts_stub.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : This is a stub to enable the test server to be called from
%%%               the command line with the full cover/batch stuff going
%%%
%%% Created     :  9 Jan 2006 by gordon <gordonguthrie@backawinner.gg>
%%%-----------------------------------------------------------------------------
-module(ts_stub).

-export([run/1]).

run(Spec) ->
    %%io:fwrite("In ts_stub~n", []),
    ts:run(Spec, [debug, cover, batch]).
