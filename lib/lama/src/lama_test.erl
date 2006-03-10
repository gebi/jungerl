%%% @private
-module(lama_test).

-export([test/0, test_error1/0, test_error2/0]).
-include("logger.hrl").

test() ->

    ?ALERT  ("Test1: ~p~n", [self()]),
    ?ERROR  ("Test2: ~p~n", [self()]),
    ?WARNING("Test3: ~p~n", [self()]),
    ?NOTICE ("Test4: ~p~n", [self()]),
    ?INFO   ("Test5: ~p~n", [self()]),
    ?DEBUG  ("Test6: ~p~n", [self()]),

    lama:add_alarm_trap(test_alarm1, testAlarm1, []),
    lama:set_alarm({test_alarm1, "My test alarm"}),

    ?DEBUG("Current alarms: ~p~n", [lama:get_alarms()]),

    lama:clear_alarm(test_alarm1).

test_error1() ->
    {ok, _}  = {math:exp(3), "Testing bad match"}.

test_error2() ->
    throw({error, "Testing throw exception"}).
