-module(epmd_test).
-compile(export_all).

test() ->
    (catch apply(epmd_svc, epmd_prog_1, [foo, 5, []])).
