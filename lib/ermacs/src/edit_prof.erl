-module(edit_prof).
-compile(export_all).

start(Filename) ->
    Edit = spawn_link(fun() -> edit:start(Filename) end),
    timer:start_link(),
    eprof:start(),
    profiling = eprof:profile([Edit, scratch]),
    ok = eprof:analyse(),
    analyse_loop().

analyse_loop() ->
    receive after 5000 ->
		    eprof:total_analyse()
	    end,
    analyse_loop().

leader() ->
    {ok, ProfLog} = file:open("prof.log", [write]),
    spawn_link(?MODULE, leader_proc, [ProfLog]).

leader_proc(Fd) ->
    receive
	{io_request, From, ReplyAs, {put_chars, C}} ->
	    file:write(Fd, C),
	    From ! {io_reply, ReplyAs, ok};
	{io_request, From, ReplyAs, {put_chars, M, F, A}} ->
	    file:write(Fd, apply(M, F, A)),
	    From ! {io_reply, ReplyAs, ok};
	{io_request, From, ReplyAs, {get_until, _, _, _}} ->
	    From ! {io_reply, ReplyAs, eof};
	X ->
	    file:write(Fd, io_lib:format("Unexpected: ~p~n", [X]))
    end,
    leader_proc(Fd).

