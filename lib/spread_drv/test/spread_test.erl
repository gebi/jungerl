
%%%
%%% To verify that something is happening when this test is run, run
%%% the "spuser" program on localhost or another spread-enabled machine,
%%% then use the command "j foo" to join the "foo" group, then run
%%% this test.
%%%

-module(spread_test).
-define(DRV, spread_drv).

-include("spread_drv.hrl").

-export([regression/0]).
-export([tiny_active_test/0]).

regression() ->
    {ok, Port1} = ?DRV:start(),
    pathetic_smoke_coverage(Port1),
    ?DRV:shutdown(Port1),

    %{ok, Port2} = ?DRV:start_pipe(),
    %pathetic_smoke_coverage(Port2),
    %?DRV:shutdown(Port2),

    ok = tiny_active_test(),

    io:format("All regression tests PASSED.\n"),
    ok.

%%% This little test will generate all of the messages that it
%%% tries to receive.  No external test setup is required.
%%% However, it won't work if the Spread daemon isn't running
%%% on the local machine ... or if someone else is using Spread
%%% to send data to the "foo" group.
pathetic_smoke_coverage(P) ->
    {ok, PrivName} = ?DRV:sp_connect(P, "", "erl1", 1, 1),
    {ok, 0} = ?DRV:sp_join(P, "foo"),
    {ok, 0} = ?DRV:sp_leave(P, "barbar"),
    {error, -14} = ?DRV:sp_leave(P, "foo#bad"),
    Msg1 = <<"Hello, world!">>,
    Msg1Len = size(Msg1),
    Msg2 = <<"Hello, again, world!">>,
    Msg2Len = size(Msg2),
    {ok, Msg1Len} = ?DRV:sp_multicast(P, ?UNRELIABLE_MESS, "foo",
				      42, Msg1),
    {ok, Msg2Len} = ?DRV:sp_multigroup_multicast(P, ?UNRELIABLE_MESS,
						 ["foo"], 42, Msg2),

    %% Avoid race conditions by leaving plenty of time for comm to catch up
    timer:sleep(1000),

    case ?DRV:sp_poll(P) of
	{ok, N} when N > 0 -> ok
    end,
		     
    {ok, _} = ?DRV:sp_receive(P, 4, 400),
    {ok, _} = ?DRV:sp_receive(P, 4, 400),
    {ok, _} = ?DRV:sp_receive(P, 4, 400),
    %% There should be nothing to receive now
    {ok, 0} = ?DRV:sp_poll(P),

    {ok, 0} = ?DRV:sp_leave(P, "foo"),
    {ok, 0} = ?DRV:sp_disconnect(P),

    ok.

%%%
%%% There is no pipe-driver version of this test, because the spread.erl
%%% module is currently hardcoded to only use a linked-in driver.
%%%

tiny_active_test() ->
    {ok, Pid} = spread:start_link("4803@localhost", "tiny_test", 0, 1),
    ok = spread:subscribe(Pid, "foo", true),
    ok = spread:subscribe(Pid, "foofoo"),	% No group membership notices
    Msg1 = "Hi there!",
    Msg1B = list_to_binary(Msg1),
    Msg1Len = size(Msg1B),
    Msg2 = "Hi again!  (twice!)",
    Msg2B = list_to_binary(Msg2),
    Msg2Len = size(Msg2B),
    {ok, _} = spread:multicast(Pid, causal_mess, "foo", 42, Msg1),
    {ok, _} = spread:multicast(Pid, safe_mess,
			       spread_drv:make_grouplist(["foo", "foofoo"]),
			       4242, Msg2),

    %% It's very important here to go to sleep for a little bit.  Otherwise,
    %% we can unsubscribe from the groups before the Spread gen_server can
    %% process the joins and the multicast messages: if you unsubscribe
    %% before you hear the multicast event come from the Spread daemon,
    %% the Spread gen_server won't have the subscriber state to know to
    %% forward those messages to you!
    %%
    %% This pause isn't necessary for typical applications: you don't
    %% usually join a group and then leave it a few milliseconds later.
    timer:sleep(1000),

    ok = spread:unsubscribe(Pid, "foo"),
    ok = spread:unsubscribe(Pid, "foofoo"),

    %% Alright, now check to make certain that our mailbox has all of the
    %% messages that we're expecting it to have.

    %% Message 1: membership to "foo"
    ok = receive
	     {spread, Pid, membership, <<"foo">>, _} ->
		 ok
	 after 5000 ->
		 io:format("ERROR: timeout, my mailbox = ~w\n", [erlang:process_info(self(), messages)]),
		 timeout_msg1
	 end,

    %% Message 2: "foofoo" doesn't get membership notices

    %% Message 2: multicast message to "foo".
    ok = receive
	     {spread, Pid, msg, <<"foo">>, causal_mess, _, 42, Msg1B} ->
		 ok
	 after 5000 ->
		 io:format("ERROR: timeout, my mailbox = ~w\n", [erlang:process_info(self(), messages)]),
		 timeout_msg2
	 end,
    
    %% Message 3: multicast message to "foo" and "foofoo" => "foo"
    ok = receive
	     {spread, Pid, msg, <<"foo">>, safe_mess, _, 4242, Msg2B} ->
		 ok
	 after 5000 ->
		 io:format("ERROR: timeout, my mailbox = ~w\n", [erlang:process_info(self(), messages)]),
		 timeout_msg3
	 end,

    %% Message 4: multicast message to "foo" and "foofoo" => "foofoo"
    ok = receive
	     {spread, Pid, msg, <<"foofoo">>, safe_mess, _, 4242, Msg2B} ->
		 ok
	 after 5000 ->
		 io:format("ERROR: timeout, my mailbox = ~w\n", [erlang:process_info(self(), messages)]),
		 timeout_msg4
	 end,


    %% Message 5: there is no message 5.  We do not get a subscription 
    %% message when we unsubscribe from "foo".  We are done!
    
    ok = spread:stop(Pid),
    ok.
