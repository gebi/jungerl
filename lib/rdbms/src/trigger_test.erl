-module(trigger_test).



%% --------------------------------------------------------
%% test code
%% 1. Create database and start mnesia
%% 2. call tables() to create mnesia tables 
%% 3. run test1(), and then test2() directly after
%% (this can be done by running two shells against the same node.)

-export([tables/0, test1/0, test2/0]).

tables() ->
    [mnesia:create_table(test,[{ram_copies,[node()]}]),
     mnesia:create_table(test2, [{ram_copies,[node()]}])].

test1() ->
    rdbms:activity(
      fun() ->
	      test_enter({test1,1}),
	      test_trigger(commit, test1),
	      test_trigger(abort, test1),
	      mnesia:write_lock_table(test),
	      timer:sleep(1000),
	      mnesia:write_lock_table(test2),
	      timer:sleep(1000),
	      mnesia:abort(test)
      end).

test2() ->
    rdbms:activity(
      fun() ->
	      test_enter({test2,1}),
	      test_trigger(commit, {test2,1}),
	      test_trigger(abort, {test2,1}),
	      mnesia:write_lock_table(test),
	      rdbms:activity(fun() ->
				     test_enter({test2,2}),
				     test_trigger(commit, {test2,2}),
				     test_trigger(abort, {test2,2})
			  end),
	      catch rdbms:activity(fun() ->
					   test_enter({test2,3}),
					   mnesia:write_lock_table(test2),
					   test_trigger(commit, {test2,3}),
					   test_trigger(abort, {test2,3}),
					   mnesia:abort({test2,3})
				   end)
      end).
	
test_enter(Tag) ->
    ok = io:format("enter test (~p)~n", [Tag]).

test_trigger(Type, Tag) ->
    F = fun() ->
		io:format("**TRIG** (~p) ~p trigger.~n", 
			  [Tag, Type])
	end,
    case Type of
	commit ->
	    rdbms:register_commit_action(F);
	abort ->
	    rdbms:register_rollback_action(F)
    end.

%% end test code
%% --------------------------------------------------------



