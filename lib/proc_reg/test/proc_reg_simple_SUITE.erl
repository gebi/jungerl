%%%-------------------------------------------------------------------
%%% File    : proc_reg_simple_SUITE.erl
%%% Author  : Ulf Wiger <etxuwig@wsb221>
%%% Description : 
%%%
%%% Created :  5 Jul 2004 by Ulf Wiger <etxuwig@wsb221>
%%%-------------------------------------------------------------------
-module(proc_reg_simple_SUITE).

-include("test_server.hrl").

-define(default_timeout, ?t:minutes(1)).
-define(application, proc_reg).


-export([
	 all/1,
	 init_per_testcase/2,
	 fin_per_testcase/2,
	 simple_reg/1,
	 reg_term/1,
	 collision/1,
	 reg_send/1,
	 proc_dies/1,
	 proc_dies_multiple_regs/1,
	 proc_dies_race/1,
	 reg_kill_reg/1,
	 proc_reg_dies/1,
	 client_dies_while_proc_reg_dead/1
	]).

all(doc) ->
    [];
all(suite) ->
    [simple_reg, reg_term, collision, reg_send,
     proc_dies, proc_dies_multiple_regs, proc_dies_race, reg_kill_reg,
     proc_reg_dies, client_dies_while_proc_reg_dead].


init_per_testcase(Case, Config) ->
    case code:is_loaded(proc_reg) of
	{file,_} ->
	    ok;
	false ->
	    code:add_path("/home/etxuwig/work/erlang/lib/proc_reg/ebin")
    end,
    case application:start(proc_reg) of
	{error, {already_started,_}} ->
	    ok;
	ok ->
	    ok
    end,
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.


simple_reg(doc) ->
    [];
simple_reg(suite) ->
    [];
simple_reg(Config) when list(Config) ->
    ?line Self = self(),
    ?line true = proc_reg:reg(foo, Self),
    ?line Self = proc_reg:where(foo),
    ?line true = proc_reg:reg(bar, Self),
    ?line Self = proc_reg:where(bar),
    ?line Self = proc_reg:where(foo),
    ?line true = proc_reg:unreg(foo),
    ?line undefined = proc_reg:where(foo),
    ?line Self = proc_reg:where(bar),
    ?line true = proc_reg:unreg(bar),
    ?line undefined = proc_reg:where(bar).

reg_term(doc) ->
    [];
reg_term(Config) when list(Config) ->
    ?line Self = self(),
    ?line Name = {a,b,c,d,[foo]},
    ?line true = proc_reg:reg(Name, Self),
    ?line Self = proc_reg:where(Name),
    ?line true = proc_reg:unreg(Name),
    ?line undefined = proc_reg:where(Name),
    ok.


collision(doc) ->
    [];
collision(Config) when list(Config) ->
    ?line F = fun() ->
		      timer:sleep(infinity)
	      end,
    ?line Pid1 = spawn_link(F),
    ?line Pid2 = spawn_link(F),
    ?line true = proc_reg:reg(foo, Pid1),
    {'EXIT', badarg} = (catch proc_reg:reg(foo, Pid2)),
    ok.

reg_send(doc) ->
    [];
reg_send(Config) when list(Config) ->
    ?line Parent = self(),
    ?line Pid1 = spawn_link(fun() ->
				    receive
					{Parent, ping} ->
					    Parent ! {self(), pong}
				    end
			    end),
    ?line proc_reg:reg(pid1, Pid1),
    ?line proc_reg:send(pid1, {self(), ping}),
    ?line receive
	      {Pid1, pong} ->
		  ok
	  end.

			
proc_dies(doc) ->
    [];
proc_dies(Config) when list(Config) ->
    ?line Pid = spawn(fun() ->
			      timer:sleep(infinity)
		      end),
    MRef = erlang:monitor(process, Pid),
    ?line true = proc_reg:reg(foo, Pid),
    ?line exit(Pid, kill),
    ?line receive
	      {'DOWN',MRef,_,_,_} ->
		  ?line undefined = proc_reg:where(foo),
		  ok
	  end.

proc_dies_multiple_regs(doc) ->
    [];
proc_dies_multiple_regs(Config) when list(Config) ->
    ?line Pid = spawn(fun() ->
			      timer:sleep(infinity)
		      end),
    ?line MRef = erlang:monitor(process, Pid),
    ?line Names = [{name, N} || N <- lists:seq(1,50)],
    ?line lists:foreach(fun(Name) ->
				proc_reg:reg(Name, Pid)
			end, Names),
    ?line exit(Pid, kill), 
    ?line receive
	      {'DOWN', MRef, _, _, _} ->
		  ?line [] = [W || W <- [proc_reg:where(N) || N <- Names],
				   W =/= undefined]
	  end,
    ok.



proc_dies_race(doc) ->
    [];
proc_dies_race(Config) when list(Config) ->
    ?line Pid = spawn(fun() ->
			      timer:sleep(infinity)
		      end),
    MRef = erlang:monitor(process, Pid),
    ?line true = proc_reg:reg(foo, Pid),
    ?line exit(Pid, kill),
    ?line IsAlive = erlang:is_process_alive(Pid),
    ?line {'EXIT',badarg} = (catch proc_reg:send(foo, hello)),
    ?line receive
	      {'DOWN',MRef,_,_,_} ->
		  ok
	  end.

reg_kill_reg(doc) ->
    [];
reg_kill_reg(Config) when list(Config) ->
    ?line F = fun() ->
		      timer:sleep(infinity)
	      end,
    ?line Pid1 = spawn(F),
    ?line MRef = erlang:monitor(process, Pid1),
    ?line Pid2 = spawn_link(F),
    ?line true = proc_reg:reg(foo, Pid1),
    ?line exit(Pid1, kill),
    ?line true = proc_reg:reg(foo, Pid2),
    ?line receive
	      {'DOWN', MRef, _, _, _} ->
		  ok
	  end.



proc_reg_dies(doc) ->
    [];
proc_reg_dies(Config) when list(Config) ->
    ?line Self = self(),
    ?line Name = {a,b,c,d,[foo]},
    ?line true = proc_reg:reg(Name, Self),
    ?line MRef = erlang:monitor(process, proc_reg),
    ?line receive
	      {'DOWN', MRef, _, _, _} ->
		  ?line exit(proc_reg_dead_prematurely)
	  after 1000 ->
		  ok
	  end,
    ?line exit(whereis(proc_reg), deliberate),
    ?line receive
	      {'DOWN', MRef, _, _, deliberate} ->
		  receive after 1000 ->
				  ok
			  end,
		  ?line Self = proc_reg:where(Name)
	  after 3000 ->
		  ?line exit(proc_reg_did_not_die)
	  end,
    ok.

client_dies_while_proc_reg_dead(doc) ->
    [];
client_dies_while_proc_reg_dead(Config) when list(Config) ->
    ?line Pid = spawn(fun() ->
			      timer:sleep(infinity)
		      end),
    ?line Name = {a,b,c,d,[foo]},
    ?line true = proc_reg:reg(Name, Pid),
    ?line MRef = erlang:monitor(process, Pid),
    ?line ok = supervisor:terminate_child(proc_reg_reg_sup, proc_reg),
    ?line Pid = proc_reg:where(Name),
    ?line exit(Pid, deliberate),
    ?line receive
	      {'DOWN', MRef, _, _, deliberate} ->
		  receive after 1000 ->
				  ok
			  end,
		  ?line {ok, _} = supervisor:restart_child(proc_reg_reg_sup, proc_reg),
		  ?line undefined = proc_reg:where(Name)
	  end,
    ok.

