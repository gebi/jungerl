%%% BEGIN ce_locker.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2003 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc Generic resource locker design pattern.
%%
%% <p>This is an example of how a resource locker can be coded in Erlang.
%% For a more practical, distributed locker, see the <code>global</code>
%% module in the <code>kernel</code> application of OTP.</p>
%%
%% <p>One locker is created per resource you wish to lock.  These lockers
%% are named and their names are stored in a public ETS table.  Locker names
%% can be arbitary terms.  This allows non-re-entrant functions to be called
%% exclusively with <code>call/3</code>.</p>
%%
%% <p>This is of course voluntary locking (unless you somehow arrange for
%% the locker to be used on otherwise normal calls, e.g. with a parse
%% transform.)</p>
%%
%% @end

-module(ce_locker).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/0, create_locker_table/1]).
-export([new/1, server/0]).
-export([lock/1, lock/2, unlock/1]).
-export([call/3]).
-export([test/0, test2/0]).

%% @spec start() -> pid()
%% @doc Starts the resource locking subsystem.  Creates an ETS table to map
%% resource names to lockers.

start() ->
  Lockers = spawn_link(?MODULE, create_locker_table, [self()]),
  receive
    {Lockers, go_ahead} ->
      Lockers
  end.

%% @spec create_locker_table(Parent::pid()) -> never_returns
%% @doc Called by <code>start/0</code>.  Should not be called by user code.

create_locker_table(Parent) ->
  ets:new(?MODULE, [public, named_table]),
  Parent ! {self(), go_ahead},
  ce_timer:sleep_forever().

%% @spec new(Name::term()) -> pid()
%% @doc Creates a new locker for a named resource.

new(Name) ->
  Locker = spawn_link(?MODULE, server, []),
  ets:insert(?MODULE, {Name, Locker}),
  Locker.

%% @spec server() -> never_returns
%% @doc Called by <code>new/1</code>.  Should not be called by user code.

server() ->
  server([]).

%% @spec server(Queue::[pid()]) -> never_returns
%% @doc Main locker server loop.  <code>Queue</code> is the list of pids that are
%% waiting for this resource; the head of this list is the pid which currently owns
%% the lock.

server(Queue) ->
  % io:fwrite("queue: ~p~n", [Queue]),
  receive
    {Client, lock} ->
      % io:fwrite("server got lock request from ~p~n", [Client]),
      case Queue of
        [] ->                               % not locked, and no one is waiting, so accept
          Client ! {self(), locked},
          server([Client]);
        _ ->                                % a client is waiting for a lock, so
          server(Queue ++ [Client])
      end;
    {Client, unlock} ->
      % io:fwrite("server got unlock request from ~p~n", [Client]),
      Client ! {self(), unlocked},
      Queue0 = Queue -- [Client],
      case Queue0 of
        [] ->                               % no one is waiting, so just continue
          server([]);
        [NewOwner | Rest] ->                % give the next on the list, the lock
          NewOwner ! {self(), locked},
          server(Rest)
      end
  end.

%% @spec lock(locker()) -> {ok, Result} | {error, Reason}
%% @equiv lock(locker(), infinity)

lock(Locker) -> lock(Locker, infinity).

%% @spec lock(locker(), Timeout::integer()) -> {ok, Result} | {error, Reason}
%%         locker() = atom() | pid()
%% @doc Locks a resource, preventing other processes from accessing it.
%% The resource may specified by name, or by locker pid.  If a locker for
%% the named resource does not exist, one is created.

lock(Name, Timeout) when is_atom(Name) ->
  case ets:lookup(?MODULE, Name) of
    [{Name, Locker}] ->
      lock(Locker, Timeout);
    [] ->
      Locker = new(Name),
      lock(Locker, Timeout);
    Else ->
      Else
  end;
lock(Locker, Timeout) ->
  Locker ! {self(), lock},
  receive
    {Locker, Result=locked} ->
      {ok, Result}
    after Timeout ->
      {error, timeout}
  end.

%% @spec unlock(locker()) -> {ok, Result} | {error, Reason}
%% @doc Unlocks a resource, allowing other processes to access it.
%% The resource may specified by name, or by locker pid.

unlock(Name) when is_atom(Name) ->
  case ets:lookup(?MODULE, Name) of
    [{Name, Locker}] ->
      unlock(Locker);
    _ ->
      {error, no_such_locker}
  end;
unlock(Locker) ->
  Locker ! {self(), unlock},
  receive
    {Locker, Result=unlocked} ->
      {ok, Result}
  end.

%% @spec call(Module::atom(), Function::atom(), Args::[term()]) -> term()
%% @doc Calls a non-re-entrant function.  Does this by first locking
%% the function by it's name, then calling the function, then unlocking it.
%% The name of the function in this case is <code>{Module, Function, Arity}</code> where
%% <code>Arity</code> is determined by the length of <code>Args</code>.

call(Module, Function, Args) ->
  Arity = length(Args),
  Name = {Module, Function, Arity},
  lock(Name),
  Result = apply(Module, Function, Args),
  unlock(Name),
  Result.

test() ->
  start(),
  Locker = new(bean),
  spawn_link(?MODULE, test2, []),
  lock(bean),
  io:fwrite("TEST lock~n"),
  timer:sleep(5000),
  io:fwrite("TEST unlock~n"),
  unlock(bean).

test2() ->
  timer:sleep(2000),
  lock(bean),
  io:fwrite("TEST2 lock~n"),
  timer:sleep(5000),
  io:fwrite("TEST2 unlock~n"),
  unlock(bean).

%%% END of ce_locker.erl %%%
