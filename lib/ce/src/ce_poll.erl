%%% BEGIN ce_poll.erl %%%
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

%% @doc Polling design pattern.
%%
%% <p>Although it is preferable to see things happening in Erlang
%% in terms of real-time events or messages, sometimes it is
%% unavoidable to <i>poll</i> (periodically check a resource.)</p>
%%
%% <p>This module provides a compromise by making polling <i>look</i> like
%% plain Erlang messages, even though it is not technically in real-time.</p>
%%
%% @end

-module(ce_poll).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/2, loop/4]).
-export([test/0]).

%% @spec start(Function::fun(), Interval::integer()) -> {ok, pid()} | {error, Reason}
%% @doc Starts a polling service.  Function (a fun/0) will be called
%% immediately, and every Interval milliseconds thereafter.
%% Each time the return value of it has changed from the previous value,
%% a message <code>{ce_poll, pid(), NewValue}</code>
%% will be sent to the process that called <code>start/2</code>.

start(Function, Interval) ->
  Value = Function(),
  Pid = spawn_link(?MODULE, loop, [Function, Interval, self(), Value]),
  {ok, Pid}.

%% @spec loop(Function::fun(), Interval::integer(), Parent::pid(), term()) -> never_returns()
%% @doc Drives the polling.  This function is spawned by <code>start/2</code>
%% and should not be used by external callers.

loop(Function, Interval, Parent, Value) ->
  timer:sleep(Interval),
  case Function() of
    Value ->
      loop(Function, Interval, Parent, Value);
    NewValue ->
      Parent ! {?MODULE, self(), NewValue},
      loop(Function, Interval, Parent, NewValue)
  end.

%% @spec test() -> never_returns()
%% @doc Test function. Monitors the current directory and displays file
%% additions and removals no more than a second later than they happen.

test() ->
  {ok, Dir} = file:get_cwd(),
  {ok, Poller} = start(fun() ->
    {ok, Files} = file:list_dir(Dir),
    Files
  end, 1000),
  test_loop(Poller).

test_loop(Poller) ->
  receive
    {ce_poll, Poller, NewDirListing} ->
      io:fwrite("~p~n", [NewDirListing]),
      test_loop(Poller)
  end.

%%% END of ce_poll.erl %%%
