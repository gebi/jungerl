%%% BEGIN ce_rec.erl %%%
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

%% @doc Remote Execution Contexts (<code>rec</code>).
%%
%% <p>'Remote Execution Context' is basically a
%% fancy way of saying 'workaround for <code>rpc</code>'.</p>
%%
%% <p>Some operations, such as <code>file:open/2</code>, return an
%% object which is owned by the process that created it.  When the
%% parent process dies, that object is expunged.</p>
%%
%% <p><code>rpc:call/4</code> is not useful for such an operation,
%% because it executes the given function with a short-lived process.</p>
%% 
%% <p>A viable workaround is to instead use <code>ce_rec:start/1</code>
%% to create a long-lived process ('execution context') on a remote node,
%% <code>ce_rec:rpc/2</code> to execute such an operation in that context,
%% and finally <code>ce_rec:stop/1</code> when the objects owned by the
%% remote execution context are no longer needed.</p>
%%
%% @end

-module(ce_rec).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/1, stop/1]).
-export([rpc/2, rpc/3]).

%% @spec start(node()) -> rec()
%%         rec() = pid()
%% @doc Starts an execution context on the given node.

start(Node) ->
  spawn_link(Node, fun() -> execution_context() end).

%% @spec stop(rec()) -> ok | {error, Reason}
%% @doc Shuts down the given execution context.

stop(Pid) ->
  Pid ! {self(), stop},
  receive
    {Pid, stopped} ->
      ok
    after 10000 ->
      {error, timeout}
  end.

%% @spec rpc(pid(), fun()) -> term()
%% @equiv rpc(Pid, Fun, 10000)

rpc(Pid, Fun) ->
  rpc(Pid, Fun, 10000).

%% @spec rpc(pid(), fun(), Timeout::integer()) -> term()
%% @doc Executes a <code>fun/0</code> in a given remote execution context.
%% Returns the result of the fun.  May exit with <code>timeout</code> if
%% the call lasts more than <code>Timeout</code> milliseconds.

rpc(Pid, Fun, Timeout) ->
  Pid ! {self(), do, Fun},
  receive
    {Pid, done, Result} ->
      Result
    after Timeout ->
      exit(timeout)
  end.

%% @spec execution_context() -> never_returns()
%% @doc Driver loop for each remote execution context.

execution_context() ->
  receive
    {Pid, do, Fun} ->
      Pid ! {self(), done, Fun()},
      execution_context();
    {Pid, stop} ->
      Pid ! {self(), stopped},
      ok;
    _ ->
      execution_context()    
  end.

%%% END of ce_rec.erl %%%
