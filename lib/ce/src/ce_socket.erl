%%% BEGIN ce_socket.erl %%%
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
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF AsDVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc Socket library.
%%
%% <p>This library contains functions which create TCP/IP socket
%% servers and clients.</p>
%%
%% @end

-module(ce_socket).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([server/5, server/6, client/6, client/4]).
-export([server_setup/7, asynch_accept/2]).
-export([couple/1, couple/2, coupler/2]).

%% @spec server(Mod::atom(), Func::atom(), Args::[term()],
%%         Port::integer(), Opts::[option()]) -> pid() | {error, reason()}
%% @equiv server(Mod, Func, Args, Port, Opts, 1000000000)

server(Mod, Func, Args, Port, Opts) ->
  server(Mod, Func, Args, Port, Opts, 1000000000).

%% @spec server(Mod::atom(), Func::atom(), Args::[term()],
%%         Port::integer(), Opts::[option()],
%%         MaxCon::integer()) -> pid() | {error, reason()}
%% @doc Implements a generic TCP/IP socket server.
%% Opens a socket with the given options on the given
%% port and spawns <code>Module:Function(Socket, Args...)</code> to handle
%% each incoming connection, up to the specified maximum number of connections.

server(Mod, Func, Args, Port, Opts, MaxCon) ->
  inet_db:start(),
  ce_timer:sleep_until_registered(inet_db),
  case gen_tcp:listen(Port, Opts) of
    {ok, LSock} ->
      spawn_link(?MODULE, server_setup,
        [Mod, Func, Args, LSock, Port, Opts, MaxCon]);
    Other ->
      Other
  end.

%% @spec server_setup(Mod::atom(), Func::atom(), Args::[term()],
%%         LSock::listen_socket(), Port::integer(), Opts::[option()],
%%         MaxCon::integer()) -> never_returns
%% @doc Used by <code>server/6</code> to handle the listen socket.
%% This function should not be called directly by users of this module.

server_setup(Mod, Func, Args, LSock, Port, Opts, MaxCon=0) ->
  % When the maximum number of connections has been reached, wait
  % until a socket connection dies before starting any more accepts.
  receive
    {'EXIT', Pid, Reason} ->
      server_setup(Mod, Func, Args, LSock, Port, Opts, MaxCon + 1)
  end;
server_setup(Mod, Func, Args, LSock, Port, Opts, MaxCon) ->
  % By setting the process flag trap_exit to true, we will recieve
  % {'EXIT',Pid,Reason} messages for any linked processes that die
  process_flag(trap_exit, true),
  % Therefore, this call is not a spawn_link because we only want to
  % get exit messages when the user socket handlers die
  Accepter = spawn(?MODULE, asynch_accept, [self(), LSock]),
  server_loop(Mod, Func, Args, LSock, Port, Opts, MaxCon, Accepter).

%% @spec server_loop(Mod::atom(), Func::atom(), Args::[term()],
%%         LSock::listen_socket(), Port::integer(), Opts::[option()],
%%         MaxCon::integer(), Accepter::pid()) -> never_returns
%% @doc Called by <code>server_setup/7</code> to handle the listen socket.
%% This function waits around for something to happen:
%% either for a message to arrive from the previous <code>asynch_accept/2</code>
%% (meaning there is now one more connection), or for the exit signal of an
%% existing socket process (meaning there is now one less connection.)  In the
%% former case, a new asynchronous <code>accept</code> call is set up by
%% looping back to <code>server_setup/7</code>.

server_loop(Mod, Func, Args, LSock, Port, Opts, MaxCon, Accepter) ->
  receive
    {Accepter, {ok, Socket}} ->
      % io:fwrite("accepter got socket: ~p~n", [Socket]),
      Pid = spawn_link(Mod, Func, [Socket] ++ Args),
      gen_tcp:controlling_process(Socket, Pid),
      server_setup(Mod, Func, Args, LSock, Port, Opts, MaxCon - 1);
    {Accepter, {error, closed}} ->
      server_setup(Mod, Func, Args, LSock, Port, Opts, MaxCon);
    {Accepter, {error, Reason}} ->
      % io:fwrite("accepter error: ~p~n", [Reason]),
      server_setup(Mod, Func, Args, LSock, Port, Opts, MaxCon);
    {'EXIT', Pid, Reason} ->
      server_loop(Mod, Func, Args, LSock, Port, Opts, MaxCon + 1, Accepter)
  end.

%% @spec asynch_accept(Parent::pid(), LSock::listen_socket()) -> never_returns
%% @doc Used by <code>server_loop/8</code> to handle the listen socket.
%% This function waits for a connection using <code>gen_tcp:accept/1</code>
%% and notifies the parent process which spawned it when a connection happens,
%% allowing the parent to process other messages in the meantime.
%% This function should not be called directly by users of this module.

asynch_accept(Parent, LSock) ->
  Result = gen_tcp:accept(LSock),
  case Result of
    {ok, Socket} ->
      gen_tcp:controlling_process(Socket, Parent);
    _ ->
      ok
  end,
  Parent ! {self(), Result}.

%% @spec client(module(), function(), args(), address(), port(), options()) -> pid() | {error, Reason}
%% @doc Implements a generic socket client.
%% Connects to a socket with the given options on the given
%% port and spawns <code>Module:Function(Socket, Args...)</code> to handle
%% the connection.

client(Module, Function, Args, Address, Port, Options) ->
  inet_db:start(),
  ce_timer:sleep_until_registered(inet_db),
  case gen_tcp:connect(Address, Port, Options) of
    {ok, Socket} ->
      Pid = spawn_link(Module, Function, [Socket] ++ Args),
      gen_tcp:controlling_process(Socket, Pid),
      Pid;
    Other ->
      Other
  end.

%% @spec client(address(), port(), options(), Interval::interval()) -> {ok, socket()} | {error, Reason}
%% @doc Waits for a server to become available, connects to it, and
%% returns a socket.  This function tries to contact the server repeatedly,
%% every <code>Interval</code> milliseconds.

client(Host, Port, Options, Interval) ->
  case gen_tcp:connect(Host, Port, Options) of
    {ok, Socket} ->
      {ok, Socket};
    _ ->
      timer:sleep(Interval),
      client(Host, Port, Options, Interval)
  end.

%% @spec couple(socket()) -> pid()
%% @equiv couple(socket(), self())

couple(Socket) -> couple(Socket, self()).

%% @spec couple(socket(), pid()) -> pid()
%% @doc Couples a socket to a process.  Messages can be sent to the
%% process and will be picked up on the other end of the socket (which is also
%% presumably coupled similarly.)  Likewise, messages sent from the other
%% end of the connection will be delivered to the owner.
%% In essence this allows two Erlang sessions to communicate with each other
%% in the same manner as communicating with any other process (that is,
%% by sending Erlang messages,) without the nodes necessarily even being in
%% the same distribution, or indeed any distribution at all.
%% The socket should be opened in binary mode.

couple(Socket, Pid) ->
  inet:setopts(Socket, [{packet, 4}, {active, true}]),
  Coupler = spawn(?MODULE, coupler, [Socket, Pid]),
  gen_tcp:controlling_process(Socket, Coupler),
  Coupler.

%% @spec coupler(socket(), pid()) -> ok
%% @doc Used by couple/2 to achieve socket/process coupling.
%% Translates tcp messages to Erlang terms and vice versa.
%% This function should not be called directly by users of this module.

coupler(Socket, Owner) ->
  receive
    {tcp, Socket, Data} ->
      Owner ! binary_to_term(Data),
      coupler(Socket, Owner);
    {tcp_closed, Socket} ->
      ok;
    {tcp_error, Socket, Reason} ->
      ok;
    Msg ->
      gen_tcp:send(Socket, term_to_binary(Msg)),
      coupler(Socket, Owner)
  end.

%%% END of ce_socket.erl %%%
