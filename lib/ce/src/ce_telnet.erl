%%% BEGIN ce_telnet.erl %%%
%%%
%%% ce_telnet - rudimentary TELNET server in Erlang
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

%% @doc Rudimentary TELNET server in Erlang.
%% 
%% @end

-module(ce_telnet).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/4, server/3]).
-export([send/2, send_file/2]).
-export([get_line/1, get_char/1, get_char/2]).

-define(WILL, 251).
-define(WONT, 252).
-define(DO,   253).
-define(DONT, 254).
-define(IAC,  255).

-define(TRANSMIT_BINARY, 0).
-define(ECHO, 1).
-define(SUPPRESS_GO_AHEAD, 3).

%% @spec start(Module::atom(), Function::atom(), Port::integer(), MaxCon::integer()) -> pid()
%% @doc Starts a telnet server on the given port.  When a connection is made,
%% <code>Module:Function/1</code> is called and passed the pid of the
%% telnet server.  This function can then use functions like
%% <code>send</code> and <code>get_line</code>, and/or it can directly
%% receive the following messages:
%% <code>{Telnet::pid(), close}</code>, which means the socket was closed
%% by the client; and
%% <code>{Telnet::pid(), {byte, integer()}}</code>, which means a byte
%% was successfully received.

start(Module, Function, Port, MaxCon) ->
  Telnet = ce_socket:server(?MODULE, server, [Module, Function], Port,
    [list, {active, true}, {nodelay, true}, {reuseaddr, true}, {packet, raw}],
    MaxCon).

%% @spec server(Socket::socket(), Module::atom(), Function::atom()) -> never_returns
%% @doc Spawned by <code>start/4</code> to handle the telnet server.

server(Socket, Module, Function) ->
  % io:fwrite("connect ~p~n", [Socket]),
  gen_tcp:send(Socket, <<?IAC, ?DONT, ?ECHO>>),
  gen_tcp:send(Socket, <<?IAC, ?DO, ?TRANSMIT_BINARY>>),  
  Pid = spawn_link(Module, Function, [self()]),
  loop(Socket, Pid, text, []).

loop(Socket, Pid, State, Buffer) ->
  receive
    {tcp, Socket, Bytes} ->
      {NewState, NewBuffer} = reduce(Pid, State, Bytes ++ Buffer),
      loop(Socket, Pid, NewState, NewBuffer);
    {tcp_closed, Socket} ->
      % io:fwrite("disconnect ~p~n", [Socket]),
      Pid ! closed,
      {error, closed};
    {tcp_error, Socket, Reason} ->
      io:fwrite("error on ~p: ~p~n", [Socket, Reason]),
      Pid ! {self(), closed},
      gen_tcp:close(Socket),
      {error, {telnet, Reason}};
    {output, List} when is_list(List) ->
      gen_tcp:send(Socket, List),
      loop(Socket, Pid, State, Buffer);
    Else ->
      io:fwrite("Huh? ~p", [Else]),
      loop(Socket, Pid, State, Buffer)
  end.

reduce(Pid, State, []) ->
  {State, []};
reduce(Pid, text, [?IAC | Tail]) ->
  reduce(Pid, command, Tail);
reduce(Pid, text, [Head | Tail]) ->
  Pid ! {self(), {byte, Head}}, reduce(Pid, text, Tail);
reduce(Pid, command, [?WILL, X | Tail]) ->
  % io:fwrite("WILL ~p~n", [option(X)]),
  reduce(Pid, text, Tail);
reduce(Pid, command, [?WONT, X | Tail]) ->
  % io:fwrite("WONT ~p~n", [option(X)]),
  reduce(Pid, text, Tail);
reduce(Pid, command, [?DO, X | Tail]) ->
  % io:fwrite("DO ~p~n", [option(X)]),
  reduce(Pid, text, Tail);
reduce(Pid, command, [?DONT, X | Tail]) ->
  % io:fwrite("DONT ~p~n", [option(X)]),
  reduce(Pid, text, Tail);
reduce(Pid, command, [X | Tail]) ->
  % io:fwrite("?? ~p~n", [X]),
  reduce(Pid, text, Tail).

option(?TRANSMIT_BINARY) -> 'TRANSMIT-BINARY';
option(?ECHO) -> 'ECHO';
option(?SUPPRESS_GO_AHEAD) -> 'SUPPRESS-GO-AHEAD';
option(N) -> N.

%% interface %%

%% @spec send(Telnet::pid(), Text::string()) -> ok | {error, Reason}
%% @doc Sends some text to a telnet session.

send(Telnet, Text) ->
  Telnet ! {output, Text},
  ok.

%% @spec send_file(Telnet::pid(), filename()) -> ok | {error, Reason}
%% @doc Sends a text file to a telnet session.

send_file(Telnet, FileName) ->
  ce_file:each_line(fun(Line, Acc) ->
      Line0 = ce_string:chomp(Line),
      Telnet ! {output, [Line0, "\n\r"]},
      ok
    end, ok, filename:join([code:priv_dir(ebbs), FileName])).

%% @spec get_line(Telnet::pid()) -> {ok, string()} | {error, Reason}
%% @doc Gets a line of text from a telnet session.

get_line(Telnet) ->
  get_line(Telnet, "").
get_line(Telnet, Acc) ->
  receive
    {Telnet, closed} ->
      {error, closed};
    {Telnet, {byte, NL}} when NL == 10; NL == 13 ->
      Telnet ! {output, "\r\n"},
      {ok, lists:reverse(Acc)};
    {Telnet, {byte, 8}} when Acc == [] ->
      Telnet ! {output, [7]},
      get_line(Telnet, Acc);
    {Telnet, {byte, 8}} ->
      Telnet ! {output, "\b \b"},
      get_line(Telnet, tl(Acc));
    {Telnet, {byte, Char}} ->
      Telnet ! {output, [Char]},
      get_line(Telnet, [Char | Acc])
  end.

%% @spec get_char(Telnet::pid()) -> {ok, char()} | {error, Reason}
%%         char() = integer()
%% @doc Gets a single character from a telnet session.

get_char(Telnet) ->
  get_char(Telnet, fun(X) -> true end).
get_char(Telnet, Fun) ->
  receive
    {Telnet, closed} ->
      {error, closed};
    {Telnet, {byte, Char}} ->
      case Fun(Char) of
        true ->
          {ok, Char};
        _ ->
          get_char(Telnet, Fun)
      end
  end.

%%% END of ce_telnet.erl %%%
