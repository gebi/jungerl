%%% BEGIN ce_smtp.erl %%%
%%%
%%% ce_smtp - RFC 821 SMTP client in Erlang
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

%% @doc Simple SMTP client and server written in Erlang.
%% 
%% <p>The SMTP client is very simple, but is capable of sending e-mail
%% via a standard SMTP server.  If something unexpected happens, the
%% entire e-mail transaction is aborted.</p>
%%
%% <p>The SMTP server is likewise trivial, and does not constitute a
%% full-fledged mail server.  A specified callback function is called
%% for each piece of incoming mail received.</p>
%%
%% <p>This module was previously called <code>smtpe</code>.</p>
%%
%% @end

-module(ce_smtp).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-include_lib("kernel/include/inet.hrl").

-export([send_email/1, client/9]).
-export([server/1, server/4]).

%% @spec send_email([option()]) -> ok | {error, Reason}
%%         option() = {atom(), term()}
%% @doc Sends an e-mail message to an SMTP server. Options include:
%% <ul>
%% <li> <code>{host, string()}</code>
%% <p> the name of the host that the SMTP server is running on.</p></li>
%% <li> <code>{port, string()}</code>
%% <p> the port on the host that is running the SMTP service, if not 25.</p></li>
%% <li> <code>{originator, string()}</code>
%% <p> the name of the host that is originating the transaction (i.e.,
%% this host.)</p></li>
%% <li> <code>{sender, string()}</code>
%% <p> the e-mail address of the message sender.</p></li>
%% <li> <code>{recipients, [string()]}</code>
%% <p> the e-mail addresses of the message recipients.</p></li>
%% <li> <code>{subject, string()}</code>
%% <p> the subject of the message.</p></li>
%% <li> <code>{headers, [string()]}</code>
%% <p> custom e-mail headers in the form <code>"Header: value"</code>,
%% one header per string.</p></li>
%% <li> <code>{message, [string()]}</code>
%% <p> the body of the e-mail message to send, one line per string.</p></li>
%% </ul>

send_email(Options) ->
  (catch application:load(?MODULE)),
  EnvHost = ce_lib:to_string(os:getenv("HOST")),
  EnvUser = ce_lib:to_string(os:getenv("USER")),
  Host = get_opt(Options, host, EnvHost),
  % get host's ip address
  case inet:gethostbyname(Host) of
    {ok, #hostent{ h_addr_list = [HeadIP | Tail] }} ->
      send_email0(HeadIP, Host, Options, EnvHost, EnvUser);
    _ ->
      {error, host_not_found}
  end.
send_email0(HostIP, Host, Options, EnvHost, EnvUser) ->
  ce_log:write("host: ~p ~p", [Host, HostIP]),
  Port = get_opt(Options, port, 25),
  EnvSender = EnvUser ++ "@" ++ EnvHost,
  Originator = get_opt(Options, originator, EnvHost),
  Sender = get_opt(Options, sender, EnvSender),
  Recipients = get_opt(Options, recipients, [EnvSender]),
  Message = get_opt(Options, message, ["line one", "line two"]),
  MessageID = ce_calendar:timestamp() ++ ".ce_smtp@" ++ EnvHost,
  Subject = get_opt(Options, subject, "(No Subject)"),
  Headers = get_opt(Options, headers, []),
  ce_socket:client(?MODULE, client,
    [self(), Originator, Sender, Recipients,
     Message, MessageID, Headers, Subject],
    HostIP, Port,
    [list, {active, false}, {packet, line}]),
  receive
    {?MODULE, client, Result} ->
      Result
  end.

%% @spec client(socket(), pid(), Originator::string(), Sender::string(),
%%         Recipients::[string()], Message::string(), MessageID::string(),
%%         Headers::[string()], Subject::string()) -> exit
%% @doc Called by <code>send_email/1</code>.  Should not be called directly by user code.

client(Socket, Parent, Originator, Sender, Recipients,
 Message, MessageID, Headers, Subject) ->
  Result =
    (catch client0(Socket, Parent, Originator, Sender, Recipients,
      Message, MessageID, Headers, Subject)),
  send_line(Socket, "QUIT"),
  expect(Socket, 221),
  case Result of
    {'EXIT', R} ->
      Parent ! {?MODULE, client, {error, R}};
    _ ->
      Parent ! {?MODULE, client, Result}
  end,
  exit(normal).
client0(Socket, Parent, Originator, Sender, Recipients,
 Message, MessageID, Headers, Subject) ->
  expect(Socket, 220),
  send_line(Socket, "HELO " ++ Originator),
  expect(Socket, 250),
  send_line(Socket, "MAIL FROM:<" ++ Sender ++ ">"),
  expect(Socket, 250),
  lists:foreach(fun(Recipient) ->
    send_line(Socket, "RCPT TO:<" ++ Recipient ++ ">"),
    expect(Socket, 250)
  end, Recipients),
  send_line(Socket, "DATA"),
  expect(Socket, 354),
  send_line(Socket, "From: <" ++ Sender ++ ">"),
  send_line(Socket, "Date: " ++ ce_calendar:rfc_1123_datetime()),
  send_line(Socket, "Message-Id: <" ++ MessageID ++ ">"),
  RecipientList = lists:foldl(fun(Recipient, A) ->
    Recipient0 = "<" ++ Recipient ++ ">",
    case A of
      "" ->
        Recipient0;
      _ ->
        A ++ ", " ++ Recipient0
    end
  end, "", Recipients),
  send_line(Socket, "To: " ++ RecipientList),
  send_line(Socket, "Subject: " ++ Subject),
  lists:foreach(fun(Header) ->
    send_line(Socket, escape(Header))
  end, Headers),
  send_line(Socket, ""),
  lists:foreach(fun(MessageLine) ->
    send_line(Socket, escape(MessageLine))
  end, Message),
  send_line(Socket, "."),
  expect(Socket, 250).

%% @spec server([option()]) -> {ok, Pid} | {error, Reason}
%% @doc Starts a simple SMTP server.  Options include:
%% <ul>
%% <li> <code>{host, string()}</code>
%% <p> the name of the host that the SMTP server shall run on, i.e.
%% the name of this computer.</p></li>
%% <li> <code>{port, string()}</code>
%% <p> the port on the host that shall run the SMTP service, if not 25.</p></li>
%% <li> <code>{handler, {module(), function()}}</code>
%% <p> the handler which is called whenever the server receives a valid
%% incoming mail message.  The specified function (<code>/3</code>)
%% is called with the reverse-path (FROM), a list of recipients (RCPT),
%% and a list of strings (DATA).</p></li>
%% </ul>

server(Options) ->
  application:load(?MODULE),
  EnvHost = ce_lib:to_string(os:getenv("HOST")),
  EnvUser = ce_lib:to_string(os:getenv("USER")),
  Host = get_opt(Options, host, EnvHost),
  ce_log:write("host: ~p", [Host]),
  Port = get_opt(Options, port, 25),
  {Module, Function} = get_opt(Options, handler, no_handler_specified),
  Pid = ce_socket:server(?MODULE, server,  [Host, Module, Function], Port,
    [list, {active, false}, {packet, line}]),
  {ok, Pid}.

%% @spec server(socket(), Host::string(), Module::atom(), Function::atom()) -> never_returns
%% @doc Called by <code>server/1</code>.  Should not be called directly by user code.

server(Socket, Host, Module, Function) ->
  send_line(Socket, "220 " ++ Host ++ " SMTP Service Ready"),
  server_loop(Socket, Host, Module, Function, "", []).

server_loop(Socket, Host, Module, Function, From, Rcpts) ->
  case get_line(Socket) of
    "HELO " ++ Helo ->
      send_line(Socket, "250 " ++ Host ++ " OK"),
      server_loop(Socket, Host, Module, Function, From, Rcpts);
    "QUIT" ++ Quit ->
      send_line(Socket, "221 " ++ Host ++ " SMTP Service Closing");
    "MAIL " ++ NewFrom ->
      send_line(Socket, "250 " ++ Host ++ " OK"),
      server_loop(Socket, Host, Module, Function, NewFrom, Rcpts);
    "RCPT " ++ NewRcpt ->
      send_line(Socket, "250 " ++ Host ++ " OK"),
      server_loop(Socket, Host, Module, Function, From, [NewRcpt | Rcpts]);
    "DATA" ++ DataEOL ->
      send_line(Socket, "354 " ++ Host ++ " Send Your Data"),
      Data = server_data_loop(Socket, []),
      Module:Function(From, lists:reverse(Rcpts), lists:reverse(Data)),
      send_line(Socket, "250 " ++ Host ++ " Mail Sent"),
      server_loop(Socket, Host, Module, Function, "", []);
    _ ->
      send_line(Socket, "502 " ++ Host ++ " Command Not Implemented"),
      server_loop(Socket, Host, Module, Function, From, Rcpts)
  end.

server_data_loop(Socket, Data) ->
  case get_line(Socket) of
    "." ->
      Data;
    Line ->
      server_data_loop(Socket, [Line | Data])
  end.

%%% --------- UTILITIES ----------

%% @spec get_line(socket()) -> string()
%% @doc Gets a line of text from the given socket.
  
get_line(Socket) ->
  {ok, L} = gen_tcp:recv(Socket, 0),
  Line = ce_string:chomp(L),
  ce_log:write("R:~s", [Line]),
  Line.

%% @spec expect(socket(), integer()) -> ok
%% @doc Expects a given return-code from the SMTP server.  If it does not
%% match what is given, an error is thrown.

expect(Socket, Code) ->
  CodeString = ce_lib:to_string(Code),
  Line = get_line(Socket),
  case string:str(Line, CodeString) of
    1 ->
      ok;
    _ ->
      throw({'EXIT', {response, Line}})
  end.

%% @spec send_line(socket(), string()) -> ok
%% @doc Sends a line of text to the given socket.

send_line(Socket, Line) ->
  ce_log:write("S:~s", [Line]),
  gen_tcp:send(Socket, Line ++ eol()),
  ok.

%% @spec escape(string()) -> string()
%% @doc Escapes a line per RFC 821, prepending a period if need be.

escape("." ++ RestOfString) -> ".." ++ RestOfString;
escape(String) -> String.

%% @spec get_opt(Options::[option()], Key::atom(), Default::term()) -> term()
%% @doc Gets an SMTP option.  If the specified option in present
%% in the list, that value is used; if not, the <code>env</code> key of
%% <code>ce_smtp.app</code> is checked, and if no value is found there
%% the given default is used.

get_opt(Options, Key, Default) ->
  case lists:keysearch(Key, 1, Options) of
    {value, {Key, P}} ->
      P;
    _ ->
      case application:get_env(?MODULE, Key) of
        {ok, R} ->
          R;
        _ ->
          Default
      end
  end.

%% @spec eol() -> string()
%% @doc Returns the internet line terminator sequence.

eol() ->
  "\r\n".

%%% END of ce_smtp.erl %%%

