%% Copyright (C) 2003 Joakim Grebenö <jocke@gleipnir.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(xmlrpc).
-author('jocke@gleipnir.com').
-export([call/3, call/4, call/5, call/6]).
-export([start_link/1, start_link/5, start_link/6, stop/1]).

-export([ssl_call/3, ssl_call/4, ssl_call/5, ssl_call/6]).

-export([cbs_new/0, cbs_record/1,
	 cbs_ip/1, cbs_ip/2, 
	 cbs_port/1, cbs_port/2, 
	 cbs_opaque/1, cbs_opaque/2
	]).


-include("log.hrl").

-record(header, {
	  %% int()
	  content_length,
	  %% close | undefined
	  connection
	 }).

%%% This record can be used by the client to be able to
%%% retrieve internal info (such as Ip,Port) and at the
%%% same time preserve the clients own state (opaque).
%%% NB: No include file is provided, use the access functions!
-record(cback_state, {
	  ip,
	  port,
	  opaque
	 }).

cbs_record(C) when record(C, cback_state) -> true;
cbs_record(_)                             -> false.

cbs_new() -> 
    #cback_state{}.

cbs_ip(C) when record(C, cback_state) -> 
    C#cback_state.ip.
cbs_ip(C, Ip) when record(C, cback_state) -> 
    C#cback_state{ip = Ip}.

cbs_port(C) when record(C, cback_state) -> 
    C#cback_state.port.
cbs_port(C, Port) when record(C, cback_state) -> 
    C#cback_state{port = Port}.

cbs_opaque(C) when record(C, cback_state) -> 
    C#cback_state.opaque.
cbs_opaque(C, Opaque) when record(C, cback_state) -> 
    C#cback_state{opaque = Opaque}.


%%%
%%% Quick and dirty solution for adding SSL support.
%%%
-define(SSL, ssl).

ssl_call(Host, Port, URI) ->
    put(proto, ?SSL),
    call(Host, Port, URI).

ssl_call(Host, Port, URI, Payload) ->
    put(proto, ?SSL),
    call(Host, Port, URI, Payload).

ssl_call(Socket, URI, Payload, KeepAlive, Timeout) ->
    put(proto, ?SSL),
    call(Socket, URI, Payload, KeepAlive, Timeout).

ssl_call(Host, Port, URI, Payload, KeepAlive, Timeout) ->
    put(proto, ?SSL),
    call(Host, Port, URI, Payload, KeepAlive, Timeout).


%% Exported: call/{3,4,5,6}

call(Host, Port, URI, Payload) ->
    call(Host, Port, URI, Payload, false, 60000).

call(Host, Port, URI, Payload, KeepAlive, Timeout) ->
    case connect(Host, Port, [{active, false}]) of
	{ok, Socket} -> call(Socket, {Host,URI}, Payload, KeepAlive, Timeout);
	{error, Reason} when KeepAlive == false -> {error, Reason};
	{error, Reason} -> {error, undefined, Reason}
    end.

call(Socket, URI, Payload) -> call(Socket, URI, Payload, false, 60000).

call(Socket, URI, Payload, KeepAlive, Timeout) ->
    ?DEBUG_LOG({decoded_call, Payload}),
    case xmlrpc_encode:payload(Payload) of
	{ok, EncodedPayload} ->
	    ?DEBUG_LOG({encoded_call, EncodedPayload}),
	    case send(Socket, URI, KeepAlive, EncodedPayload) of
		ok ->
		    case parse_response(Socket, Timeout) of
			{ok, Header} ->
			    handle_payload(Socket, KeepAlive, Timeout, Header);
			{error, Reason} when KeepAlive == false ->
			    close(Socket),
			    {error, Reason};
			{error, Reason} -> {error, Socket, Reason}
		    end;
		{error, Reason} when KeepAlive == false ->
		    close(Socket),
		    {error, Reason};
		{error, Reason} ->
		    {error, Socket, Reason}
	    end;
	{error, Reason} when KeepAlive == false ->
	    close(Socket),
	    {error, Reason};
	{error, Reason} -> {error, Socket, Reason}
    end.

send(Socket, URI, false, Payload) ->
    send(Socket, URI, "Connection: close\r\n", Payload);
send(Socket, URI, true, Payload) -> send(Socket, URI, "", Payload);
send(Socket, {Host,URI}, Header, Payload) ->
    Request =
	["POST ", URI, " HTTP/1.1\r\n",
	 "Content-Length: ", integer_to_list(lists:flatlength(Payload)),
	 "\r\n",
	 "User-Agent: Erlang XML-RPC Client 1.13\r\n",
	 "Content-Type: text/xml\r\n",
	 "Host: ", Host, "\r\n",
	 Header, "\r\n",
	 Payload],
    send(Socket, Request);
send(Socket, URI, Header, Payload) ->
    Request =
	["POST ", URI, " HTTP/1.1\r\n",
	 "Content-Length: ", integer_to_list(lists:flatlength(Payload)),
	 "\r\n",
	 "User-Agent: Erlang XML-RPC Client 1.13\r\n",
	 "Content-Type: text/XML\r\n",
	 Header, "\r\n",
	 Payload],
    send(Socket, Request).

parse_response(Socket, Timeout) ->
    setopts(Socket, [{packet, line}]),
    case recv(Socket, 0, Timeout) of
	{ok, "HTTP/1.1 200 OK\r\n"} -> parse_header(Socket, Timeout);
	{ok, StatusLine} -> {error, StatusLine};
	{error, Reason} -> {error, Reason}
    end.

parse_header(Socket, Timeout) -> parse_header(Socket, Timeout, #header{}).

parse_header(Socket, Timeout, Header) ->
    case recv(Socket, 0, Timeout) of
	{ok, "\r\n"} when Header#header.content_length == undefined ->
	    {error, missing_content_length};
	{ok, "\r\n"} -> {ok, Header};
	{ok, HeaderField} ->
	    case string:tokens(HeaderField, " \r\n") of
		["Content-Length:", ContentLength] ->
		    case catch list_to_integer(ContentLength) of
			Value when integer(Value) ->
			    parse_header(Socket, Timeout,
					 Header#header{content_length =
						       Value});
			_ -> {error, {invalid_content_length, ContentLength}}
		    end;
		["Connection:", "close"] ->
		    parse_header(Socket, Timeout,
				 Header#header{connection = close});
		_ ->
		    parse_header(Socket, Timeout, Header)
	    end;
	{error, Reason} -> {error, Reason}
    end.

handle_payload(Socket, KeepAlive, Timeout, Header) ->
    case get_payload(Socket, Timeout, Header#header.content_length) of
	{ok, Payload} ->
	    ?DEBUG_LOG({encoded_response, Payload}),
	    case xmlrpc_decode:payload(Payload) of
		{ok, DecodedPayload} when KeepAlive == false ->
		    ?DEBUG_LOG({decoded_response, DecodedPayload}),
		    close(Socket),
		    {ok, DecodedPayload};
		{ok, DecodedPayload} when KeepAlive == true,
					  Header#header.connection == close ->
		    ?DEBUG_LOG({decoded_response, DecodedPayload}),
		    close(Socket),
		    {ok, Socket, DecodedPayload};
		{ok, DecodedPayload} ->
		    ?DEBUG_LOG({decoded_response, DecodedPayload}),
		    {ok, Socket, DecodedPayload};
		{error, Reason} when KeepAlive == false ->
		    close(Socket),
		    {error, Reason};
		{error, Reason} when KeepAlive == true,
				     Header#header.connection == close ->
		    close(Socket),
		    {error, Socket, Reason};
		{error, Reason} ->
		    {error, Socket, Reason}
	    end;
	{error, Reason} when KeepAlive == false ->
	    close(Socket),
	    {error, Reason};
	{error, Reason} when KeepAlive == true,
			     Header#header.connection == close ->
	    close(Socket),
	    {error, Socket, Reason};
	{error, Reason} -> {error, Socket, Reason}
    end.

get_payload(Socket, Timeout, ContentLength) ->
    setopts(Socket, [{packet, raw}]),
    recv(Socket, ContentLength, Timeout).

%% Exported: start_link/{1,5,6}

start_link(Handler) ->
    start_link(4567, 1000, 60000, Handler, undefined).

start_link(Port, MaxSessions, Timeout, Handler, State) ->
    start_link(all, Port, MaxSessions, Timeout, Handler, State).

start_link(IP, Port, MaxSessions, Timeout, Handler, State) ->
    OptionList = [{active, false}, {reuseaddr, true}|ip(IP)],
    SessionHandler = {xmlrpc_http, handler, [Timeout, Handler, State]}, 
    tcp_serv:start_link([Port, MaxSessions, OptionList, SessionHandler]).

ip(all) -> [];
ip(IP) when tuple(IP) -> [{ip, IP}].

%% Exported: stop/1

stop(Pid) -> tcp_serv:stop(Pid).


%%%
%%% Switch on bearer protocol to be used
%%%
connect(Host, Port, Opts) ->
    connect(get(proto), Host, Port, Opts).

connect(?SSL, Host, Port, Opts) -> ssl:connect(Host, Port, Opts);
connect(_,    Host, Port, Opts) -> gen_tcp:connect(Host, Port, Opts).


close(Socket) ->
    close(get(proto), Socket).

close(?SSL, Socket) -> ssl:close(Socket);
close(_   , Socket) -> gen_tcp:close(Socket).


send(Socket, Request) ->
    send(get(proto), Socket, Request).

send(?SSL, Socket, Request) -> ssl:send(Socket, Request);
send(_   , Socket, Request) -> gen_tcp:send(Socket, Request).


recv(Socket, Length, Timeout) ->
    recv(get(proto), Socket, Length, Timeout).

recv(?SSL, Socket, Length, Timeout) -> ssl:recv(Socket, Length, Timeout);
recv(_   , Socket, Length, Timeout) -> gen_tcp:recv(Socket, Length, Timeout).



setopts(Socket, Opts) ->
    setopts(get(proto), Socket, Opts).

setopts(?SSL, Socket, Opts) -> ssl:setopts(Socket, Opts);
setopts(_,    Socket, Opts) -> inet:setopts(Socket, Opts).




