-module(test).
-compile(export_all).

-include("../../xmerl/inc/xmerl.hrl").

all() ->
    t10(),
    t20(),
    t30(),
    t40(),
    t50(),
    t60(),
    t100().

%% Test: Call external XML-RPC servers

%% Test: Non keep-alive handler <-> Non keep-alive client

t10() ->
    {ok, Pid} = xmlrpc:start_link(4567, 1000, 60000, {test, h10}, undefined),
    Fun = fun({call, echo, Params} = Payload) ->
		  io:format("t10: ~p~n", [Payload]),
		  {ok, {response, [{array, Params}]}} =
		      xmlrpc:call("localhost", 4567, "/", Payload)
	  end,
    lists:foreach(Fun, payload_calls()), 
    {ok, {response, {fault, _, _}}} =
	xmlrpc:call("localhost", 4567, "/", {call, baz, []}),
    xmlrpc:stop(Pid).

h10(State, {call, echo, Params}) -> {false, {response, [{array, Params}]}};
h10(State, Payload) ->
    FaultString = lists:flatten(io_lib:format("~p", [Payload])),
    {false, {response, {fault, -1, FaultString}}}.

%% Test: Non keep-alive handler <-> Keep-Alive client

t20() ->
    {ok, Pid} = xmlrpc:start_link(4567, 1000, 60000, {test, h20}, undefined),
    Params = ["foo"],
    Payload = {call, echo, Params},
    io:format("t20: ~p~n", [Payload]),
    {ok, Socket, {response, [{array, Params}]}} =
	xmlrpc:call("localhost", 4567, "/", Payload, true, 60000),
    {error, Socket, closed} = xmlrpc:call(Socket, "/", Payload, true, 60000),
    xmlrpc:stop(Pid).

h20(State, {call, echo, Params}) -> {false, {response, [{array, Params}]}}.

%% Test: Keep-Alive handler <-> Non keep-alive client

t30() ->
    {ok, Pid} = xmlrpc:start_link(4567, 1000, 60000, {test, h30}, 0),
    Params = ["foo"],
    Payload = {call, echo, Params},
    io:format("t30: ~p~n", [Payload]),
    {ok, {response, [{array, [0|Params]}]}} =
	xmlrpc:call("localhost", 4567, "/", Payload, false, 60000),
    {ok, {response, [{array, [0|Params]}]}} =
	xmlrpc:call("localhost", 4567, "/", Payload, false, 60000),
    {ok, {response, {fault, _, _}}} =
	xmlrpc:call("localhost", 4567, "/", {call, baz, []}, false, 60000),
    xmlrpc:stop(Pid).

h30(N, {call, echo, Params}) ->
    {true, 60000, N+1, {response, [{array, [N|Params]}]}};
h30(N, Payload) ->
    FaultString = lists:flatten(io_lib:format("~p", [Payload])),
    {true, 60000, N, {response, {fault, -1, FaultString}}}.

%% Test: Keep-Alive handler <-> Keep-Alive client

t40() ->
    {ok, Pid} = xmlrpc:start_link(4567, 1000, 60000, {test, h40}, 0),
    Params = ["foo"],
    Payload = {call, echo, Params},
    io:format("t40: ~p~n", [Payload]),
    {ok, Socket, {response, [{array, [0|Params]}]}} =
	xmlrpc:call("localhost", 4567, "/", Payload, true, 60000),
    {ok, Socket, {response, [{array, [1|Params]}]}} =
	xmlrpc:call(Socket, "/", Payload, true, 60000),
    {ok, {response, [{array, [2|Params]}]}} =
	xmlrpc:call(Socket, "/", Payload, false, 60000),
    {error, closed} = xmlrpc:call(Socket, "/", Payload, false, 60000),
    {ok, Socket2, {response, [{array, [0|Params]}]}} =
	xmlrpc:call("localhost", 4567, "/", Payload, true, 60000),
    {ok, Socket2, {response, [{array, [1|Params]}]}} =
	xmlrpc:call(Socket2, "/", Payload, true, 60000),
    {ok, Socket2, {response, [{array, [2|Params]}]}} =
	xmlrpc:call(Socket2, "/", Payload, true, 60000),
    {ok, Socket2, {response, ["ok"]}} =
	xmlrpc:call(Socket2, "/", {call, close, []}, true, 60000),
    {error, Socket2, closed} =
	xmlrpc:call(Socket2, "/", Payload, true, 60000),
    xmlrpc:stop(Pid).

h40(N, {call, close, []}) -> {false, {response, ["ok"]}};
h40(N, {call, echo, Params}) ->
    {true, 60000, N+1, {response, [{array, [N|Params]}]}};
h40(N, Payload) ->
    FaultString = lists:flatten(io_lib:format("~p", [Payload])),
    {true, 60000, N, {response, {fault, -1, FaultString}}}.

%% Test: Apache XML-RPC client 

t50() ->
    {ok, Pid} = xmlrpc:start_link(4567, 1000, 60000, {test, h50}, undefined),
    Cmd = "java -classpath xmlrpc-1.1.jar:. T50",
    io:format("t50: ~p~n", [Cmd]),
    "[[42, foo], {bar=45.5, baz=4711}]\norg.apache.xmlrpc.XmlRpcException: Unknown call: {call,foo,[]}\n" = os:cmd(Cmd),
    xmlrpc:stop(Pid).

h50(State, {call, echo, Params}) -> {false, {response, [{array, Params}]}};
h50(State, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    {false, {response, {fault, -1, FaultString}}}.

%% Test: Keep-Alive Apache XML-RPC client 

t60() ->
    {ok, Pid} = xmlrpc:start_link(4567, 1000, 60000, {test, h60}, 0),
    Cmd = "java -classpath xmlrpc-1.1.jar:. T60",
    io:format("t60: ~p~n", [Cmd]),
    "[0, [42, foo], {bar=45.5, baz=4711}]\n[1, [42, foo], {bar=45.5, baz=4711}]\norg.apache.xmlrpc.XmlRpcException: Unknown call: {call,foo,[]}\n" =
	os:cmd(Cmd),
    xmlrpc:stop(Pid).

h60(N, {call, echo, Params}) ->
    {true, 60000, N+1, {response, [{array, [N|Params]}]}};
h60(N, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    {false, {response, {fault, -1, FaultString}}}.

%% Test: Timeouts

%% Test: Negative testing

%% Test: Encoding and decoding

t100() ->
    io:format("t100~n"),
    encode_decode(payload_calls()++payload_responses()).

encode_decode([]) -> ok;
encode_decode([Payload|Rest]) ->
    case xmlrpc_encode:payload(Payload) of
	{ok, EncodedPayload} ->
	    case xmlrpc_decode:payload(lists:flatten(EncodedPayload)) of
		{ok, Payload} -> encode_decode(Rest);
		{ok, MismatchedPayload} ->
		    {error, {mismatch, Payload, EncodedPayload,
			     MismatchedPayload}};
		{error, Reason} ->
		    {error, {decode, Payload, EncodedPayload, Reason}}
	    end;
	{error, Reason} -> {error, {encode, Payload, Reason}}
    end.

payload_calls() ->
    [{call, echo, []},
     {call, echo, [true]},
     {call, echo, [false]},
     {call, echo, [12]},
     {call, echo, [12.1]},
     {call, echo, ["bar"]},
     {call, echo, [{date, "19980717T14:08:55"}]},
     {call, echo, [{base64, "eW91IGNhbid0IHJlYWQgdGhpcyE="}]},
     {call, echo, [{array, [1, 5.8, "baz"]}]},
     {call, echo, [{struct, [{yes, true}, {no,false}]}]},
     {call, echo,
      [{array, [{array, [1, 5.8, "baz",
			 {array, [{date, "19980717T14:08:55"},
				  {base64, "eW91IGNhbid0IHJlYWQgdGhpcyE="}]},
			 {struct, [{yes, true}, {no, false}]}]},
		"slartibartfast"]}]}].

payload_responses() ->
    [{response, [true]},
     {response, [false]},
     {response, [12]},
     {response, [12.1]},
     {response, ["bar"]},
     {response, [{date, "19980717T14:08:55"}]},
     {response, [{base64, "eW91IGNhbid0IHJlYWQgdGhpcyE="}]},
     {response, [{array, [1, 5.8, "baz"]}]},
     {response, [{struct, [{yes, true}, {no,false}]}]},
     {response, 
      [{array, [{array, [1, 5.8, "baz",
			 {array, [{date, "19980717T14:08:55"},
				  {base64, "eW91IGNhbid0IHJlYWQgdGhpcyE="}]},
			 {struct, [{yes, true}, {no, false}]}]},
		"slartibartfast"]}]},
     {response, {fault, 42, "foo"}}].
