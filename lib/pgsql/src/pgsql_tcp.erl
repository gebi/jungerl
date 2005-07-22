%%% File    : pgsql_tcp.erl
%%% Author  : Blah <cos@local>
%%% Description : Unwrapping of TCP line protocol packages to postgres messages.
%%% Created : 22 Jul 2005

-module(pgsql_tcp).

-export([loop0/2]).

loop0(Sock, ProtoPid) ->
    %% Async, but flow controlled, delivery of data blocks.
    inet:setopts(Sock, [{active, once}]),
    loop(Sock, ProtoPid, <<>>).
loop(Sock, ProtoPid, Buffer) ->
    receive
	{tcp, Sock, Bin} ->
	    {ok, Rest} = process_buffer(ProtoPid, <<Buffer/binary, Bin/binary>>),
	    inet:setopts(Sock, [{active, once}]),
	    loop(Sock, ProtoPid, Rest);
	
	{tcp_closed, Sock} ->
	    io:format("Sock closed~n", []),
	    ProtoPid ! {socket, Sock, closed},
	    exit(tcp_close);
	{tcp_error, Sock, Reason} ->
	    io:format("Sock error~n", []),
	    ProtoPid ! {socket, Sock, {error, Reason}},
	    exit({tcp_error, Reason})
    end.

%% Given a binary that begins with a proper message header the binary
%% will be processed for each full message it contains, and it will
%% return any trailing incomplete messages.
process_buffer(ProtoPid, Bin = <<Code:8/integer, Size:4/integer-unit:8, Rest/binary>>) ->
    Payload = Size - 4,
    if
	size(Rest) >= Payload ->
	    <<Packet:Payload/binary, Rest1/binary>> = Rest,
	    {ok, Message} = pgsql_proto:decode_packet(Code, Packet),
	    ProtoPid ! {pgsql, Message},
	    process_buffer(ProtoPid, Rest1);
	true ->
	    {ok, Bin}
    end;
process_buffer(ProtoPid, Bin) when is_binary(Bin) ->
    {ok, Bin}.

