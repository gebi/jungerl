-module(url).

%IA Joe Armstrong
%ID 970212
%IK [url,htttp,get,disk,cache]
%IH Get a URL from the network or cache
%IT <p><b>raw_get_url(URL, Timeout, Proxy) -> ok{Bin} | error{Why}</b>
% does a raw get from the network, <b>Proxy = noproxy | {IP,Port}</b> 
% <p>To use the cache you must write:
% <pre>
% url:start_cache("File").
%   url:get(URL, Timeout, Proxy),
%   ...
% url:stop_cache().
% </pre>
% <P>.Here <b>File</b> will be used as a cache.

% hacked 2003.0225 by Chris Pressey to work with inet & gen_tcp
% modified by Mickael Remond to allow retrieval of file or HTTP url

-include_lib("kernel/include/inet.hrl").

-export([test/0,
	 start_cache/1, 
	 stop_cache/0, 
	 get/3, 
	 raw_get_url/2, 
	 raw_get_url/3]).

-import(lists, [reverse/1]).

-define(SERVER, url_proxy_server).
-define(RPC(X), gen_server:call(?SERVER, X, 10000)).

test() ->
    raw_get_url("http://www.ericsson.se/", 60000, 
		{"super.du.etx.ericsson.se",888}).

%% raw_get_url(URL, Timeout) -> {ok, {URL, #Bin}} |
%% raw_get_url(URL, Timeout, {Proxy,Port}) -> {URL, #Bin} raises url

start_cache(Cache) -> disk_cache:start(Cache).
stop_cache()       -> disk_cache:stop().

get(URL, Proxy, Timeout) ->
    case disk_cache:fetch(URL) of
	{ok, Val} ->
	    io:format("+ ~n", []),
	    {ok, Val};
	not_found ->
	    io:format("! ", []),
	    case raw_get_url(URL, Timeout, Proxy) of
		{ok, Data} ->
		    io:format("$", []),
		    disk_cache:store(URL, Data),
                    io:nl(),
		    {ok, Data};
		{error, What} ->
		    {error, What}
	    end
    end.

raw_get_url(URL, Timeout) -> 
    case url_parse:parse(URL) of
	{error, Why} ->
	    {error, {badURL,URL}};
	{http, HostName, Port, File} ->
	    get_http(HostName, Port, File, ["Host: ", HostName], Timeout);
	{file, Location} ->
	    get_file(Location)
    end.

raw_get_url(URL, Timeout, {IP, Port}) ->
    get_http(IP, Port, URL, [], Timeout).

get_file(Location) ->
    file:read_file(Location).

get_http(IP, Port, URL, Opts, Timeout) ->
    %% io:format("ip = ~p, port = ~p, url = ~p~n", [ IP, Port, URL ]),
    Cmd = ["GET ", URL, " HTTP/1.1\r\n", Opts, "\r\n\r\n"],
    %% io:format("Cmd=~p\n", [Cmd]),
    %% io:format("url_server: fetching ~p ~p ~p~n", [IP, Port, URL]),
    case catch
      gen_tcp:connect(IP, Port,
       [binary, {packet, raw}, {nodelay, true}, {active, true}]) of
	{'EXIT', Why} -> 
	    %% io:format("Socket exit:~p~n", [Why]),
	    {error, {socket_exit, Why}};
	{error, Why} -> 
	    %% io:format("Socket error:~p~n", [Why]),
	    {error, {socket_error, Why}};
	{ok, Socket} ->
	    %% io:format("Socket = ~p~n", [Socket]),
	    gen_tcp:send(Socket, Cmd),
	    receive_data(Socket, Timeout, list_to_binary([]))
    end.

receive_data(Socket, Timeout, Bin) ->
    receive
	{tcp, Socket, B} ->
	    %io:format(".", []),
	    receive_data(Socket, Timeout, concat_binary([Bin,B]));
	{tcp_closed, Socket} ->
	    Data0 = binary_to_list(Bin),
	    %% io:fwrite("Socket closed: ~p~n", [Data0]),
	    {Data1, Info} = get_header(Data0, []),
	    Bin1 = list_to_binary(Data1),
	    {ok, Bin1};
	Other ->
            %% io:fwrite("Other: ~p~n", [Other]),
	    {error, {socket, Other}}
	after
	    Timeout ->
		{error, timeout}
    end.

get_header([$\r,$\n | T], Info) -> 
    header_end(T, Info);
get_header(Cs, Info) -> 
    header_line(Cs, [], Info).

header_line([$\r,$\n | T], Acc, Info) ->
    get_header(T, [split_info(reverse(Acc)) | Info]);
header_line([C | Cs], Acc, Info) ->
    header_line(Cs, [C | Acc], Info);
header_line([], Acc, Info) ->
    header_end([], [split_info(reverse(Acc)) | Info]).

header_end([$\r,$\n | T], Info) -> 
    header_end(T, Info);
header_end(T, Info) -> 
    {T, Info}.

split_info(String) ->
    case string:chr(String, $:) of
	0 -> {trim(String), []};
	Ix ->
	    {trim(string:substr(String, 1, Ix-1)),
	     trim(string:substr(String, Ix+1, length(String)))}
    end.

trim(String) ->
    reverse(strip(reverse(strip(String)))).

strip([$ | Cs]) -> strip(Cs);
strip([$\t | Cs]) -> strip(Cs);
strip(Cs) -> Cs.
