-module(pico_http_server).

%% File    : pico_http_server.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%% Purpose : stand alone HTTP server

-include_lib("kernel/include/inet.hrl").

%% History
%% This is derived from
%%   http_filter.erl  (tobbe)
%%   httpd.erl        (jocke)
%%   httpd_parse.erl  (jocke)
%%   http_data.erl    (jocke)
%%

-export([start/4, stop/2]).

-export([internal/4]).

-import(lists, [reverse/1]).
-import(pico_utils, [header/1, body/1]).

port_name(Port) when integer(Port) ->
    list_to_atom("picoHttpServer_" ++ integer_to_list(Port)).

start(Port, Max, Mod, Args) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    Pid = spawn_link(?MODULE, internal,
			     [Port, Max, Mod, Args]),
	    register(Name, Pid),
	    true;
	Pid ->
	    false
    end.

stop(Port, Reason) ->
    Name = port_name(Port),
    case whereis(Name) of
	undefined ->
	    error;
	Pid ->
	    Pid ! {self(), {stop, Reason}},
	    receive
		{Pid, {stopped, State}} ->
		    {ok, State};
		{Pid, {exit, Why}} ->
		    {error, Why}
	    end
    end.

internal(Port, Max, Mod, Args) ->
    process_flag(trap_exit, true),    
    State = Mod:start_handler(Args),
    Self = self(),
    Fun = fun(I) -> handle_request(I, Self) end,
    pico_socket_server:start_server(Port, 
				    fun(Socket,Bin) ->
					    get_header(Socket,Bin,[],Fun) end,
				    Max),
    loop(Mod, State).


%% This gets done *inside* the socket serevr
%% It always sends back a reply to the socket and then terminates

handle_request({Op, URI, Args, Socket}, Server) ->
    %% This is done *within* the socket process
    %% i.e.- while connected to the client
    HostName = hostname(Socket),
    Server ! {self(), {request, {Op, HostName, URI, Args}}},
    receive
	{Server, {reply_and_exit, Str}} ->
	    gen_tcp:send(Socket, Str),
	    {'EXIT', normal}
    end.

loop(Mod, State) ->
    receive
	{Pid, {request, Req}} ->
	    case (catch Mod:event_handler(Req, State)) of
		{'EXIT', Why} ->
		    io:format("no way:~p~n",[Why]),
		    Pid ! {self(), {reply_and_exit,handler_error("EXIT")}},
		    loop(Mod, State);
		{Reply, State1} ->
		    case reply_header_ok(Reply) of
			true ->
			    Pid ! {self(), {reply_and_exit, Reply}},
			    loop(Mod, State1);
			false ->
			    io:format("bad header:~p~n",[Reply]),
	       	    Pid ! {self(), {reply_and_exit,
					    handler_error("Bad header")}},
			    loop(Mod, State)
		    end
	    end;
	{Pid, {stop, Reason}} ->
	    case (catch Mod:stop_handler(Reason, State)) of
		{'EXIT', Why} ->
		    Pid ! {self(), {exit, Why}};
		State1 ->
		    Pid ! {self(), {stopped, State1}}
	    end;
	Other ->
	    io:format("uuugh:~p~n", [Other])
    end.

%% Check we have put in a header

reply_header_ok("HTTP/" ++ _) -> true;
reply_header_ok([H|_])        -> reply_header_ok(H);
reply_header_ok(_)            -> false.

get_header(Socket, Bin, Buff, Fun) ->
    Data = binary_to_list(Bin),
    case scan_header(Data, Buff) of
	{yes, Header, After} ->
	    %% We've got enough to be going on with
	    case pico_utils:parse_header(Header) of
		{Op, ContentLen, _Vsn, URI, Args, _Env} ->
                    case ContentLen of
                        0 ->
                            case After of
                                [] -> true;
                                _  -> io:format("**** ERROR2 FIXIT pico:~p~n",
						[After])
                            end,
                            Fun({Op, URI, Args, Socket});
                        _ -> collect_post_data(Socket,list_to_binary([]),
                                               lists:reverse(After),
                                               ContentLen,Fun, Op,URI)
                    end;
                Other ->
                    io:format("Oops ~p ~n", [Other]),
		    exit(debug)
	    end;
	{no, Buff1} ->
	    fun(S,X) -> get_header(S, X, Buff1, Fun) end
    end.

collect_post_data(Socket,Bin,Buff,Len,Fun,Op,URI) ->
    Data = binary_to_list(Bin),
    case collect_chunk(Data,Buff,Len) of
        {yes,PostData} ->
            Args = pico_utils:parse_uri_args(PostData),
            Fun({Op,URI,Args,Socket});
        {no,PostData} ->
            fun(S,X) ->
                    collect_post_data(S,X,PostData,Len,Fun,Op,URI) end
    end.

default_form_handler(X, Socket) ->
    Str = io_lib:format("~p", [X]),
    gen_tcp:send(Socket, [header({ok,html}),
			  "<html> <body>Request was <p><pre>", Str, "</pre></body> </html>"]),
    gen_tcp:close(Socket).

handler_error(X) ->
    [header({error,"500 Handler error",X})].


scan_header([$\n|T], [$\r,$\n,$\r|L]) -> {yes, reverse(L), T};
scan_header([H|T],  L)                -> scan_header(T, [H|L]);
scan_header([], L)                    -> {no, L}.

collect_chunk([H|T],Data,Size) ->
    collect_chunk(T,[H|Data],Size);
collect_chunk([],Buf,Size) ->
    Len = length(Buf),
    if
        Len >= Size -> {yes, lists:reverse(Buf)};
        true        -> {no, Buf}
    end.

hostname(Socket)  ->
    case inet:peername(Socket) of
	{ok,{Addr,_}} ->	
	    case inet:gethostbyaddr(Addr) of
		{ok,HostIdent} -> {ok, HostIdent#hostent.h_name};	
		_ -> unknown
	    end;
	_ ->
	    unknown
    end.
