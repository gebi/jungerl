%%% File    : erl_cgi.erl
%%% Author  : Tony Rogvall <tony@localhost.localdomain>
%%% Description : Simple cgi interface
%%% Created : 26 Mar 2002 by Tony Rogvall <tony@localhost.localdomain>

-module(erl_cgi).

-compile(export_all).
-export([start/0, run/1]).
-export([collect/0]).

-import(lists, [map/2]).

-include("erl_cgi.hrl").

%%
%% Envirnoment variables in CGI:
%%
%% SERVER_SOFTWARE 
%%
%%     The name and version of the information server software answering
%%     the request (and running the gateway). Format: name/version 
%%
%% SERVER_NAME 
%%
%%     The server's hostname, DNS alias, or IP address as it would appear
%%      in self-referencing URLs. 
%%
%% GATEWAY_INTERFACE 
%%
%%     The revision of the CGI specification to which this server complies.
%%     Format: CGI/revision
%%
%%
%% SERVER_PROTOCOL 
%%
%%     The name and revision of the information protcol this request came in 
%%     with. Format: protocol/revision 
%%
%% SERVER_PORT 
%%
%%     The port number to which the request was sent. 
%%
%% REQUEST_METHOD
%%
%%     The method with which the request was made. For HTTP, this is 
%%     "GET", "HEAD", "POST", etc. 
%%
%% PATH_INFO
%%
%%     The extra path information, as given by the client. In other words, 
%%     scripts can be accessed by their virtual pathname, followed by extra 
%%     information at the end of this path. The
%%     extra information is sent as PATH_INFO. This information should be 
%%     decoded by the server if it comes from a URL before it is passed to 
%%     the CGI script.
%%
%% PATH_TRANSLATED
%%
%%     The server provides a translated version of PATH_INFO, which takes 
%%     the path and does any virtual-to-physical mapping to it. 
%%
%% SCRIPT_NAME 
%%
%%     A virtual path to the script being executed, used for self-referencing 
%%     URLs. 
%%
%% QUERY_STRING 
%%
%%     The information which follows the ? in the URL which referenced this
%%     script. This is the query information. It should not be decoded in 
%%     any fashion. This variable should
%%     always be set when there is query information, regardless of command 
%%     line decoding. 
%%
%% REMOTE_HOST 
%%
%%     The hostname making the request. If the server does not have this 
%%     information, it should set REMOTE_ADDR and leave this unset.
%%
%% REMOTE_ADDR
%%
%%     The IP address of the remote host making the request. 
%%
%% AUTH_TYPE 
%%
%%     If the server supports user authentication, and the script is 
%%     protects, this is the protocol-specific authentication method
%%     used to validate the user. 
%%
%% REMOTE_USER
%%
%%     If the server supports user authentication, and the script is 
%%     protected, this is the username they have authenticated as. 
%%
%% REMOTE_IDENT
%%
%%     If the HTTP server supports RFC 931 identification, then this 
%%     variable will be set to the remote user name retrieved from the 
%%     server. Usage of this variable should be limited
%%     to logging only. 
%%
%% CONTENT_TYPE
%%
%%     For queries which have attached information, such as HTTP POST and PUT,
%%     this is the content type of the data. 
%%
%% CONTENT_LENGTH
%%
%%     The length of the said content as given by the client. 
%%
%%  In addition to these, the header lines received from the client, if any,
%%  are placed into the environment with the prefix HTTP_ followed by the 
%%  header name. Any - characters in the
%%  header name are changed to _ characters. The server may exclude any 
%%  headers which it has already processed, such as Authorization, 
%%  Content-type, and Content-length. If necessary,
%%  the server may choose to exclude any or all of these headers if
%%  including them would exceed any system environment limits. 
%%
%% HTTP_ACCEPT 
%%
%%     The MIME types which the client will accept, as given by HTTP headers.
%%     Other protocols may need to get this information from elsewhere. 
%%     Each item in this list should be
%%     separated by commas as per the HTTP spec. 
%%
%%     Format: type/subtype, type/subtype 
%%
%% HTTP_USER_AGENT
%%
%%     The browser the client is using to send the request. 
%%     General format: software/version library/version.
%%

send(Cgi, Data) ->
    (Cgi#cgi.send)(Data).

recv(Cgi, N) ->
    (Cgi#cgi.recv)(N).

send(Data) ->
    (get(cgi_send))(Data).

recv(N) ->
    (get(cgi_recv))(N).


esp_error(Reason) ->
    Env = format_env(ets:tab2list(get(cgi_env))),
    io_lib:format("<HTML>\n"
		  "<HEAD><TITLE>Error</TITLE></HEAD>\n"
		  "<BODY><PRE>Reason: ~p\nEnvironment: ~s\n</PRE></BODY>\n"
		  "</HTML>\n", [Reason, Env]).


init() ->
    #cgi {
      env = ets:new(env, [set]),
      send = fun(Data) -> stdio:write(Data) end,
      recv = fun(N) -> stdio:readn(N) end
     }.
    

start() ->
    stdio:open_stdin(),
    stdio:open_stdout(),
    Cgi = init(),
    load_env(Cgi),
    run(Cgi),
    receive
    after 1000 -> ok
    end,
    stdio:close(stdout),
    wait_for_eof(),
    stdio:close(stdin),
    halt(0).


wait_for_eof() ->
    case stdio:read() of
	eof ->
	    ok;
	{ok, Data} ->
	    wait_for_eof();
	Other ->
	    ok
    end.


run(Cgi) ->
    put(cgi_env, Cgi#cgi.env),
    put(cgi_send, Cgi#cgi.send),
    put(cgi_recv, Cgi#cgi.recv),

    new_headers("text/html"),

    Script = esp_path(Cgi),
    Doc = case catch esp_script(Cgi, Script) of
	       {'EXIT', Reason} ->
		   esp_error(Reason);
	       Data -> 	
		  Data
	  end,
    Status = case get(redirect) of
		 undefined ->
		     case is_nph_script(Script) of
			 true ->
			     set_header("Connection", "close"),
			     status(200, "Ok");
			 false ->
			     add_header("Script-Control", "no-abort"),
			     ""
		     end;
		 Url ->
		     add_header("Location", Url),
		     case is_nph_script(Script) of
			 true  -> 
			     set_header("Connection", "close"),
			     status(302, "Moved");
			 false ->
			     add_header("Script-Control", "no-abort"),
			     ins_header("Status", "302 Moved"),
			     ""
		     end
	     end,
    Hs = get_headers(),
    write_http(Cgi, Status, get_headers()),
    write_doc(Cgi, list_to_binary([Doc])).


write_http(Cgi, Status, Headers) ->
    send(Cgi,[Status,
	      map(
		fun({Key,Value}) ->
			Key++": " ++ Value ++ "\r\n"
		end, Headers),
	      "\r\n"
	     ]).

write_doc(Cgi, Bin) ->
    write_chunks(Cgi, Bin).


write_chunks(Cgi, <<Bin:4096/binary, Rest/binary>>) ->
    send(Cgi, Bin),
    write_chunks(Cgi, Rest);
write_chunks(Cgi, <<>>) -> ok;
write_chunks(Cgi, Bin) ->
    send(Cgi, Bin).


%% read a cgi body
collect() ->
    case getvar("CONTENT_LENGTH") of
	"" -> "";
	SN ->
	    N = list_to_integer(SN),
	    case recv(N) of
		eof -> "";
		{ok,Data}  -> Data
	    end
    end.

%%
%% Generate a status reply
%%
status(Code, Phrase) ->
    io_lib:format("~s ~w ~s\r\n", 
		  [getvar("SERVER_PROTOCOL", "HTTP/1.0"),
		   Code, Phrase]).

%%
%% Check for nph (none parse headers) IIS workaround, to disable
%% header parsing. The nph scripts must generate a full HTTP response
%% with status and headers
%%
is_nph_script(Script) ->
    case os:type() of
	{win32,_} -> %% FIXME: check for SERVER=IIS
	    case filename:basename(Script) of
		"nph-" ++ _ -> true;
		_ -> false
	    end;
	_ ->
	    false
    end.

esp_path(Cgi) ->
    getvar(Cgi#cgi.env, "PATH_TRANSLATED", "").


esp_script(Cgi, ScriptName) ->
    Bindings = cgi_bindings(Cgi),
    case esp:load(ScriptName, Bindings) of
	{ok, {document,Cs}} -> 
	    esp_html:format({document,Cs});
	{ok, {app,Mime,Data}} ->
	    set_header("Content-type", Mime),
	    Data;
	Error -> exit({Error, ScriptName})
    end.

new_headers(Mime) ->
    erase(cgi_headers),
    set_header("Content-type", Mime),
    set_header("Pragma", "no-cache"),
    set_header("Cache-Control", "no-cache").


%% Get a list of all HTTP headers
get_headers() ->
    case get(cgi_headers) of
	undefined -> [];
	Headers -> Headers
    end.

put_headers(Hs) ->
    put(cgi_headers,Hs).

%% Add or replace
set_header(Key,Value) ->
    Hs0 = get_headers(),
    Hs1 =
	case lists:keysearch(Key,1,Hs0) of
	    {value,_} ->
		lists:keyreplace(Key,1,Hs0,{Key,Value});
	    false ->
		Hs0++[{Key,Value}]
	end,
    put_headers(Hs1).


%% Add header
add_header(Key,Value) ->
    put_headers(get_headers()++[{Key,Value}]).

%% Prepend header
ins_header(Key,Value) ->
    put_headers([{Key,Value}]++get_headers()).   


%% Remove (first) header
remove_header(Key) ->
    Hs = lists:keydelete(Key,1,get_headers()),
    put_headers(Hs).

redirect(Url) ->
	put(redirect, Url).


script_path(CGI_dir, [$/|Name]) ->
    filename:join(CGI_dir, Name);
script_path(CGI_dir, Name) ->
    filename:join(CGI_dir, Name).
    

%% parse post data on form content-type: appication/url-encoded??
decode_urlencoded(Data) ->
    map(fun(KeyVal) ->
		case string:tokens(KeyVal, "=") of
		    [Key,Val] -> 
			{list_to_atom(decode_urlstring(Key)),
			 decode_urlstring(Val)};
		    [Key] ->
			{list_to_atom(decode_urlstring(Key)),""}
		end
	end, string:tokens(Data, "&")).

decode_urlstring([$+ | Cs]) ->
    [$\s | decode_urlstring(Cs)];
decode_urlstring([$%,H1,H0 | Cs]) ->
    C = hex(H1,H0),
    if C == 0 ->
	    decode_urlstring(Cs);
       true ->
	    [C | decode_urlstring(Cs)]
    end;
decode_urlstring([0|Cs]) ->
    decode_urlstring(Cs);
decode_urlstring([C|Cs]) ->
    [C|decode_urlstring(Cs)];
decode_urlstring([]) ->
    [].

hex(H1,H0) ->    
    (hex(H1) bsl 4) bor hex(H0).

hex(H) when H >= $A, H =< $F -> (H - $A) + 10;
hex(H) when H >= $a, H =< $f -> (H - $a) + 10;
hex(H) when H >= $0, H =< $9 -> (H - $0).


format_env([{K,V} | T]) ->
    [ K, "=", V, "\n" | format_env(T)];
format_env([]) ->
    [].

%% lookup a variable in the cgi_env table
getvar(Var) ->
    getvar(Var, "").

getvar(Var, Default) ->
    getvar(get(cgi_env), Var, Default).

getvar(Tab, Var, Default) ->
    case ets:lookup(Tab, Var) of
	[{_,Value}] -> Value;
	_ -> Default
    end.


%% extract CGI bindings into the esp environment

cgi_bindings() ->
    bindings(get(cgi_env)).

cgi_bindings(Cgi) ->
    bindings(Cgi#cgi.env).


bindings(Tab) ->
    bindings(erl_eval:new_bindings(), ets:tab2list(Tab)).

bindings(B0, [{Key,Val} | Tail]) ->
    case is_cgi_var(Key) of
	false  -> bindings(B0, Tail);
	CgiVar ->
	    B1 = erl_eval:add_binding(CgiVar, Val, B0),
	    bindings(B1, Tail)
    end;
bindings(B0, []) ->
    B0.

%% Check CGI variables
is_cgi_var("HTTP_" ++ K)     -> list_to_atom("HTTP_"++K);
is_cgi_var("SERVER_" ++ K)   ->  list_to_atom("SERVER_"++K);
is_cgi_var("GATEWAY_INTERFACE") -> 'GATEWAY_INTERFACE';
is_cgi_var("REQUEST_METHOD") -> 'REQUEST_METHOD';
is_cgi_var("PATH_INFO") -> 'PATH_INFO';
is_cgi_var("PATH_TRANSLATED") -> 'PATH_TRANSLATED';
is_cgi_var("SCRIPT_NAME") -> 'SCRIPT_NAME';
is_cgi_var("QUERY_STRING") -> 'QUERY_STRING';
is_cgi_var("REMOTE_HOST") -> 'REMOTE_HOST';
is_cgi_var("REMOTE_ADDR") -> 'REMOTE_ADDR';
is_cgi_var("AUTH_TYPE") -> 'AUTH_TYPE';
is_cgi_var("CONTENT_LENGTH") -> 'CONTENT_LENGTH';
is_cgi_var("CONTENT_TYPE") -> 'CONTENT_TYPE';
is_cgi_var("REMOTE_IDENT") -> 'REMOTE_IDENT';
is_cgi_var("REMOTE_USER") -> 'REMOTE_USER';
is_cgi_var(_) -> false.

%% Load environment into table
load_env(Cgi) ->
    load_env(Cgi#cgi.env, os:getenv()).

load_env(Tab, [KV|Ks]) ->
    case string:chr(KV, $=) of
	0 ->
	    ets:insert(Tab, {KV,""}),
	    load_env(Tab,Ks);
	I ->
	    Key = string:substr(KV, 1, I-1),
	    Val = string:substr(KV, I+1, length(KV)),
	    ets:insert(Tab, {Key,Val}),
	    load_env(Tab,Ks)
    end;
load_env(Tab, []) -> ok.


