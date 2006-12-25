-module(pico_utils).

%% Copyright (C) 1999, Bluetail AB
%% File    : pico.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%% Purpose : Microscopically small web server

-export([str2urlencoded/1, urlencoded2str/1]).
-export([show/1, header/1, body/1, pre/1, h1/1, h3/1, b/1, str/1, classify/1]).
-export([parse_header/1, parse_uri_args/1]).
-export([permit/2]).

-include_lib("kernel/include/inet.hrl").

-import(lists,[map/2,member/2,reverse/1]).

permit([{allow, Re}|T], Name) ->
    case matches(Name, Re) of
	true -> true;
	false -> permit(T, Name)
    end;
permit([{reject, Re}|T], Name) ->
    case matches(Name, Re) of
	true -> false;
	false -> permit(T, Name)
    end;
permit([], _) ->
    false.

matches(File, Re) ->
    case regexp:match(File, Re) of
        {match, _, _} -> true;
        _ -> false
    end.

%%----------------------------
%% Utilities
%% Notes on the encoding of URI's
%% This comes from secition 8.2.1 of RFC1866

%% The default encoding for all forms is `application/x-www-form-urlencoded'.
%% A form data set is represented in this media type as follows:
%% 
%%   1. The form field names and values are escaped: space characters are
%%      replaced by `+', and then reserved characters are escaped as per [URL];
%%      that is, non-alphanumeric characters are replaced by `%HH', a percent
%%      sign and two hexadecimal digits representing the ASCII code of the
%%      character. Line breaks, as in multi-line text field values, are
%%      represented as CR LF pairs, i.e. `%0D%0A'.
%% 
%%   2. The fields are listed in the order they appear in the document with the
%%      name separated from the value by `=' and the pairs separated from each
%%      other by `&'. Fields with null values may be omitted. In particular,
%%      unselected radio buttons and checkboxes should not appear in the
%%      encoded data, but hidden fields with VALUE attributes present should.
%% 

str2urlencoded([$ |T]) ->
    [$+|str2urlencoded(T)];
str2urlencoded([$\n|T]) ->
    "%0D%0A" ++ str2urlencoded(T);
str2urlencoded([H|T]) ->
    case is_alphanum(H) of
	true ->
	    [H|str2urlencoded(T)];
	false ->
	    {Hi,Lo} = byte2hex(H),
	    [$%,Hi,Lo|str2urlencoded(T)]
    end;
str2urlencoded([]) -> [].

byte2hex(X) ->
    {nibble2hex(X bsr 4), nibble2hex(X band 15)}.

nibble2hex(X) when X < 10 -> $0 + X;
nibble2hex(X) -> $A + X - 10.

is_alphanum(X) when $0 =< X, X =< $9 -> true;
is_alphanum(X) when $a =< X, X =< $z -> true;
is_alphanum(X) when $A =< X, X =< $Z -> true;
is_alphanum(_) -> false.

%%----------------------------------------------------------------------

show(X) ->
    io_lib:format("~p~n",[X]).
	   
quote_lt([$<|T]) -> "&lt;" ++ quote_lt(T);
quote_lt([H|T])  -> [H|quote_lt(T)];
quote_lt([])     -> [].

mime(text) -> "text/plain";
mime(html) -> "text/html";
mime(jpg)  -> "image/jpeg";
mime(gif)  -> "image/gif";
mime(_)    -> unknown.

header({ok,Type})  -> 
    ["HTTP/1.0 200 Ok\r\n", powered_by(), content_type(mime(Type)), "\r\n"];
header({error,Code,Resp})  -> 
    ["HTTP/1.0 ", Code, "\r\n", powered_by(), content_type("text/plain"), "\r\n", Resp];
header({redirect,To}) ->
    ["HTTP/1.0 302 Come and get it!\r\n",
     powered_by(), "Location: " ++ To ++ "\r\n\r\n"].

powered_by() ->
    "X-Powered-By: Erlang (pico-11.0)\r\n".

content_type(unknown) -> [];
content_type(X) ->
    ["Content-Type: ", X, "\r\n"].

body(X) -> ["<body bgcolor=\"", X, "\">"].
pre(X)  -> ["<pre>",X,"</pre>"].
b(X)    -> ["<b>",X,"</b>"].
str(X)  -> io_lib:format("~p", [X]).
h1(X)   -> ["<h1>",X,"</h1>"].
h3(X)   -> ["<h3>",X,"</h3>"].

port_name(Port) when integer(Port) ->
    list_to_atom("portApplication" ++ integer_to_list(Port)).

classify(FileName) ->
    case filename:extension(FileName) of
	".GIF" -> gif;
	".gif" -> gif;
	".JPG" -> jpg;
	".jpg" -> jpg;
	".jpeg" -> jpg;
	".JPEG" -> jpg;
	".HTML" -> html;
	".html" -> html;
	".HTM" -> html;
	".htm" -> html;
	".txt" -> text;
	".TXT" -> text;
	_ -> binary
    end.

content_length([{"content-length",Str}|T]) ->
    list_to_integer(Str);
content_length([_|T]) -> 
    content_length(T);
content_length([]) -> 
    0.

parse_header(Str) ->
    {ok, Fields} = regexp:split(Str, "\r\n"),
    PRequest = parse_request(hd(Fields)),
    %% Args = "KeyWord: Str" ..
    PArgs = map(fun isolate_arg/1, tl(Fields)),
    make_return_value({PRequest, PArgs}).

make_return_value({{Op,Vsn,{URI,Args}}, Env}) ->
    {Op, content_length(Env), Vsn, URI, Args, Env}.

parse_request(Str) ->
    {ok, Args} = regexp:split(Str, " "),
    case Args of
	["POST", URI, Vsn] ->
	    {post, parse_vsn(Vsn), parse_uri(URI)};
	["GET", URI, Vsn] ->
	    {get, parse_vsn(Vsn), parse_uri(URI)};
	_  -> 
	    oops
    end.

parse_vsn("HTTP/1.0") -> {1,0};
parse_vsn(X) -> X.

%% A typical URI looks
%% like
%% URI = "/a/b/c?password=aaa&invisible=A+hidden+value"

parse_uri(URI) ->
    case string:tokens(URI, "?") of
	[Root] ->
	    {Root, []};
	[Root, Args] ->
	    {Root, parse_uri_args(Args)}
    end.

parse_uri_args(Args) ->
    Args1 = string:tokens(Args, "&;"),
    map(fun(KeyVal) ->
	       case string:tokens(KeyVal, "=") of
		   [Key, Val] ->
		       {urlencoded2str(Key), urlencoded2str(Val)};
		   [Key] ->
		       {urlencoded2str(Key), ""};
		   _ ->
		       io:format("Invalid str:~p~n",[KeyVal]),
		       {"error", "error"}
	       end
       end, Args1).

urlencoded2str([$%,Hi,Lo|T]) -> [decode_hex(Hi, Lo)|urlencoded2str(T)];
urlencoded2str([$+|T])       -> [$ |urlencoded2str(T)];
urlencoded2str([H|T])        -> [H|urlencoded2str(T)];
urlencoded2str([])           -> [].

isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {httpd_util:to_lower(reverse(L)), T};
isolate_arg([H|T], L)     -> isolate_arg(T, [H|L]).

%% decode_hex %%

decode_hex(Hex1, Hex2) ->
    hex2dec(Hex1)*16 + hex2dec(Hex2).

hex2dec(X) when X >=$0, X =<$9 -> X-$0;
hex2dec($A) -> 10;
hex2dec($B) -> 11;
hex2dec($C) -> 12;
hex2dec($D) -> 13;
hex2dec($E) -> 14;
hex2dec($F) -> 15;
hex2dec($a) -> 10;
hex2dec($b) -> 11;
hex2dec($c) -> 12;
hex2dec($d) -> 13;
hex2dec($e) -> 14;
hex2dec($f) -> 15.
