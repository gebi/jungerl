-module(html_tokenise_types).

%IA Joe Armstrong
%ID 970314
%IK [html,token]
%IH Tokenise an html file
%IT Tokenises an HTML file:
% <p><b>html_tokenise:file2toks(File) -> Toks tokenises a file
% <p><b>html_tokenise:bin2toks(Bin)   -> Toks tokenises a binary
% <p><b>html_tokenise:string2toks(String)   -> Toks. tokenises a string
% <p><b>html_tokenise:toks2file(Toks, File) -> ok write Toks to File.html


-export([file2toks/1, bin2toks/1, string2toks/1, toks2file/2]).

-import(lists, [foreach/2, reverse/1]).

-deftype token() = tagEnd{string()} 
|                  tagError{int()|string()} 
|                  tagStart{string()}
|                  tagStart{string(), [{string(), string()}]}
|                  raw{string()}.

-exportdeftype([token/0]).

-type file2toks(string()) -> [token()].

file2toks(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    bin2toks(Bin);
	{error, _} ->
	    exit(badfile)
    end.

-type binary(bin()) -> [token()].

bin2toks(Bin) -> string2toks(binary_to_list(Bin)).

-type toks2file([token()], string()) -> true.

toks2file(Toks, File) ->
    %% io:format("Dumping tokens to:~p~n", [File]),
    {ok, Out} = file:open(File ++ ".html", write),
    foreach(fun(X) -> dump_token(Out, X) end, Toks),
    file:close(Out),
    true.

-type string2toks(string()) -> [token()].

string2toks(Str) -> tokenise(Str, []).

-type tokenise(string(), [token()]) -> [token()].

tokenise([$<|T], L) ->
    case collect_names(T) of
	{[$/|Rest], [], [$>|T1]} ->
	    tokenise(T1, [{tagEnd, to_lower(Rest)}|L]);
	{[$/|Rest], _, [$>|T1]} ->
	    tokenise(T1, [{tagError, to_lower(Rest)}|L]);
	{Tag, [], [$>|T1]} ->
	    tokenise(T1, [{tagStart, to_lower(Tag)}|L]);
	{Tag, Args, [$>|T1]} ->
	    tokenise(T1, [{tagStart, to_lower(Tag), Args}|L]);
	{_, _, [$>|T1]} ->
	    tokenise(T1, [{tagError, 3}|L]);
	{_, _, T1} ->
	    tokenise(T1, [{tagError, 4}|L])
    end;
tokenise([H|T], L) ->
    {Raw, T1} = collect_raw([H|T]),
    tokenise(T1, [{raw, Raw}|L]);
tokenise([], L) ->
    reverse(L).

-type to_lower(string()) -> string().

to_lower([H|T]) when $A =< H, H =< $Z -> [H - $A + $a|to_lower(T)];
to_lower([H|T])                       -> [H|to_lower(T)];
to_lower([])                          -> [].

%% collect names is called after we hit <

-type collect_names(string()) -> {string(), [{string(), string()}], string()}.

collect_names(Str) ->
    Str1 = skip_white(Str),
    case Str1 of
	[$>|T] ->
	    {[], [], [$>|T]};
	[] ->
	    {[], [], []};
	_ ->
	    {Name, Str2} = collect_name(Str1),
	    {Args, Str3} = collect_args(Str2),
	    {Name, Args, Str3}
    end.

%% Args = (name = arg)*

-type collect_args(string()) -> {[{string(), string()}], string()}.

collect_args(Str) ->
    %% io:format("here collect args:~s\n", [Str]),
    Str1 = skip_white(Str),
    case Str1 of
	[$>|T] ->
	    {[], [$>|T]};
	[] ->
	    {[], []};
	_ ->
	    {Name, Str2} = collect_name(Str1),
	    Str3 = skip_white(Str2),
	    case Str3 of
		[$=|Str4] ->
		    Str5 = skip_white(Str4),
		    {Val, Str6} = collect_name(Str5),
		    {ArgT, Str7} = collect_args(Str6),
		    {[{to_lower(Name),Val}|ArgT], Str7};
		_ ->
		    Str4 = skip_to($>, Str3),
		    {[], Str4}
	    end
    end.

-type skip_to(char(), string()) -> string().

skip_to(H, [H|T]) -> [H|T];
skip_to(H, [])    -> [H];
skip_to(H, [_|T]) -> skip_to(H, T).

-type skip_white(string()) -> string().

skip_white([$ |T])  -> skip_white(T);
skip_white([$\n|T]) -> skip_white(T);
skip_white([13|T])  -> skip_white(T);
skip_white(Str)     -> Str.

-type collect_name(string()) -> {string(), string()}.
-type collect_name(string(), string()) -> {string(), string()}.
-type collect_quoted_name(string(), string()) -> {string(), string()}.

collect_name([$"|T]) -> collect_quoted_name(T, []);
collect_name(Str)    -> collect_name(Str, []).

collect_name([$ |T], L)  -> {reverse(L), T};
collect_name([$>|T], L)  -> {reverse(L), [$>|T]};
collect_name([$=|T], L)  -> {reverse(L), [$=|T]};
collect_name([$\n|T], L) -> {reverse(L), T};
collect_name([13|T], L)  -> {reverse(L), T};
collect_name([H|T], L)   -> collect_name(T, [H|L]);
collect_name([], L)      -> {reverse(L), []}.

collect_quoted_name([$\\,$"|T], L) -> collect_quoted_name(T, [$"|L]); 
collect_quoted_name([$"|T], L)     -> {reverse(L), T};
collect_quoted_name([$\n|T], L)    -> collect_quoted_name(T, [$\n|L]);
collect_quoted_name([13|T], L)     -> collect_quoted_name(T, [$\n|L]);
collect_quoted_name([H|T], L)      -> collect_quoted_name(T, [H|L]);
collect_quoted_name([], L)         -> {reverse(L), []}.

%% collect_raw(Str) -> {Raw', Str'}

-type collect_raw(string()) -> {string(), string()}.

collect_raw(Str) -> collect_raw(Str, []).

-type collect_raw(string(), string()) -> {string(), string()}.

collect_raw([$\\,$<|T], L) -> collect_raw(T, [$<|L]);
collect_raw([$\n|T], L)    -> collect_raw(T, [$\n|L]);
collect_raw([13|T], L)     -> collect_raw(T, [$\n|L]);
collect_raw([$<|T], L)     -> {reverse(L), [$<|T]};
collect_raw([$&|T], L) ->
    {NT, Name} = collect_amp(T, []),
    case translate_amp(Name) of
	error -> collect_raw(T, [$&|L]);
	Code ->  collect_raw(NT, [Code | L])
    end;
collect_raw([H|T], L) ->
    collect_raw(T, [H|L]);
collect_raw([], L) ->
     {reverse(L), []}.

-type collect_amp(string(), string()) -> {string(), string()}.

collect_amp([$ | T], L)   -> {T, reverse(L)};
collect_amp([$\n | T], L) -> {T, reverse(L)};
collect_amp([$\r | T], L) -> {T, reverse(L)};
collect_amp([$; | T], L)  -> {T, reverse(L)};
collect_amp([H | T], L)   -> collect_amp(T, [H|L]);
collect_amp([], L)        -> {[], reverse(L)}.

-type translate_amp(string()) -> int() | error.

translate_amp([$# | Ds]) ->
    amp_digits(Ds, 0);
translate_amp(Name) ->
    case Name of
	"lt" -> $<;
	"gt" -> $>;
	"amp" -> $&;
	"quot" -> $";
	"nbsp" -> 160;
	"iexcl" -> 161;
	"cent" -> 162;
	"pound" -> 163;
	"curren" -> 164;
	"yen" -> 165;
	"brvbar" -> 166;
	"sect" -> 167;
	"uml" -> 168;
	"copy" -> 169;
	"ordf" -> 170;
	"laquo" -> 171;
	"not" -> 172;
	"shy" -> 173;
	"reg" -> 174;
	"macr" -> 175;
	"deg" -> 176;
	"plusmn" -> 177;
	"sup2" -> 178;
	"sup3" -> 179;
	"acute" -> 180;
	"micro" -> 181;
	"para" -> 182;
	"middot" -> 183;
	"cedil" -> 184;
	"sup1" -> 185;
	"ordm" -> 186;
	"raquo" -> 187;
	"frac14" -> 188;
	"frac12" -> 189;
	"frac34" -> 190;
	"iquest" -> 191;
	"Agrave" -> 192;
	"Aacute" -> 193;
	"Acirc" -> 194;
	"Atilde" -> 195;
	"Auml" -> 196;
	"Aring" -> 197;
	"AElig" -> 198;
	"Ccedil" -> 199;
	"Egrave" -> 200;
	"Eacute" -> 201;
	"Ecirc" -> 202;
	"Euml" -> 203;
	"Igrave" -> 204;
	"Iacute" -> 205;
	"Icirc" -> 206;
	"Iuml" -> 207;
	"ETH" -> 208;
	"Ntilde" -> 209;
	"Ograve" -> 210;
	"Oacute" -> 211;
	"Ocirc"-> 212;
	"Otilde" -> 213;
	"Ouml" -> 214;
	"times" -> 215;
	"Oslash" -> 216;
	"Ugrave" -> 217;
	"Uacute" -> 218;
	"Ucirc" -> 219;
	"Uuml" -> 220;
	"Yacute" -> 221;
	"THORN" -> 222;
	"szlig" -> 223;
	"agrave" -> 224;
	"aacute" -> 225;
	"acirc" -> 226;
	"atilde" -> 227;
	"auml" -> 228;
	"aring" -> 229;
	"aelig" -> 230;
	"ccedil" -> 231;
	"egrave" -> 232;
	"eacute" -> 233;
	"ecirc" -> 234;
	"euml" -> 235;
	"igrave" -> 236;
	"iacute" -> 237;
	"icirc" -> 238;
	"iuml" -> 239;
	"eth" -> 240;
	"ntilde" -> 241;
	"ograve" -> 242;
	"oacute" -> 243;
	"ocirc" -> 244;
	"otilde" -> 245;
	"ouml" -> 246;
	"divide" -> 247;
	"oslash" -> 248;
	"ugrave" -> 249;
	"uacute" -> 250;
	"ucirc" -> 251;
	"uuml" -> 252;
	"yacute" -> 253;
	"thorn" -> 254;
	"yuml" -> 255;
	_ -> error
    end.

-type amp_digits(string(), int()) -> int() | error.

amp_digits([X | Xs], N) when X >= $0, X =< $9 ->
    amp_digits(Xs, N*10 + (X-$0));
amp_digits([], N) ->
    if
	N >= 0, N =< 8 -> error;
	N >= 127, N =< 159 -> error;
	N > 255 -> error;
	true -> N
    end.


dump_token(O, {raw, R}) -> 
    io:format(O, "~s", [R]);
dump_token(O, {tagStart, Tag, Args}) ->
    io:format(O, "<~s", [Tag]),
    foreach(fun({Key,Val}) ->
		   io:format(O, ' ~s="~s"', [Key,Val])
	   end, Args),
    io:format(O, ">", []);
dump_token(O, {tagStart, Tag}) ->
    io:format(O, "<~s>", [Tag]);
dump_token(O, {tagEnd, Tag}) ->
    io:format(O, "</~s>", [Tag]);
dump_token(O, Other) ->
    io:format("dump_token ????~p~n", [Other]).

