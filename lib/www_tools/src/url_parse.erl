-module(url_parse).

%IA Joe Armstrong
%ID 970314
%IK [url,parse]
%IH Parse a url. Resolve a relative url.
%IT Parses a URL. Given a URL and a partial (or relative) URL 
% works out the absolute URL.

%% -compile(export_all).

-export([parse/1, resolve/2]).

-import(lists, [reverse/1, member/2]).

%%----------------------------------------------------------------------
%% parse(URL) -> {http, Site, Port, File} | 
%%               {file, File}             | {error,Why}

parse([$h,$t,$t,$p,$:,$/,$/|T]) ->  parse(http, T);
parse([$f,$t,$p,$:,$/,$/|T])    ->  {error, no_ftp};
parse([$f,$i,$l,$e,$:,$/,$/|F]) ->  {file, F};
parse(X)                        ->  {error, parse_url}.

parse(Tag, X) ->
    case string:chr(X, $:) of
        0 ->
            %% no colon
	    Port = 80,
	    case string:chr(X,$/) of
		0 ->
		    {Tag, X, Port, ""};
		N ->
		    Site = string:substr(X,1,N-1),
		    File = string:substr(X, N, length(X)),
		    {Tag, Site, Port, File}
	    end;
	N1 ->
	    case string:chr(X,$/) of
		0 ->
		    error;
		N2 ->
		    PortStr = string:substr(X,N1+1, N2-N1-1),
		    case catch list_to_integer(PortStr) of
			{'EXIT', _} ->
			    error;
			Port ->
			    Site = string:substr(X,1,N1-1),
			    File = string:substr(X, N2+1, length(X)-N2),
			    {Tag, Site, Port, File}
		    end
	    end
    end.

%% heurstics:
%%    If tail of file has a "." in it

resolve(Root, [$/|T]) ->
    %% The relative bit is absolute
    %% the easy case
    case parse(Root) of
	{http, Site, Port, File} ->
	    "http://" ++ Site ++ port_str(Port) ++  [$/|T];
	{file, _} ->
	    "file://" ++ [$/|T];
	Other ->
	    Root ++ [$/|T]
    end;
resolve(Root, Rel) ->
    %% The Rel bit is relative so we need to parse the root
    case parse(Root) of
	{http, Site, Port, File} ->
	    "http://" ++  Site ++ port_str(Port) 
                      ++ rootDir(File) ++ "/" ++ Rel;
	{file, File} ->
	    "file://" ++  rootDir(File) ++ "/" ++ Rel;
	Other ->
	    Root ++ Rel
    end.

port_str(80) -> "";
port_str(N)  -> ":" ++ integer_to_list(N).

%%  /a/b/c/d.f  => "/a/b/c"
%%  a/b/c.d     => "a/b"
%%  a.b         => ""
%%  /a.b        => "/"
%%  /a/b/c      => "/a/b/c"
%%  a           => "a"

rootDir(Name) -> 
    case member($/, Name) of
	true ->
	    {Dir, File} = split_dir(Name),
	    case member($., File) of
		true ->
		    Dir;
		false ->
		    Name
	    end;
	false ->
	    case member($., Name) of
		true ->
		    "";
		false ->
		    Name
	    end
    end.

%% this seems to be a bug

%% 39> string:tokens("/a/b", "/").
%% ["a","b"]
%% 40> string:tokens("a/b", "/"). 

%% C is known to be in the string

split(Str, C) -> split(Str, C, []).

split([C|T], C, L) -> {reverse(L), T};
split([H|T], C, L) -> split(T, C, [H|L]).

split_dir(Dir) ->
    {A, B} = split(reverse(Dir), $/),
    {reverse(B), reverse(A)}.

%% BaseName                 Rel                  
%%
%%http://www.ericsson.se  test/images/b__bf4.gif  
%%
%%                        http://www.ericsson.se/test/images/b__bf4.gif   
%%                        /pictures/triangle.gif
%%                        http://www.ericsson.se/pictures/triangle.gif
%%
%%http://www.ericsson.se/  test/images/b__bf4.gif  
%%                         http://www.ericsson.se/test/images/b__bf4.gif   
%%                         /pictures/triangle.gif
%%                         http://www.ericsson.se/pictures/triangle.gif
%%
%%Root: http://www.viasat.se/tvkan.html
%%Rel:  viasatv/bas.gif
%%Abs:  http://www.viasat.se/viasat/bas.gif
%%
%%Root: http://www.bla.blags/a/b/c.ghi
%%Rel:  x/y/z.g
%%Abs:  http://www.bla.blags/a/b/x/y/z.g
%%
%%Root: http://www.bla.blags/a/b/c
%%Rel:  x/y/z.g 
%%Abs:  http://www.bla.blags/a/b/x/y/z.g
%%
%%
