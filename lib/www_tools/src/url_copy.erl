-module(url_copy).

%IA Joe Armstrong
%ID 970314
%IK [url,copy]
%IH Makes a deep copy of a HTTP page
%IT Copies a remote URL to the local file system. This resursivly copies
% all pictures in the origonal to the local file system. References in the
% origonal are renamed in a consistent manner.
% <p><b>url_copy:deep_copy(URL, Name, DestDir, Proxy, Timeout)</b>
% makes a copy of <b>URL</b>. The clocal name of the copy will be
% <b>Name.html</b>. And images referred to in URL will be renamed as
% <b>Name_N</b>. All the created files will be stored in the directory
% <b>DestDir</b>. Note the contents of DestDir is relocatable. The file
% DestDir/cache.dets can be safely deleted at any time when this program 
% is not active.
% <b>Proxy = {DomainName, IP} | noproxy</b>
% for example: <b>{"super.du.etx.ericsson.se", 888}</b>. <b>Timeout</b>
% is a timeout vaule to be used when querying the network.

-export([deep_copy/4, deep_copy/5]).

-import(lists, [reverse/1]).

%% deep_copy:rul(Url, Name, Dir)
%%   Fetches Url
%%      Renames as Dir/Name.html
%%   Fetches all pictures in Url
%%      Renames as Dir/Name_NN
%%   Changes all references to External images in URL to
%%   Local file references.

%% Note we should check that the main url
%% is terminated with a /
%%  -- not done yet
%% strangle multiple slashes in a name are counted as one slash
%% i.e. http://www.ericsson.se////test/foo/bar is interpreted as the same as 
%%      http://www.ericsson.se/test/foo/bar

deep_copy(URL, File, Dir, Proxy) ->
    %% 5 minute default
    deep_copy(URL, File, Dir, Proxy, 300000).

deep_copy(URL, File, Dir, Proxy, Timeout) ->
    %% First check that Dir 
    %% Is a directory and that Name
    %% Doesn't exist
    case misc:is_dir(Dir) of
	true ->
	    case misc:is_file(Dir ++ "/" ++ File) of
		true ->
		    {error, file_exists};
		false ->
		    Cache = Dir ++ "/cache.dets",
		    deep_copy1(URL, Dir, File, Cache, Proxy, Timeout)
	    end;
	false ->
	    {error, {no_such_directory, Dir}}
    end.
    
deep_copy1(URL, Dir, File, Cache, Proxy, Timeout) ->
    url:start_cache(Cache),
    case url:get(URL, Proxy, Timeout) of
	{ok, Bin} ->
	    deep_copy2(URL, Bin, Dir, File, Proxy, Timeout);
	{error, Why} ->
	    error
    end,
    url:stop_cache().

%% When this is called URL has been fetched
%% The contents is Bin
%% We want to store the file In Dir with name File
%% Proxy and Timeout tell where to get the images from

deep_copy2(URL, Bin, Dir, File, Proxy, Timeout) ->
    Toks = html_tokenise:bin2toks(Bin),
    {URLs, Images} = html_analyse:analyse(Toks),
    %% io:format("I must fetch:~p~n", [Images]),
    Got = fetch_images(Images, URL, Dir, File, 0, Proxy, Timeout, []),
    %% io:format("Got:~p~n", [Got]),
    Toks1 = add_images(Toks, Got, []),
    html_tokenise:toks2file(Toks1, Dir ++ "/" ++ File).

fetch_images([H|T], URL, Dir, File, N, Proxy, Timeout, Result) ->
    case url:get(resolve(URL, H), Proxy, Timeout) of
	{ok, Bin} ->
	    Name = File ++ "_" ++ integer_to_list(N),
	    FullName = Dir ++ "/" ++ Name,
	    %% io:format("Writing:~p~n", [FullName]),
	    file:write_file(FullName, Bin),
	    fetch_images(T, URL, Dir, File, N+1, Proxy, Timeout,
			 [{H,Name}|Result]);
	{error, _} ->
	    fetch_images(T, URL, Dir, File, N, Proxy, Timeout, 
			 [{H,"noname"}|Result])
    end;
fetch_images([], _, _, _, _, _, _, R) ->
    R.

add_images([{tagStart, "img", Args}|T], Dict, L) ->
    Args1 = replace_image("src", Args, Dict),
    add_images(T, Dict, [{tagStart, "img", Args1}|L]);
add_images([{tagStart, "body", Args}|T], Dict, L) ->
    Args1 = replace_image("background", Args, Dict),
    add_images(T, Dict, [{tagStart, "body", Args1}|L]);
add_images([H|T], Dict, L) ->
    add_images(T, Dict, [H|L]);
add_images([], _, L) ->
    reverse(L).

replace_image(Tag, [{Tag, Image}|T], Dict) ->
    %% io:format("Looking for:~s in ~p~n", [Image, Dict]),
    case lists:keysearch(Image, 1, Dict) of
	{value, {_, Image1}} ->
	    [{Tag, Image1}|T];
	_ ->
	    [{Tag, "could_not_get"}|T]
    end;
replace_image(Tag, [H|T],  G) ->
    [H|replace_image(Tag, T, G)];
replace_image(Tag, [], G) ->
    [].

resolve(URL, Rel) ->
    New = url_parse:resolve(URL, Rel),
    io:format("Resolve: ~s ~s~n"
              "  =>   : ~s ", [URL, Rel, New]),
    New.




    

