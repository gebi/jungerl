%%% File    : erl_img.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Image processing stuff
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(erl_img).

-include_lib("kernel/include/file.hrl").

-include("erl_img.hrl").

-export([magic_info/1]).
-export([mime_type/1]).
-export([dir_info/1]).
-export([read_file_info/1]).
-export([read/2, write/2]).
-export([load/1, save/1, save/2, to_binary/1]).
-export([attribute/2, attribute/3, set_attribute/3]).
-export([extensions/1]).
-export([write_info/3]).
-export([hex32/1, hex16/1, hex8/1]).

hex32(X) ->
    hex8(X bsr 24) ++ hex8(X bsr 16) ++ hex8(X bsr 8) ++ hex8(X).

hex16(X) ->
    hex8(X bsr 8) ++ hex8(X).

%% convert a hex byte into two ascii letters
hex8(X) ->
    [nib((X bsr 4) band 16#f),nib(X band 16#f)].

nib(N) when N =< 9 -> N+$0;
nib(N) -> (N-10)+$A.

%% Read magic info check MAGIC type and width and height (depth)
%% of image

%% Check a header of least 64 bytes
magic([Type|Ts], Bin) ->
    case apply(Type, magic, [Bin]) of
	true -> {true, Type };
	false -> magic(Ts, Bin)
    end;
magic([], _Bin) -> 
    false.


%% Read file mtime information
file_info(File, _IMG) ->
    case file:read_file_info(File) of
	{ok, Info} when Info#file_info.type == regular,
			Info#file_info.size > 0 ->
	    {ok, {Info#file_info.mtime,Info#file_info.size}};
	{ok, _Other} ->
	    {error, bad_file};
	Error ->
	    Error
    end.



read_magic_info(Fd) ->
    file:position(Fd, 0),
    case file:read(Fd, 64) of
	{ok, Bin} ->
	    case magic(?IMAGE_TYPES, Bin) of
		{true, Type} ->
		    read_info(Type, Fd);
		false ->
		    {error, not_supported}
	    end;
	Error -> 
	    Error
    end.

    
magic_info(File) ->
    case file:open(File,[raw,binary,read]) of
	{ok,Fd} ->
	    Res = read_magic_info(Fd),
	    file:close(Fd),
	    case Res of 
		{ok,IMG} ->
		    {ok,IMG#erl_image { filename = File,
					name = filename:basename(File) }};
		Error ->
		    Error
	    end;
	Error -> Error
    end.



mime_type(IMG) ->
    apply(IMG#erl_image.type, mime_type, []).

extensions(IMG) ->
    apply(IMG#erl_image.type, extensions, []).


read_file_info(File) ->
    case file_info(File,  #erl_image { }) of
	{ok, {MTime,Size}} -> 
	    case magic_info(File) of
		{ok,IMG} ->
		    {ok,IMG#erl_image { mtime = MTime,
					size = Size}};
		Error ->
		    Error
	    end;
	Error  -> Error
    end.


load(File) ->
    case file:open(File, [raw, binary, read]) of
	{ok,Fd} ->
	    Res = case read_magic_info(Fd) of
		      {ok, IMG} -> 
			  read(Fd, IMG#erl_image { filename = File });
		      Error -> 
			  Error
		  end,
	    file:close(Fd),
	    Res;
	Error -> Error
    end.

save(IMG) ->
    save(IMG#erl_image.filename, IMG).

save(File, IMG) ->
    case file:open(File, [raw, binary, write]) of
	{ok,Fd} ->
	    Res = write(Fd, IMG),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.

to_binary(IMG) ->
    case file:open(<<>>, [ram, binary, write]) of
	{ok,Fd} ->
	    ok = write(Fd, IMG),
	    case ram_file:get_file_close(Fd) of
		{ok, Data} ->
		    {ok,Data};
		Error -> 
		    Error
	    end;
	Error ->
	    Error
    end.

    
		    

read_info(Type, Fd) ->
    file:position(Fd, 0),
    apply(Type, read_info, [Fd]).

write_info(Type, Fd, IMG) ->
    file:position(Fd, 0),
    apply(Type, write_info, [Fd,IMG]).
    
read(Fd,IMG) ->
    file:position(Fd, 0),
    apply(IMG#erl_image.type, read, [Fd,IMG]).

write(Fd,IMG) ->
    file:position(Fd, 0),
    apply(IMG#erl_image.type, write, [Fd,IMG]).



attribute(IMG, Key) ->
    {value, {_, Value}} = lists:keysearch(Key, 1, IMG#erl_image.attributes),
    Value.

attribute(IMG, Key, Default) ->
    case lists:keysearch(Key, 1, IMG#erl_image.attributes) of
	{value, {_, Value}} -> Value;
	false -> Default
    end.

set_attribute(IMG, Key, Value) ->	     
    As = IMG#erl_image.attributes, 
    As1 = case lists:keysearch(Key, 1, As) of
	      false -> [{Key,Value}|As];
	      {value,_} ->
		  lists:keyreplace(Key, 1, As, {Key,Value})
	  end,
    IMG#erl_image { attributes = As1 }.
	    

dir_info(Dir) ->
    case file:list_dir(Dir) of
	{ok, Listing} ->
	    dir_list(Listing,Dir);
	Error ->
	    Error
    end.

dir_list([File|Fs], Dir) ->
    case read_file_info(filename:join(Dir, File)) of
	{ok,IMG} ->
	    [IMG|dir_list(Fs, Dir)];
	_Error ->
	    dir_list(Fs, Dir)
    end;
dir_list([], _Dir) ->
    [].
