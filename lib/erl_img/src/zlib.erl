%%% File    : zlib.erl
%%% Author  : Tony Rogvall <tony@bit.hemma.se>
%%% Description : small zlib wrapper interface
%%% Created :  8 Apr 2003 by Tony Rogvall <tony@bit.hemma.se>

-module(zlib).

-export([deflate/1, inflate/1]).

-define(MAGIC1, 16#1f).
-define(MAGIC2, 16#8b).
-define(Z_DEFLATED, 8).

-define(GZ_HEADER(Flags,Time,XFlags,OsCode), 
	?MAGIC1, ?MAGIC2, 
	?Z_DEFLATED,  
	(Flags):8, %% flags
	(Time):32, %% time
	(XFlags):8,          %% xflags
	(OsCode):8 ).

-define(ZLIB_HEADER, 120, 156).



deflate(Bin) when binary(Bin)  ->
    case file:open(Bin, [ram, binary, read, write]) of
	{ok, Fd} ->
	    {ok,CompressedSize} = ram_file:compress(Fd),
	    case ram_file:get_file_close(Fd) of
		{ok, <<?GZ_HEADER(_, _, _, _), Compressed/binary>>} ->
		    {ok, <<?ZLIB_HEADER, Compressed/binary>>};
		{ok, Compressed} ->
		    {error, {bad_gzip_header,Compressed}};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.


inflate(Compressed) when binary(Compressed) ->
    case Compressed of
	<<?ZLIB_HEADER, Bin/binary>> ->
	    case file:open(<<?GZ_HEADER(0,0,0,3), Bin/binary>>,
			   [ram, binary, read, write]) of
		{ok, Fd} ->
		    case ram_file:uncompress(Fd) of
			{ok,Size} ->
			    ram_file:get_file_close(Fd);
			Error -> Error
		    end;
		Error ->
		    Error
	    end;
	_ ->
	    {error, bad_zlib_header}
    end.
    
    


