%%% File    : zlib.erl
%%% Author  : Tony Rogvall <tony@bit.hemma.se>
%%% Description : ZLib interface
%%% Created : 27 Apr 2003 by Tony Rogvall <tony@bit.hemma.se>

-module(zlib).

-compile(export_all).

%% flush argument encoding
-define(Z_NO_FLUSH,      0).
-define(Z_PARTIAL_FLUSH, 1). %% use Z_SYNC_FLUSH instead
-define(Z_SYNC_FLUSH,    2).
-define(Z_FULL_FLUSH,    3).
-define(Z_FINISH,        4).

%% compression level
-define(Z_NO_COMPRESSION,         0).
-define(Z_BEST_SPEED,             1).
-define(Z_BEST_COMPRESSION,       9).
-define(Z_DEFAULT_COMPRESSION,  (-1)).

%% compresssion strategy
-define(Z_FILTERED,            1).
-define(Z_HUFFMAN_ONLY,        2).
-define(Z_DEFAULT_STRATEGY,    0).


%% deflate compression method
-define(Z_DEFLATED,  8).

-define(Z_NULL, 0).

-define(MAX_WBITS, 15).

%% gzip defs (rfc 1952)

-define(ID1, 16#1f).
-define(ID2, 16#8b).

-define(FTEXT,     16#01).
-define(FHCRC,     16#02).
-define(FEXTRA,    16#04).
-define(FNAME,     16#08).
-define(FCOMMENT,  16#10).
-define(RESERVED,  16#E0).

-define(OS_MDDOS,   0).
-define(OS_AMIGA,   1).
-define(OS_OPENVMS, 2).
-define(OS_UNIX,    3).
-define(OS_VMCMS,   4).
-define(OS_ATARI,   5).
-define(OS_OS2,     6).
-define(OS_MAC,     7).
-define(OS_ZSYS,    8).
-define(OS_CPM,     9).
-define(OS_TOP20,  10).
-define(OS_NTFS,   11).
-define(OS_QDOS,   12).
-define(OS_ACORN,  13).
-define(OS_UNKNOWN,255).

-record(gzip, 
	{
	  method = ?Z_DEFLATED, 
	  flags = 0,         %% :8
	  mtime = 0,         %% :32/little
	  xflags = 0,        %% :8
	  ostype = ?OS_UNIX, %% :8 = unix
	  extra,
	  name,
	  comment,
	  crc
	 }).
	  

-define(DEFLATE_INIT,    1).
-define(DEFLATE_INIT2,   2).
-define(DEFLATE_SETDICT, 3).
-define(DEFLATE_RESET,   4).
-define(DEFLATE_END,     5).
-define(DEFLATE_PARAMS,  6).
-define(DEFLATE,         7).

-define(INFLATE_INIT,    8).
-define(INFLATE_INIT2,   9).
-define(INFLATE_SETDICT, 10).
-define(INFLATE_SYNC,    11).
-define(INFLATE_RESET,   12).
-define(INFLATE_END,     13).
-define(INFLATE,         14).

-define(CRC32_0,         15).
-define(CRC32_1,         16).
-define(CRC32_2,         17).

-define(SET_BUFSZ,       18).
-define(GET_BUFSZ,       19).
-define(GET_QSIZE,       20).


load() ->
    erl_ddll:load_driver(code:priv_dir(zlib), "zlib_drv").    

%% open a z_stream
open() ->
    ok = load(),
    open_port({spawn, zlib_drv}, [binary]).

%% close and release z_stream
close(Z) ->
    port_close(Z).

deflateInit(Z, Level) ->
    call(Z, ?DEFLATE_INIT, <<(arg_level(Level)):32>>).

deflateInit(Z, Level, Method, WindowBits, MemLevel, Strategy) ->
    call(Z, ?DEFLATE_INIT2, <<(arg_level(Level)):32, 
			     (arg_method(Method)):32,
			     WindowBits:32, 
			     MemLevel:32,
			     (arg_strategy(Strategy)):32>>).

deflateSetDictionary(Z, Dictionary) ->
    call(Z, ?DEFLATE_SETDICT, Dictionary).

deflateReset(Z) ->
    call(Z, ?DEFLATE_RESET, []).

deflateParams(Z, Level, Strategy) ->
    call(Z, ?DEFLATE_PARAMS, <<(arg_level(Level)):32, 
			      (arg_strategy(Strategy)):32>>).

deflate(Z, Data, Flush) ->
    case catch port_command(Z, Data) of
	true ->
	    Res = call(Z, ?DEFLATE, <<(arg_flush(Flush)):32>>),
	    collect(Z);
	Error -> 
	    flush(Z), 
	    Error
    end.
	    

deflateEnd(Z) ->
    call(Z, ?DEFLATE_END, []).    

inflateInit(Z) ->
    call(Z, ?INFLATE_INIT, []).    

inflateInit(Z, WindowBits) -> 
    call(Z, ?INFLATE_INIT2, <<WindowBits:32>>).

inflateSetDictionary(Z, Dictionary) -> 
    call(Z, ?INFLATE_SETDICT, Dictionary).

inflateSync(Z) -> 
    call(Z, ?INFLATE_SYNC, []).

inflateReset(Z) -> 
    call(Z, ?INFLATE_RESET, []).    

inflate(Z, Data, Flush) ->
    case catch port_command(Z, Data) of
	true -> 
	    Res = call(Z, ?INFLATE, <<(arg_flush(Flush)):32>>),
	    collect(Z);
	Error -> flush(Z), Error
    end.

inflateEnd(Z) ->
    call(Z, ?INFLATE_END, []).

setBufsz(Z, Size) ->
    call(Z, ?SET_BUFSZ, <<Size:32>>).

getBufsz(Z) ->
    call(Z, ?GET_BUFSZ, []).


crc32(Z) ->
    call(Z, ?CRC32_0, []).

crc32(Z, Binary) ->
    call(Z, ?CRC32_1, Binary).

crc32(Z, CRC, Binary) ->
    call(Z, ?CRC32_2, <<CRC:32, Binary/binary>>).
    
getQSize(Z) ->
    call(Z, ?GET_QSIZE, []).    

%% compress/uncompress zlib with header
compress(Binary) ->
    Z = open(),
    ok = deflateInit(Z, default),
    {ok,Bs1} = deflate(Z, Binary, none),
    {ok,Bs2} = deflate(Z, <<>>, finish),
    ok = deflateEnd(Z),
    close(Z),
    {ok, list_to_binary(Bs1++Bs2)}.

uncompress(Binary) ->
    Z = open(),
    ok = inflateInit(Z),
    {ok,Bs1} = inflate(Z, Binary, none),
    {ok,Bs2} = inflate(Z, <<>>, finish),
    ok = inflateEnd(Z),
    close(Z),
    {ok, list_to_binary(Bs1++Bs2)}.

%% unzip/zip zlib without header (zip members)
zip(Binary) ->
    Z = open(),
    ok = deflateInit(Z, default, deflated, -?MAX_WBITS, 8, default),
    {ok,Bs1} = deflate(Z, Binary, none),
    {ok,Bs2} = deflate(Z, <<>>, finish),
    ok = deflateEnd(Z),
    close(Z),
    {ok, list_to_binary(Bs1++Bs2)}.

unzip(Binary) ->
    Z = open(),
    ok = inflateInit(Z, -?MAX_WBITS),
    {ok,Bs1} = inflate(Z, Binary, none),
    {ok,Bs2} = inflate(Z, <<>>, finish),
    ok = inflateEnd(Z),
    close(Z),
    {ok, list_to_binary(Bs1++Bs2)}.


%% gunzip/gzip versions
gzip_file(Src) ->
    case file:read_file(Src) of
	{ok,Bin} ->
	    gzip(Bin);
	Error ->
	    Error
    end.
    
gzip(Data) ->
    Bin0 = if list(Data) -> 
		   list_to_binary(Data);
	      binary(Data) ->
		   Data
	   end,
    Z = open(),
    ok = deflateInit(Z, default, deflated, -?MAX_WBITS, 8, default),
    {ok,Bs1} = deflate(Z, Bin0, none),
    {ok,Bs2} = deflate(Z, <<>>, finish),
    ok = deflateEnd(Z),
    {ok,Crc} = crc32(Z, Bin0),
    close(Z),
    %% add header and crc
    Head = write_gzip_header(#gzip {}),
    Tail = <<Crc:32/little, (size(Bin0)):32/little>>,
    {ok, list_to_binary([Head,Bs1,Bs2,Tail])}.


gunzip_file(Src) ->
    case file:read_file(Src) of
	{ok, Bin} ->
	    gunzip(Bin);
	Error -> 
	    Error
    end.

gunzip(Bin0 = <<?ID1, ?ID2, Method, Flags, MTime:32, 
	       XFlags, OsType, _/binary>>) ->
    Gz0 = #gzip { method = Method,
		  flags = Flags, 
		  mtime = MTime, 
		  xflags = XFlags, 
		  ostype = OsType },
    {Gz1,Bin1} = read_gzip_header(Flags, Bin0, 10, Gz0),
    %% io:format("gunzip header = ~p\n", [Gz1]),
    Z = open(),
    ok = inflateInit(Z, -?MAX_WBITS),
    {ok,Bs1} = inflate(Z, Bin1, none),
    {ok,Bs2} = inflate(Z, <<>>, finish),
    {ok,Crc} = crc32(Z),
    {ok,Remain} = getQSize(Z),
    ok = inflateEnd(Z),
    close(Z),
    Offset = size(Bin1) - Remain,
    Bin2 = list_to_binary(Bs1++Bs2),
    <<_:Offset/binary, Crc32:32/little, Length:32/little, _/binary>> = Bin1,
    if Crc32 =/= Crc ->
	    {error, bad_crc};
       Length =/= size(Bin2) ->
	    {error, bad_length};
       true ->
	    {ok, Bin2}
    end.

write_gzip_header(Gz) ->
    {D1,F1} = 
	if Gz#gzip.extra == undefined ->
		{<<>>, 0};
	   true ->
		Extra = list_to_binary([Gz#gzip.extra]),
		{ <<(size(Extra)):16/little, Extra/binary>>, ?FEXTRA}
	end,
    {D2,F2} =
	if Gz#gzip.name == undefined ->
		{<<>>, 0};
	   true ->
		Name = list_to_binary([Gz#gzip.name, 0]),
		{ Name, ?FNAME }
	end,
    {D3,F3} =
	if Gz#gzip.comment == undefined ->
		{<<>>, 0};
	   true ->
		Comment = list_to_binary([Gz#gzip.comment, 0]),
		{ Comment, ?FCOMMENT }
	end,
    {D4,F4} =
	if Gz#gzip.crc == undefined ->
		{<<>>, 0};
	   true ->
		{ <<(Gz#gzip.crc):16/little >>, ?FHCRC }
	end,
    << ?ID1, ?ID2, 
     (Gz#gzip.method):8,
     (F1 bor F2 bor F3 bor F4):8,
     (Gz#gzip.mtime):32/little,
     (Gz#gzip.xflags):8,
     (Gz#gzip.ostype):8,
     D1/binary, D2/binary, D3/binary, D4/binary>>.


%% read the variable part of the gzip header
read_gzip_header(Flags, Binary, Offs0, Gz0) ->
    {Gz1,Offs1} =
	if (Flags band ?FEXTRA) =/= 0 ->
		<<_:Offs0/binary, Len:16/little, _/binary>> = Binary,
		Offs00 = Offs0+2,
		<<_:Offs00/binary, Extra:Len/binary,_/binary>> = Binary,
		{Gz0#gzip { extra = Extra }, Offs0 + 2 + Len};
	   true -> 
		{Gz0,Offs0}
	end,
     {Gz2,Offs2} = 
	if (Flags band ?FNAME) =/= 0 ->
		Name = cname(Binary, Offs1),
		{Gz1#gzip { name = Name}, Offs1 + length(Name)+1 };
	   true ->
		{Gz1, Offs1}
	end,
    {Gz3, Offs3} = 
	if (Flags band ?FCOMMENT) =/= 0 ->
		Comment = cname(Binary, Offs2),
		{Gz2#gzip { comment = Comment}, Offs2 + length(Comment)+1};
	   true ->
		{Gz2, Offs2}
	end,
    {Gz4, Offs4} = 
	if (Flags band ?FHCRC) =/= 0 ->
		<<_:Offs3, Crc:16/little, _/binary>> = Binary,
		{Gz3#gzip { crc = Crc }, Offs3+2};
	   true ->
		{Gz3, Offs3}
	end,
    <<_:Offs4/binary, Body/binary>> = Binary,
    {Gz4, Body}.


cname(Binary, Offs) ->
    case Binary of
	<<_:Offs/binary, C, _/binary>> ->
	    if C == 0 -> [];
	       true -> [C|cname(Binary, Offs+1)]
	    end;
	<<_:Offs/binary>> ->
	    []
    end.    
    
	    
collect(Z) -> 
    collect(Z,[]).

collect(Z,Acc) ->
    receive 
	{Z, {data, Bin}} ->
	    collect(Z,[Bin|Acc])
    after 0 ->
	    {ok,lists:reverse(Acc)}
    end.

flush(Z) ->
    receive
	{Z, {data,Bin}} ->
	    flush(Z)
    after 0 ->
	    ok
    end.

    
arg_flush(none)    -> ?Z_NO_FLUSH;
arg_flush(partial) -> ?Z_PARTIAL_FLUSH;
arg_flush(sync)    -> ?Z_SYNC_FLUSH;
arg_flush(full)    -> ?Z_FULL_FLUSH;
arg_flush(finish)  -> ?Z_FINISH.

arg_level(none)             -> ?Z_NO_COMPRESSION;
arg_level(best_speed)       -> ?Z_BEST_SPEED;
arg_level(best_compression) -> ?Z_BEST_COMPRESSION;
arg_level(default)          -> ?Z_DEFAULT_COMPRESSION;
arg_level(Level) when Level >= 0, Level =< 9  -> Level.

arg_strategy(filtered) -> ?Z_FILTERED;
arg_strategy(huffman_only) -> ?Z_HUFFMAN_ONLY;
arg_strategy(default) ->      ?Z_DEFAULT_STRATEGY.

arg_method(deflated) -> ?Z_DEFLATED.

    
    

call(Z, Cmd, Arg) ->
    case port_control(Z, Cmd, Arg) of
	[0|Res] -> list_to_atom(Res);
	[1|Res] -> {error, list_to_atom(Res)};
	[2,A,B,C,D] -> {ok, (A bsl 24)+(B bsl 16)+(C bsl 8)+D}
    end.
