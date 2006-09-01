%%% File    : img_tif.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : TIFF image format
%%% Created :  6 Mar 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(image_tiff).

-include("erl_img.hrl").
-include("tiff.hrl").

-include("api.hrl").
-include("dbg.hrl").


-export([scan_ifd/6, scan_ifd_bin/6]).
-export([decode_tag/1]).

-export([scan_fd/3, scan_file/3, scan_binary/3]).
-export([dump_file/1, dump_binary/1]).

-import(lists, [map/2, reverse/1]).

-define(II, 16#4949).  %% little-endian
-define(MM, 16#4D4D).  %% big-endian
-define(MAGIC, 42).

magic(<<?II:16,?MAGIC:16/little,_Offset:32/little,_/binary>>) -> true;
magic(<<?MM:16,?MAGIC:16/big,_Offset:32/big,_/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/tiff".
    
extensions() -> [".tiff", ".tif" ].


read_info(Fd) ->
    case scan_fd(Fd, fun collect_fun/3, #erl_image { type = ?MODULE }) of
	{ok, IMG} ->
	    Bps = erl_img:attribute(IMG, 'BitsPerSample'),
	    Xs  = erl_img:attribute(IMG,'ExtraSamples',[]),
	    Format = 
		case erl_img:attribute(IMG, 'PhotoMetricInterpretation') of
		    [0] ->
			case Bps of
			    [4] -> gray4;
			    [8] -> gray8
			end;
		    [1] ->
			case Bps of
			    [4] -> gray4;
			    [8] -> gray8
			end;
		    [2] ->
			case Bps of
			    [8,8,8] ->
				case Xs of
				    []  ->  r8g8b8;
				    [_] -> r8g8b8a8
				end;
			    [8,8,8,8] ->
				r8g8b8a8
			end;
		    [3] ->
			case Bps of
			    [4] -> palette4;
			    [8] -> palette8
			end
		end,
	    {ok, IMG#erl_image { format = Format }};
	Error -> 
	    Error
    end.
		    

write_info(_Fd, _IMG) ->
    ok.

read(Fd, IMG) ->
    read(Fd, IMG, 
	 fun(_, Row, Ri, St) ->
		 ?dbg("tiff: load row ~p\n", [Ri]),
		 [{Ri,Row}|St] end, 
	 []).


read(Fd,IMG, RowFun, St0) ->
    SOffset       = erl_img:attribute(IMG, 'StripOffset'),
    SCount        = erl_img:attribute(IMG, 'StripByteCounts'),
    [Compression] = erl_img:attribute(IMG, 'Compression',[1]),
    [Predict]     = erl_img:attribute(IMG, 'Predictor', [0]),
    [FillOrder]   = erl_img:attribute(IMG, 'FillOrder', [1]),
    {SampleOrder,Y0,Ys} = case erl_img:attribute(IMG, 'Orientation', [1]) of
			      [1] -> {left_to_right, 0, 1};
			      [2] -> {right_to_left, 0, 1};
			      [3] -> {right_to_left, IMG#erl_image.height-1,-1};
			      [4] -> {left_to_right, IMG#erl_image.height-1,-1}
			end,
    BytesPerRow = (IMG#erl_image.depth div 8) * IMG#erl_image.width,
    ?dbg("BytesPerRow = ~p\n", [BytesPerRow]),
    IMG1 = IMG#erl_image { order = SampleOrder },
    PIX = #erl_pixmap { width = IMG1#erl_image.width,
			height = IMG1#erl_image.height,
			format = IMG1#erl_image.format },
    case read_strips(Fd,PIX,RowFun,St0,Y0,Ys,BytesPerRow,
		     Compression, Predict, FillOrder,SOffset,SCount) of
	{ok, PIX1} ->
	    {ok, IMG1#erl_image { pixmaps = [PIX1]}};
	Error ->
	    Error
    end.


read_strips(_Fd,PIX,_RowFun,St0,_Ri,_Rs,
	    _BytesPerRow,_Compression, _Predict, _Fill, [], []) ->
    {ok, PIX#erl_pixmap { pixels = St0 }};
read_strips(Fd,PIX,RowFun,St0,Ri,Rs,
	    BytesPerRow,Compression, Predict, Fill,
	    [Offs|SOffset], [Size|SCount]) ->
    case file:pread(Fd,Offs,Size) of
	{ok,Bin} ->
	    case Compression of
		1 -> %% no compression
		    {St1,Rj} = split_strip(PIX,Bin,BytesPerRow,
					   RowFun,St0,Ri,Rs),
		    read_strips(Fd,PIX,RowFun,St1,Rj,Rs,BytesPerRow,
				Compression,Predict,Fill,SOffset,SCount);

		5 -> %% lzw compression
		    Bin1 = lzw:decompress_tiff(Bin, 8, Fill),
		    Bin2 = undo_differencing(Bin1, Predict,
					     PIX#erl_pixmap.format,
					     PIX#erl_pixmap.width),
		    {St1,Rj} = split_strip(PIX,Bin2,BytesPerRow,
					   RowFun,St0,Ri,Rs),
		    read_strips(Fd,PIX,RowFun,St1,Rj,Rs,BytesPerRow,
				Compression, Predict, Fill, SOffset, SCount);
		
		32773 ->
		    Bin1 = unpack_bits(Bin),
		    {St1,Rj} = split_strip(PIX,Bin1,BytesPerRow,
					   RowFun,St0,Ri,Rs),
		    read_strips(Fd,PIX,RowFun,St1,Rj,Rs,BytesPerRow,
				Compression, Predict, Fill, SOffset, SCount);
		_ ->
		    {error, {unknown_compression,Compression}}
	    end;
	Error ->
	    Error
    end.


split_strip(PIX,Strip,RowWidth,RowFun,St0,Ri,Rs) ->
    case Strip of
	<<Row:RowWidth/binary, Tail/binary>> ->
	    St1 = RowFun(PIX,Row,Ri,St0),
	    split_strip(PIX,Tail,RowWidth,RowFun,St1,Ri+Rs,Rs);
	_ ->
	    {St0,Ri}
    end.


write(_Fd,_IMG) ->
    ok.

%% Image info collector functions
collect_fun(_Fd, T, St) ->
    Key = decode_tag(T#tiff_entry.tag),
    Value = T#tiff_entry.value,
    As = [{Key,Value} | St#erl_image.attributes],
    case Key of
	'ImageWidth' ->
	    [Width] = Value,
	    St#erl_image { width = Width, attributes = As };
	'ImageLength' ->
	    [Length] = Value,
	    St#erl_image { height = Length, attributes = As };
	'BitsPerSample' ->
	    St#erl_image { depth = lists:sum(Value), attributes = As };
	'ImageDescription' ->
	    St#erl_image { comment = Value, attributes = As };
	'DateTime' ->
	    [V] = Value,
	    case string:tokens(V, ": ") of
		[YYYY,MM,DD,H,M,S] ->
		    DateTime = {{list_to_integer(YYYY),
				 list_to_integer(MM),
				 list_to_integer(DD)},
				{list_to_integer(H),
				 list_to_integer(M),
				 list_to_integer(S)}},
		    St#erl_image { itime = DateTime, attributes = As };
		_ ->
		    St#erl_image { attributes = As }
	    end;

	_ ->
	    St#erl_image { attributes = As }
    end.


dump_fun(_Fd, T, St) ->
    Key = decode_tag(T#tiff_entry.tag),
    io:format("~s ~s ~w\n", [Key,T#tiff_entry.type,T#tiff_entry.value]),
    St.

dump_binary(Bin) when binary(Bin) ->
    scan_binary(Bin, fun dump_fun/3, ok).

dump_file(File) ->
    scan_file(File, fun dump_fun/3, ok).


scan_file(File, Callback, St) ->
    case file:open(File, [raw, binary, read]) of
	{ok,Fd} ->
	    Res = scan_fd(Fd, Callback, St),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.	

scan_binary(Bin, Callback, St) ->
    case file:open(Bin, [ram, binary, read]) of
	{ok,Fd} ->
	    Res = scan_fd(Fd, Callback, St),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.


scan_fd(Fd, Callback, St) ->
    case file:read(Fd, 8) of
	{ok, <<?II:16,?MAGIC:16/little,Offset:32/little>>} ->
	    %% io:format("TIFF: LITTLE endian\n"),
	    scan_ifd(Fd, [$0], Offset, little, Callback, St);
	{ok, <<?MM:16,?MAGIC:16/big,Offset:32/big>>} ->
	    %% io:format("TIFF: BIG endian\n"),
	    scan_ifd(Fd, [$0], Offset, big, Callback, St);
	{ok,_} ->
	    {error, bad_magic};
	Error -> 
	    Error
    end.

%% Scan entry point for special Exif/MakerNote
scan_ifd_bin(Bin, IFD, Offset, Endian, Callback, St) ->
    case file:open(Bin, [ram, binary, read]) of
	{ok,Fd} ->
	    Res = scan_ifd(Fd, IFD, Offset, Endian, Callback, St),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.

scan_ifd(_Fd, _IFD, 0, _Endian, _Callback, St) ->
    {ok,St};
scan_ifd(Fd, IFD, Offset, Endian, Callback,St) ->
    file:position(Fd, Offset),
    case read_u16(Fd,Endian) of
	{ok,N} ->
	    scan_entries(Fd, IFD, Endian, N, Callback, St);
	Error -> Error
    end.

scan_entries(Fd, [I|IFD], Endian, 0, Callback, St) ->
    case read_u32(Fd,Endian) of
	{ok,Offset} ->
	    scan_ifd(Fd,[I+1|IFD],Offset,Endian,Callback,St);
	Error -> Error
    end;
scan_entries(Fd, IFD, Endian, I, Callback,St) ->
    case read_entry(Fd,Endian) of
	{ok,{Tag,Type,N,Data}} ->
	    TiffTag = 
		case is_value(Type, N) of
		    true ->
			Value = decode_value(Type,Endian,N,Data),
			#tiff_entry { ifd = IFD, 
				      tag = Tag, 
				      endian = Endian,
				      type = Type, 
				      value = Value };
		    false ->
			Offset = if Endian == little ->
					 <<Offs:32/little>> = Data, Offs;
				    true ->
					 <<Offs:32/big>> = Data, Offs
				 end,
			Value = decode_offs_value(Fd,Offset,Type,Endian,N),
			#tiff_entry { ifd = IFD, 
				      tag = Tag, 
				      endian = Endian,
				      type = Type, 
				      offs = Offset,
				      value = Value }
		end,
	    %% Save file position inorder to allwo callback to parse Sub ifd's
	    {ok,Save} = file:position(Fd, cur),
	    St1 = Callback(Fd, TiffTag, St),
	    file:position(Fd, Save),
	    scan_entries(Fd, IFD, Endian, I-1, Callback, St1);
	Error -> Error
    end.

read_u16(Fd, Endian) ->
    case file:read(Fd, 2) of
	{ok, <<N:16/little>>} when Endian == little ->  {ok, N};
	{ok, <<N:16/big>>} when Endian == big       ->  {ok, N};
	{ok, _} -> {error, truncated};
	Error -> Error
    end.

read_u32(Fd, Endian) ->
    case file:read(Fd, 4) of
	{ok, <<N:32/little>>} when Endian == little ->  {ok, N};
	{ok, <<N:32/big>>} when Endian == big       ->  {ok, N};
	{ok, _} -> {error, truncated};
	Error -> Error
    end.

read_entry(Fd,Endian) ->
    case file:read(Fd,12) of
	{ok,<<Tag:16/little,T:16/little,N:32/little,V:4/binary>>} when 
	      Endian == little ->
	    {ok,{Tag,decode_type(T),N,V}};
	{ok,<<Tag:16/big,T:16/big,N:32/big,V:4/binary>>} when 
	      Endian == big ->
	    {ok,{Tag,decode_type(T),N,V}};
	{ok,_} -> {error, truncated};
	Error -> Error
    end.


%% Decode TIFF types
decode_type(?BYTE)      -> byte;
decode_type(?ASCII)     -> ascii;
decode_type(?SHORT)     -> short;
decode_type(?LONG)      -> long;
decode_type(?RATIONAL)  -> rational;
decode_type(?SBYTE)     -> sbyte;
decode_type(?UNDEFINED) -> undefined;
decode_type(?SSHORT)    -> sshort;
decode_type(?SLONG)     -> slong;
decode_type(?SRATIONAL) -> srational;
decode_type(?FLOAT)     -> float;
decode_type(?DOUBLE)    -> double;
decode_type(_) -> unknown.

decode_tag(Tag) ->
    case Tag of
	?NewSubfileType -> 'NewSubfileType';
	?SubfileType -> 'SubfileType';
	?ImageWidth -> 'ImageWidth';
	?ImageLength -> 'ImageLength';
	?BitsPerSample -> 'BitsPerSample';
	?Compression -> 'Compression';
	?PhotoMetricInterpretation -> 'PhotoMetricInterpretation';
	?Threshholding -> 'Threshholding';
	?CellWidth -> 'CellWidth';
	?CellLength -> 'CellLength';
	?FillOrder -> 'FillOrder';
	?DocumentName -> 'DocumentName';
	?ImageDescription -> 'ImageDescription';
	?Make -> 'Make';
	?Model -> 'Model';
	?StripOffset -> 'StripOffset';
	?Orientation -> 'Orientation';
	?SamplesPerPixel -> 'SamplesPerPixel';
	?RowsPerStrip -> 'RowsPerStrip';
	?StripByteCounts -> 'StripByteCounts';
	?MinSampleValue -> 'MinSampleValue';
	?MaxSampleValue -> 'MaxSampleValue';
	?XResolution -> 'XResolution';
	?YResolution -> 'YResolution';
	?PlanarConfiguration -> 'PlanarConfiguration';
	?PageName -> 'PageName';
	?XPosition -> 'XPosition';
	?YPosition -> 'YPosition';
	?FreeOffsets -> 'FreeOffsets';
	?FreeByteCounts -> 'FreeByteCounts';
	?GrayResponseUnit -> 'GrayResponseUnit';
	?GrayResponseCurve -> 'GrayResponseCurve';
	?T4Options -> 'T4Options';
	?T6Options -> 'T6Options';
	?ResolutionUnit -> 'ResolutionUnit';
	?PageNumber -> 'PageNumber';
	?TransferFunction -> 'TransferFunction';
	?Software -> 'Software';
	?DateTime -> 'DateTime';
	?Artist -> 'Artist';
	?HostComputer -> 'HostComputer';
	?Predictor -> 'Predictor';
	?WhitePoint -> 'WhitePoint';
	?PrimaryChromaticities -> 'PrimaryChromaticities';
	?ColorMap -> 'ColorMap';
	?HalftoneHints -> 'HalftoneHints';
	?TileWidth -> 'TileWidth';
	?TileLength -> 'TileLength';
	?TileOffset -> 'TileOffset';
	?TileByteCounts -> 'TileByteCounts';
	?InkSet -> 'InkSet';
	?InkNames -> 'InkNames';
	?NumberOfInks -> 'NumberOfInks';
	?DotRange -> 'DotRange';
	?TargetPrinter -> 'TargetPrinter';
	?ExtraSamples -> 'ExtraSamples';
	?SampleFormat -> 'SampleFormat';
	?SMinSampleValue -> 'SMinSampleValue';
	?SMaxSampleValue -> 'SMaxSampleValue';
	?TransferRange -> 'TransferRange';
	?JPEGProc -> 'JPEGProc';
	?JPEGInterchangeFormat -> 'JPEGInterchangeFormat';
	?JPEGInterchangeFormatLength -> 'JPEGInterchangeFormatLength';
	?JPEGRestartInterval -> 'JPEGRestartInterval';
	?JPEGLosslessPredictors -> 'JPEGLosslessPredictors';
	?JPEGPointTransforms -> 'JPEGPointTransforms';
	?JPEGQTables -> 'JPEGQTables';
	?JPEGDCTables -> 'JPEGDCTables';
	?JPEGACTables -> 'JPEGACTables';
	?YCbCrCoefficients -> 'YCbCrCoefficients';
	?YCbCrSampling -> 'YCbCrSampling';
	?YCbCrPositioning -> 'YCbCrPositioning';
	?ReferenceBlackWhite -> 'ReferenceBlackWhite';
	?Copyright -> 'Copyright';
	_ -> Tag
    end.


is_value(byte,N) when N =< 4      -> true;
is_value(sbyte,N) when N =< 4     -> true;
is_value(undefined,N) when N =< 4 -> true;
is_value(ascii,N) when N =< 4     -> true;
is_value(short,N) when N =< 2     -> true;
is_value(sshort,N) when N =< 2    -> true;
is_value(long,1) -> true;
is_value(slong,1) -> true;
is_value(float,1) -> true;
is_value(_,_) -> false.

%% calculate the size of the type
sizeof(byte)      -> 1;
sizeof(ascii)     -> 1;
sizeof(short)     -> 2;
sizeof(long)      -> 4;
sizeof(rational)  -> 8;
sizeof(sbyte)     -> 1;
sizeof(undefined) -> 1;
sizeof(sshort)    -> 2;
sizeof(slong)     -> 4;
sizeof(srational) -> 8;
sizeof(float)     -> 4;
sizeof(double)    -> 8;
sizeof(_)         -> 0.

%% decode an offseted value
decode_offs_value(Fd,Offset,Type,Endian,N) ->
    Sz = sizeof(Type)*N,
    case file:pread(Fd, Offset, Sz) of
	{ok,Bin} when size(Bin) == Sz ->
	    decode_value(Type,Endian,N,Bin);
	{ok,_} ->
	    {error, truncated};
	Error ->
	    Error
    end.

decode_value(_,_,0,_) -> [];

%% little
decode_value(short,little,I,<<V:16/little,VT/binary>>) ->
    [V|decode_value(short,little,I-1,VT)];

decode_value(long,little,I,<<V:32/little,VT/binary>>) ->
    [V|decode_value(long, little,I-1,VT)];

decode_value(rational,little,I, <<N:32/little,D:32/little,VT/binary>>) ->
    [{N,D}|decode_value(rational,little,I-1,VT)];

decode_value(sshort,little,I,<<V:16/signed-little,VT/binary>>) ->
    [V|decode_value(sshort,little,I-1,VT)];

decode_value(slong,little,I,<<V:32/signed-little,VT/binary>>) ->
    [V|decode_value(slong, little,I-1,VT)];

decode_value(srational,little,I, 
	     <<N:32/signed-little,D:32/signed-little,VT/binary>>) ->
    [{N,D}|decode_value(srational,little,I-1,VT)];

decode_value(float,little,I,<<V:32/little-float,VT/binary>>) ->
    [V|decode_value(float, little,I-1,VT)];

decode_value(double,little,I,<<V:64/little-float,VT/binary>>) ->
    [V|decode_value(double,little,I-1,VT)];

%% big
decode_value(short,big,I,<<V:16/big,VT/binary>>) ->
    [V|decode_value(short,big,I-1,VT)];

decode_value(long,big,I,<<V:32/big,VT/binary>>) ->
    [V|decode_value(long,big,I-1,VT)];

decode_value(rational,big,I,<<N:32/big,D:32/big,VT/binary>>) ->
    [{N,D}|decode_value(rational,big,I-1,VT)];

decode_value(sshort,big,I,<<V:16/signed-big,VT/binary>>) ->
    [V|decode_value(sshort,big,I-1,VT)];

decode_value(slong,big,I,<<V:32/signed-big,VT/binary>>) ->
    [V|decode_value(slong,big,I-1,VT)];

decode_value(srational,big,I,<<N:32/signed-big,D:32/signed-big,VT/binary>>) ->
    [{N,D}|decode_value(srational,big,I-1,VT)];

decode_value(float,big,I,<<V:32/big-float,VT/binary>>) ->
    [V|decode_value(float,big,I-1,VT)];

decode_value(double,big,I,<<V:64/big-float,VT/binary>>) ->
    [V|decode_value(double,big,I-1,VT)];

%% Any endian single fields
decode_value(sbyte,_Endian,N,Bin) ->
    <<V:N/binary,_/binary>> = Bin,
    map(fun(I) when I >= 16#80 -> I - 16#100;
	   (I) -> I
	end, binary_to_list(V));


decode_value(byte,_Endian,N,Bin) ->
    <<V:N/binary,_/binary>> = Bin,
    binary_to_list(V);
decode_value(ascii,_Endian,N,Bin) ->
    <<V:N/binary,_/binary>> = Bin,
    decode_strings(binary_to_list(V));
decode_value(undefined,_Endian,N,Bin) ->
    <<V:N/binary,_/binary>> = Bin,
    V;
decode_value(unknown,_Endian,_N,Bin) ->
    Bin.


%% decode a sequence of strings
decode_strings(Cs) ->
    decode_strings(Cs,[],[]).

decode_strings([0|Cs], String, Acc) ->
    decode_strings(Cs, [], [reverse(String)|Acc]);
decode_strings([C|Cs], String, Acc) ->
    decode_strings(Cs, [C|String], Acc);
decode_strings([], [], Acc) ->
    reverse(Acc);
decode_strings([], String, Acc) ->
    reverse([reverse(String)|Acc]).



undo_differencing(Data,2,r8g8b8a8,Width) ->
    undo_differencing4(Data, Width);
undo_differencing(Data,2,r8g8b8,Width) ->
    undo_differencing3(Data, Width);
undo_differencing(Data,_,_,_) ->
    Data.

undo_differencing4(Data, Width) ->
    if binary(Data) ->
	    undo_differencing4(0, Width, binary_to_list(Data),0,0,0,0, []);
       list(Data) ->
	    undo_differencing4(0, Width, Data, 0,0,0,0, [])
    end.


undo_differencing4(W, W, Rest, _,_,_,_,Ack) ->
    undo_differencing4(0,W, Rest, 0,0,0,0,Ack);
undo_differencing4(C, W, [R,G,B,A|Rest], AR,AG,AB,AA, Ack) ->
    %% io:format("undo ~p ~n", [[{R,G,B,A}, {AR,AG,AB,AA}]]),
    RR = (R + AR) rem 256,    
    RG = (G + AG) rem 256, 
    RB = (B + AB) rem 256,
    RA = (A + AA) rem 256,
    undo_differencing4(C+1, W, Rest, RR,RG,RB,RA, [RA,RB,RG,RR|Ack]); 
undo_differencing4(_, _, [], _,_,_,_, Ack) ->
    list_to_binary(reverse(Ack)).


undo_differencing3(Data, Width) ->
    if binary(Data) ->
	    undo_differencing3(0, Width, binary_to_list(Data),0,0,0, []);
       list(Data) ->
	    undo_differencing3(0, Width, Data, 0, 0, 0, [])
    end.

undo_differencing3(W, W, Rest, _,_,_, Ack) ->
    undo_differencing3(0,W, Rest, 0,0,0, Ack);
undo_differencing3(C, W, [R,G,B|Rest], AR,AG,AB, Ack) ->
    RR = (R + AR) rem 256,    
    RG = (G + AG) rem 256, 
    RB = (B + AB) rem 256,
    undo_differencing3(C+1, W, Rest, RR,RG,RB, [RB,RG,RR|Ack]);
undo_differencing3(_, _, [], _,_,_, Ack) ->
    list_to_binary(reverse(Ack)).


unpack_bits(Bin) ->
    unpack_bits(Bin, []).

unpack_bits(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
unpack_bits(<<128:8/signed,Tail/binary>>, Acc) ->
    unpack_bits(Tail, Acc);
unpack_bits(<<Code:8/signed,Tail/binary>>, Acc) when Code >= 0 ->
    Count = Code + 1,
    <<Bin1:Count/binary, Tail1/binary>> = Tail,
    unpack_bits(Tail1, [Bin1|Acc]);
unpack_bits(<<Code:8/signed,Tail/binary>>, Acc) ->
    Count = -Code + 1,
    <<Re:8, Tail1/binary>> = Tail,
    unpack_bits(Tail1, [list_to_binary(lists:duplicate(Count, Re))|Acc]).
