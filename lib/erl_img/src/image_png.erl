%%% File    : image_png.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : PNG Files
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_png).

-include("erl_img.hrl").
-include("api.hrl").

-define(debug, true).
-include("dbg.hrl").

-import(lists, [reverse/1]).

-define(MAGIC, 137,$P,$N,$G,$\r,$\n,26,$\n).

-define(IHDR, "IHDR"). %% image header
-define(PLTE, "PLTE"). %% palette
-define(IDAT, "IDAT"). %% image data
-define(IEND, "IEND"). %% image trailer

-define(bKGD, "bKGD"). %% background color
-define(cHRM, "cHRM"). %% primary chromaticites and white point
-define(gAMA, "gAMA"). %% Image gamma
-define(hIST, "hIST"). %% Image histogram
-define(pHYs, "pYHs"). %% Physical pixel dimensions
-define(sBIT, "sBIT"). %% Significant bits
-define(tEXt, "tEXt"). %% Textual data
-define(tIME, "tIME"). %% Image last modification time
-define(tRNS, "tRNS"). %% Transparency
-define(zTXt, "zTXt"). %% Compressed textual data

magic(<<?MAGIC, _/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/png".

extensions() -> [ ".png" ].
    
read_info(Fd) ->
    case file:read(Fd, 8) of
	{ok, << ?MAGIC >> } ->
	    scan_info(Fd, #erl_image { type = ?MODULE }, true);
	{ok, _} ->
	    {error, bad_magic};
	Error ->
	    Error
    end.

scan_info(Fd, IMG, First) ->
    case file:read(Fd, 8) of
	{ok, <<Length:32, Type:4/binary>>} ->
	    T = binary_to_list(Type),
	    ?dbg("chunke type: ~p, length: ~p\n", [T, Length]),
	    scan_chunk(Fd, IMG, First, T, Length);
	Error -> Error
    end.

scan_chunk(Fd, IMG, true, ?IHDR, Length) ->
    case file:read(Fd, Length) of
	{ok, Chunk = 
	 <<Width:32,
	  Height:32,
	  Depth:8,
	  ColorType:8,
	  CompressionMethod:8,
	  FilterMethod:8,
	  InterlaceMethod:8, _/binary >>} ->
	    scan_chunk_crc(Fd, Chunk,
			   IMG#erl_image {
			     width = Width,
			     height = Height,
			     depth = Depth,
			     format = format(ColorType,Depth),
			     attributes = 
			     [ {'ColorType', ColorType},
			       {'Compression', CompressionMethod},
			       {'Filter', FilterMethod },
			       {'Interlace', InterlaceMethod }]});
	Error -> Error
    end;
scan_chunk(Fd, IMG, false, ?tEXt, Length) ->
    case file:read(Fd, Length) of
	{ok, Bin} ->
	    scan_txt_chunk(Fd, IMG, Bin, Bin);
	Error -> Error
    end;
scan_chunk(Fd, IMG, false, ?zTXt, Length) ->
    case file:read(Fd, Length) of
	{ok, CBin} ->
	    scan_txt_chunk(Fd, IMG, CBin, zlib:inflate(CBin));
	Error -> Error
    end;
scan_chunk(Fd, IMG, false, ?tIME, Length) ->
    %% add time attribute
    scan_skip_chunk(Fd, IMG, Length);
scan_chunk(Fd, IMG, false, ?IEND, 0) ->
    {ok, IMG};
scan_chunk(Fd, IMG, false, _, Length) ->
    %% add time attribute
    scan_skip_chunk(Fd, IMG, Length).

scan_txt_chunk(Fd, IMG, Chunk, Txt) ->
    IMG1 = 
	case txt(binary_to_list(Txt), []) of
	    {value,{Key,Value}} ->
		case Key of
		    'Comment' ->
			IMG#erl_image { comment = Value };
		    _ ->
			As = [{Key,Value} | IMG#erl_image.attributes],
			IMG#erl_image { attributes = As }
		end;
	    false ->
		IMG
	end,
    %% extract attributes
    scan_chunk_crc(Fd, Chunk, IMG1).


%% do crc check on chunk and continue to next chunk
scan_chunk_crc(Fd, Chunk, IMG) ->
    case file:read(Fd, 4) of
	{ok, <<CRC:32>>} ->
	    %% Fixme: add crc check if wanted
	    scan_info(Fd, IMG, false);
	Error ->
	    Error
    end.

%% skip chunk and crc
scan_skip_chunk(Fd, IMG, Length) ->
    file:position(Fd, {cur,Length+4}),
    scan_info(Fd, IMG, false).



%% determine the erl_image format
format(0, 1)  -> gray1;
format(0, 2)  -> gray2;
format(0, 4)  -> gray4;
format(0, 8)  -> gray8;
format(0, 16) -> gray16;

format(2, 8)  -> r8g8b8;
format(2, 16) -> r16g16b16;

format(3, 1)  -> palette1;
format(3, 2)  -> palette2;
format(3, 4)  -> palette4;
format(3, 8)  -> palette8;

format(4, 8)  -> gray8a8;
format(4, 16) -> gray16a16;

format(6, 8)  -> r8g8b8a8;
format(6, 16) -> r16g16b16a16.

%% process text chunk
txt([0|Value], RKey) ->
    {value, {list_to_atom(reverse(RKey)), Value}};
txt([C|Cs], RKey) ->
    txt(Cs,[C|RKey]);
txt([], _) ->
    false.

%% read palette
plte(<<R,G,B, Data/binary>>) ->
    [{R*255,G*255,B*255} | plte(Data)];
plte(<<>>) -> [].


write_info(Fd, IMG) ->
    ok.

read(Fd, IMG) ->
    read(Fd, IMG, 
	 fun(_, Row, Ri, St) ->
		 ?dbg("png: load row ~p\n", [Ri]),
		 [{Ri,Row}|St] end, 
	 []).
	 

read(Fd, IMG, RowFun, St0) ->
    file:position(Fd, 8),
    case read_image(Fd, [], undefined) of
	{ok, Chunks, Palette} ->
	    CBin = list_to_binary(Chunks),
	    io:format("compress size=~p: ~p\n", [size(CBin), CBin]),
	    case zlib:inflate(CBin) of
		{ok, Bin} -> 
		    Pixmap = create_pixmap(IMG, Bin, Palette),
		    {ok, IMG#erl_image { pixmaps = [Pixmap],
				       palette = Palette }};
		Error -> Error
	    end;
	Error -> Error
    end.



write(Fd, IMG) ->
    ok.

create_pixmap(IMG, Bin, Palette) ->
    #erl_pixmap {
	      width  = IMG#erl_image.width,
	      height = IMG#erl_image.height,
	      palette = Palette,
	      format  = IMG#erl_image.format,
	      pixels = Bin }.


read_image(Fd, Acc, Palette) ->
    case file:read(Fd, 8) of
	{ok, <<Length:32, Type:4/binary>>} ->
	    ?dbg("type = ~p\n", [binary_to_list(Type)]),
	    case binary_to_list(Type) of 
		?IDAT ->
		    case read_chunk(Fd, Length) of
			{ok, Chunk} ->
			    read_image(Fd, [Chunk|Acc], Palette);
			Error -> Error
		    end;
		?IEND ->
		    {ok, reverse(Acc), Palette};

		?PLTE ->
		    case read_chunk(Fd, Length) of
			{ok, Chunk} ->
			    read_image(Fd, Acc, plte(Chunk));
			Error ->
			    Error
		    end;
		_ ->
		    skip_chunk(Fd, Length),
		    read_image(Fd, Acc, Palette)
	    end;
	Error ->
	    Error
    end.


skip_chunk(Fd, Length) ->
    file:position(Fd, {cur,Length+4}).

%% read a chunk
read_chunk(Fd, 0) ->
    case file:read(Fd, 4) of
	{ok, <<CRC:32>>} ->{ok,<<>>};
	Error -> Error
    end;
read_chunk(Fd, Length) ->
    case file:read(Fd, Length) of
	{ok, Chunk} ->
	    case file:read(Fd, 4) of
		{ok, <<CRC:32>>} ->{ok,Chunk};
		Error -> Error
	    end;
	Error -> Error
    end.

    
    
	     

    
