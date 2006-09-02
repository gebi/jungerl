%%% File    : image_png.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : PNG Files
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_png).

-include("erl_img.hrl").
-include("api.hrl").

-include("dbg.hrl").

-import(lists, [reverse/1]).
-import(erl_img, [attribute/3, set_attribute/3]).
-export([filter/4]).

-define(MAGIC, 137,$P,$N,$G,$\r,$\n,26,$\n).

-define(IHDR, "IHDR"). %% image header
-define(PLTE, "PLTE"). %% palette
-define(IDAT, "IDAT"). %% image data
-define(IEND, "IEND"). %% image trailer

-define(bKGD, "bKGD"). %% background color
-define(cHRM, "cHRM"). %% primary chromaticites and white point
-define(gAMA, "gAMA"). %% Image gamma
-define(hIST, "hIST"). %% Image histogram
-define(pHYs, "pHYs"). %% Physical pixel dimensions
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
    case read_chunk_hdr(Fd) of
	{ok, Length, Type} ->
	    scan_info(Fd, IMG, First, Type, Length);
	Error -> 
	    Error
    end.

scan_info(Fd, IMG, true, ?IHDR, Length) ->
    case read_chunk_crc(Fd,Length) of
	{ok,  <<Width:32, Height:32, BitDepth:8,
	       ColorType:8, CompressionMethod:8,
	       FilterMethod:8, InterlaceMethod:8, _/binary >>} ->
	    scan_info(Fd, IMG#erl_image {
			    width = Width,
			    height = Height,
			    depth = BitDepth,
			    format = format(ColorType,BitDepth),
			    order  = left_to_right,
			    attributes = 
			    [ {'ColorType', ColorType},
			      {'Compression', CompressionMethod},
			      {'Filter', FilterMethod },
			      {'Interlace', InterlaceMethod }]}, false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?tEXt, Length) ->
    case read_chunk_crc(Fd, Length) of
	{ok, Bin} ->
	    scan_info(Fd, update_txt(IMG, Bin), false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?zTXt, Length) ->
    case read_chunk_crc(Fd, Length) of
	{ok, CBin} ->
	    Bin = zlib:uncompress(CBin),
	    scan_info(Fd, update_txt(IMG, Bin), false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?bKGD, Length) ->
    CT = attribute(IMG, 'ColorType', undefined),
    case read_chunk_crc(Fd, Length) of
	{ok, <<Index:8>>} when CT==3 ->
	    scan_info(Fd, set_attribute(IMG, 'Background', Index), false);
	{ok, <<Gray:16>>} when CT==0; CT==4 ->
	    scan_info(Fd, set_attribute(IMG, 'Background', Gray), false);
	{ok, <<R:16,G:16,B:16>>} when CT==2; CT==6 ->
	    scan_info(Fd, set_attribute(IMG, 'Background', {R,G,B}), false);
	{ok, _Data} ->
	    ?dbg("bKGD other=~p\n", [_Data]),
	    scan_info(Fd, IMG, false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?tIME, Length) ->
    case read_chunk_crc(Fd, Length) of
	{ok, <<Year:16, Mon:8, Day:8, H:8, M:8, S:8>>} ->
	    scan_info(Fd, IMG#erl_image { mtime = {{Year,Mon,Day},
						   {H,M,S}} }, false);
	{ok, _Data} ->
	    ?dbg("tIME other=~p\n", [_Data]),
	    scan_info(Fd, IMG, false);
	Error -> Error
    end;
scan_info(Fd, IMG, false, ?pHYs, Length) ->
    case read_chunk_crc(Fd, Length) of
	{ok, <<X:32, Y:32, _Unit:8>>} ->
	    scan_info(Fd, set_attribute(IMG,'Physical',{X,Y,meter}),false);
	{ok, _Data} ->
	    ?dbg("pHYs other=~p\n", [_Data]),
	    scan_info(Fd, IMG, false);
	Error -> Error
    end;
scan_info(_Fd, IMG, false, ?IEND, 0) ->
    {ok, IMG};
scan_info(Fd, IMG, false, _Type, Length) ->
    ?dbg("~s skipped=~p\n", [_Type,Length]),
    skip_chunk(Fd, Length),
    scan_info(Fd, IMG, false).

%% Update txt attributes
update_txt(IMG, Txt) ->
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
    end.


%% determine the erl_image format
bytes_per_row(gray1,W) -> W div 8;
bytes_per_row(gray2,W) -> W div 4;
bytes_per_row(gray4,W) -> W div 2;
bytes_per_row(gray8,W) -> W;
bytes_per_row(gray16,W) -> W*2;
bytes_per_row(r8g8b8,W) -> W*3;
bytes_per_row(r16g16b16,W) -> W*6;
bytes_per_row(palette1,W) -> W div 8;
bytes_per_row(palette2,W) -> W div 4;
bytes_per_row(palette4,W) -> W div 2;
bytes_per_row(palette8,W) -> W;
bytes_per_row(gray8a8,W) -> W*2;
bytes_per_row(gray16a16,W) -> W*4;
bytes_per_row(r8g8b8a8,W) -> W*4;
bytes_per_row(r16g16b16a16,W) -> W*8.


bpp(gray1) -> 1;
bpp(gray2) -> 1;
bpp(gray4) -> 1;
bpp(gray8) -> 1;
bpp(gray16) -> 2;
bpp(r8g8b8) -> 3;
bpp(r16g16b16) -> 6;
bpp(palette1) -> 1;
bpp(palette2) -> 1;
bpp(palette4) -> 1;
bpp(palette8) -> 1;
bpp(gray8a8)  -> 2;
bpp(gray16a16) -> 4;
bpp(r8g8b8a8) -> 4;
bpp(r16g16b16a16) -> 8.


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

%% IMPLEMENT This:
write_info(_Fd, _IMG) ->
    ok.


read(Fd, IMG) ->
    read(Fd, IMG, 
	 fun(_, Row, Ri, St) ->
		 ?dbg("png: load row ~p\n", [Ri]),
		 [{Ri,Row}|St] end, 
	 []).
	 

read(Fd, IMG, RowFun, St0) ->
    file:position(Fd, 8), %% skip magic
    Z = zlib:open(),
    zlib:inflateInit(Z),
    Resp = read_image(Fd, [], undefined, Z),
    zlib:close(Z),
    case Resp of
	{ok, Binary, Palette} ->
	    {ok,Pixmap} = create_pixmap(IMG, Binary, Palette, RowFun, St0),
	    {ok, IMG#erl_image { pixmaps = [Pixmap],
				 palette = Palette }};
	Error -> Error
    end.

create_pixmap(IMG, Bin, Palette, RowFun, St0) ->
    Pix0 = #erl_pixmap { width  = IMG#erl_image.width,
			 height = IMG#erl_image.height,
			 palette = Palette,
			 format  = IMG#erl_image.format },
    Bpp = bpp(IMG#erl_image.format),
    BytesPerRow = bytes_per_row(IMG#erl_image.format,IMG#erl_image.width),
    raw_data(Bin,Pix0,RowFun,St0,0,Bpp,BytesPerRow).


raw_data(Bin,Pix,RowFun,St0,Ri,Bpp,Width) ->
    case Bin of
	<<Filter:8,Row:Width/binary,Bin1/binary>> ->
	    Prior = case St0 of
			[] -> <<>>;
			[{_,Row0}|_] -> Row0
		    end,
	    Row1 = filter(Filter,Bpp,Row,Prior), %% Filter method=0 assumed
	    St1 = RowFun(Pix,Row1,Ri,St0),
	    raw_data(Bin1,Pix,RowFun,St1,Ri+1,Bpp,Width);
	_ ->
	    {ok, Pix#erl_pixmap { pixels = St0 }}
    end.

filter(0,_,Row,_Prior) -> Row;
filter(1, Bpp, Row, Prior) -> filter_sub(Row,Prior,Bpp);
filter(2, Bpp, Row,Prior) -> filter_up(Row,Prior,Bpp);
filter(3, Bpp, Row,Prior) -> filter_avg(Row,Prior,Bpp);
filter(4, Bpp, Row,Prior) -> filter_paeth(Row,Prior,Bpp).

%%
%% Raw(x) = Sub(x) + Raw(x-bpp)  [ Sub(x) = Raw(x) - Raw(x-bpp) ]
%%    
filter_sub(Sub,_Prior,Bpp) ->
    Rn = lists:duplicate(Bpp, 0),
    Rm = [],
    filter_sub(Sub, 0, size(Sub), [], Rn, Rm).

filter_sub(_Sub, X, X, Acc, _, _) ->
    list_to_binary(reverse(Acc));
filter_sub(Sub, X, N, Acc, [Rxb|Rn], Rm) ->
    <<_:X/binary, Sx:8, _/binary>> = Sub,
    Rx = (Sx + Rxb) band 16#ff,
    if Rn == [] ->
	    filter_sub(Sub,X+1,N,[Rx|Acc],reverse([Rx|Rm]),[]);
       true ->
	    filter_sub(Sub,X+1,N,[Rx|Acc],Rn,[Rx|Rm])
    end.
%%
%% Raw(x) = Up(x) + Prior(x) [ Up(x) = Raw(x) - Prior(x) ]
%% 
filter_up(Up, Prior, _Bpp) ->
    filter_up(Up, Prior, 0, size(Up), []).
    
filter_up(_Up, _Prior, X, X, Acc) ->
    list_to_binary(reverse(Acc));
filter_up(Up, Prior, X, N, Acc) ->
    <<_:X/binary,Ux:8,_/binary>> = Up,
    Px = case Prior of
	     <<_:X/binary,Pi,_/binary>> -> Pi;
	     _ -> 0
	 end,
    Rx = (Ux + Px) band 16#ff,
    filter_up(Up,Prior,X+1,N,[Rx|Acc]).

%%
%% Raw(x) = Avarage(x) + floor((Raw(x-bpp)+Prior(x))/2)
%%    [ Avarage(x) = Raw(x) - floor((Raw(x-bpp)+Prior(x))/2) ]
%%

filter_avg(Avg, Prior,Bpp) ->
    Rn = lists:duplicate(Bpp, 0),
    Rm = [],
    filter_avg(Avg, Prior,  0, size(Avg), [], Rn, Rm).

filter_avg(_Avg,_Prior, X, X, Acc, _, _) ->
    list_to_binary(reverse(Acc));
filter_avg(Avg, Prior, X, N, Acc, [Rxb|Rn], Rm) ->
    <<_:X/binary, Ax:8, _/binary>> = Avg,
    Px = case Prior of
	     <<_:X/binary,Pi,_/binary>> -> Pi;
	     _ -> 0
	 end,
    Rx = (Ax + ((Rxb+Px) div 2)) band 16#ff,
    if Rn == [] ->
	    filter_avg(Avg,Prior,X+1,N,[Rx|Acc],reverse([Rx|Rm]),[]);
       true ->
	    filter_avg(Avg,Prior,X+1,N,[Rx|Acc],Rn,[Rx|Rm])
    end.

%%
%% Paeth(x) = Raw(x) -
%%            PaethPredictor(Raw(x-bpp),Prior(x),Prior(x-bpp))
%%
%% Raw(x) = Paeth(x) + PaethPredictor(Raw(x-bpp),Prior(x),Prior(x-bpp))
%%
filter_paeth(Pae,Prior,Bpp) ->
    Pn = Rn = lists:duplicate(Bpp, 0),
    Pm = Rm = [],
    filter_pae(Pae, Prior, 0, size(Pae), [], Rn, Rm, Pn, Pm).


filter_pae(_Pae, _Prior, X, X, Acc, _Rn, _Rm, _Pn, _Pm) ->
    list_to_binary(reverse(Acc));
filter_pae(Pae, Prior, X, N, Acc, [Rxb|Rn], Rm, [Pxb|Pn], Pm) ->
    <<_:X/binary, PAx:8, _/binary>> = Pae,
    Px = case Prior of
	     <<_:X/binary,Pi,_/binary>> -> Pi;
	     _ -> 0
	 end,
    Rx = (PAx + paethPredictor(Rxb, Px, Pxb)) band 16#ff,
    if Rn == [] ->
	    filter_pae(Pae,Prior,X+1,N,[Rx|Acc],
		       reverse([Rx|Rm]),[],
		       reverse([Px|Pm]),[]);
       true ->
	    filter_pae(Pae,Prior,X+1,N,[Rx|Acc],
		       Rn,[Rx|Rm],
		       Pn,[Px|Pm])
    end.

-define(dabs(X,Y),
	if (X) > (Y) -> (X) - (Y);
	   true -> (Y) - (X)
	end).
		
paethPredictor(A,B,C) ->
    P = A + B - C,
    PA = ?dabs(P,A),
    PB = ?dabs(P,B),
    PC = ?dabs(P,C),
    if PA =< PB, PA =< PC -> A;
       PB =< PC -> B;
       true -> C
    end.
	    
        
    


    
    

write(_Fd, _IMG) ->
    ok.

read_image(Fd, Acc, Palette, Z) ->
    case read_chunk_hdr(Fd) of
	{ok, Length, ?IDAT} ->
	    case read_chunk_crc(Fd, Length) of
		{ok, CBin} ->
		    Blocks = zlib:inflate(Z, CBin),
		    read_image(Fd, [Blocks|Acc], Palette, Z);
		Error -> Error
	    end;
	{ok, _Length, ?IEND} ->
	    zlib:inflateEnd(Z),
	    {ok, list_to_binary(reverse(Acc)), Palette};

	{ok, Length, ?PLTE} ->
	    case read_chunk_crc(Fd, Length) of
		{ok, Chunk} ->
		    read_image(Fd, Acc, plte(Chunk), Z);
		Error ->
		    Error
	    end;
	{ok, Length, _} ->
	    skip_chunk(Fd, Length),
	    read_image(Fd, Acc, Palette, Z)
    end.

%%
%% Given chunk header read chunk and check crc
%%
read_chunk_crc(Fd, Length) ->
    file:position(Fd, {cur,-4}),
    LengthWithType = Length+4,
    case file:read(Fd, LengthWithType+4) of
	{ok,<<TypeChunk:LengthWithType/binary, CRC:32>>} ->
	    case valid_crc32(TypeChunk, CRC) of
		true -> 
		    <<_:32, Chunk/binary>> = TypeChunk,
		    {ok, Chunk};
		false ->
		    {error, bad_crc}
	    end;
	{ok,_} ->
	    {error, bad_chunk};
	Error ->
	    Error
    end.

%%
%% Read the chunk header
%%    

read_chunk_hdr(Fd) ->
    case file:read(Fd, 8) of
	{ok, <<Length:32, Type:4/binary>>} ->
	    Tag = binary_to_list(Type),
	    ?dbg("chunk: type = ~p, length=~p\n", [Tag,Length]),
	    {ok, Length, Tag};
	Error ->
	    Error
    end.


skip_chunk(Fd, Length) ->
    file:position(Fd, {cur,Length+4}).

valid_crc32(Binary, CRC32) ->
    Z = zlib:open(),
    Value = zlib:crc32(Z, Binary),
    zlib:close(Z),
    ?dbg("crc check: ~p == ~p\n", [CRC32, Value]),
    CRC32 == Value.

