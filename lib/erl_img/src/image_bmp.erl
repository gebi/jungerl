%%% File    : image_bmp.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : BMP Files
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_bmp).

-include("erl_img.hrl").
-include("api.hrl").
-include("dbg.hrl").

-import(lists, [reverse/1]).


-define(BMP_HEADER(FileSz, Offset),
        $B:8,$M:8, 
	FileSz:32/little, 
	0:16, 0:16, 
	Offset:32/little).

-define(BMP_INFO(HSize,Width,Height,Planes,BitCount,Compression,
		 ImageSize,XRes,YRes,ColorsUsed,ImportantColors),
	HSize:32/little,
	Width:32/little, 
	Height:32/little, 
	Planes:16/little,
	BitCount:16/little, 
	Compression:32/little, 
	ImageSize:32/little,
	XRes:32/little,
	YRes:32/little,
        ColorsUsed:32/little, 
	ImportantColors:32/little).

magic(<<$B,$M, _/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/bmp".

extensions() -> [".bmp" ].


read_info(Fd) ->
    case file:read(Fd, 54) of
	{ok, << ?BMP_HEADER(Size,Offset),
	        ?BMP_INFO(_,Width,Height,Planes,BitCount,
			  Compression,_,_,_,_,_) >> } ->
	    {ok, #erl_image  { type      = ?MODULE,
			     width     = Width,
			     height    = Height,
			     depth     = Planes,
			     format    = b8g8r8,
			     bytes_pp  = 3,
			     alignment = 4,
			     order = left_to_right,
			     attributes = [{'Compression',Compression}]
			    }};
	{ok, _} ->
	    {error, bad_magic};
	Error ->
	    Error
    end.


write_info(Fd, IMG) ->
    ok.

read(Fd, IMG, RowFun, St0) ->
    file:position(Fd, 54),
    case read_pixels(Fd, IMG, RowFun, St0) of
	{ok,PIX} ->
	    {ok, IMG#erl_image { pixmaps = [PIX] }};
	Error -> Error
    end.

%% load image
read(Fd, IMG) ->
    read(Fd, IMG, 
	 fun(_, Row, Ri, St) ->
		 ?dbg("bmp: load row ~p\n", [Ri]),
		 [{Ri,Row}|St] end, 
	 []).

%% save image
write(Fd, IMG) ->
    ok.

%% Read all rows
read_pixels(Fd, IMG, RowFun, St0) ->
    Width = IMG#erl_image.width,
    Height = IMG#erl_image.height,
    RowLength =  Width*3 + ?PAD_Len(Width*3, 4),
    PIX = #erl_pixmap { width = Width, height = Height,
			format = IMG#erl_image.format },
    read_pixels(Fd, PIX, 0, Height, RowLength, RowFun, St0).


read_pixels(Fd, PIX, NRows, NRows, BytesPerRow, RowFun, St) ->
    {ok,PIX#erl_pixmap { pixels = St }};
read_pixels(Fd, PIX, Ri, NRows, BytesPerRow, RowFun, St) ->
    case file:read(Fd, BytesPerRow) of
	{ok,Row} ->
	    St1 = RowFun(PIX, Row, Ri, St),
	    read_pixels(Fd, PIX, Ri+1, NRows, BytesPerRow, RowFun, St1);
	Error ->
	    Error
    end.






    
