%%% File    : image_gif.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : GIF image processing
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_gif).

-include("erl_img.hrl").

-include("api.hrl").

-define(debug, true).
-include("dbg.hrl").

-import(lists, [reverse/1]).


-define(APP_EXTENSION, 16#ff).
-define(COM_EXTENSION, 16#fe).
-define(CTL_EXTENSION, 16#f9).
-define(TXT_EXTENSION, 16#01).

-define(EXTENSION, 16#21).   %% $!
-define(IMAGE,   16#2c).     %% $,
-define(TRAILER, 16#3b).     %% $;

%% Read magic info check MAGIC type and width and height (depth)
%% of image
-define(MAGIC87, $G,$I,$F,$8,$7,$a).
-define(MAGIC89, $G,$I,$F,$8,$9,$a).

magic(<<?MAGIC87,_/binary>>) -> true;
magic(<<?MAGIC89,_/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/gif".

extensions() -> [ ".gif" ].
    

read_info(Fd) ->
    case file:read(Fd, 10) of
	{ok, <<?MAGIC87,
	      Width:16/little-unsigned-integer,
	      Height:16/little-unsigned-integer,_/binary>>} ->
	    {ok,#erl_image { type = ?MODULE,
			   width = Width,
			   height = Height,
			   format = palette8,
			   order = left_to_right,
			   depth = 8 }};
	{ok, <<?MAGIC89,
	      Width:16/little-unsigned-integer,
	      Height:16/little-unsigned-integer,_/binary>>} ->
	    {ok,#erl_image { type = ?MODULE,
			   width = Width,
			   height = Height,
			   format = palette8,
			   order = left_to_right,
			   depth  = 8 }};
	{ok, _} ->
	    {error, bad_magic};
	Error ->
	    Error
    end.

write_info(Fd, IMG) ->
    ok.


%% The Grammar.
%% <GIF Data Stream> ::=     Header <Logical Screen> <Data>* Trailer
%%
%% <Logical Screen> ::=      Logical Screen Descriptor [Global Color Table]
%%
%% <Data> ::=                <Graphic Block>  |
%%                           <Special-Purpose Block>
%%
%% <Graphic Block> ::=  [Graphic Control Extension] <Graphic-Rendering Block>
%%
%% <Graphic-Rendering Block> ::=  <Table-Based Image>  |
%%                                Plain Text Extension
%%
%% <Table-Based Image> ::=   Image Descriptor [Local Color Table] Image Data
%%
%% <Special-Purpose Block> ::=    Application Extension  |
%%                                Comment Extension

read(Fd,IMG,RowFun,St0) ->
    file:position(Fd, 6),
    case file:read(Fd, 7) of
	{ok, <<Width:16/little, Hight:16/little,
	      Map:1, Cr:3, Sort:1, Pix:3,
	      Background:8,
	      AspectRatio:8>>} ->
	    ColorRes = Cr+1,
	    Palette = read_palette(Fd, Map, Pix+1, undefined),
	    ?dbg("sizeof(palette)=~p pix=~w\n", 
		[length(Palette),1 bsl (Pix+1)]),
	    read_data(Fd, IMG#erl_image { palette = Palette }, RowFun, St0, []);
	Error ->
	    Error
    end.


read(Fd, IMG) ->
    read(Fd, IMG, 
	 fun(_, Row, Ri, St) ->
		 ?dbg("gif: load row ~p\n", [Ri]),
		 [{Ri,Row}|St] end, 
	 []).


read_data(Fd, IMG, RowFun, St0, As) ->
    case file:read(Fd, 1) of
	{ok, <<?EXTENSION>>} ->
	    ?dbg("Extension\n",[]),
	    read_extension(Fd, IMG, RowFun, St0, As);
	{ok, <<?IMAGE>>} ->
	    ?dbg("Image\n",[]),
	    case file:read(Fd, 9) of
		{ok, <<Left:16/little, Top:16/little,
		      Width:16/little, Height:16/little,
		      Map:1, Interlaced:1, Sort:1,_:2, Pix:3>>} ->
		    Palette = read_palette(Fd, Map, Pix+1,
					   IMG#erl_image.palette),
		    Pixmap0 =
			#erl_pixmap { top = Top,
				      left = Left,
				      width = Width,
				      height = Height,
				      format = IMG#erl_image.format,
				      palette = Palette,
				      attributes = As
				     },
		    ?dbg("sizeof(palette)=~p pix=~w\n", 
			[length(Palette),1 bsl (Pix+1)]),
		    case read_pixels(Fd,Pixmap0,RowFun,St0,
				     Width,Height,Interlaced) of
			{ok, Pixmap1} ->
			    Pixmap2 = setup_transparent(Pixmap1),
			    %% ?dbg("Pixmap = ~p\n", [Pixmap2]),
			    Ps = IMG#erl_image.pixmaps ++ [Pixmap2],
			    read_data(Fd, IMG#erl_image { pixmaps = Ps },
				      RowFun, St0, []);
			Error -> Error
		    end;
		Error -> Error
	    end;
	{ok, <<?TRAILER>>} ->
	    ?dbg("Trailer\n",[]),
	    {ok, IMG};
	Error -> 
	    Error
    end.


setup_transparent(Pix) ->
    As = Pix#erl_pixmap.attributes,
    case lists:keysearch('Transparent', 1, As) of
	{value, {_, 1}} ->
	    case lists:keysearch('TransparentColor', 1, As) of
		{value, {_, Index}} ->
		    Pix#erl_pixmap { transparent = Index };
		false -> Pix
	    end;
	_ -> Pix
    end.

    


read_extension(Fd, IMG,RowFun,St0,As) ->
    case file:read(Fd, 1) of
	{ok,<<?COM_EXTENSION>>} ->
	    read_comment(Fd, IMG,RowFun,St0,As);
	{ok,<<?APP_EXTENSION>>} ->
	    read_app(Fd, IMG,RowFun,St0,As);
	{ok,<<?CTL_EXTENSION>>} ->
	    read_ctl(Fd, IMG,RowFun,St0,As);
	{ok,<<?TXT_EXTENSION>>} ->
	    read_txt(Fd, IMG,RowFun,St0,As);
	{ok, Unknown} ->
	    read_blocks(Fd), %% skip
	    read_data(Fd, IMG,RowFun,St0,As)
    end.


read_ctl(Fd, IMG,RowFun,St0,As) ->
    ?dbg("Control block\n",[]),
    case read_block(Fd) of
	{ok, <<_:3, DisposalMethod:3, UserInput:1, Transparent:1,
	      DelayTime:16/unsigned-little,
	      TransparentColor:8>>} ->
	    case read_block(Fd) of
		terminator ->
		    As1 = [{'TransparentColor', TransparentColor},
			   {'DelayTime', DelayTime},
			   {'UserInput', UserInput},
			   {'Transparent', Transparent},
			   {'DisposalMethod', DisposalMethod} | As],
		    read_data(Fd,IMG,RowFun,St0,As1);
		{ok,_} ->
		    {error, bad_ctl_block};
		Error -> Error
	    end;
	Error -> Error
    end.


read_comment(Fd, IMG,RowFun,St0,As) ->
    ?dbg("Comment block\n",[]),
    case read_blocks(Fd) of
	{ok, Comment} ->
	    read_data(Fd, 
		      IMG#erl_image { comment = binary_to_list(Comment)}
		      ,RowFun,St0,As);
	Error -> Error
    end.


read_txt(Fd, IMG,RowFun,St0,As) ->
    ?dbg("Text block\n",[]),
    case read_block(Fd) of
	{ok, <<GridLeft:16/little, GridTop:16/little,
	       GridWidth:16/little, GridHeight:16/little,
	       CellWidth:8, CellHeight:8,
	      Foreground:8, Background:8>>} ->
	    case read_blocks(Fd) of
		{ok,Bin} ->
		    As1 = 
			[{'TextGridLeftPosition', GridLeft},
			 {'TextGridTopPosition', GridTop},
			 {'CharacterCellWidth', CellWidth},
			 {'CharacterCellHeight', CellHeight},
			 {'TextForegroundColorIndex', Foreground},
			 {'TextBackgroundColorIndex', Background},
			 {'Text', binary_to_list(Bin)} | As],
		    read_data(Fd,IMG,RowFun,St0,As1);
		Error ->
		    Error
	    end;
	terminator ->
	    {error, bad_txt_block};
	Error -> Error
    end.
		    
read_app(Fd, IMG,RowFun,St0,As) ->
    ?dbg("Application block\n",[]),
    case read_block(Fd) of
	{ok, <<Ident:8/binary, AuthCode:3/binary>>} ->
	    case read_blocks(Fd) of
		{ok, AppData} ->
		    As1 =
			[{'ApplicationIdentifier', binary_to_list(Ident)},
			 {'ApplicationAuthenticationCode',
			  binary_to_list(AuthCode)},
			 {'ApplicationData', AppData} | 
			 IMG#erl_image.attributes],
		    read_data(Fd,IMG#erl_image { attributes = As1},
			      RowFun,St0,As);
		Error ->
		    Error
	    end;
	terminator ->
	    {error, bad_app_block};
	Error ->
	     Error
    end.
	
%%
%% Read One block
%% return 
%%        {ok, Block}
%%        terminator
%%      | {error,Reason}
%%
%%      
%%
read_block(Fd) ->
    case file:read(Fd, 1) of
	{ok, <<0>>} -> 
	    terminator;
	{ok, <<Size>>} ->
	    file:read(Fd, Size);
	Error ->
	    Error
    end.

%%
%% Read a list of blocks
%%
read_blocks(Fd) ->
    read_blocks(Fd,<<>>).

read_blocks(Fd,Acc) ->
    case read_block(Fd) of
	{ok,Bin} -> read_blocks(Fd,<<Acc/binary,Bin/binary>>);
	terminator -> {ok, Acc};
	Error -> Error
    end.


write(Fd, IMG) ->
    ok.

read_palette(Fd, 0, Pixel, Palette) -> 
    Palette;
read_palette(Fd, 1, Pixel, DefaultMap) ->
    Sz = (1 bsl Pixel),
    case file:read(Fd, Sz*3) of
	{ok, Bin} -> 
	    palette(Bin, [], Sz)
    end.

palette(Bin, Map, 0) -> 
    reverse(Map);
palette(<<R:8,G:8,B:8, Bin/binary>>, Map, I) ->
    palette(Bin, [{R*255,G*255,B*255} | Map], I-1).


read_pixels(Fd,Pix0,RowFun,St0,Width,Height,Interlaced) ->
    case file:read(Fd, 1) of
	{ok, <<LZWCodeSize>>} ->
	    case read_image(Fd,LZWCodeSize,Width,Height) of
		{ok,Data} ->
		    case Interlaced of
			1 ->
			    interlaced_data(Data,Pix0,RowFun,St0,Width,Height);
			0 ->
			    raw_data(Data,Pix0,RowFun,St0,0,Width)
		    end;
		Error ->
		     Error
	    end;
	Error ->
	    Error
    end.

read_image(Fd, LZWCodeSize, Width, Height) ->
    case read_blocks(Fd) of
	{ok,Bin} ->
	    ?dbg("LZWCodeSize=~p compressed=~p\n", [LZWCodeSize, size(Bin)]),
	    {ok,lzw:decompress_gif(Bin, LZWCodeSize)};
	Error ->
	    Error
    end.


%%
%% Read raw data
%%
raw_data(Bin,Pix,RowFun,St0,Ri,Width) ->
    case Bin of
	<<Row:Width/binary, Bin1/binary>> ->
	    St1 = RowFun(Pix,Row,Ri,St0),
	    raw_data(Bin1,Pix,RowFun,St1,Ri+1,Width);
	<<>> ->
	    {ok, Pix#erl_pixmap { pixels = St0 }}
    end.

%% Read interlaced data
%%
%% 0  R1a
%% 1                 R4a
%% 2            R3a           
%% 3                 R4b
%% 4       R2a
%% 5                 R4c
%% 6            R3b
%% 7                 R4d
%% ...

interlaced_data(Bin,Pix,RowFun,St0,Width,Height) ->
    {St1,Bin1} = raster_data(Bin, Pix,RowFun,St0, Height,0,8, Width),
    {St2,Bin2} = raster_data(Bin1,Pix,RowFun,St1, Height,4,8, Width),
    {St3,Bin3} = raster_data(Bin2,Pix,RowFun,St2, Height,2,4, Width),
    {St4,Bin4} = raster_data(Bin3,Pix,RowFun,St3, Height,1,2, Width),
    {ok, Pix#erl_pixmap{ pixels = St4 }}.

raster_data(Bin,Pix,RowFun,St,Height,Ri,Rs,Width) when Ri >= Height ->
    {St, Bin};
raster_data(Bin,Pix,RowFun,St0,Height,Ri,Rs,Width) ->
    <<Row:Width/binary, Bin1/binary>> = Bin,
    St1 = RowFun(Pix,Row,Ri,St0),
    raster_data(Bin1,Pix,RowFun,St1,Height,Ri+Rs,Rs,Width).
    



	    

