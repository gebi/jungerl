%%% File    : image_gif.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : GIF image processing
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_gif).

-include("erl_img.hrl").

-include("api.hrl").

%% -define(debug, true).
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
    %% Should version be configurable?
    file:write(Fd, <<?MAGIC89,
		    (IMG#erl_image.width):16/little-unsigned-integer,
		    (IMG#erl_image.height):16/little-unsigned-integer>>).


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
	{ok, <<_Width:16/little, _Hight:16/little,
	      Map:1, _Cr:3, Sort:1, Pix:3,
	      Background:8,
	      AspectRatio:8>>} ->
	    Palette = read_palette(Fd, Map, Pix+1),
	    ?dbg("sizeof(palette)=~p Map=~w, Cr=~w, Sort=~w, Pix=~w\n", 
		 [length(Palette),Map,Cr,Sort,Pix]),
	    ?dbg("Background=~w, AspectRatio=~w\n",
		 [Background, AspectRatio]),
	    As = [{'Background',Background},
		  {'AspectRatio',AspectRatio},
		  {'Sort',Sort} | IMG#erl_image.attributes],
	    IMG1 = IMG#erl_image { palette = Palette, attributes = As},
	    read_data(Fd, IMG1, RowFun, St0, []);
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
		    Palette = read_palette(Fd, Map, Pix+1),
		    As1 = [{'Interlaced', Interlaced},
			   {'Sort', Sort} | As],
		    Pixmap0 =
			#erl_pixmap { top = Top,
				      left = Left,
				      width = Width,
				      height = Height,
				      format = IMG#erl_image.format,
				      palette = Palette,
				      attributes = As1
				     },
		    ?dbg("pix=~w, sizeof(palette)=~p\n", [Pix,Palette]),
		    case read_pixels(Fd,Pixmap0,RowFun,St0,
				     Width,Height,Interlaced) of
			{ok, Pixmap1} ->
			    %% ?dbg("Pixmap = ~p\n", [Pixmap2]),
			    Ps = IMG#erl_image.pixmaps ++ [Pixmap1],
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
	{ok, _} ->
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
	       _GridWidth:16/little, _GridHeight:16/little,
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



read_palette(_Fd, 0, _Pixel) -> 
    undefined;
read_palette(Fd, 1, Pixel) ->
    Sz = (1 bsl Pixel),
    case file:read(Fd, Sz*3) of
	{ok, Bin} -> 
	    rd_palette(Bin, [], Sz)
    end.


rd_palette(_Bin, Map, 0) -> 
    reverse(Map);
rd_palette(<<R:8,G:8,B:8, Bin/binary>>, Map, I) ->
    rd_palette(Bin, [{R,G,B} | Map], I-1).



write(Fd, IMG) ->
    write_info(Fd, IMG),
    Palette = IMG#erl_image.palette,
    Background = attribute('Background',IMG#erl_image.attributes,0),
    AspectRatio = attribute('AspectRatio', IMG#erl_image.attributes,0),
    if list(Palette) ->
	    PLen = length(Palette),
	    ColorRes = if PLen > 0, PLen =< 256 ->
			       trunc(math:log(PLen)/math:log(2))+1;
			  PLen > 0 ->
			       8;
			  true -> 
			       1
		       end,
	    Map = 1,
	    Cr = ColorRes - 1,
	    Sort = 0,
	    Pix = ColorRes - 1,
	    file:write(Fd, <<Map:1, Cr:3, Sort:1, Pix:3>>),
	    file:write(Fd, <<Background:8, AspectRatio:8>>),	    
	    write_palette(Fd, IMG#erl_image.palette, Pix+1);
       true ->
	    Map = 0,
	    Cr = 0,
	    Sort = 0,
	    Pix = 0,
	    file:write(Fd, <<Map:1, Cr:3, Sort:1, Pix:3>>),
	    file:write(Fd, <<Background:8, AspectRatio:8>>)	    
    end,
    write_data(Fd, IMG),
    file:write(Fd, <<?TRAILER>>).


write_palette(Fd, Map, Pixel) ->
    wr_palette(Fd, Map, (1 bsl Pixel)).

wr_palette(_Fd, _, 0) -> ok;
wr_palette(Fd, [{R,G,B}|Map], I) ->
    file:write(Fd, <<R:8, G:8, B:8>>),
    wr_palette(Fd, Map, I-1);
wr_palette(Fd, [], I) ->
    file:write(Fd, <<0:8, 0:8, 0:8>>),
    wr_palette(Fd, [], I-1).

write_data(Fd, IMG) ->
    write_pixmaps(Fd, IMG, IMG#erl_image.pixmaps).

write_pixmaps(Fd, IMG, [Pm|Pms]) ->
    DisposalMethod = attribute('DisposalMethod',Pm#erl_pixmap.attributes, 0),
    UserInput = attribute('UserInput', Pm#erl_pixmap.attributes, 0),
    DelayTime = attribute('DelayTime', Pm#erl_pixmap.attributes, 0),
    Transparent = attribute('Transparent', Pm#erl_pixmap.attributes, 0),
    TransparentColor = attribute('TransparentColor', 
				 Pm#erl_pixmap.attributes, 0),
    file:write(Fd, <<?EXTENSION, ?CTL_EXTENSION>>),
    write_blocks(Fd, <<0:3, DisposalMethod:3, 
		      UserInput:1, Transparent:1, 
		      DelayTime:16/unsigned-little,
		      TransparentColor:8>>),
    write_image(Fd, Pm),
    write_pixmaps(Fd, IMG, Pms);
write_pixmaps(_Fd, _IMG, []) ->
    ok.


write_image(Fd, Pm) ->
    file:write(Fd, <<?IMAGE>>),
    file:write(Fd,
	       <<(Pm#erl_pixmap.left):16/little,
		(Pm#erl_pixmap.top):16/little,
		(Pm#erl_pixmap.width):16/little,
		(Pm#erl_pixmap.height):16/little>>),
    Palette = Pm#erl_pixmap.palette,
    Interlaced = attribute('Interlaced', Pm#erl_pixmap.attributes, 0),
    %% Special code for none compressed data!!!
    Inline     = attribute('Inline', Pm#erl_pixmap.attributes, 0),
    if list(Palette) ->
	    PLen = length(Palette),
	    ColorRes = if PLen > 0, PLen =< 256 ->
			       trunc(math:log(PLen)/math:log(2))+1;
			  PLen > 0 ->
			       8;
			  true -> 
			       1
		       end,
	    Sort = 0,
	    Pix = ColorRes - 1,
	    Map = 1,
	    file:write(Fd, <<Map:1, Interlaced:1, Sort:1, 0:2, Pix:3>>),
	    write_palette(Fd, Palette, Pix+1);
       true ->
	    Sort = 0,
	    Pix = 0,
	    Map = 0,
	    file:write(Fd, <<Map:1, Interlaced:1, Sort:1, 0:2, Pix:3>>)
    end,
    write_pixels(Fd,
		 Pm#erl_pixmap.pixels, 
		 Pm#erl_pixmap.width, 
		 Pm#erl_pixmap.height, Interlaced, Inline).

write_pixels(Fd, Pixels, Width, Height, Interlaced, Inline) ->
    Bin = collect_pixels(Pixels, Width, Height, Interlaced),
    {LZWCodeSize, Bin1} = 
	if Inline == 1 ->
		%% FIXME: check that all pixels are 7 bit !!!!!
		{7,<<128, Bin/binary, 129>>};
	   true ->
		lzw:compress_gif(Bin)
	end,
    ?dbg("compress: orig_size=~w, size=~w codesize=~w\n",
	 [size(Bin), size(Bin1), LZWCodeSize]),
    file:write(Fd, <<LZWCodeSize>>),
    write_blocks(Fd, Bin1).

%%
%% Fixme check that all rows are present and
%% implement interlaced order
%%
collect_pixels(Rows, Width, Height, Interlaced) ->
    SortedRows = lists:sort(Rows),
    if Interlaced == 1 ->
	    collect_interlaced(SortedRows,Width,Height,[],[],[],[]);
       true ->
	    collect_raw(SortedRows,Width,Height,[])
    end.

collect_raw([{Ri,Row} | Rows], Width, Height,Acc) when Ri < Height ->
    Sz = size(Row),
    R = if Sz > Width ->
		%% remove pixels
		<<Bin:Width/binary, _/binary>> = Row,
		Bin;
	   Sz < Width ->
		%% add pixels
		<<Row/binary, 
		 (list_to_binary(lists:duplicate(Width-Sz,0)))/binary>>;
	   true ->
		Row
	end,
    collect_raw(Rows, Width, Height, [R | Acc]);
collect_raw([{_Ri,_Row} | Rows], Width, Height, Acc) ->
    %% ignore line out of range
    collect_raw(Rows, Width, Height, Acc);
collect_raw([], _Width, _Height, Acc) ->
    list_to_binary(reverse(Acc)).

collect_interlaced([{Ri,Row}|Rows],Width,Height,R1,R2,R3,R4) ->
    case Ri band 7 of
	0 -> collect_interlaced(Rows,Width,Height,[Row|R1],R2,R3,R4);
	1 -> collect_interlaced(Rows,Width,Height,R1,R2,R3,[Row|R4]);
	2 -> collect_interlaced(Rows,Width,Height,R1,R2,[Row|R3],R4);
	3 -> collect_interlaced(Rows,Width,Height,R1,R2,R3,[Row|R4]);
	4 -> collect_interlaced(Rows,Width,Height,R1,[Row|R2],R3,R4);
	5 -> collect_interlaced(Rows,Width,Height,R1,R2,R3,[Row|R4]);
	6 -> collect_interlaced(Rows,Width,Height,R1,R2,[Row|R3],R4);
	7 -> collect_interlaced(Rows,Width,Height,R1,R2,R3,[Row|R4])
    end;
collect_interlaced([],_Width,_Height,R1,R2,R3,R4) ->
    list_to_binary([reverse(R1),reverse(R2),reverse(R3),reverse(R4)]).

    
    
write_blocks(Fd, Bin) ->
    write_blocks(Fd, Bin, 0, size(Bin)).

write_blocks(Fd, Bin, Pos, Size) ->
    Sz = Size - Pos,
    if Sz > 255 ->
	    <<_:Pos/binary, Block:255/binary, _/binary>> = Bin,
	    file:write(Fd, <<255, Block/binary>>),
	    write_blocks(Fd, Bin, Pos+255, Size);
       true ->
	    <<_:Pos/binary, Block:Sz/binary, _/binary>> = Bin,
	    file:write(Fd, <<Sz, Block/binary>>),
	    file:write(Fd, <<0>>)
    end.


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

read_image(Fd, LZWCodeSize, _Width, _Height) ->
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
    {St4,_Bin4} = raster_data(Bin3,Pix,RowFun,St3, Height,1,2, Width),
    {ok, Pix#erl_pixmap{ pixels = St4 }}.

raster_data(Bin,_Pix,_RowFun,St,Height,Ri,_Rs,_Width) when Ri >= Height ->
    {St, Bin};
raster_data(Bin,Pix,RowFun,St0,Height,Ri,Rs,Width) ->
    <<Row:Width/binary, Bin1/binary>> = Bin,
    St1 = RowFun(Pix,Row,Ri,St0),
    raster_data(Bin1,Pix,RowFun,St1,Height,Ri+Rs,Rs,Width).
    

attribute(Name, List, Default) ->
    case lists:keysearch(Name, 1, List) of
	false ->
	    Default;
	{value,{_,Value}} ->
	    Value
    end.

	    


	    

