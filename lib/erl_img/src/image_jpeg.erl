%%% File    : img_jpg.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : JPG image processing (Exif/JPG files)
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_jpeg).

-include("erl_img.hrl").

-include("jpeg.hrl").
-include("tiff.hrl").
-include("exif.hrl").

-include("api.hrl").
%% -define(debug, true).
-include("dbg.hrl").

%% YCbCr => RGB
-define(R(Y,Cb,Cr), (Y + (1.402)*((Cr)-128))).
-define(G(Y,Cb,Cr), (Y - 0.34414*((Cb)-128) - 0.71414*((Cr)-128))).
-define(B(Y,Cb,Cr), (Y + 1.772*(Cb-128))).

%% RGB => YCbCr
-define(Y(R,G,B), (0.299*(R) + 0.587*(G) + 0.114*(B))).
-define(Cb(R,G,B), (0.1687*(R) - 0.3313*(G) + 0.5*(B) + 128)).
-define(Cr(R,G,B), (0.5*R - 0.4187*(G) - 0.0813*(B) + 128)).



magic(<<?M_SOI:16,?M_APP1:16,Len:16,"Exif",0,0,_/binary>>) -> true;
magic(<<?M_SOI:16,?M_JFIF:16,Len:16,"JFIF",_,_,_/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/jpeg".

extensions() -> [".jpeg", ".jpg"].
    

read_info(Fd) ->
    case file:read(Fd, 2) of
	{ok, <<?M_SOI:16>>} ->
	    read_sections(Fd, 
			  #erl_image { type = ?MODULE,
				     order = upper_left
				    });
	{ok,_} ->
	    {error, bad_magic};
	Error -> Error
    end.

write_info(Fd, IMG) ->
    ok.


read(Fd,IMG) ->
    {ok,IMG}.

read(Fd,IMG,RowFun,St0) ->
    {ok,IMG}.

write(Fd,IMG) ->
    ok.


read_sections(Fd, IMG) ->
    case file:read(Fd, 4) of
	eof -> 
	    {ok,IMG};
	{ok,<<Marker:16,Len:16>>} ->
	    read_section(Fd,Marker,Len-2,IMG);
	{ok,_} ->
	    {error, bad_file};
	Error -> Error
    end.

read_section(Fd,Marker,Len,IMG) ->
    if Marker == ?M_SOS -> {ok,IMG};
       Marker == ?M_EOI -> {ok,IMG};	
       Marker == ?M_COM ->
	    case file:read(Fd, Len) of
		{ok,Bin} ->
		    read_sections(Fd, IMG#erl_image {comment=
						   binary_to_list(Bin)});
		Error ->
		    {error, bad_file}
	    end;
       Marker == ?M_APP1 ->
	    case file:read(Fd, Len) of
		{ok,<<"Exif",0,0,Bin/binary>>} ->
		    read_sections(Fd, process_exif(Bin,IMG));
		{ok,_} ->
		    read_sections(Fd, IMG)
	    end;
       Marker == ?M_SOF0;
       Marker == ?M_SOF1;
       Marker == ?M_SOF2;
       Marker == ?M_SOF3;
       Marker == ?M_SOF5;
       Marker == ?M_SOF6;
       Marker == ?M_SOF7;
       Marker == ?M_SOF9;
       Marker == ?M_SOF10;
       Marker == ?M_SOF11;
       Marker == ?M_SOF13;
       Marker == ?M_SOF14;
       Marker == ?M_SOF15 ->
	    case file:read(Fd, Len) of
		{ok,Bin} ->
		    read_sections(Fd, process_sofn(Bin,IMG));
		Error ->
		    Error
	    end;
       true ->
	    file:position(Fd, {cur,Len}),
	    read_sections(Fd, IMG)
    end.

process_sofn(<<Depth:8,Height:16,Width:16,Components:8,Bin/binary>>, IMG) ->
    IMG#erl_image { depth  = Depth,
		  height = Height,
		  width  = Width }.

%% Maker OLYMP
collect_olymp(Fd, T, St) ->
    Key = erl_img:hex16(T#tiff_entry.tag),
    ?dbg("OLYMP(~s) ~p ~p ~p\n", 
	[T#tiff_entry.ifd,Key,T#tiff_entry.type, T#tiff_entry.value]),
    St.

%% Maker Nikon
collect_nikon(Fd, T, St) ->
    Key = erl_img:hex16(T#tiff_entry.tag),
    ?dbg("Nikon(~s) ~p ~p ~p\n", 
	[T#tiff_entry.ifd,Key,T#tiff_entry.type, T#tiff_entry.value]),
    St.

%% Maker FUJIFILM
collect_fujifilm(Fd, T, St) ->
    Key = erl_img:hex16(T#tiff_entry.tag),
    ?dbg("Fujifilm(~s) ~p ~p ~p\n", 
	[T#tiff_entry.ifd,Key,T#tiff_entry.type, T#tiff_entry.value]),
    St.

%% Maker Sony DSC
collect_sony(Fd, T, St) ->
    Key = erl_img:hex16(T#tiff_entry.tag),
    ?dbg("Sony(~s) ~p ~p ~p\n", 
	[T#tiff_entry.ifd,Key,T#tiff_entry.type, T#tiff_entry.value]),
    St.    

%% Maker other
collect_other(Fd, T, St) ->
    Key = erl_img:hex16(T#tiff_entry.tag),
    ?dbg("Maker(~s) ~p ~p ~p\n", 
	[T#tiff_entry.ifd,Key,T#tiff_entry.type, T#tiff_entry.value]),
    St.

collect_maker(Fd, T, St) ->
    {ok, St}.

collect_maker_fixme(Fd, T, St) ->
    ?dbg("Tif entry=~p\n", [T]),
    MakerBin = T#tiff_entry.value,
    case MakerBin of
	<<"OLYMP",0,1,0,_/binary>> ->
	    image_tiff:scan_ifd(Fd,
				[$0,$:|T#tiff_entry.ifd],
				T#tiff_entry.offs+8,
				T#tiff_entry.endian,
				fun collect_olymp/3, St);
	<<"Nikon",0,1,0,_/binary>> ->
	    image_tiff:scan_ifd(Fd,
				[$0,$:|T#tiff_entry.ifd],
				T#tiff_entry.offs+8,
				T#tiff_entry.endian,
				fun collect_nikon/3, St);
	<<"SONY DSC ",0,0,0,_/binary>> ->
	    %% NOT working - what is SONY doing ?
	    image_tiff:scan_ifd(Fd,
				[$0,$:|T#tiff_entry.ifd],
				T#tiff_entry.offs+14,
				T#tiff_entry.endian,
				fun collect_sony/3, St);
	<<"FUJIFILM",Offset:32/little>> ->
	    image_tiff:scan_ifd_bin(MakerBin, 
				    [$0,$:|T#tiff_entry.ifd],
				    Offset, little,
				    fun collect_fujifilm/3, St);
	_ ->
	    image_tiff:scan_ifd(Fd,
				[$0,$:|T#tiff_entry.ifd],
				T#tiff_entry.offs+8,
				T#tiff_entry.endian,
				fun collect_other/3, St)
    end.


collect_exif(Fd, T, St) ->
    Key = exif:decode_tag(T#tiff_entry.tag),
    ?dbg("EXIF(~s) ~p ~p ~p\n", 
	[T#tiff_entry.ifd,Key,T#tiff_entry.type, T#tiff_entry.value]),
    case T#tiff_entry.tag of
	?ExifInteroperabilityOffset ->
	    [Offset] = T#tiff_entry.value,
	    %% could be handle by a collect_interop?
	    case image_tiff:scan_ifd(Fd, [$0,$.|T#tiff_entry.ifd],
				     Offset, T#tiff_entry.endian,
				     fun collect_exif/3, St) of
		{ok, St1} ->
		    St1;
		Error ->
		    St
	    end;
	?MakerNote ->
	    case collect_maker(Fd, T, St) of
		{ok,St1} ->
		    St1;
		Error ->
		    St
	    end;
	_ ->
	    St
    end.


%% Image info collector functions
collect_tiff(Fd, T, St) ->
    Key = image_tiff:decode_tag(T#tiff_entry.tag),
    ?dbg("TIFF(~s) ~p ~p ~p\n", 
	[T#tiff_entry.ifd,Key,T#tiff_entry.type, T#tiff_entry.value]),
    case T#tiff_entry.tag of
	?ImageWidth ->
	    [Width] = T#tiff_entry.value,
	    St#erl_image { width = Width };
	?ImageLength ->
	    [Length] = T#tiff_entry.value,
	    St#erl_image { height = Length };
	?BitsPerSample ->
	    Bs = T#tiff_entry.value,
	    St#erl_image { depth = lists:sum(Bs) };
	?ImageDescription ->
	    [Value] = T#tiff_entry.value,
	    St#erl_image { comment = Value };
	?DateTime ->
	    [Value] = T#tiff_entry.value,
	    case string:tokens(Value, ": ") of
		[YYYY,MM,DD,H,M,S] ->
		    DateTime = {{list_to_integer(YYYY),
				 list_to_integer(MM),
				 list_to_integer(DD)},
				{list_to_integer(H),
				 list_to_integer(M),
				 list_to_integer(S)}},
		    St#erl_image { itime = DateTime};
		_ ->
		    St
	    end;
	?ExifOffset ->
	    [Offset] = T#tiff_entry.value,
	    case image_tiff:scan_ifd(Fd, [$0,$.|T#tiff_entry.ifd],
				     Offset, T#tiff_entry.endian,
				     fun collect_exif/3, St) of
		{ok, St1} ->
		    St1;
		Error ->
		    St
	    end;
	_ ->
	    Value = T#tiff_entry.value,
	    As = St#erl_image.attributes,
	    St#erl_image { attributes = [{Key,Value}|As]}
    end.

process_exif(Bin, IMG) ->
    case image_tiff:scan_binary(Bin, fun collect_tiff/3, IMG) of
	{ok, IMG1} ->
	    IMG1;
	Error ->
	    IMG
    end.
