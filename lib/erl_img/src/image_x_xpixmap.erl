%%% File    : image_x_xpixmap.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : XPM image processing
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_x_xpixmap).

-include("erl_img.hrl").
-include("api.hrl").
-include("dbg.hrl").

-export([scan_xpm/1]).

-import(lists,[reverse/1]).
-define(upper(C), (C)>=$A, (C)=<$Z).
-define(lower(C), (C)>=$a, (C)=<$z).
-define(digit(C), (C)>=$0, (C)=<$9).

-define(ID1(C), ?upper(C); ?lower(C); C==$_).
-define(ID(C), ?upper(C); ?lower(C); ?digit(C); C==$_).

%% Read magic info check MAGIC type and width and height (depth)
%% of image

magic(<<"/* XPM */\n", _Data/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/x-xpixmap".

extensions() -> [".xpm"].


read_info(Fd) ->
    case file:read(Fd, 10) of
	{ok,Bin} ->
	    case magic(Bin) of
		true ->
		    case scan_xpm(Fd,fun({string,_}) -> false;
					(_) -> true
				     end) of
			{ok,Ts} ->
			    [{string,Fmt}|_] = reverse(Ts),
			    {ok,[Width,Height,_NColors,_CharPerPixel],_Rest} = 
				io_lib:fread("~d ~d ~d ~d",Fmt),
			    {ok,#erl_image {
			       type   = ?MODULE,
			       width  = Width,
			       height = Height,
			       format = palette8,
			       order  = left_to_right,
			       depth  = 8 }};
			Error ->
			    Error
		    end;
		false ->
		    {error, bad_magic}
	    end;
	Error ->
	    Error
    end.


write_info(_Fd, _IMG) ->
    ok.


read(Fd, IMG, RowFun, St0) ->
    case scan_xpm(Fd) of
	{ok,Ts} ->
	    case parse_xpm(Ts) of
		{ok,Strings} ->
		    tr_rows(IMG,Strings,RowFun, St0);
		Error -> Error
	    end;
	Error -> Error
    end.
    
read(Fd,IMG) ->
    read(Fd, IMG, 
	 fun(_, Row, Ri, St) -> 
		 ?dbg("xpm: read row ~p\n", [Ri]),
		 [{Ri,Row}|St] end, 
	 []).

write(_Fd,_IMG) ->
    ok.

tr_rows(IMG,[Fmt|Data],RowFun,St) ->
    {ok,[Width,Height,NColors,CharPerPixel],_Rest} = 
	io_lib:fread("~d ~d ~d ~d",Fmt),
    {Colors,Data1} = tr_colors(Data,NColors,[],CharPerPixel),
    ColorMap = color_tab(Colors),
    IMG1 = IMG#erl_image { palette = ColorMap },
    PIX0 = #erl_pixmap { height = Height,
			 width = Width,
			 format = IMG1#erl_image.format,
			 palette = ColorMap },
    case tr_pixels(Data1,Colors,PIX0,0,Height,RowFun,St) of
	{ok, PIX1} ->
	    {ok, IMG1#erl_image { pixmaps = [ PIX1]}};
	Error  ->
	    Error
    end.

tr_pixels([Line|Ls],Colors,PIX,Ri,NRows,RowFun,St0) when Ri =/= NRows ->
    Row = tr_line(Line, [], Colors),
    St1 = RowFun(PIX,Row,Ri,St0),
    tr_pixels(Ls, Colors, PIX, Ri+1, NRows, RowFun, St1);
tr_pixels([],_Colors,PIX,NRows,NRows,_RowFun,St0) ->
    {ok, PIX#erl_pixmap { pixels = St0 }};
tr_pixels(_, _Colors, _IMG, _, _, _RowFun, _St0) ->
    {error, bad_data}.
    

color_tab([{_, " c #"++Hex} | Cs]) ->
    Color = hex_to_integer(Hex),
    R = (Color bsr 16) band 16#ff,
    G = (Color bsr 8) band 16#ff,
    B = (Color bsr 0) band 16#ff,
    [ {R,G,B} | color_tab(Cs)];
color_tab([{_, " c "++Name} | Cs]) ->
    [ Name | color_tab(Cs)];
color_tab([]) -> [].

hex_to_integer(Cs) ->
    hex_to_integer(Cs, 0).

hex_to_integer([C|Cs], N) when C >= $0, C =< $9 ->
    hex_to_integer(Cs, (N bsl 4) + (C - $0));
hex_to_integer([C|Cs], N) when C >= $a, C =< $f ->
    hex_to_integer(Cs, (N bsl 4) + ((C - $a)+10));
hex_to_integer([C|Cs], N) when C >= $A, C =< $F ->
    hex_to_integer(Cs, (N bsl 4) + ((C - $A)+10));
hex_to_integer([], N)  -> N.
    
    
%%
%% Translate a line into color index binary line
%%

tr_line([], Acc, _Colors) ->
    list_to_binary(reverse(Acc));
tr_line(Line, Acc, Colors) ->
    {Line1,Index} = tr_pixel(Colors, Line, 1),
    tr_line(Line1, [Index|Acc], Colors).

tr_pixel([{Chars,_}|Cs], Line, I) ->
    case lists:prefix(Chars,Line) of
	true -> {lists:nthtail(length(Chars),Line), I};
	false -> tr_pixel(Cs, Line, I+1)
    end.

tr_colors(Data, 0, Acc, _Cpp) ->
    {reverse(Acc), Data};
tr_colors([Color|Cs], N, Acc, Cpp) ->
    {Chars,Spec} = get_color(Color,[],Cpp),
    tr_colors(Cs, N-1, [{Chars,Spec}|Acc], Cpp).

get_color(Cs, Acc, 0) -> {reverse(Acc), Cs};
get_color([C|Cs],Acc,N) -> get_color(Cs,[C|Acc],N-1).


%% parse the xpm file 
parse_xpm([{id,"static"},{id,"char"},'*',{id,_Name},'[', ']','=', '{' | Ts]) ->
    parse_xpm(Ts, []);
parse_xpm(Ts) ->
    {error,{bad_format,Ts}}.

parse_xpm([{string,Str},',' | Ts], Acc) ->
    parse_xpm(Ts, [Str|Acc]);
parse_xpm([{string,Str},'}',';'], Acc) ->
    {ok, reverse([Str|Acc])};
parse_xpm(['}', ';'], Acc) ->
    {ok, reverse(Acc)};
parse_xpm(Ts,_) ->
    {error, {bad_format,Ts}}.

scan_xpm(Fd) ->
    scan_xpm(Fd, fun(_) -> true end).

scan_xpm(Fd, While) ->
    more(Fd, fun(Cs) -> scan_xpm(Cs,Fd,[],While) end, fun() -> {ok,[]} end).

%% scan xpm (C subset) tokens
scan_xpm([C|Cs], Fd, Ts, W) ->
    if C == $\s; C == $\t; C == $\n; C == $\r ->
	    scan_xpm(Cs,Fd,Ts,W);
       C == $" ->
	    scan_string(Cs,Fd,Ts,W);
       ?ID1(C) ->
	    scan_id(Cs,Fd,[C],Ts,W);
       C == $/ ->
	    scan_comment(Cs,Fd,Ts,W);
       C == $*; C == $[; C == $]; C == ${; C == $};
       C == $,; C == $=; C == $; ->
	    next(Cs,Fd,list_to_atom([C]),Ts,W);
       true ->
	    {error,{bad_format,[C|Cs]}}
    end;
scan_xpm([],Fd,Ts,W) ->
    more(Fd, fun(Cs) -> scan_xpm(Cs,Fd,Ts,W) end,
	 fun() -> {ok,reverse(Ts)} end).
	    

%% seen /
scan_comment([$*|Cs],Fd,Ts,W) -> scan_c1(Cs,Fd,Ts,W);
scan_comment([C|Cs],Fd,Ts,W)  -> scan_xpm([C|Cs],Fd,['/'|Ts],W);
scan_comment([],Fd,Ts,W) ->
    more(Fd, fun(Cs) -> scan_comment(Cs,Fd,Ts,W) end,
	 fun() -> {error,bad_format} end).
%% seen /*
scan_c1([$*|Cs],Fd,Ts,W) -> scan_c2(Cs,Fd,Ts,W);
scan_c1([_|Cs],Fd,Ts,W) ->  scan_c1(Cs,Fd,Ts,W);
scan_c1([],Fd,Ts,W) ->
    more(Fd, fun(Cs) -> scan_c1(Cs,Fd,Ts,W) end,fun() -> {error,comment} end).

%% seen /*... *
scan_c2([$/|Cs],Fd,Ts,W) -> scan_xpm(Cs,Fd,Ts,W);
scan_c2([C|Cs],Fd,Ts,W) -> scan_c1([C|Cs],Fd,Ts,W);
scan_c2([],Fd,Ts,W) ->
    more(Fd, fun(Cs) -> scan_c2(Cs,Fd,Ts,W) end,fun() -> {error,comment} end).
	 
scan_string(Cs,Fd,Ts,W) ->
    scan_string(Cs,Fd,[],Ts,W).
    
scan_string([$\\,C | Cs],Fd,Str,Ts,W) ->
    scan_string(Cs,Fd,[C,$\\|Str],Ts,W);
scan_string([$" | Cs],Fd,Str,Ts,W) ->
    next(Cs,Fd,{string,reverse(Str)},Ts,W);
scan_string([C|Cs],Fd, Str, Ts,W) ->
    scan_string(Cs,Fd,[C|Str],Ts,W);
scan_string(Cs1,Fd,Str,Ts,W) ->
    more(Fd, fun(Cs) -> scan_string(Cs1++Cs,Fd,Str,Ts,W) end,
	 fun() -> {error,string} end).

scan_id([C|Cs],Fd,ID,Ts,W) when ?ID(C) ->
    scan_id(Cs,Fd,[C|ID],Ts,W);
scan_id([],Fd,ID,Ts,W) ->
    more(Fd, fun(Cs) -> scan_id(Cs,Fd,ID,Ts,W) end,
	 fun() -> next([],Fd,{id,reverse(ID)},Ts,W) end); 
scan_id(Cs,Fd,ID,Ts,W) ->
    next(Cs,Fd,{id,reverse(ID)},Ts,W).


next(Cs,Fd,Token,Ts,While) ->
    case While(Token) of
	true -> scan_xpm(Cs,Fd,[Token|Ts],While);
	false -> {ok,reverse([Token|Ts])}
    end.


more(Fd,Fun,Eof) ->
    case file:read(Fd,64) of
	{ok,Data} -> 
	    Fun(binary_to_list(Data));
	eof -> 
	    Eof();
	Error -> 
	    Error
    end.


