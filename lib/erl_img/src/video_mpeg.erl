%%% File    : video_mpeg.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : MPEG image processing
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(video_mpeg).

-include("erl_img.hrl").
-include("api.hrl").

magic(_) -> false.

mime_type() -> "video/mpeg".

extensions() -> [".mpg", ".mpeg"].

read_info(Fd) ->
    {error, bad_magic}.


write_info(Fd, IMG) ->
    {error, bad_image}.

read(Fd,IMG) ->
    {error, bad_image}.

read(Fd,IMG,PixFun,PixSt) ->
    {error, bad_image}.

write(Fd,IMG) ->
    {error, bad_image}.



