%%% File    : image_undef.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Undefined format catch module
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_undef).

-include("erl_img.hrl").
-inlcude("api.hrl").

magic(_) -> false.

mime_type() -> "".

extensions() -> [].

    
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


