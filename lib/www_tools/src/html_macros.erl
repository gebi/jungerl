-module(html_macros).

-export([dp/1]).

dp([{raw,[H|T]}]) ->
    "<P><B><I><FONT COLOR=\"#COCO\"><FONT SIZE=+2>" ++ [H] ++
    "</FONT>" ++ T ++ "</FONT></I></B>".
