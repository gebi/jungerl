-module(m2).
-author('etxuwig@etxb.ericsson.se').

-compile(export_all).
%%-export([Function/Arity, ...]).

'#inheritance#'() -> [m3,m4].
