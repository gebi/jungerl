-module(m1).
-author('etxuwig@etxb.ericsson.se').

-compile(export_all).
%%-export([Function/Arity, ...]).

'#inheritance#'() -> [m2].
