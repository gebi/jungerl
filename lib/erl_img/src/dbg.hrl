
-ifdef(debug).
-define(dbg(Fmt,Args), io:format((Fmt),(Args))).
-else.
-define(dbg(Fmt,Args), ok).
-endif.
