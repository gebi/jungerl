-ifndef(SYSBITS_HRL).
-define(SYSBITS_HRL, true).

-define(uint8(X,E),  X:8/E-unsigned-integer).
-define(uint16(X,E), X:16/E-unsigned-integer).
-define(uint32(X,E), X:32/E-unsigned-integer).
-define(uint64(X,E), X:64/E-unsigned-integer).

-define(int8(X,E),  X:8/E-integer).
-define(int16(X,E), X:16/E-integer).
-define(int32(X,E), X:32/E-integer).
-define(int64(X,E), X:64/E-integer).

%% "standard" 32 bit machine defs
-define(char(X,S,E),      X:8/E-S-integer).
-define(short(X,S,E),     X:16/E-S-integer).
-define(long(X,S,E),      X:32/E-S-integer).
-define(long_long(X,S,E), X:64/E-S-integer).
-define(int(X,S,E),       X:32/E-S-integer).
-define(float(X,E),       X:32/E-float).
-define(double(X,E),      X:64/E-float).

-endif.
