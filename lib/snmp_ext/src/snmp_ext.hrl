%%
%% Int to bytes
%%
-define(int8(X), [(X) band 16#ff]).

-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int24(X), [((X) bsr 16) band 16#ff,
                   ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int32(X), 
        [((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
         ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int64(X), 
        [((X) bsr 56) band 16#ff, ((X) bsr 48) band 16#ff,
	 ((X) bsr 40) band 16#ff, ((X) bsr 32) band 16#ff,
	 ((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
         ((X) bsr 8) band 16#ff, (X) band 16#ff]).

%% Bytes to unsigned
-define(u64(X7,X6,X5,X4X3,X2,X1,X0), 
	(((X7) bsl 56) bor ((X6) bsl 48) bor ((X5) bsl 40) bor ((X4) bsl 32) bor
	 ((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u24(X2,X1,X0),
        (((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u16(X1,X0),
        (((X1) bsl 8) bor (X0))).
 
-define(u8(X0), (X0)).

%% Bytes to signed
-define(i32(X3,X2,X1,X0),
        (?u32(X3,X2,X1,X0) - 
         (if (X3) > 127 -> 16#100000000; true -> 0 end))).

-define(i24(X2,X1,X0),
        (?u24(X2,X1,X0) - 
         (if (X2) > 127 -> 16#1000000; true -> 0 end))).
        
-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(i8(X0),
        (?u8(X0) -
         (if (X0) > 127 -> 16#100; true -> 0 end))).

-define(tst_flag(N, Flags), ((N band Flags) == N)).
	       
