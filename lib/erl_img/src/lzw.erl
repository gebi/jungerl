%% File    : img_lzw.erl
%%% Author  : Dan Gudmundsson
%%%           Tony Rogvall <tony@bit.hemma.se>
%%%
%%% Description : LZW compresion
%%% Created : 26 Mar 2003 by Tony Rogvall <tony@bit.hemma.se>
%%%
%%% lzw derived from wings e3d__tiff updated and fixed to
%%%  handle gif and tiff fillorder=2
%%%
-module(lzw).

-export([decompress_tiff/1,
	 decompress_tiff/3,
	 decompress_gif/1,
	 decompress_gif/2]).

-export([compress/1,
	 compress/2,
	 compress/5]).

-export([rbits8/1, rbits/2]).

-import(lists, [reverse/1, foldl/3]).

-include("erl_img.hrl").

-include("dbg.hrl").

%% should go
-define(LZW_CLEAR,           256).
-define(LZW_EOI,             257).
-define(LZW_FIRST,           258).
-define(LZW_STARTBITLEN,     9).

-define(LZW_SWAP_9,          510).
-define(LZW_SWAP_10,        1022).
-define(LZW_SWAP_11,        2046).
-define(LZW_MAX,            4094).

-define(get_lzw(Code), get(Code)).
-define(add_lzw(Code, Str), put(Code, Str)).

%% should finalize the dictionary (if not using special process)
init_decomp(Lim) ->
    init_decomp(0,Lim).
    
init_decomp(No,Lim) when No >= Lim ->
    case erase(No) of
        undefined -> ok;
        _ -> init_decomp(No+1,Lim)
    end;
init_decomp(No,Lim) ->
    ?add_lzw(No, [No]),
    init_decomp(No+1,Lim).

init_comp() ->
    erase(),
    init_comp(257).
init_comp(No) when No >= 0 ->
    ?add_lzw([No], No),
    init_comp(No - 1);
init_comp(_) ->
    ok.


%% reverse bytes and read the codes backwards
%% needed for gif bit packing
%%
decompress_gif(Bin) ->
    decompress_gif(Bin, 8). 

decompress_gif(Bin, MinCodeSize) ->
    RBin = list_to_binary(reverse(binary_to_list(Bin))),
    ?dbg("rev-binary=~p\n", [RBin]),
    ReadCode = fun(Len, Offs) ->
		       Offs1 = Offs-Len,
		       Pad = ?PAD_Len8(Offs),
		       case RBin of
			   <<_:Offs1, Code:Len, _:Pad, _/binary>> ->
			       ?dbg("offs=~p, len=~p, code=~p\n",[Offs1,Len,Code]),
			       {Code, Offs1};
			   _ ->
			       {error, eof}
		       end
	       end,
    Next = fun(Len) -> 1 bsl Len end,
    decomp(size(RBin)*8,ReadCode,MinCodeSize,Next).
    
decompress_tiff(Bin) ->
    decompress_tiff(Bin, 8, 1).


decompress_tiff(Bin, MinCodeSize, 2) ->
    RBin = list_to_binary(reverse(binary_to_list(Bin))),
    ?dbg("rev-binary=~p\n", [RBin]),
    ReadCode = fun(Len, Offs) ->
		       Offs1 = Offs-Len,
		       Pad = ?PAD_Len8(Offs),
		       case RBin of
			   <<_:Offs1, Code:Len, _:Pad, _/binary>> ->
			       ?dbg("offs=~p, len=~p, code=~p\n",[Offs1,Len,Code]),
			       {rbits(Code,Len), Offs1};
			   _ ->
			       {error, eof}
		       end
	       end,
    Next = fun(Len) -> (1 bsl Len)-1 end,
    decomp(size(RBin)*8,ReadCode,MinCodeSize,Next);
decompress_tiff(Bin, MinCodeSize, 1) ->
    ?dbg("binary=~p\n", [Bin]),
    ReadCode = fun(Len, Offs) ->
		       Offs1 = Offs+Len,
		       Pad = ?PAD_Len8(Offs1),
		       case Bin of
			   <<_:Offs, Code:Len, _:Pad, _/binary>> ->
			       ?dbg("offs=~p, len=~p, code=~p\n",
				   [Offs,Len,Code]),
			       {Code, Offs1};
			   _ ->
			       {error, eof}
		       end
	       end,
    Next = fun(Len) -> (1 bsl Len)-1 end,
    decomp(0, ReadCode, MinCodeSize, Next).


decomp(S, Read, MinCodeSize, Next) ->
    LZW_Clear = (1 bsl MinCodeSize),
    LZW_Eoi   = (1 bsl MinCodeSize)+1,
    LZW_First = (1 bsl MinCodeSize)+2,
    LZW_StartLen = MinCodeSize+1,
    LZW_Next = Next(LZW_StartLen),
    %% init_decomp(LZW_First), GIF/TIF will always? start with LZW_Clear code
    ?dbg("MinCodeSize=~p,Clear=~p,Eoi=~p,First=~p,Next=~p,StartLen=~p\n",
	      [MinCodeSize, LZW_Clear, LZW_Eoi,LZW_First,LZW_Next,LZW_StartLen]),
    decomp(S,Read,0,LZW_First,LZW_StartLen,
	   LZW_Clear,LZW_Eoi,LZW_First,LZW_Next,LZW_StartLen,Next,[]).

decomp(S, Read, PrevCode, Count, BitLen,
       LZW_Clear,LZW_Eoi,LZW_First,LZW_Next,LZW_StartLen,Next,Acc) when Count == LZW_Next ->
    ?dbg("NEXT BITLEN=~p\n", [BitLen+1]),
    decomp(S, Read, PrevCode, Count, BitLen+1,
	   LZW_Clear, LZW_Eoi, LZW_First, Next(BitLen+1), LZW_StartLen, Next, Acc);

decomp(S, Read, PrevCode, Count, BitLen,
       LZW_Clear, LZW_Eoi, LZW_First, LZW_Next, LZW_StartLen, Next, Acc) ->
    ?dbg("Len=~p,Count=~p,Clear=~p,Eoi=~p,First=~p,Next=~p,StartLen=~p\n",
	[BitLen,Count,LZW_Clear,LZW_Eoi,LZW_First,LZW_Next,LZW_StartLen]),
    case (catch Read(BitLen,S)) of
        {LZW_Eoi, _} ->
	    ?dbg("EOI:~p\n",[LZW_Eoi]),
            list_to_binary(reverse(Acc));
        {LZW_Clear, NS} ->
	    ?dbg("CLEAR:~p\n",[LZW_Clear]),
	    init_decomp(LZW_First),
            case catch Read(LZW_StartLen, NS) of
                {LZW_Eoi, _} ->
		    list_to_binary(reverse(Acc));
                {NewCode, NS2} when integer(NewCode) -> 
                    Str = ?get_lzw(NewCode),
                    decomp(NS2,Read,NewCode,LZW_First,LZW_StartLen,
			   LZW_Clear, LZW_Eoi, LZW_First,
			   Next(LZW_StartLen), LZW_StartLen, Next,
			   [Str|Acc]);
                Else ->
                    io:format("~n~p: Error ~p Args: ~p ~n", 
                              [?MODULE, Else, {NS, PrevCode, Count, BitLen}]),
                    erlang:fault({?MODULE, decomp, {badly_compressed_data}})
            end;
        {NewCode, NS} when integer(NewCode) ->
	    ?dbg("CODE:~p\n",[NewCode]),
            case ?get_lzw(NewCode) of
                undefined when Count == NewCode ->
                    OldStr = [H|_] = ?get_lzw(PrevCode),
                    NewStr = OldStr ++ [H],
                    ?add_lzw(Count, NewStr),
		    decomp(NS, Read, NewCode, Count+1, BitLen,
			   LZW_Clear, LZW_Eoi, LZW_First,
			   LZW_Next,LZW_StartLen, Next,
			   [NewStr|Acc]);
		Str = [H|_]->
                    ?add_lzw(Count, ?get_lzw(PrevCode) ++ [H]),
		    decomp(NS, Read, NewCode, Count+1, BitLen,
			   LZW_Clear, LZW_Eoi, LZW_First,
			   LZW_Next,LZW_StartLen, Next,
			   [Str|Acc]);
		Else ->
                    io:format("~n~p: Error Case Clause ~p ~p Args ~p ~n", 
                              [?MODULE, Else, NewCode, {S, PrevCode, Count, BitLen}]),
                    erlang:fault({?MODULE, decomp, {badly_compressed_data}})
            end;
        Else ->
            io:format("~n~p: Error ~p Args: ~p ~n", 
                      [?MODULE, Else, {S, PrevCode, Count, BitLen}]),
            erlang:fault({?MODULE, decomp, {badly_compressed_data}})
    end.

bl(2,   Len) -> Len+1;
bl(6,   Len) -> Len+1;
bl(14,  Len) -> Len+1;
bl(30,  Len) -> Len+1;
bl(62,  Len) -> Len+1;
bl(126, Len) -> Len+1;
bl(254, Len) -> Len+1;
bl(510, Len) -> Len+1;
bl(1022,Len) -> Len+1;
bl(2046,Len) -> Len+1;
bl(_,Len) -> Len.


%% reverse bits in a byte
rbits8(Code) ->
    case Code of
	2#00000000 -> 2#00000000;
        2#00000001 -> 2#10000000;
        2#00000010 -> 2#01000000;
        2#00000011 -> 2#11000000;
        2#00000100 -> 2#00100000;
        2#00000101 -> 2#10100000;
        2#00000110 -> 2#01100000;
        2#00000111 -> 2#11100000;
        2#00001000 -> 2#00010000;
        2#00001001 -> 2#10010000;
        2#00001010 -> 2#01010000;
        2#00001011 -> 2#11010000;
        2#00001100 -> 2#00110000;
        2#00001101 -> 2#10110000;
        2#00001110 -> 2#01110000;
        2#00001111 -> 2#11110000;
        2#00010000 -> 2#00001000;
        2#00010001 -> 2#10001000;
        2#00010010 -> 2#01001000;
        2#00010011 -> 2#11001000;
        2#00010100 -> 2#00101000;
        2#00010101 -> 2#10101000;
        2#00010110 -> 2#01101000;
        2#00010111 -> 2#11101000;
        2#00011000 -> 2#00011000;
        2#00011001 -> 2#10011000;
        2#00011010 -> 2#01011000;
        2#00011011 -> 2#11011000;
        2#00011100 -> 2#00111000;
        2#00011101 -> 2#10111000;
        2#00011110 -> 2#01111000;
        2#00011111 -> 2#11111000;
        2#00100000 -> 2#00000100;
        2#00100001 -> 2#10000100;
        2#00100010 -> 2#01000100;
        2#00100011 -> 2#11000100;
        2#00100100 -> 2#00100100;
        2#00100101 -> 2#10100100;
        2#00100110 -> 2#01100100;
        2#00100111 -> 2#11100100;
        2#00101000 -> 2#00010100;
        2#00101001 -> 2#10010100;
        2#00101010 -> 2#01010100;
        2#00101011 -> 2#11010100;
        2#00101100 -> 2#00110100;
        2#00101101 -> 2#10110100;
        2#00101110 -> 2#01110100;
        2#00101111 -> 2#11110100;
        2#00110000 -> 2#00001100;
        2#00110001 -> 2#10001100;
        2#00110010 -> 2#01001100;
        2#00110011 -> 2#11001100;
        2#00110100 -> 2#00101100;
        2#00110101 -> 2#10101100;
        2#00110110 -> 2#01101100;
        2#00110111 -> 2#11101100;
        2#00111000 -> 2#00011100;
        2#00111001 -> 2#10011100;
        2#00111010 -> 2#01011100;
        2#00111011 -> 2#11011100;
        2#00111100 -> 2#00111100;
        2#00111101 -> 2#10111100;
        2#00111110 -> 2#01111100;
        2#00111111 -> 2#11111100;
        2#01000000 -> 2#00000010;
        2#01000001 -> 2#10000010;
        2#01000010 -> 2#01000010;
        2#01000011 -> 2#11000010;
        2#01000100 -> 2#00100010;
        2#01000101 -> 2#10100010;
        2#01000110 -> 2#01100010;
        2#01000111 -> 2#11100010;
        2#01001000 -> 2#00010010;
        2#01001001 -> 2#10010010;
        2#01001010 -> 2#01010010;
        2#01001011 -> 2#11010010;
        2#01001100 -> 2#00110010;
        2#01001101 -> 2#10110010;
        2#01001110 -> 2#01110010;
        2#01001111 -> 2#11110010;
        2#01010000 -> 2#00001010;
        2#01010001 -> 2#10001010;
        2#01010010 -> 2#01001010;
        2#01010011 -> 2#11001010;
        2#01010100 -> 2#00101010;
        2#01010101 -> 2#10101010;
        2#01010110 -> 2#01101010;
        2#01010111 -> 2#11101010;
        2#01011000 -> 2#00011010;
        2#01011001 -> 2#10011010;
        2#01011010 -> 2#01011010;
        2#01011011 -> 2#11011010;
        2#01011100 -> 2#00111010;
        2#01011101 -> 2#10111010;
        2#01011110 -> 2#01111010;
        2#01011111 -> 2#11111010;
        2#01100000 -> 2#00000110;
        2#01100001 -> 2#10000110;
        2#01100010 -> 2#01000110;
        2#01100011 -> 2#11000110;
        2#01100100 -> 2#00100110;
        2#01100101 -> 2#10100110;
        2#01100110 -> 2#01100110;
        2#01100111 -> 2#11100110;
        2#01101000 -> 2#00010110;
        2#01101001 -> 2#10010110;
        2#01101010 -> 2#01010110;
        2#01101011 -> 2#11010110;
        2#01101100 -> 2#00110110;
        2#01101101 -> 2#10110110;
        2#01101110 -> 2#01110110;
        2#01101111 -> 2#11110110;
        2#01110000 -> 2#00001110;
        2#01110001 -> 2#10001110;
        2#01110010 -> 2#01001110;
        2#01110011 -> 2#11001110;
        2#01110100 -> 2#00101110;
        2#01110101 -> 2#10101110;
        2#01110110 -> 2#01101110;
        2#01110111 -> 2#11101110;
        2#01111000 -> 2#00011110;
        2#01111001 -> 2#10011110;
        2#01111010 -> 2#01011110;
        2#01111011 -> 2#11011110;
        2#01111100 -> 2#00111110;
        2#01111101 -> 2#10111110;
        2#01111110 -> 2#01111110;
        2#01111111 -> 2#11111110;
        2#10000000 -> 2#00000001;
        2#10000001 -> 2#10000001;
        2#10000010 -> 2#01000001;
        2#10000011 -> 2#11000001;
        2#10000100 -> 2#00100001;
        2#10000101 -> 2#10100001;
        2#10000110 -> 2#01100001;
        2#10000111 -> 2#11100001;
        2#10001000 -> 2#00010001;
        2#10001001 -> 2#10010001;
        2#10001010 -> 2#01010001;
        2#10001011 -> 2#11010001;
        2#10001100 -> 2#00110001;
        2#10001101 -> 2#10110001;
        2#10001110 -> 2#01110001;
        2#10001111 -> 2#11110001;
        2#10010000 -> 2#00001001;
        2#10010001 -> 2#10001001;
        2#10010010 -> 2#01001001;
        2#10010011 -> 2#11001001;
        2#10010100 -> 2#00101001;
        2#10010101 -> 2#10101001;
        2#10010110 -> 2#01101001;
        2#10010111 -> 2#11101001;
        2#10011000 -> 2#00011001;
        2#10011001 -> 2#10011001;
        2#10011010 -> 2#01011001;
        2#10011011 -> 2#11011001;
        2#10011100 -> 2#00111001;
        2#10011101 -> 2#10111001;
        2#10011110 -> 2#01111001;
        2#10011111 -> 2#11111001;
        2#10100000 -> 2#00000101;
        2#10100001 -> 2#10000101;
        2#10100010 -> 2#01000101;
        2#10100011 -> 2#11000101;
        2#10100100 -> 2#00100101;
        2#10100101 -> 2#10100101;
        2#10100110 -> 2#01100101;
        2#10100111 -> 2#11100101;
        2#10101000 -> 2#00010101;
        2#10101001 -> 2#10010101;
        2#10101010 -> 2#01010101;
        2#10101011 -> 2#11010101;
        2#10101100 -> 2#00110101;
        2#10101101 -> 2#10110101;
        2#10101110 -> 2#01110101;
        2#10101111 -> 2#11110101;
        2#10110000 -> 2#00001101;
        2#10110001 -> 2#10001101;
        2#10110010 -> 2#01001101;
        2#10110011 -> 2#11001101;
        2#10110100 -> 2#00101101;
        2#10110101 -> 2#10101101;
        2#10110110 -> 2#01101101;
        2#10110111 -> 2#11101101;
        2#10111000 -> 2#00011101;
        2#10111001 -> 2#10011101;
        2#10111010 -> 2#01011101;
        2#10111011 -> 2#11011101;
        2#10111100 -> 2#00111101;
        2#10111101 -> 2#10111101;
        2#10111110 -> 2#01111101;
        2#10111111 -> 2#11111101;
        2#11000000 -> 2#00000011;
        2#11000001 -> 2#10000011;
        2#11000010 -> 2#01000011;
        2#11000011 -> 2#11000011;
        2#11000100 -> 2#00100011;
        2#11000101 -> 2#10100011;
        2#11000110 -> 2#01100011;
        2#11000111 -> 2#11100011;
        2#11001000 -> 2#00010011;
        2#11001001 -> 2#10010011;
        2#11001010 -> 2#01010011;
        2#11001011 -> 2#11010011;
        2#11001100 -> 2#00110011;
        2#11001101 -> 2#10110011;
        2#11001110 -> 2#01110011;
        2#11001111 -> 2#11110011;
        2#11010000 -> 2#00001011;
        2#11010001 -> 2#10001011;
        2#11010010 -> 2#01001011;
        2#11010011 -> 2#11001011;
        2#11010100 -> 2#00101011;
        2#11010101 -> 2#10101011;
        2#11010110 -> 2#01101011;
        2#11010111 -> 2#11101011;
        2#11011000 -> 2#00011011;
        2#11011001 -> 2#10011011;
        2#11011010 -> 2#01011011;
        2#11011011 -> 2#11011011;
        2#11011100 -> 2#00111011;
        2#11011101 -> 2#10111011;
        2#11011110 -> 2#01111011;
        2#11011111 -> 2#11111011;
        2#11100000 -> 2#00000111;
        2#11100001 -> 2#10000111;
        2#11100010 -> 2#01000111;
        2#11100011 -> 2#11000111;
        2#11100100 -> 2#00100111;
        2#11100101 -> 2#10100111;
        2#11100110 -> 2#01100111;
        2#11100111 -> 2#11100111;
        2#11101000 -> 2#00010111;
        2#11101001 -> 2#10010111;
        2#11101010 -> 2#01010111;
        2#11101011 -> 2#11010111;
        2#11101100 -> 2#00110111;
        2#11101101 -> 2#10110111;
        2#11101110 -> 2#01110111;
        2#11101111 -> 2#11110111;
        2#11110000 -> 2#00001111;
        2#11110001 -> 2#10001111;
        2#11110010 -> 2#01001111;
        2#11110011 -> 2#11001111;
        2#11110100 -> 2#00101111;
        2#11110101 -> 2#10101111;
        2#11110110 -> 2#01101111;
        2#11110111 -> 2#11101111;
        2#11111000 -> 2#00011111;
        2#11111001 -> 2#10011111;
        2#11111010 -> 2#01011111;
        2#11111011 -> 2#11011111;
        2#11111100 -> 2#00111111;
        2#11111101 -> 2#10111111;
        2#11111110 -> 2#01111111;
        2#11111111 -> 2#11111111
    end.

rbits(Code, Len) ->
    rbits(Code, Len, 0).

rbits(Code, 0, Acc) -> 
    Acc;
rbits(Code, Len, Acc) when Len >= 8 ->
    rbits(Code bsr 8, Len - 8,
	  (Acc bsl 8) bor rbits8(Code band 16#ff));
rbits(Code, Len, Acc) ->
    A8 = rbits8(Code band 16#ff),
    (Acc bsl Len) bor (A8 bsr (8 - Len)).



compress(Bin) ->
    compress(Bin, size(Bin), ?LZW_STARTBITLEN, {0,[]}, []).

compress(Bin, Strips) ->
    compress(Bin,  Strips, ?LZW_STARTBITLEN, {0,[]}, []).

compress(Bin, W, BitLen, Build, Acc) ->
    init_comp(),
    {NBuild, Nacc} = write({BitLen,?LZW_CLEAR}, Build, Acc),
    compress(Bin, 0, W, [], ?LZW_STARTBITLEN, ?LZW_FIRST, NBuild, Nacc).

compress(<<>>, CC, W, Omega, BitLen, TabCount, Build, Acc) ->
    Code =?get_lzw(Omega),
    {NBuild, Nacc} =  write({BitLen,Code}, Build, Acc),
    %%    NewBL = lzw_bl(TabCount, BitLen),
    NewBL = BitLen,
    {{TotBitLen, Codes}, N2acc} = write({NewBL,?LZW_EOI}, NBuild, Nacc),
    PaddL = 8 - (TotBitLen rem 8),
    case catch buildbin(reverse([{PaddL, 0}|Codes])) of
        Bin when binary(Bin) -> 
            list_to_binary(reverse([Bin|N2acc]));
        Else ->
            io:format("~p:~p Error ~p ~p ~n",[?MODULE,?LINE,{PaddL,Codes},CC]),
            erlang:fault({?MODULE, compress, {internal_error, ?LINE}})
    end;
%% FIXME: this clause is for tif?
compress(Bin, CC, W, Omega, BitLen, TabCount, Build, Acc) when CC == W ->
    Code =?get_lzw(Omega),
    {NBuild, Nacc} = write({BitLen,Code}, Build, Acc),
    compress(Bin, W, bl(TabCount-1,BitLen), NBuild, Nacc);
compress(<<Char:8, Bin/binary>>, CC, W, Omega, BitLen, TabC, Build, Acc) ->
    NewOmega = [Char|Omega],
    case ?get_lzw(NewOmega) of
        undefined ->
            Code = ?get_lzw(Omega),
            {NBuild, Nacc} = write({BitLen,Code}, Build, Acc),
            ?add_lzw(NewOmega, TabC),
            case TabC of
                ?LZW_MAX - 1 ->
                    Code2 =?get_lzw([Char]),
                    {NBuild2, Nacc2} = write({BitLen,Code2},NBuild,Nacc),
		    compress(Bin,W,bl(TabC-1,BitLen), NBuild2, Nacc2);
                _ ->
                    compress(Bin, CC+1, W, [Char], bl(TabC-1, BitLen), 
			     TabC + 1, NBuild, Nacc)
            end;
        _ ->
            compress(Bin, CC +1, W, NewOmega, BitLen, TabC, Build, Acc)
    end.


write({_,undefined}, _, _) -> 
    erlang:fault({undef,value});
write({CLen, Code}, {Totlen, List}, Acc) ->
    NewLen = CLen + Totlen,
    if 
        NewLen rem 8 == 0 ->
            case catch buildbin(reverse([{CLen,Code}|List])) of
                Bin when binary(Bin) ->
                    {{0, []}, [Bin|Acc]};
                {Bin, NewList} when binary(Bin) ->
                    Sum = foldl(fun({X,_}, Sum) -> X + Sum end, 0, NewList),
                    {{Sum, reverse(NewList)}, [Bin|Acc]};
                Else ->
                    io:format("~p:~p Error ~p ~p ~n", [?MODULE, ?LINE, Else, 
                                                       [{CLen, Code}, {Totlen, List}]]),
                    erlang:fault({?MODULE, write, {internal_error, ?LINE}})
            end;
        NewLen > 100 -> 
            case catch buildbin(reverse([{CLen,Code}|List])) of
                {Bin, NewList} when binary(Bin) ->                  
                    Sum = foldl(fun({X,_}, Sum) -> X + Sum end, 0, NewList),
                    {{Sum, reverse(NewList)}, [Bin|Acc]};
                Else ->
                    io:format("~p:~p Error ~p ~p ~n", [?MODULE, ?LINE, Else, 
                                                       [{CLen, Code}, {Totlen, List}]]),
                    erlang:fault({?MODULE, write, {internal_error, ?LINE}})
            end;
        true ->
            {{Totlen + CLen,[{CLen,Code}|List]}, Acc}
    end.

buildbin([{8,0}]) ->
    <<>>;
buildbin([{L1, C1},{L2,C2}]) ->
    <<C1:L1, C2:L2>>;
buildbin([{L1,C1},{L2,C2},{L3,C3}]) ->
    <<C1:L1,C2:L2,C3:L3>>;
buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4>>;
buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5>>;
buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5},{L6,C6}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5,C6:L6>>;
buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5},{L6,C6},{L7,C7}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5,C6:L6,C7:L7>>;
buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5},{L6,C6},{L7,C7},{L8,C8}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5,C6:L6,C7:L7,C8:L8>>;
buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5},
	  {L6,C6},{L7,C7},{L8,C8},{L9,C9} | Rest]) ->
    RemL = (L1+L2+L3+L4+L5+L6+L7+L8) rem 8,
    AddL = 8 - RemL,
    KeepL = L9 - AddL,
    SkipL = 16 - (AddL + KeepL),
    TempFill = 8 - (L9 rem 8),    
    <<P9:AddL,Keep9:KeepL, _:SkipL>> = <<C9:L9,0:TempFill>>,
    Bin = <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5,C6:L6,C7:L7,C8:L8,P9:AddL>>,
    NewList = [{KeepL, Keep9}|Rest],
    {Bin, NewList}.


