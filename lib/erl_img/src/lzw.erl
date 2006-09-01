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
	 compress/5,
	 compress_gif/1,
	 compress_gif/2]).

-export([rbits8/1, rbits/2]).
-compile(export_all).

-import(lists, [reverse/1, foldl/3]).

-include("erl_img.hrl").

-include("dbg.hrl").

-record(z,
	{
	  clear,
	  eoi,
	  first,
	  startlen,
	  next,
	  max,
	  nextfn
	 }).

-define(get_lzw(Code),      get(Code)).
-define(add_lzw(Code, Str), put(Code, Str)).

%% most significant bits first
read_bits_msb(Bin, Offs, Len) ->
    Offs1 = Offs+Len,
    Pad = ?PAD_Len8(Offs1),
    <<_:Offs, Code:Len, _:Pad,_/binary>> = Bin,
    {Code,Offs1}.

%% least significant bytes first
read_bits_lsb(Bin, Offs, Len) ->
    Offs1  = Offs+Len,
    B0     = Offs  div 8,
    B1     = (Offs1-1) div 8,
    R0     = Offs  rem 8,
    BL = ((B1 - B0)+1),
    ?dbg("blen=~w,b0=~w,r0=~w b1=~w\n", [BL,B0,R0,B1]),
    <<_:B0/binary,BCode:BL/little-unsigned-integer-unit:8,_/binary>> = Bin,
    Code = (BCode bsr R0) band ((1 bsl Len)-1),
    {Code,Offs1}.

%% least significant bits and bytes first 
read_bits_LSB(Bin, Offs, Len) ->
    {Code,Offs1} = read_bits_lsb(Bin,Offs,Len),
    {rbits(Code,Len),Offs1}.


%% should finalize the dictionary (if not using special process)
init_decomp(Lim) ->
    erase(),
    init_decomp(0,Lim).
    
init_decomp(Lim,Lim) ->
    ok;
init_decomp(I,Lim) ->
    ?add_lzw(I, [I]),
    init_decomp(I+1,Lim).

init_comp(Lim) ->
    erase(),
    init_comp(0, Lim).

init_comp(Lim,Lim) ->
    ok;
init_comp(I,Lim) ->
    ?add_lzw([I], I),
    init_comp(I+1,Lim).


%% reverse bytes and read the codes backwards
%% needed for gif bit packing
%%
decompress_gif(Bin) ->
    decompress_gif(Bin, 8). 

decompress_gif(Bin, MinCodeSize) ->
    ReadFn = fun(Len,Offs) ->
		     read_bits_lsb(Bin,Offs,Len)
	     end,
    NextFn = fun(Len) -> (1 bsl Len) end,
    decomp(0,ReadFn,MinCodeSize,NextFn).
    


decompress_tiff(Bin) ->
    decompress_tiff(Bin, 8, 1).

decompress_tiff(Bin, MinCodeSize, 2) ->
    ReadFn = fun(Len,Offs) ->
		     read_bits_LSB(Bin,Offs,Len)
	     end,
    NextFn = fun(Len) -> (1 bsl Len)-1 end,
    decomp(0,ReadFn,MinCodeSize,NextFn);
decompress_tiff(Bin, MinCodeSize, 1) ->
    ReadFn = fun(Len,Offs) ->
		     read_bits_msb(Bin,Offs,Len)
	     end,    
    NextFn = fun(Len) -> (1 bsl Len)-1 end,
    decomp(0, ReadFn, MinCodeSize, NextFn).


decomp(S, Read, MinCodeSize, NextFn) ->
    First      = (1 bsl MinCodeSize)+2,
    StartLen   = MinCodeSize+1,
    Z = #z { clear    = (1 bsl MinCodeSize),
	     eoi      = (1 bsl MinCodeSize)+1,
	     first    = First,
	     startlen = StartLen,
	     next     = NextFn(StartLen),
	     max      = 0,  %% not used here
	     nextfn   = NextFn
	    },
    ?dbg("decomp: mincodesize=~p, z = ~p\n",[MinCodeSize, Z]),
    decomp(S,Read,0,First,StartLen,Z,[]).



decomp(S,Read,PrevCode,Count,BitLen,Z,Acc) when BitLen<12,Count == Z#z.next ->
    NextBitLen = BitLen+1,
    ?dbg("NEXT BITLEN=~p\n", [NextBitLen]),
    NextCode = (Z#z.nextfn)(NextBitLen),
    decomp(S, Read, PrevCode, Count, NextBitLen,
	   Z#z { next = NextCode }, Acc);
decomp(S,Read,PrevCode,Count,BitLen,Z,Acc) ->
    {NewCode,NS} = Read(BitLen,S),
    ?dbg("read: ~w/~w count=~w\n", [NewCode, BitLen,Count]),
    if NewCode == Z#z.eoi ->
	    ?dbg("EOI:~p\n",[Z#z.eoi]),
            list_to_binary(reverse(Acc));
       NewCode == Z#z.clear ->
	    ?dbg("CLEAR:~p\n",[Z#z.clear]),
	    init_decomp(Z#z.first),
	    StartLen = Z#z.startlen,
            {NewCode1,NS1} = Read(StartLen, NS),
	    ?dbg("read: ~w/~w count=~w\n", [NewCode1,StartLen,Count]),
	    if NewCode1 == Z#z.eoi ->
		    list_to_binary(reverse(Acc));
	       true ->
                    Str = ?get_lzw(NewCode1),
		    NextCode = (Z#z.nextfn)(StartLen),
                    decomp(NS1,Read,NewCode1, Z#z.first, StartLen,
			   Z#z { next = NextCode }, [Str|Acc])
	    end;
       true ->
	    ?dbg("CODE: prev=~p new=~p\n",[PrevCode,NewCode]),
            case ?get_lzw(NewCode) of
                undefined ->
                    OldStr = [H|_] = ?get_lzw(PrevCode),
                    NewStr = OldStr ++ [H],
                    ?add_lzw(NewCode, NewStr),
		    decomp(NS,Read,NewCode,Count+1,BitLen,Z,[NewStr|Acc]);
		Str = [H|_]->
                    ?add_lzw(Count, ?get_lzw(PrevCode) ++ [H]),
		    decomp(NS,Read,NewCode,Count+1,BitLen,Z,[Str|Acc])
	    end
    end.

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

rbits(_Code, 0, Acc) -> 
    Acc;
rbits(Code, Len, Acc) when Len >= 8 ->
    rbits(Code bsr 8, Len - 8,
	  (Acc bsl 8) bor rbits8(Code band 16#ff));
rbits(Code, Len, Acc) ->
    A8 = rbits8(Code band 16#ff),
    (Acc bsl Len) bor (A8 bsr (8 - Len)).


compress(Bin) ->
    compress(Bin, size(Bin)).

compress(Bin, Stripe) ->
    NextFn = fun(Len) -> (1 bsl Len)-1 end,
    compress(Bin, Stripe, 8, 12, NextFn).

compress_gif(Bin) ->
    compress_gif(Bin, 8).

compress_gif(Bin,MinCodeSize) ->
    NextFn = fun(Len) -> (1 bsl Len) end,
    compress(Bin,size(Bin), MinCodeSize, 14, NextFn).


compress(Bin,Stripe,MinCodeSize,MaxCodeSize,NextFn) ->
    First      = (1 bsl MinCodeSize)+2,
    StartLen   = MinCodeSize+1,
    Z = #z { clear    = (1 bsl MinCodeSize),
	     eoi      = (1 bsl MinCodeSize)+1,
	     first    = First,
	     startlen = StartLen,
	     next     = NextFn(StartLen),
	     max      = NextFn(MaxCodeSize+1),
	     nextfn   = NextFn
	    },
    ?dbg("compress: mincodesize=~w, z=~w\n",[MinCodeSize,Z]),
    comp0(Bin,Stripe,StartLen,Z,{0,[],[]}).


comp0(Bin,Stripe,BitLen,Z,Build) ->
    init_comp(Z#z.first-3),
    NBuild = write(Z#z.clear, BitLen, Build),
    comp1(Bin,0,Stripe,[],Z#z.startlen,Z#z.first,Z,NBuild).


comp1(<<>>,_CC,_Stripe,Omega,BitLen,_Count,Z,Build) ->
    Code = ?get_lzw(Omega),
    NBuild =  write(Code, BitLen, Build),
    {TotBitLen,Codes, Acc} = write(Z#z.eoi, BitLen, NBuild),
    PaddL = 8 - (TotBitLen rem 8),
    Bin = if PaddL == 8 ->
		  buildbin(reverse(Codes));
	     true ->
		  buildbin(reverse([{PaddL, 0}|Codes]))
	  end,
    {Z#z.startlen-1, list_to_binary(reverse([Bin|Acc]))};

comp1(Bin,CC,Stripe,Omega,BitLen,Count,Z,Build) when CC == Stripe ->
    Code =?get_lzw(Omega),
    NBuild = write(Code, BitLen, Build),
    if Count+2 == Z#z.next ->
	    BitLen1 = BitLen+1,
	    NextCode = (Z#z.nextfn)(BitLen1),
	    Z1 = Z#z { next = NextCode },
	    comp0(Bin,Stripe,BitLen1,Z1,NBuild);
       true ->
	    comp0(Bin,Stripe,BitLen,Z,NBuild)
    end;

comp1(<<Char:8, Bin/binary>>,CC,Stripe,Omega,BitLen,Count,Z,Build) ->
    NewOmega = [Char|Omega],
    case ?get_lzw(NewOmega) of
        undefined ->
            Code = ?get_lzw(Omega),
            NBuild = write(Code,BitLen,Build),
            ?add_lzw(NewOmega, Count),
	    if Count+2 == Z#z.next ->
		    BitLen1 = BitLen+1,
		    NextCode = (Z#z.nextfn)(BitLen1),
		    Z1 = Z#z { next = NextCode },
		    if Z#z.next == Z#z.max ->
			    Code2 =?get_lzw([Char]),
			    NBuild2 = write(Code2,BitLen,NBuild),
			    comp0(Bin,Stripe,BitLen1,Z1,NBuild2);
		       true ->
			    comp1(Bin,CC+1,Stripe,[Char],BitLen1,
				  Count+1,Z1,NBuild)
		    end;
	       true ->
		    comp1(Bin,CC+1,Stripe,[Char],BitLen,
			  Count+1,Z,NBuild)
	    end;
        _ ->
            comp1(Bin,CC+1,Stripe,NewOmega,BitLen,Count,Z,Build)
    end.

%% FIXME: must be able to write lsb byte order and bit order!
write(Code, CLen, {Totlen, List, Acc}) ->
    ?dbg("write: ~w/~w\n", [Code, CLen]),
    NewLen = CLen + Totlen,
    if 
        NewLen rem 8 == 0 ->
            case buildbin(reverse([{CLen,Code}|List])) of
                Bin when binary(Bin) ->
                    {0, [],[Bin|Acc]};
                {Bin, NewList} when binary(Bin) ->
                    Sum = foldl(fun({X,_}, Sum) -> X + Sum end, 0, NewList),
                    {Sum,reverse(NewList),[Bin|Acc]}
	    end;

        NewLen > 100 ->
            {Bin,NewList} = buildbin(reverse([{CLen,Code}|List])),
	    Sum = foldl(fun({X,_}, Sum) -> X + Sum end, 0, NewList),
	    {Sum,reverse(NewList),[Bin|Acc]};

        true ->
            {Totlen+CLen,[{CLen,Code}|List], Acc}
    end.

buildbin([{_L,C}]) ->
    <<C>>;
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
    {Bin, NewList};
buildbin([]) ->
    <<>>.



