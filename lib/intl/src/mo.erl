%%% File    : mo.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : MO file reader
%%% Created : 29 Dec 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(mo).

-export([load/1]).

-define(MAGIC_LE, 16#de120495).
-define(MAGIC_BE, 16#950412de).


load(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    case load_mo(Bin) of
		{ok, Tab} ->
		    lists:foreach(
		      fun({M,T}) ->
			      io:format("~s -> ~s\n",
					[binary_to_list(M),
					 binary_to_list(T)])
		      end, Tab),
		    {ok,Tab}
	    end;
	Error ->
	    Error
    end.


load_mo(Bin = <<?MAGIC_BE:32, Version:32,
	    NStr:32, O_Offs:32, T_Offs:32, _/binary>>) ->
    load_be_strings(NStr, O_Offs, T_Offs, [], Bin);
load_mo(Bin = <<?MAGIC_LE:32, Version:32/little, 
	    NStr:32/little, O_Offs:32/little, T_Offs:32/little, _/binary>>) ->
    load_le_strings(NStr, O_Offs, T_Offs, [], Bin);
load_mo(_) ->
    {error, bad_magic}.

load_be_strings(0, _, _, Acc, Bin) ->
    {ok,Acc};
load_be_strings(I, O_Offs, T_Offs, Acc, Bin) ->
    <<_:O_Offs/binary, O_StrLen:32, O_StrOffs:32, _/binary>> = Bin,
    <<_:T_Offs/binary, T_StrLen:32, T_StrOffs:32, _/binary>> = Bin,
    <<_:O_StrOffs/binary, MsgId:O_StrLen/binary, _/binary>> = Bin,
    <<_:T_StrOffs/binary, MsgStr:T_StrLen/binary, _/binary>> = Bin,
    load_be_strings(I-1, O_Offs+8, T_Offs+8, 
		    [{MsgId,MsgStr}|Acc], Bin).

load_le_strings(0, _, _, Acc, Bin) ->
    {ok,Acc};
load_le_strings(I, O_Offs, T_Offs, Acc, Bin) ->
    <<_:O_Offs/binary, O_StrLen:32/little, O_StrOffs:32/little,_/binary>>=Bin,
    <<_:T_Offs/binary, T_StrLen:32/little, T_StrOffs:32/little,_/binary>>=Bin,
    <<_:O_StrOffs/binary, MsgId:O_StrLen/binary, _/binary>> = Bin,
    <<_:T_StrOffs/binary, MsgStr:T_StrLen/binary, _/binary>> = Bin,
    load_le_strings(I-1, O_Offs+8, T_Offs+8, 
		    [{MsgId,MsgStr}|Acc], Bin).


    
    
    
    
    

