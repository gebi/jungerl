%%% File    : mo.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : MO file reader
%%% Created : 29 Dec 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(mo).

-export([load/1, load/2, load/4, load/5]).
-export([gettext/2, ngettext/4]).
-export([charset/1, content_type/1, transfer_encoding/1]).

-compile(export_all).

-define(MAGIC_LE, 16#de120495).
-define(MAGIC_BE, 16#950412de).


gettext(Tab, MsgId) ->
    case ets:lookup(Tab, list_to_binary(MsgId)) of
	[{_,[MsgStr|_]}] -> 
	    binary_to_list(MsgStr);	    
	[{_,MsgStr}] -> 
	    binary_to_list(MsgStr);
	[] ->
	    MsgId
    end.

ngettext(Tab,MsgId,MsgIdPlural,N) ->
    case ets:lookup(Tab, list_to_binary(MsgId)) of
	[{_,List}] when list(List) ->
	    if N > 0, N =< length(List) ->
		    binary_to_list(lists:nth(N, List));
	       N =/= 1 ->
		    binary_to_list(lists:last(List));
	       true ->
		    MsgId
	    end;
	[] ->
	    if N == 1 -> MsgId;
	       true -> MsgIdPlural
	    end
    end.

charset(Tab) ->
    Str = gettext(Tab, ""),
    Lines = string:tokens(Str, "\n"),
    case find_field("Content-Type", Lines) of
	false ->
	    "ASCII";
	{value,{_, Value}} ->
	    case string:tokens(Value, "; \t") of
		[Type, "charset="++CharSet] ->
		    CharSet;
		_ ->
		    "ASCII"
	    end
    end.

content_type(Tab) ->
    Str = gettext(Tab, ""),
    Lines = string:tokens(Str, "\n"),
    case find_field("Content-Type", Lines) of
	false ->
	    "text/plain";
	{value,{_, Value}} ->
	    case string:tokens(Value, "; \t") of
		[Type, _] ->
		    Type;
		_ ->
		    "text/plain"
	    end
    end.

transfer_encoding(Tab) ->    
    Str = gettext(Tab, ""),
    Lines = string:tokens(Str, "\n"),
    case find_field("Content-Transfer-Encoding", Lines) of
	false ->
	    "8bit";
	{value,{_, Value}} ->
	    case string:tokens(Value, " \t") of
		[Enc | _] ->
		    Enc;
		_ ->
		    "8bit"
	    end
    end.
    

	
find_field(Fld, [Line|Lns]) ->
    case string:tokens(Line, ":") of
	[Key,Value] ->
	    case strcase_eq(Fld, Key) of
		true -> {value,{Key,Value}};
		false ->
		    find_field(Fld, Lns)
	    end;
	_ ->
	    find_field(Fld, Lns)
    end;
find_field(Fld, []) ->
    false.

strcase_eq(X, X) ->  true;
strcase_eq(X, Y) ->  strcase_eq1(X, Y).

strcase_eq1([X|Xs],[X|Ys]) -> strcase_eq1(Xs, Ys);
strcase_eq1([X|Xs],[Y|Ys]) ->
    if X >= $a, X =< $z, (X-$a)+$A == Y -> strcase_eq1(Xs, Ys);
       X >= $A, X =< $Z, (X-$A)+$a == Y -> strcase_eq1(Xs, Ys);
       true -> false
    end;
strcase_eq1([], []) -> true;
strcase_eq1(_, _) -> false.

	


load(Dir, Locale, Category, Domain) ->
    Tab = ets:new(intl, []),
    case load(Tab, Dir, Locale, Category, Domain) of
	Error={error,_}  ->
	    ets:delete(Tab),
	    Error;
	Res ->
	    Res
    end.

load(Tab, Dir, Locale, all, Domain) ->
    lists:foreach(
      fun(CatDir) ->
	      File = filename:join([Dir, Locale, CatDir, Domain++".mo"]),
	      load(Tab, File)
      end, category_all()),
    {ok, Tab};
load(Tab, Dir, Locale, Category, Domain) ->
    CatDir = category_dir(Category),
    File = filename:join([Dir, Locale, CatDir, Domain++".mo"]),    
    load(Tab, File).

load(File) ->
    Tab = ets:new(intl, []),
    case load(Tab, File) of
	Error={error,_} ->
	    ets:delete(Tab),
	    Error;
	Res ->
	    Res
    end.


load(Tab, File) ->
    io:format("loading mo file ~s\n", [File]),
    case file:read_file(File) of
	{ok,Bin} ->
	    io:format("ok\n", []),
	    load_mo(Tab, Bin);
	Error ->
	    io:format("~p\n", [Error]),
	    Error
    end.


load_mo(Tab, Bin = <<?MAGIC_BE:32, Version:32,
		    NStr:32, O_Offs:32, T_Offs:32, _/binary>>) ->
    load_be_strings(Tab, NStr, O_Offs, T_Offs, Bin);
load_mo(Tab, Bin = <<?MAGIC_LE:32, Version:32/little, 
	    NStr:32/little, O_Offs:32/little, T_Offs:32/little, _/binary>>) ->
    load_le_strings(Tab, NStr, O_Offs, T_Offs, Bin);
load_mo(_, _) ->
    {error, bad_magic}.

load_be_strings(Tab, 0, _, _, Bin) ->
    {ok,Tab};
load_be_strings(Tab, I, O_Offs, T_Offs, Bin) ->
    <<_:O_Offs/binary, O_StrLen:32, O_StrOffs:32, _/binary>> = Bin,
    <<_:T_Offs/binary, T_StrLen:32, T_StrOffs:32, _/binary>> = Bin,
    <<_:O_StrOffs/binary, MsgId:O_StrLen/binary, _/binary>> = Bin,
    <<_:T_StrOffs/binary, MsgStr:T_StrLen/binary, _/binary>> = Bin,
    load_translation(Tab, MsgId, MsgStr),
    load_be_strings(Tab, I-1, O_Offs+8, T_Offs+8, Bin).

load_le_strings(Tab, 0, _, _, Bin) ->
    {ok,Tab};
load_le_strings(Tab, I, O_Offs, T_Offs, Bin) ->
    <<_:O_Offs/binary, O_StrLen:32/little, O_StrOffs:32/little,_/binary>>=Bin,
    <<_:T_Offs/binary, T_StrLen:32/little, T_StrOffs:32/little,_/binary>>=Bin,
    <<_:O_StrOffs/binary, MsgId:O_StrLen/binary, _/binary>> = Bin,
    <<_:T_StrOffs/binary, MsgStr:T_StrLen/binary, _/binary>> = Bin,
    load_translation(Tab, MsgId, MsgStr),
    load_le_strings(Tab, I-1, O_Offs+8, T_Offs+8, Bin).

load_translation(Tab, MsgId, MsgStr) ->
    case plural_msgid(MsgId) of
	<<>> ->
	    ets:insert(Tab, {MsgId, MsgStr});
	MsgId1 ->
	    MsgStrs = split_msgstr(MsgStr),
	    ets:insert(Tab, {MsgId1, MsgStrs})
    end.

plural_msgid(MsgId) ->
    case bin_index(MsgId, 0) of
	-1 -> <<>>;
	Pos ->
	    <<MsgId1:Pos/binary, _/binary>> = MsgId,
	    MsgId1
    end.

split_msgstr(MsgStr) ->
    split_msgstr(MsgStr, []).

split_msgstr(<<>>, Acc) ->
    lists:reverse(Acc);
split_msgstr(Bin, Acc) ->
    case bin_index(Bin, 0) of
	-1 -> 
	    lists:reverse([Bin | Acc]);
	0  ->
	    <<0:8, Bin2/binary>> = Bin,
	    split_msgstr(Bin2, [<<>> | Acc]);
	Pos ->
	    <<MsgStr:Pos/binary,0:8,Bin2/binary>> = Bin,
	    split_msgstr(Bin2, [MsgStr | Acc])
    end.


bin_index(Bin, C) ->
    bin_index(Bin, 0, size(Bin), C).
	
bin_index(Bin, Pos, Pos, C) -> 
    -1;
bin_index(Bin, Pos, Size, C) ->
    case Bin of
	<<_:Pos/binary, C:8, _/binary>> ->
	    Pos;
	_ ->
	    bin_index(Bin, Pos+1, Size, C)
    end.
    


category_all() ->
    ["LC_CTYPE", "LC_NUMERIC", "LC_TIME",  "LC_COLLATE",
     "LC_MONETARY", "LC_MESSAGES", "LC_PAPER", "LC_NAME",
     "LC_ADDRESS", "LC_TELEPHONE", "LC_MEASUREMENT", 
     "LC_IDENTIFICATION"].

category_dir(C) ->
    case C of
	ctype -> "LC_CTYPE";
	numeric -> "LC_NUMERIC";
	time -> "LC_TIME";
	collate -> "LC_COLLATE";
	monetary -> "LC_MONETARY";
	messages -> "LC_MESSAGES";
	all -> "LC_ALL";
	paper -> "LC_PAPER";
	name -> "LC_NAME";
	address -> "LC_ADDRESS";
	telephone -> "LC_TELEPHONE";
	measurement -> "LC_MEASUREMENT";
	identification -> "LC_IDENTIFICATION" 
    end.



    
    
    
    
    

