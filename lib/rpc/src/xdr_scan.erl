%% Copyright (c) 2000 Sendmail, Inc.  All rights reserved.
%%
%% Scanner for XDR specifications
%%

-module(xdr_scan).

-export([file/1, string/1]).
-export([format_error/1]).

%% read file and return tokens
file(File) when atom(File) ->
    file(atom_to_list(File));
file(File) when list(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    scan(File, binary_to_list(Bin));
	Error -> Error
    end.

string(String) when list(String) ->
    scan("", String).

scan(File, String) ->
    case catch scan(String, [], 1) of
	{ok, Tokens} -> Tokens;
	{error, Description} ->
	    {Line, Message} = Description,
	    {error, {Line, File, Message}};
	{'EXIT', Error} -> 
	    io:format("xdr_scan: internal error : ~p~n", [Error]),
	    {error, {0, ?MODULE, "internal error"}}
    end.

format_error(Message) -> 
    Message.


scan([$/,$* | Cs], Ts, Line) -> scan_comment(Cs, Ts, Line);
scan([$  | Cs], Ts, Line) -> scan(Cs, Ts, Line);
scan([$\t | Cs], Ts, Line) -> scan(Cs, Ts, Line);
scan([$\n | Cs], Ts, Line) -> scan(Cs, Ts, Line+1);
scan([C | Cs], Ts, Line) when C >= $A, C =< $Z -> 
    scan_identifier(Cs, [C], Ts, Line);
scan([C | Cs], Ts, Line) when C >= $a, C =< $z ->
    scan_identifier(Cs, [C], Ts, Line);
scan([C | Cs], Ts, Line) when C >= $0, C =< $9 ->
    scan_constant(Cs, 1, C-$0, Ts, Line);
scan([$-, C | Cs], Ts, Line) when C >= $0, C =< $9 ->
    scan_constant(Cs, -1, C-$0, Ts, Line);
scan([C | Cs], Ts, Line) ->
    case delimiter(C) of
	false -> scan(Cs, [{C, Line} | Ts], Line);
	Tok -> scan(Cs, [{Tok, Line} | Ts], Line)
    end;
scan([], Ts, Line) ->
    {ok, lists:reverse([{'$end', Line} | Ts])}.

%%
%% Comments
%%
scan_comment([$*,$/ | Cs], Ts, Line) -> 
    scan(Cs, Ts, Line);
scan_comment([$\n | Cs], Ts, Line) -> 
    scan_comment(Cs,Ts,Line+1);
scan_comment([_ | Cs], Ts, Line) -> 
    scan_comment(Cs, Ts, Line);
scan_comment([], Ts, Line) -> 
    throw({error, {Line, ?MODULE, "comment not terminated"}}).

%%
%% Identifiers
%%
scan_identifier([C | Cs], Acc, Ts, Line) when C >= $a, C =< $z ->
    scan_identifier(Cs, [C|Acc], Ts, Line);
scan_identifier([C | Cs], Acc, Ts, Line) when C >= $A, C =< $Z ->
    scan_identifier(Cs, [C|Acc], Ts, Line);
scan_identifier([C | Cs], Acc, Ts, Line) when C >= $0, C =< $9 ->
    scan_identifier(Cs, [C|Acc], Ts, Line);
scan_identifier([$_ | Cs], Acc, Ts, Line) ->
    scan_identifier(Cs, [$_|Acc], Ts, Line);
scan_identifier(Cs, Acc, Ts,  Line) ->
    case predefined(lists:reverse(Acc)) of
	{true, Tok} -> scan(Cs, [{Tok, Line} | Ts], Line);
	{false, Id} -> scan(Cs, [{identifier,Line,Id} | Ts], Line)
    end.


%%
%% Constant values 
%%
scan_constant([$x | Cs], Sign, 0, Ts, Line) ->
    scan_hex(Cs, Sign, 0, Ts, Line);
scan_constant(Cs, Sign, 0, Ts, Line) ->
    scan_oct(Cs, Sign, 0, Ts, Line);
scan_constant(Cs, Sign, N, Ts, Line) ->
    scan_dec(Cs, Sign, N, Ts, Line).


scan_oct([C | Cs], Sign, N, Ts, Line) when C >= $0, C =< $7 ->
    scan_oct(Cs, Sign, N*8 + (C - $0), Ts, Line);
scan_oct(Cs, Sign, N, Ts, Line) ->
    scan(Cs, [{constant, Line, Sign * N} | Ts], Line).


scan_dec([C | Cs], Sign, N, Ts, Line) when C >= $0, C =< $9 ->
    scan_dec(Cs, Sign, N*10 + (C - $0), Ts, Line);
scan_dec(Cs, Sign, N, Ts, Line) ->
    scan(Cs, [{constant, Line, Sign * N} | Ts], Line).

scan_hex([C | Cs], Sign, N, Ts, Line) when C >= $0, C =< $9 ->
    scan_hex(Cs, Sign, N*16 + (C - $0), Ts, Line);
scan_hex([C | Cs], Sign, N, Ts, Line) when C >= $A, C =< $F ->
    scan_hex(Cs, Sign, N*16 + (C - $A) + 10, Ts, Line);
scan_hex([C | Cs], Sign, N, Ts, Line) when C >= $a, C =< $f ->
    scan_hex(Cs, Sign, N*16 + (C - $a) + 10, Ts, Line);
scan_hex(Cs, Sign, N, Ts, Line) ->
    scan(Cs, [{constant, Line, Sign * N} | Ts], Line).

predefined("int") ->      {true,int};
predefined("string") ->   {true,string};
predefined("opaque") ->   {true,opaque};
predefined("void") ->     {true, void};
predefined("float") ->    {true,float};
predefined("double") ->   {true,double};
predefined("hyper") ->    {true,hyper};
predefined("bool") ->     {true,bool};
predefined("unsigned") -> {true,unsigned};
predefined("enum") ->     {true,enum};
predefined("struct") ->   {true,struct};
predefined("union") ->    {true,union};
predefined("case") ->     {true,'case'};
predefined("default") ->  {true,default};
predefined("typedef") ->  {true,typedef};
predefined("switch") ->   {true,switch};
predefined("const") ->    {true, const};
predefined("program") ->  {true, program};
predefined("version") ->  {true, version};
predefined(Id) ->         {false, Id}.




delimiter($[) -> '[';
delimiter($]) -> ']';
delimiter(${) -> '{';
delimiter($}) -> '}';
delimiter($() -> '(';
delimiter($)) -> ')';
delimiter($<) -> '<';
delimiter($>) -> '>';
delimiter($,) -> ',';
delimiter($;) -> ';';
delimiter($:) -> ':';
delimiter($*) -> '*';
delimiter($=) -> '=';
delimiter(_) -> false.
