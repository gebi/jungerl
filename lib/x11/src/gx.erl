%%% File    : gx.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : Protocol encode/decode generator
%%% Created : 30 Jan 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(gx).
-compile(export_all).

-import(lists, [reverse/1, map/2, sum/1]).

start([InFile]) ->
    generate(atom_to_list(InFile)),
    halt().

%%
%% input syntax:
%%
%%  #define NAME VALUE
%%
%%  struct {
%%      <type> elem[,elem]* ';'
%%      [ <type> elem[,elem]* ';' ]*
%%  }
%%
%%  union {
%%      elem1
%%      elem2
%%  }
%%
%%
    

tokenize(File) ->
    {ok,Bin} = file:read_file(File),
    reverse(ts(binary_to_list(Bin))).

remove_comments([{comment,C} | Ts]) ->
    remove_comments(Ts);
remove_comments([T|Ts]) ->
    [T | remove_comments(Ts)];
remove_comments([]) -> [].


parse(File) ->
    ps(remove_comments(tokenize(File)),[]).

generate(In) ->
    Ext  = filename:extension(In),
    Base = filename:basename(In, Ext),
    Dir  = filename:dirname(In),
    generate(In, Dir, Base).
    
generate(In, Dir, Out) ->
    case parse(In) of
	{forms, Fs} ->
	    {Fs1,Types,Defines} = rs(Fs),
	    %% FIXME use typeinfo for typedef of sturctures
	    HRL = Out++".gx.hrl",
	    ERL = Out++".gx.erl",
	    generate_hrl(filename:join(Dir, HRL), Fs, Types),
	    generate_erl(filename:join(Dir, ERL), Fs, Out, HRL),
	    ok;
	Error ->
	    Error
    end.


ps(['#', 'define', {ident,Name} | Ts], Forms) ->
    case Ts of
	[{dec,N}|Ts1] -> 
	    ps(Ts1, [{define,Name,{dec,N}} | Forms]);
	[{hex,N}|Ts1] ->
	    ps(Ts1, [{define,Name,{hex,N}} | Forms]);
	['(', A, '<<', B, ')'|Ts1] ->
	    ps(Ts1, [{define,Name,lshift(A,B)} | Forms]);
	[{ident,Def}|Ts1] ->
	    ps(Ts1, [{define,Name,{ident,Def}} | Forms]);
	_ ->
	    {error, {syntax_error,Ts}}
    end;
ps(['#', 'undef', {ident,Name} | Ts], Forms) ->
    ps(Ts, [{undef,Name} |Forms]);
ps(['#', 'endif', Name | Ts], Forms) ->
    ps(Ts, Forms);
ps(['typedef' | Ts], Forms) ->
    case ps_type(Ts) of
	{{type,T}, [{ident,TypeName},';'|Ts1]} ->
	    case T of
		{struct,_,Elems} ->
		    ps(Ts1, [{typedef,{ident,TypeName},
			      {struct,{ident,TypeName},Elems}} | Forms]);
		{union,_,Elems}  ->
		    ps(Ts1, [{typedef,{ident,TypeName},
			      {union,{ident,TypeName},Elems}} | Forms]);
		_ ->
		    ps(Ts1, [{typedef,{ident,TypeName},T} | Forms])
	    end;
	{{type,T}, Ts1} ->
	    {error, {syntax_error,Ts1}};
	Error ->
	    Error
    end;
ps(['struct',{ident,Name} | Ts], Forms) ->
    case ps_struct(Ts, {ident,Name}) of
	{{struct,Nm,Elems},[';'|Ts1]} ->
	    ps(Ts1, [{typedef,Nm,{struct,Nm,Elems}} | Forms]);
	Error -> Error
    end;
ps(['union',{ident,Name} | Ts], Forms) ->
    case ps_union(Ts, {ident,Name}) of
	{{union,Nm,Elems},[';'|Ts1]} ->
	    ps(Ts1, [{typedef,Nm,{union,Nm,Elems}} | Forms]);
	Error -> Error
    end;
ps([{comment,C}|Ts], Forms) ->
    ps(Ts, Forms);
ps([], Forms) ->
    {forms, reverse(Forms)}.

%%
%% parse a type
%%
%%  [unsigned|signed] char|short|int|long|long long
%%  double
%%  float
%%  struct { <elems> }
%%  union { <elems> }
%%
ps_type([{ident,Name} | Ts]) ->
    {{type, {ident,Name}}, Ts};
ps_type(['struct',{ident,Name}|Ts]) ->
    case ps_struct(Ts, {ident,Name}) of
	{{struct,Nm,Elems},Ts1} ->
	    {{type,{struct,Nm,Elems}}, Ts1};
	Error -> Error
    end;
ps_type(['struct' |Ts]) ->
    case ps_struct(Ts, none) of
	{{struct,Nm,Elems},Ts1} ->
	    {{type,{struct,Nm,Elems}}, Ts1};
	Error -> Error
    end;
ps_type(['union',{ident,Name}|Ts]) ->
    case ps_union(Ts, {ident,Name}) of
	{{union,Nm,Elems},Ts1} ->
	    {{type,{union,Nm,Elems}}, Ts1};
	Error -> Error
    end;
ps_type(['union'|Ts]) ->
    case ps_union(Ts, none) of
	{{union,Nm,Elems},Ts1} ->
	    {{type,{union,Nm,Elems}}, Ts1};
	Error -> Error
    end;
ps_type(['char' | Ts]) ->
    {{type, ['signed','char']}, Ts};
ps_type(['short' | Ts])-> 
    {{type, ['signed','short']}, Ts};
ps_type(['long' | Ts])-> 
    {{type, ['signed','long']}, Ts};
ps_type(['long','long' | Ts])-> 
    {{type, ['signed','long_long']}, Ts};
ps_type(['int' | Ts])-> 
    {{type, ['signed','int']}, Ts};
ps_type(['float' | Ts])-> 
    {{type, 'float'}, Ts};
ps_type(['double' | Ts])-> 
    {{type, 'double'}, Ts};

ps_type(['unsigned', 'char' | Ts]) -> 
    {{type, ['unsigned','char']}, Ts};
ps_type(['unsigned', 'short' | Ts])->
    {{type, ['unsigned','short']}, Ts};
ps_type(['unsigned', 'long' | Ts])->
    {{type, ['unsigned','long']}, Ts};
ps_type(['unsigned', 'long','long' | Ts]) ->
    {{type, ['unsigned','long_long']}, Ts};
ps_type(['unsigned', 'int' | Ts])-> 
    {{type, ['unsigned','int']}, Ts};
ps_type(['unsigned' | Ts])->
    {{type, ['unsigned','int']}, Ts};


ps_type(['signed', 'char' | Ts]) ->
    {{type, ['signed','char']}, Ts};
ps_type(['signed', 'short' | Ts])-> 
    {{type, ['signed','short']}, Ts};
ps_type(['signed', 'long' | Ts])->
    {{type, ['signed','long']}, Ts};
ps_type(['signed', 'long','long' | Ts])->
    {{type, ['signed','long_long']}, Ts};
ps_type(['signed', 'int' | Ts])->
    {{type, ['signed','int']}, Ts};
ps_type(['signed' | Ts])->
    {{type, ['signed','int']}, Ts};
ps_type(Ts) ->
    {error, {unknown_type,Ts}}.

ps_struct(['{'|Ts], Name) ->
    case ps_elems(Ts) of
	{{elems,Elems}, ['}'|Ts1]} ->
	    {{struct,Name,Elems}, Ts1};
	{{elems,Elems}, _} ->
	    {error, {syntax_error,Ts}};
	Error -> Error
    end.

ps_union(['{'|Ts], Name) ->
    case ps_elems(Ts) of
	{{elems,Elems}, ['}'|Ts1]} ->
	    {{union,Name,Elems}, Ts1};
	{{elems,Elems}, _} ->
	    {error, {syntax_error,Ts}};
	Error -> Error
    end.
    
%%
%% parse  <type> <elem>[,<elem>]* ;
%% and expand to
%% <type> elem1
%% <type> elem1
%% ..
%%
ps_elems(Ts) ->
    ps_elems(Ts,[]).


ps_elems(['}'|Ts], Decls) ->
    {{elems,expand_elems(reverse(Decls))}, ['}'|Ts]};

ps_elems(Ts, Decls) ->
    case ps_elem_decl(Ts) of
	{{decl,Type,Elems}, [';'|Ts1]} ->
	    ps_elems(Ts1, [{decl,Type,Elems}|Decls]);
	Error -> Error
    end.

expand_elems([{decl,Type,Es} | Decls]) ->
    map(fun(E) -> {decl,Type,E} end, Es) ++
	expand_elems(Decls);
expand_elems([]) ->
    [].

ps_elem_decl(Ts) ->  
    case ps_type(Ts) of
	{{type,T}, Ts0} ->
	    case ps_field_list(Ts0,[]) of
		{{fields, Fs}, Ts1} ->
		    {{decl,T,Fs}, Ts1};
		Error -> Error
	    end;
	Error -> Error
    end.

ps_field_list(Ts, Fs) ->
    case ps_field(Ts) of
	{{field,Id,Range,Bits},[','|Ts1]} ->
	    ps_field_list(Ts1,[{field,Id,Range,Bits}|Fs]);
	{{field,Id,Range,Bits},Ts1} ->
	    {{fields,reverse([{field,Id,Range,Bits}|Fs])},Ts1};
	Error -> Error
    end.

ps_field([{ident,Id},':',{dec,N} | Ts]) ->
    {{field,{ident,Id},none,list_to_integer(N)}, Ts};
ps_field([{ident,Id},'[',{dec,N}, ']' | Ts]) ->
    {{field,{ident,Id},list_to_integer(N),all}, Ts};
ps_field([{ident,Id} | Ts]) ->
    {{field,{ident,Id},none,all}, Ts};
ps_field(Ts) ->
    {error, {syntax_error, Ts}}.

%%
%% Resolve types in forms
%%
rs(Fs) ->
    rs(Fs,[],[],[]).

rs([F|Fs],RFs,Types,Defines) ->
    case F of
	{typedef,{ident,Name},Type} ->
	    case resolve(Type,Types,Defines) of
		{ok,Type1} ->
		    rs(Fs,[{typedef,{ident,Name},Type1}|RFs],
		       [{Name,Type1}|Types],Defines);
		Error -> Error
	    end;
	{define,Name,Value} ->
	    rs(Fs,[{define,Name,Value}|RFs],
	       Types, [{Name,Value}|Defines]);
	{undef,Name} ->
	    rs(Fs,RFs,Types,
	       lists:keydelete(Name,1,Defines))
    end;
rs([], RFs, Types, Defines) ->
    {reverse(RFs), reverse(Types), reverse(Defines)}.


resolve({ident,Name}, Types, Defines) ->
    resolve_ident(Name, Types, Defines);
resolve({struct,Name,Elems}, Types, Defines) ->
    case resolve_elems(Elems,Types,Defines) of
	{ok, Elems1} -> {ok,{struct,Name,Elems1}};
	Error -> Error
    end;
resolve({union,Name,Elems}, Types, Defines) ->
    case resolve_elems(Elems,Types,Defines) of
	{ok, Elems1} -> {ok,{union,Name,Elems1}};
	Error -> Error
    end;
resolve(Type, Types, Defines) ->
    {ok, Type}.



resolve_elems(Es,Types,Defines) ->
    resolve_elems(Es,[],Types,Defines).

resolve_elems([{decl,{ident,Name},F} | Es],Acc,Types,Defines) ->
    case resolve_ident(Name, Types, Defines) of
	{ok, Type} ->
	    resolve_elems(Es, [{decl,Type,F}|Acc], Types, Defines);
	Error -> Error
    end;
resolve_elems([{decl,Type,F} | Es], Acc, Types, Defines) ->
    case resolve(Type, Types, Defines) of
	{ok,Type1} -> 
	    resolve_elems(Es, [{decl,Type1,F}|Acc], Types, Defines);
	Error -> Error
    end;
resolve_elems([],Acc,Types,Defines) ->
    {ok, reverse(Acc)}.


resolve_ident(Id, Types, Defines) ->    
    case lists:keysearch(Id, 1, Defines) of
	{value, {_, Value}} -> 
	    resolve(Value, Types, Defines);
	false ->
	    case lists:keysearch(Id, 1, Types) of
		{value, {_, Value}} -> 
		    {ok,Value};
		false ->
		    {error, {undefined,Id}}
	    end
    end.

resolve_type(T, Ts) -> 
    case T of
	{ident,Nm} ->
	    case lists:keysearch(Nm, 1, Ts) of
		false -> exit({bad_type,T});
		{value,{_,T1}} -> resolve_type(T1, Ts)
	    end;
	_ -> T
    end.

is_struct(T, Ts) ->
    case resolve_type(T, Ts) of
	{struct,_,_} -> true;
	_ -> false
    end.

%% calculate sizeof type in bytes
sizeof(['signed',T], Ts) -> sizeof(T,Ts);
sizeof(['unsigned',T], Ts) -> sizeof(T,Ts);
sizeof('char',_)  -> 1;
sizeof('short',_) -> 2;
sizeof('long',_)  -> 4;
sizeof('int',_)   -> 4;
sizeof('long_long',_) -> 8;
sizeof('float',_) -> 4;
sizeof('double',_) -> 8;
sizeof({ident,T}, Ts) ->
    sizeof(resolve_type({ident,T},Ts),Ts);
sizeof({struct,_,Elems},Ts) ->
    sum(map(fun({decl,Type,{field,_,Range,Bits}}) ->
		    Sz0 =  if integer(Bits) -> Bits;
			      Bits == all -> sizeof(Type,Ts)
			   end,
		    if Range == none ->
			    Sz0;
		       true ->
			    Sz0*Range
		    end
	    end, Elems)).

%%
%% Tokenizer
%%

ts(Cs) ->
    ts(Cs, []).

ts([$\s | Cs], Ts)   -> ts(Cs,Ts);
ts([$\t | Cs], Ts)   -> ts(Cs,Ts);
ts([$\n | Cs], Ts)   -> ts(Cs,Ts);
ts([$\r | Cs], Ts)   -> ts(Cs,Ts);
ts([$/,$/ | Cs], Ts) -> ts_ln_comment(Cs,[],Ts);
ts([$/,$* | Cs], Ts) -> ts_comment(Cs,[],[],Ts);
ts([$-,$0,$x |Cs], Ts) -> ts_hex(Cs,[$-],Ts);
ts([$0,$x |Cs], Ts) -> ts_hex(Cs,[],Ts);
ts([$0,$x |Cs], Ts) -> ts_hex(Cs,[],Ts);
ts([$-,C|Cs], Ts) when C >=$0,C =< $9 -> ts_dec(Cs,[C,$-],Ts);
ts([C|Cs], Ts) when C >=$0,C =< $9 -> ts_dec(Cs,[C],Ts);
ts([$<,$<|Cs], Ts) -> ts(Cs, ['<<'|Ts]);
ts([$>,$>|Cs], Ts) -> ts(Cs, ['>>'|Ts]);
ts([$<|Cs], Ts) -> ts(Cs, ['<'|Ts]);
ts([$>|Cs], Ts) -> ts(Cs, ['>'|Ts]);
ts([$-|Cs], Ts) -> ts(Cs, ['-'|Ts]);
ts([C|Cs], Ts) when C == ${; C == $}; 
		    C == $(; C == $);
		    C == $[; C == $];
		    C == $;; C == $:; C == $,; C == $.;
		    C == $# ->
    ts(Cs, [list_to_atom([C]) | Ts]);
ts([C|Cs], Ts) -> ts_word(Cs,[C],Ts);
ts([], Ts) -> Ts.

%% decimal number
ts_dec([C|Cs], Int, Ts) when C >=$0,C =< $9 -> ts_dec(Cs, [C|Int], Ts);
ts_dec(Cs, Int, Ts) -> ts(Cs, [{dec,reverse(Int)} | Ts]).

%% hex number
ts_hex([C|Cs], Hex, Ts) when C >=$0,C =< $9;
			     C >=$a,C =< $f;
			     C >=$A,C =< $F ->
    ts_hex(Cs, [C|Hex], Ts);
ts_hex(Cs, Hex, Ts) -> ts(Cs, [{hex,reverse(Hex)} | Ts]).

%% line comment
ts_ln_comment([$\n|Cs],Acc,Ts) -> 
    ts(Cs,[{comment,[reverse(Acc)]}|Ts]);
ts_ln_comment([C|Cs],Acc,Ts) ->
    ts_ln_comment(Cs, [C|Acc], Ts);
ts_ln_comment([], Acc, Ts) ->
    [{comment,[reverse(Acc)]}].

%% multi line comment
ts_comment([$*,$/|Cs],Acc,Lns,Ts) -> 
    ts(Cs,[{comment,reverse([reverse(Acc)|Lns])}|Ts]);
ts_comment([$\n|Cs],Acc,Lns,Ts) ->
    ts_comment(Cs,[],[reverse(Acc)|Lns],Ts);
ts_comment([C|Cs],Acc,Lns,Ts) ->
    ts_comment(Cs, [C|Acc], Lns, Ts);
ts_comment([], Acc, Lns, Ts) ->
    [{comment,reverse([reverse(Acc)|Lns])}].


ts_word([C|Cs], Word, Ts) ->
    if
        C == $\s; C == $\t; C == $\n; C == $\r ->
            ts(Cs,[word(reverse(Word))|Ts]);
        C >=$a,C =< $z; C >=$A,C =< $Z; C >= $0, C =< $9; C == $_ ->
            ts_word(Cs, [C|Word], Ts);
        true ->
            ts([C|Cs],[word(reverse(Word))|Ts])
    end;
ts_word(Cs, Word, Ts) ->
    ts(Cs,[word(reverse(Word))|Ts]).


word("define") -> 'define';
word("undef") -> 'undef';
word("if") -> 'if';
word("ifdef") -> 'ifdef';
word("ifndef") -> 'ifndef';
word("endif") -> 'endif';
word("typedef") -> 'typedef';
word("struct") -> 'struct';
word("union") -> 'union';
word("char") -> 'char';
word("short") -> 'short';
word("long") -> 'long';
word("int") -> 'int';
word("float") -> 'float';
word("double") -> 'double';
word("unsigned") -> 'unsigned';
word("signed") -> 'signed';
word(Ws) -> {ident, Ws}.

    
%% evaluate an (N << M) into a hex mask
lshift(A, B) ->
    {hex,to_hex(to_int(A) bsl to_int(B))}.

to_hex(0) -> "0";
to_hex(N) when N < 0 -> [$-|to_hex(-N, [])];
to_hex(N) -> to_hex(N, []).

to_hex(0, Acc) -> Acc;
to_hex(N, Acc) ->
    D = N rem 16,
    if D < 10 -> to_hex(N div 16, [D+$0 | Acc]);
       true -> to_hex(N div 16, [(D-10)+$a | Acc])
    end.
	    
to_int({dec,Cs}) -> list_to_integer(Cs);
to_int({hex,Cs}) -> hex_to_integer(Cs).

hex_to_integer([$-|Cs]) ->
    -hex_to_integer(Cs,0);
hex_to_integer(Cs) ->
    hex_to_integer(Cs,0).
    
hex_to_integer([C|Cs],N) when C >= $0, C =< $9 ->
    hex_to_integer(Cs, N*16 + (C-$0));
hex_to_integer([C|Cs],N) when C >= $a, C =< $f ->
    hex_to_integer(Cs, N*16 + ((C-$a)+10));
hex_to_integer([C|Cs],N) when C >= $A, C =< $F ->
    hex_to_integer(Cs, N*16 + ((C-$A)+10));
hex_to_integer([],N) -> N.


to_erl({hex,[$-|Cs]}) -> "-16#"++Cs;
to_erl({hex,Cs}) -> "16#"++Cs;
to_erl({dec,Cs}) -> Cs.


record_name([C|Cs]) when C >= $A, C =< $Z ->
    [(C-$A)+$a | Cs];
record_name(Cs) -> Cs.

field_name([C|Cs]) when C >= $A, C =< $Z ->
    [(C-$A)+$a | Cs];
field_name(Cs)  ->
    Cs.

var_name([C|Cs])  when C >= $a, C =< $z ->
    [(C-$a)+$A | Cs];
var_name([C|Cs]) ->
    [C|Cs].

macro_name(Cs) ->
    Cs.

is_pad([$p,$a,$d|_]) -> true;
is_pad([$P,$A,$D|_]) -> true;
is_pad([$P,$a,$d|_]) -> true;
is_pad(_) -> false.
    
%% Generate hrl file

generate_hrl(File, Fs, Types) ->
    Hrl = gen_hrl(Fs, Types),
    Hdr = [
	   "%%\n",
	   "%% generated by gx.erl\n",
	   "%%\n",
	   "-include(\"stdbits.hrl\").\n\n"],
    file:write_file(File,[Hdr,Hrl]).

gen_hrl([{define,Name,Value} | Fs], Ts) ->
    case Value of
	{dec,_} ->
	    ["-define(" ++ macro_name(Name) ++ "," ++ to_erl(Value) ++ ").\n"
	     | gen_hrl(Fs, Ts)];
	{hex,_} ->
	    ["-define(" ++ macro_name(Name) ++ "," ++ to_erl(Value) ++ ").\n"
	     | gen_hrl(Fs, Ts)];
	_ ->
	    gen_hrl(Fs, Ts)
    end;
gen_hrl([{undef,_} | Fs], Ts) ->
    gen_hrl(Fs, Ts);
gen_hrl([{typedef,{ident,Name},{struct,_,Elems}} | Fs], Ts) ->
    [ "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
      "%% " ++ Name ++ "\n",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
      gen_record(Name, Elems),
      gen_macro(Name, Elems,Ts),
      gen_enc_macro(Name, Elems),
      "\n",
      gen_dec_macro(Name, Elems),
      "\n\n" | gen_hrl(Fs, Ts)];

gen_hrl([{typedef,{ident,Name},Type} | Fs], Ts) ->
    case resolve_type(Type, Ts) of
	{struct,_,Elems} ->
	    [ "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
	      "%% " ++ Name ++ "\n",
	      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",
	      gen_record(Name, Elems),
	      gen_macro(Name, Elems, Ts),
	      gen_enc_macro(Name, Elems),
	      "\n",
	      gen_dec_macro(Name, Elems),
	      "\n\n" | gen_hrl(Fs, Ts)];
	_ ->
	    %% If Type is a type name of a structure then use more arguments!
	    [ "-define(" ++ macro_name(Name) ++ "(X)," ++ 
	      gen_macro_type(Type,"X",none,all,Ts) ++ ").\n" |
	      gen_hrl(Fs, Ts) ]
    end;

gen_hrl([{typedef,{ident,Name},_} | Fs], Ts) ->
    gen_hrl(Fs, Ts);

gen_hrl([], _) -> [].

%% Nothing right now
generate_erl(File, Fs, Mod, Hdr) ->
    Erl = gen_erl(Fs, Mod, Hdr),
    file:write_file(File, Erl).

gen_erl(Fs, Mod, Hdr) ->
    [
     "%%\n",
     "%% generated by gx.erl\n",
     "%%\n",
     "-module(", Mod, ").\n",
     "\n",
     "-include(\"", Hdr, "\").\n",
     "\n"].

%%
%% Generate a record definition of a struct declaration
%% but remove all pad* elements from the record structure
%%
gen_record(Name, Elems) ->
    ["-record(" ++ record_name(Name) ++ ",{\n",
     gen_record_fields(remove_pads(Elems)),
     "}).\n\n"].

%% remove all items called "pad*"
remove_pads([F={decl,_,{field,{ident,N},_,_}} | Fs]) ->
    case is_pad(N) of
	true -> remove_pads(Fs);
	false -> [F|remove_pads(Fs)]
    end;
remove_pads([]) -> [].
	    
gen_record_fields([]) ->
    "";
gen_record_fields([{decl,_,{field,{ident,N},_,_}}]) ->
    "    " ++ field_name(N) ++ "\n";
gen_record_fields([{decl,_,{field,{ident,N},_,_}}|Fs]) ->
    [ "    " ++ field_name(N) ++ ",\n" | gen_record_fields(Fs)].

%%
%% Generate a macro for binary syntax encoding/decoding a structure
%%
gen_macro(Name, Elems,Ts) ->
    [ "-define(" ++ macro_name(Name) ++ "(" ++ 
      gen_macro_args(Elems) ++ "),\n" ++
      gen_macro_fields(Elems,Ts),
      ").\n\n" ].

%%
%% Encoding macro is nearly straight forward
%% X_enc(R) -> binary
%%
gen_enc_macro(Name, Elems) ->
    [ "-define(" ++ macro_name(Name) ++ "_enc(R),\n" ++
      "  << ?"++macro_name(Name)++"(", gen_enc_macro_args(Name,Elems) ++ ")>>).\n"].

gen_enc_macro_args(Name,[]) ->
    [ ];
gen_enc_macro_args(Name,[F]) ->
    [ gen_enc_macro_arg(Name,F) ];
gen_enc_macro_args(Name,[F|Es]) ->
    [ gen_enc_macro_arg(Name,F), "," | gen_enc_macro_args(Name,Es)].

gen_enc_macro_arg(Name, {decl,_,{field,{ident,N},_,_}}) ->
    case is_pad(N) of
	true -> "0";
	false -> "R#" ++ record_name(Name) ++ "." ++ field_name(N)
    end.

%%
%% Decode macro is not so staight forward
%% X_dec(Bin) -> {R, Bin'}
%%
gen_dec_macro(Name, Elems) ->
    [ "-define(" ++ macro_name(Name) ++ "_dec(B0),\n" ++
      " begin <<?"++macro_name(Name)++"("++ 
               gen_dec_macro_args(Name,Elems) ++ "),B1/binary>> = B0,\n",
      "  { #"++record_name(Name)++
      "{"++gen_dec_macro_assign(Name,Elems) ++ " }, B1} end).\n"].

gen_dec_macro_args(Name,Fs) ->
    comma_list(gen_dec_macro_args2(Name,Fs)).

gen_dec_macro_assign(Name, Fs) ->
    comma_list(gen_dec_macro_args3(Name,Fs)).
    

%% macro call
gen_dec_macro_args2(Name,[{decl,_,{field,{ident,N},_,_}}|Fs]) ->
    case is_pad(N) of
	true -> 
	    [ "_" | gen_dec_macro_args2(Name,Fs)];
	false -> 
	    [ "R_" ++ record_name(Name) ++ "_" ++ field_name(N) |
	      gen_dec_macro_args2(Name,Fs)]
    end;
gen_dec_macro_args2(Name, []) -> [].

%% record assign
gen_dec_macro_args3(Name,[{decl,_,{field,{ident,N},_,_}}|Fs]) ->
    case is_pad(N) of
	true -> gen_dec_macro_args3(Name,Fs);
	false -> 
	    [ field_name(N) ++ "=R_" ++ record_name(Name) ++ "_" ++ 
	      field_name(N) |
	      gen_dec_macro_args3(Name,Fs)]
    end;
gen_dec_macro_args3(Name, []) -> [].



comma_list([]) -> [];
comma_list([F]) -> [F];
comma_list([F|Fs]) -> [F, ", " | comma_list(Fs)].



gen_macro_args([{decl,_,{field,{ident,N},_,_}}]) ->
    var_name(N);
gen_macro_args([{decl,_,{field,{ident,N},_,_}}|Fs]) ->
    var_name(N)++","++gen_macro_args(Fs);
gen_macro_args([]) ->
    "".

gen_macro_fields([F],Ts) ->
    [ "    " ++ gen_macro_field(F,Ts) ];
gen_macro_fields([F|Fs],Ts) ->
    ["    " ++ gen_macro_field(F,Ts)++",\n" | gen_macro_fields(Fs,Ts)];
gen_macro_fields([],_) ->
    "".

gen_macro_field({decl,Type,{field,{ident,N},Range,Bits}}, Ts) ->
    case is_struct(Type, Ts) of
	true -> 
	    Sz0 = sizeof(Type, Ts),
	    Sz = if Range == none -> Sz0;
		    true -> Range*Sz0
		 end,
	    var_name(N) ++ ":" ++ integer_to_list(Sz) ++ "/binary";
	false ->
	    gen_macro_type(Type,var_name(N),Range,Bits,Ts)
    end.


gen_macro_type({struct,{ident,TypeName},Elems},Name,none,_,Ts) ->
    "?" ++ macro_name(TypeName) ++ "(" ++ Name ++ ")";
gen_macro_type({union,{ident,TypeName},Elems},Name,none,_,Ts) ->
    "?" ++ macro_name(TypeName) ++ "(" ++ Name ++ ")";
gen_macro_type({union,none,Elems},Name,none,_,Ts) ->
    "UNION";
gen_macro_type({ident,TypeName},Name,none,_,Ts) ->
    "?" ++ macro_name(TypeName) ++ "(" ++ Name ++ ")";
gen_macro_type(['unsigned',T],Name,none,all,Ts) ->
    "?" ++ atom_to_list(T) ++ "(" ++ 
	Name ++ ",unsigned,?ENDIAN)";
gen_macro_type(['signed',T],Name,none,all,Ts) ->
    "?" ++ atom_to_list(T) ++ "(" ++ 
	Name ++ ",signed,?ENDIAN)";
gen_macro_type('float',Name,none,all,Ts) ->
    "?" ++ "float(" ++ 
	Name ++ ",?ENDIAN)";
gen_macro_type('double',Name,none,all,Ts) ->
    "?" ++ "double(" ++ 
	Name ++ ",?ENDIAN)";
gen_macro_type(['unsigned','int'],Name,none,N,Ts) ->
    Name ++ ":" ++ integer_to_list(N) ++
	"/?ENDIAN-unsigned-integer";
gen_macro_type(['signed','int'],Name,none,N,Ts) ->
    Name ++ ":" ++ integer_to_list(N) ++
	"/?ENDIAN-signed-integer";
gen_macro_type(['unsigned','char'],Name,N,all,Ts) ->
    Name ++ ":" ++ integer_to_list(N) ++
	"/binary";
gen_macro_type(['signed','char'],Name,N,all,Ts) ->
    Name ++ ":" ++ integer_to_list(N) ++
	"/binary";
gen_macro_type(T,Name,Range,Bits, Ts) ->
    T1 = resolve_type(T, Ts),
    if T == T1 ->
	    exit({bad_type,T});
       true ->
	    gen_macro_type(T1,Name,Range,Bits,Ts)
    end.
    
    
    












     





    


    

	
    

				
				
				

