%%% File    : ucs_data_build.erl
%%% Author  : Lon Willett <Lon.Willett@sse.ie>,
%%%           Johan Blom <johan.blom@mobilearts.se>
%%% Description : Build the database (dets) files used by the ucs_data module

-module(ucs_data_build).

-vsn('0.3').
-author('Lon.Willett@sse.ie').
-modified_by('johan.blom@mobilearts.se').
-compile([verbose,report_warnings,warn_unused_vars]).

-include("unidata.hrl").

-export([rebuild/3,create_tuple/1]).
-import(ucs,[is_iso646_basic/1]).

rebuild(all,Dir,MappingDir) ->
    ok = unidata(Dir),
    ok = mnemonics(Dir),
    ok = mibenum(Dir),
    ok = mappings(Dir,MappingDir),
    ok;
rebuild(unidata,Dir,_MappingDir) ->
    unidata(Dir);
rebuild(mnemonics,Dir,_MappingDir) ->
    mnemonics(Dir);
rebuild(mibenum,Dir,_MappingDir) ->
    mibenum(Dir);
rebuild(mappings,Dir,MappingDir) ->
    mappings(Dir,MappingDir).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mnemonics(Dir) ->
    %% Build the mnemonics database
    SrcPath = filename:join(Dir,"ucsmnem.edat"),
    UnidataPath = filename:join(Dir,"db/unidata.dets"),
    Tmp1Path = filename:join(Dir,"db/ucsmnem1.tmp"),
    Tmp2Path = filename:join(Dir,"db/ucsmnem2.tmp"),
    DB1Path = filename:join(Dir,"db/ucsmnem1.dets"),
    DB2Path = filename:join(Dir,"db/ucsmnem2.dets"),
    %% Read the data as erlang terms
    {ok,List} = file:consult(SrcPath),
    %% Use the unidata.dets file to check the names
    {ok,unicode_data} =
	dets:open_file(unicode_data,
		       [{type,set},
			{keypos,#unidata.code},
			{file,UnidataPath},
			{access,read}]),
    %% Create new dets files
    N = length(List),
    file:delete(Tmp1Path),
    file:delete(Tmp2Path),
    {ok,new_ucs_mnemonics_bycode} =
	dets:open_file(new_ucs_mnemonics_bycode,
		       [{type,set},
			{keypos,1},
			{file,Tmp1Path},
			{access,read_write},
			{estimated_no_objects,N}]),
    {ok,new_ucs_mnemonics_bymnem} =
	dets:open_file(new_ucs_mnemonics_bymnem,
		       [{type,set},
			{keypos,2},
			{file,Tmp2Path},
			{access,read_write},
			{estimated_no_objects,N}]),
    %% Add the elements and close the dets files
    ok = lists:foreach(fun add_mnemonic/1, List),
    ok = dets:close(new_ucs_mnemonics_bycode),
    ok = dets:close(new_ucs_mnemonics_bymnem),
    ok = dets:close(unicode_data),
    file:delete(DB1Path),
    file:delete(DB2Path),
    ok = file:rename(Tmp1Path,DB1Path),
    ok = file:rename(Tmp2Path,DB2Path),
    ok.

add_mnemonic({Mnem, Code, Name, Comment}) ->
    %% Verify that code and mnemonic are unique
    [] = dets:lookup(new_ucs_mnemonics_bycode,Code),
    [] = dets:lookup(new_ucs_mnemonics_bymnem,Mnem),
    %%
    %% Verify that mnemonic is legal:
    %%
    %%	Mnemonics are composed of ISO-646.basic graphic chars.
    %%
    %%	The ISO-646.basic graphic chars must have themselves defined
    %%	as their mnemonic.
    %%
    %%	No mnemonic can start with "_" (other than "_" by itself).
    %%
    %%	No mnemonic can contain "&" (other than "&" by itself).
    %%
    %%	No long mnemonic can contain "_".
    %%
    %%	No long mnemonic can start with "?u".
    %%
    case Mnem of
	[Code] ->
	    true = ((Code =/= $\s) and is_iso646_basic(Code));
	[Ch1,Ch2] ->
	    false = ((Code =/= $\s) and is_iso646_basic(Code)),
	    true = (is_iso646_basic(Ch1)
		    and (Ch1 =/= $\s) and (Ch1 =/= $&) and (Ch1 =/= $_)),
	    true = (is_iso646_basic(Ch2)
		    and (Ch2 =/= $\s) and (Ch2 =/= $&));
	[Ch1,Ch2|_] ->
	    false = is_iso646_basic(Code),
	    false = ((Ch1 =:= $?) and (Ch2 =:= $u)),
	    VerifyValidChar =
		fun(Ch) ->
			true = (is_iso646_basic(Ch) and
				(Ch =/= $\s) and (Ch =/= $&) and (Ch =/= $_))
		end,
	    lists:foreach(VerifyValidChar, Mnem)
    end,
    %% Verify that Name and Comment match that in the unicode data
    %% (except for the control chars)
    case dets:lookup(unicode_data,Code) of
	[Unidata] when record(Unidata,unidata) ->
	    {NameCheck,CommentCheck} =
		if Unidata#unidata.name =:= "<control>" ->
			%% Control characters are special
			true = (((Code >= 0) and (Code < 32)) or
				((Code >= 127) and (Code < 160))),
			case Unidata#unidata.old_name of
			    "" ->
				%% No check at all
				{Name,Comment};
			    OldName ->
				%% Check the name against the Unicode 1.0 name.
				%% Don't check the 10646 comment.
				{OldName,Comment}
			end;
		   true ->
			{Unidata#unidata.name, Unidata#unidata.comment}
		end,
	    if Name =/= NameCheck; Comment =/= CommentCheck ->
		    error_logger:info_msg(
    "UCS DB WARNING: Name/comment mismatch for character ~w: ~s (~s) =/= ~s (~s)~n",
		      [Code,Name,Comment,NameCheck,CommentCheck]);
	       true -> ok
	    end;
	[] ->
	    error_logger:info_msg(
	      "UCS DB WARNING: Char ~w missing from unicode data~n",
	      [Code])
    end,
    %% Insert the records
    ok = dets:insert(new_ucs_mnemonics_bycode,{Code,Mnem}),
    ok = dets:insert(new_ucs_mnemonics_bymnem,{Code,Mnem}),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unidata(Dir) ->
    %% Build the unicode database ("unidata.dets") from "UnicodeData.txt"
    SrcPath = filename:join(Dir,"UnicodeData.txt"),
    TmpPath = filename:join(Dir,"db/unidata.tmp"),
    DBPath = filename:join(Dir,"db/unidata.dets"),
    {ok,InputFile} = file:open(SrcPath,[read]),
    file:delete(TmpPath),
    {ok,new_unicode_data} =
	dets:open_file(new_unicode_data,
		       [{type,set},
			{keypos,#unidata.code},
			{file,TmpPath},
			{access,read_write},
			{estimated_no_objects,11000}]),
    Blocks = lists:map(fun ({Low,High}) -> {Low,High,false} end,
		       ucs_data:unicode_fixed_data_blocks()),
    ok = process_unicode_data_file(InputFile,Blocks),
    ok = dets:close(new_unicode_data),
    ok = file:close(InputFile),
    file:delete(DBPath),
    ok = file:rename(TmpPath,DBPath),
    ok.

process_unicode_data_file(InputFile,Blocks) ->
    case io:get_line(InputFile,"") of
	eof ->
	    %% Make sure all the blocks were processed
	    case lists:all(fun({_,_,Flag}) -> Flag end, Blocks) of
		true -> ok;
		false ->
		    error_logger:info_msg(
    "UCS DB WARNING: the definition of ucs_data:unicode_fixed_data_blocks/0 seems to be incorrect~n")
	    end,
	    ok;
	Line ->
	    Blocks1 = process_unidata_line(Line,Blocks),
	    process_unicode_data_file(InputFile,Blocks1)
    end.

process_unidata_line(Line,Blocks) ->
    case catch parse_unicode_data(Line) of
	Unidata when record(Unidata,unidata) ->
	    %% The blocks of fixed-data elements need some mangling
	    %% and extra tracking...
	    case fixup_blocks(Unidata,Blocks) of
		false ->
		    %% Normal case: no in a special block
		    insert_unidata_record(Unidata),
		    Blocks;
		Unidata1 when record(Unidata1,unidata) ->
		    %% Start of block: insert modified record, "Blocks"
		    %% state doesn't change.
		    insert_unidata_record(Unidata1),
		    Blocks;
		Blocks1 when list(Blocks1) ->
		    %% End of block: don't insert anything, but "Blocks"
		    %% state has been modified (indicating that the block
		    %% has been fully processed).
		    Blocks1
	    end;
	{'EXIT',_} ->
	    error_logger:info_msg(
	      "UCS DB WARNING: failed to parse UnicodeData.txt line: ~s~n",
	      [Line]),
	    Blocks
    end.

insert_unidata_record(Unidata) ->
    case dets:lookup(new_unicode_data,Unidata#unidata.code) of
	[] ->
	    ok = dets:insert(new_unicode_data,Unidata);
	true ->
	    error_logger:info_msg(
	      "UCS DB WARNING: duplicate definition for character ~w~n",
	      [Unidata#unidata.code])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Creates a single dets table for both encoding/decoding of MIBenum values
%%% Build the mibenum database ("mibenum.dets") from "character-sets"
mibenum(Dir) ->
    SrcPath = filename:join(Dir,"character-sets"),
    SrcPath2 = filename:join(Dir,"character-sets-not-IANA"),
    TmpPath = filename:join(Dir,"db/mibenum.tmp"),
    DBPath = filename:join(Dir,"db/mibenum.dets"),
    {ok,InputFile} = file:open(SrcPath,[read]),
    file:delete(TmpPath),
    {ok,new_mibenum_data} =
	dets:open_file(new_mibenum_data,
		       [{type,set},
			{keypos,1},
			{file,TmpPath},
			{access,read_write},
			{estimated_no_objects,1000}]),
    ok = process_mibenum_data_file(InputFile),
    ok = process_nonIANA_charsets(SrcPath2),
    ok = dets:close(new_mibenum_data),
    ok = file:close(InputFile),
    file:delete(DBPath),
    ok = file:rename(TmpPath,DBPath),
    ok.

%%% Note that sometimes a character set has no alias which may be indicated by
%%% - the alias field is missing completely
%%% - the alias entry says "None"
%%% - emtpy alias field
process_mibenum_data_file(InputFile) ->
    case scan_name_line(InputFile) of
	eof ->
	    ok;
	Name ->
%	    io:format("scanned ~p~n",[Name]),
	    MIBenum=scan_mibenum_line(InputFile),
%	    io:format("   MIBenum ~p~n",[MIBenum]),
	    AliasList=scan_alias_lines(InputFile),
%	    io:format("   AliasList ~p~n",[AliasList]),
	    ok=insert_mibenum_data(MIBenum,Name,AliasList),
	    process_mibenum_data_file(InputFile)
    end.

%%% Handle character sets wich are not yet registred with IANA.
%%% Note that these do not have a MIB number, insert the name instead.
process_nonIANA_charsets(InputFile) ->
    case catch file:consult(InputFile) of
	{ok,NameList} when list(NameList) ->
	    process_nonIANA_charsets2(NameList);
	_ ->
	    {error,bad_charset_file}
    end.

process_nonIANA_charsets2([]) ->
    ok;
process_nonIANA_charsets2([Name|NameList]) ->
    ok=insert_mibenum_data(Name,Name,[]),
    process_nonIANA_charsets2(NameList).


scan_name_line(InputFile) ->
    case io:get_line(InputFile,"") of
	eof ->
	    eof;
	"Name:"++Line ->
	    Line2=lists:dropwhile(fun is_space/1, Line),
	    list_to_atom(httpd_util:to_lower(scan_name(Line2)));
	_ ->
	    scan_name_line(InputFile)
    end.

scan_mibenum_line(InputFile) ->
    case io:get_line(InputFile,"") of
	eof ->
	    throw({error,eof});
	"MIBenum:"++Line ->
	    Line2=lists:dropwhile(fun is_space/1, Line),
	    list_to_integer(httpd_util:to_lower(scan_name(Line2)));
	_ ->
	    scan_mibenum_line(InputFile)
    end.

%%% Store all aliases.
scan_alias_lines(InputFile) ->
    case io:get_line(InputFile,"") of
	eof ->
	    throw({error,eof});
	"Alias:"++Line ->
	    Line2=lists:dropwhile(fun is_space/1, Line),
	    case scan_name(Line2) of
		[] ->
		    [];
		"None" ->
		    [];
		Name ->
		    Alias=list_to_atom(httpd_util:to_lower(Name)),
		    [Alias|scan_alias_lines2(InputFile)]
	    end;
	"\n" -> % Patch to fix bug(?) in character-sets
	    [];
	_ ->
	    scan_alias_lines(InputFile)
    end.

scan_alias_lines2(InputFile) ->
    case io:get_line(InputFile,"") of
	eof ->
	    throw({error,eof});
	"Alias:"++Line ->
	    Line2=lists:dropwhile(fun is_space/1, Line),
	    case scan_name(Line2) of
		[] ->
		    [];
		"None" -> % Patch to fix bug(?) in character-sets
		    [];
		Name ->
		    Alias=list_to_atom(httpd_util:to_lower(Name)),
		    [Alias|scan_alias_lines2(InputFile)]
	    end;
	_ ->
	    []
    end.

scan_name([Char|Line]) ->
    case is_space(Char) of
	false ->
	    [Char|scan_name(Line)];
	true ->
	    []
    end;
scan_name([]) ->
    [].


insert_mibenum_data(MIBenum,Name,AliasList) ->
    ok = dets:insert(new_mibenum_data,{Name,MIBenum}),
    ok = dets:insert(new_mibenum_data,{MIBenum,Name}),
    insert_aliases(MIBenum,AliasList).

insert_aliases(_,[]) ->
    ok;
insert_aliases(MIBenum,[Alias|AliasList]) ->
    ok = dets:insert(new_mibenum_data,{Alias,MIBenum}),
    insert_aliases(MIBenum,AliasList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifndef(UNICODE_MAPPING_DIR).
-define(UNICODE_MAPPING_DIR,".").
-endif.

mappings(Dir,MappingDir) ->
    %% Build mapping tables ("unicode-CHARSET.dets" from "unicode-CHARSET.txt")
    %% where CHARSET is a character set name, as defined by IANA.
    {ok,Dirlist}=file:list_dir(MappingDir),
    mappings2(Dir,MappingDir,scan_dir(Dirlist)).

%%% Scans a directory for "unicode-*.txt" files, i.e.files that contains mapping
%%% tables between Unicode and a character sets.
scan_dir([]) ->
    [];
scan_dir(["unicode-"++CharsetFile|Rest]) ->
    case lists:reverse(CharsetFile) of
	"txt."++Charset ->
	    [list_to_atom(lists:reverse(Charset)) | scan_dir(Rest)];
	_ ->
	    scan_dir(Rest)
    end;
scan_dir([_|Rest]) ->
    scan_dir(Rest).

mappings2(_,_,[]) ->
    ok;
mappings2(Dir,MappingDir,[Charset|Rest]) ->
    Name="unicode-"++atom_to_list(Charset),
    SrcPath = filename:join([MappingDir,Name++".txt"]),
    DBPath = filename:join([Dir,"db/",Name++".tab"]),

    {ok,InputFile} = file:open(SrcPath,[read]),
    case ets:info(Charset) of
	undefined ->
	    ok;
	_ ->
	    io:format("Charset known: ~p ~p !~n",[Charset,ets:all()]),
	    ets:delete(Charset)
    end,
    CharsetTab = ets:new(Charset,[set,named_table]),
    ok = process_mapping_data_file(Charset,InputFile),
    ok = file:close(InputFile),
    ok = ets:tab2file(CharsetTab,DBPath),
    true = ets:delete(Charset),
    io:format("Processed charset: ~p successfully!~n",[Charset]),
    mappings2(Dir,MappingDir,Rest).


%%% Mapping files consists of three fields, seprated by whitespace
%%% - Character set code
%%% - Unicocde
%%% - Unicode name
%%% Lines with comments start with '#'
process_mapping_data_file(Tab,InputFile) ->
    case scan_map_line(InputFile) of
	eof ->
	    ok;
	{Charnum,Unicode} ->
	    insert_mapping_data(Tab,Charnum,Unicode),
	    process_mapping_data_file(Tab,InputFile)
    end.


scan_map_line(InputFile) ->
    case io:get_line(InputFile,"") of
	eof ->
	    eof;
	"0x"++Str ->
	    {Char,Len1,Str1} = scan_hex(Str),
	    true = (Len1 > 0),
	    "0x"++Str2=lists:dropwhile(fun is_space/1,Str1),
	    {UniChar,Len3,_} = scan_hex(Str2),
	    true = (Len3 > 0),
	    {Char,UniChar};
	_ ->
	    scan_map_line(InputFile)
    end.

insert_mapping_data(Tab,Char,UniChar) when integer(Char),integer(UniChar) ->
    true = ets:insert(Tab,{Char,UniChar}),
    true = ets:insert(Tab,{{UniChar},Char}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Support functions for handling the fixed-data character blocks

fixup_blocks(Unidata,Blocks) ->
    case find_code_in_blocks(1,Unidata#unidata.code,Blocks) of
	false -> false;
	{low,N} ->
	    Name = Unidata#unidata.name,
	    BlockName = delete_suffix(Name, ", First>"),
	    if Name =/= BlockName ->
		    Unidata#unidata{name=BlockName};
	       true ->
		    error_logger:info_msg(
    "UCS DB WARNING: block first character (~w) name failed to match expected form~n",
		      [Unidata#unidata.code]),
		    Unidata
	    end;
	{high,N} ->
	    #unidata{code=Code,name=Name} = Unidata,
	    BlockName = delete_suffix(Name, ", Last>"),
	    if Name =/= BlockName ->
		    ok;
	       true ->
		    error_logger:info_msg(
    "UCS DB WARNING: block last character (~w) name failed to match expected form~n",
		      [Unidata#unidata.code])
	    end,
	    {Low,Code,_} = lists:nth(N,Blocks),
	    case dets:lookup(new_unicode_data,Low) of
		[] ->
		    error_logger:info_msg(
		      "UCS DB WARNING: block start missing (char ~w)~n",
		      [Low]);
		[Unidata0] ->
		    BlockName0 = Unidata0#unidata.name,
		    Unidata1 = Unidata#unidata{code=Low,name=BlockName0},
		    if Unidata0 =:= Unidata1 -> ok;
		       true ->
			    error_logger:info_msg(
    "UCS DB WARNING: start/end of block definitions don't match (chars ~w - ~w)~n",
			      [Low,Unidata#unidata.code])
		    end
	    end,
	    set_block_processed_flag(N,Blocks);
	{inside,N} ->
	    error_logger:info_msg(
	      "UCS DB WARNING: character (~w) inside fixed block ignored~n",
	      [Unidata#unidata.code]),
	    Blocks
    end.

set_block_processed_flag(1,[{Low,High,Flag}|Blocks]) ->
    if Flag =/= false ->
	    error_logger:info_msg(
    "UCS DB WARNING: multiple definitions of fixed block end (char ~w)~n",
		      [High]);
       true -> ok
    end,
    [{Low,High,true}|Blocks];
set_block_processed_flag(N,[Block|Blocks]) ->
    [Block|set_block_processed_flag(N-1,Blocks)].

find_code_in_blocks(N,Code,[{Low,High,_Flag}|_]) when Code =< High ->
    if Code < Low ->
	    false;
       Code =:= Low ->
	    {low,N};
       Code =:= High ->
	    {high,N};
       true ->
	    {inside,N}
    end;
find_code_in_blocks(N,Code,[_|Blocks]) ->
    find_code_in_blocks(N+1,Code,Blocks);
find_code_in_blocks(_,_,[]) ->
    false.

delete_suffix(Suffix,Suffix) ->
    [];
delete_suffix([Chr|Rest],Suffix) ->
    [Chr|delete_suffix(Rest,Suffix)];
delete_suffix([],_) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Support functions to take a "UnicodeData.txt" line, and generate
%%% a 'unidata' record.

parse_unicode_data(Line) ->

    {Code,LenCode,[$;|T0]} = scan_hex(Line),
    true = (LenCode > 0),

    {Name,T1} = next_field(T0),

    {Fld2,T2} = next_field(T1),
    Category = parse_category(Fld2),

    {Combining,LenComb,[$;|T3]} = scan_integer(T2),
    true = (LenComb > 0),
    true = ((Combining >= 0) and (Combining < 256)),

    {Fld4,T4} = next_field(T3),
    Bidi = parse_bidi_category(Fld4),

    {Fld5,T5} = next_field(T4),
    Decomp = parse_decomposition(Fld5),

    {Fld6,T6} = next_field(T5),
    {Fld7,T7} = next_field(T6),
    {Fld8,T8} = next_field(T7),
    Numeric = parse_numeric_values(Fld6,Fld7,Fld8),

    [Fld9C,$;|T9] = T8,
    Mirrored = case Fld9C of $N -> false; $Y -> true end,

    {OldName,T10} = next_field(T9),
    {Comment,T11} = next_field(T10),

    {CaseMapping1,[$;|T12]} = optional_code(T11,upper,[]),
    {CaseMapping2,[$;|T13]} = optional_code(T12,lower,CaseMapping1),
    {CaseMapping3,T14} = optional_code(T13,title,CaseMapping2),
    CaseMapping=lists:reverse(CaseMapping3),

    [] = lists:dropwhile(fun is_space/1, T14),

    #unidata{code=Code,
	     name=Name,
	     category=Category,
	     combining=Combining,
	     bidi=Bidi,
	     decomp=Decomp,
	     numeric=Numeric,
	     mirrored=Mirrored,
	     old_name=OldName,
	     comment=Comment,
	     case_mapping=CaseMapping
	    }.

is_space(Char) ->
    if Char =:= $\s; Char =:= $\t; Char =:= $\r; Char =:= $\n -> true;
       true -> false
    end.

next_field(Str) -> next_field(Str,[]).

next_field([$;|Str],Acc) -> {lists:reverse(Acc),Str};
next_field([Chr|Str],Acc) -> next_field(Str,[Chr|Acc]).

optional_code(Str,Key,List) ->
    %% Add {Key,Code} to head of List, where Code is read from the
    %% start of string, and return {NewList,RestOfStr}.  If Str
    %% does not start with a character code, return {List,Str}
    %% unmodified.
    case scan_hex(Str) of
	{0,0,Str} -> {List,Str};
	{Code,Len,Rest} when Len > 0 -> {[{Key,Code}|List],Rest}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scan_integer(Str) ->
    scan_integer_d(0,0,Str,10).

scan_hex(Str) ->
    scan_integer_x(0,0,Str,16).

%  scan_integer(Str,Base) when integer(Base), Base > 1, Base =< 10 ->
%      scan_integer_d(0,0,Str,Base);
%  scan_integer(Str,Base) when integer(Base), Base > 10, Base =< 36 ->
%      scan_integer_x(0,0,Str,Base).

scan_integer_d(N,Len,[Digit|Str],Base)
  when integer(Digit), Digit >= $0, Digit =< $0+Base-1 ->
    scan_integer_d(Base*N+Digit-$0,Len+1,Str,Base);
scan_integer_d(N,Len,Str,_) ->
    {N,Len,Str}.

scan_integer_x(N,Len,[Digit|Str],Base)
  when integer(Digit), Digit >= $0, Digit =< $9 ->
    scan_integer_x(Base*N+Digit-$0,Len+1,Str,Base);
scan_integer_x(N,Len,[Digit|Str],Base)
  when integer(Digit), Digit >= $A, Digit =< $A+Base-11 ->
    scan_integer_x(Base*N+Digit-($A-10),Len+1,Str,Base);
scan_integer_x(N,Len,[Digit|Str],Base)
  when integer(Digit), Digit >= $a, Digit =< $a+Base-11 ->
    scan_integer_x(Base*N+Digit-($a-10),Len+1,Str,Base);
scan_integer_x(N,Len,Str,_) ->
    {N,Len,Str}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_category(Str) ->
    Category = list_to_atom(Str),
    case Category of
	'Lu' -> ok;	% Letter, Uppercase
	'Ll' -> ok;	% Letter, Lowercase
	'Lt' -> ok;	% Letter, Titlecase
	'Lm' -> ok;	% Letter, Modifier
	'Lo' -> ok;	% Letter, Other
	'Mn' -> ok;	% Mark, Non-Spacing
	'Mc' -> ok;	% Mark, Spacing Combining
	'Me' -> ok;	% Mark, Enclosing
	'Nd' -> ok;	% Number, Decimal Digit
	'Nl' -> ok;	% Number, Letter
	'No' -> ok;	% Number, Other
	'Zs' -> ok;	% Separator, Space
	'Zl' -> ok;	% Separator, Line
	'Zp' -> ok;	% Separator, Paragraph
	'Cc' -> ok;	% Other, Control
	'Cf' -> ok;	% Other, Format
	'Cs' -> ok;	% Other, Surrogate
	'Co' -> ok;	% Other, Private Use
	'Cn' -> ok;	% Other, Not Assigned
	'Pc' -> ok;	% Punctuation, Connector
	'Pd' -> ok;	% Punctuation, Dash
	'Ps' -> ok;	% Punctuation, Open
	'Pe' -> ok;	% Punctuation, Close
	'Pi' -> ok;	% Punctuation, Initial quote
	'Pf' -> ok;	% Punctuation, Final quote
	'Po' -> ok;	% Punctuation, Other
	'Sm' -> ok;	% Symbol, Math
	'Sc' -> ok;	% Symbol, Currency
	'Sk' -> ok;	% Symbol, Modifier
	'So' -> ok;	% Symbol, Other
	_ ->
	    error_logger:info_msg(
	      "UCS DB WARNING: unknown general category: ~s~n", [Category])
    end,
    Category.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_bidi_category(Str) ->
    Bidi = list_to_atom(Str),
    case Bidi of
	'L' -> ok;	% Left-to-Right
	'LRE' -> ok;	% Left-to-Right Embedding
	'LRO' -> ok;	% Left-to-Right Override
	'R' -> ok;	% Right-to-Left
	'AL' -> ok;	% Right-to-Left Arabic
	'RLE' -> ok;	% Right-to-Left Embedding
	'RLO' -> ok;	% Right-to-Left Override	
	'PDF' -> ok;	% Pop Directional Format
	'EN' -> ok;	% European Number
	'ES' -> ok;	% European Number Separator
	'ET' -> ok;	% European Number Terminator
	'AN' -> ok;	% Arabic Number
	'CS' -> ok;	% Common Number Separator
	'NSM' -> ok;	% Non-Spacing Mark
	'BN' -> ok;	% Boundary Neutral
	'B' -> ok;	% Paragraph Separator
	'S' -> ok;	% Segment Separator
	'WS' -> ok;	% Whitespace
	'ON' -> ok;	% Other Neutrals
	_ ->
	    error_logger:info_msg(
	      "UCS DB WARNING: unknown bidirectional category: ~s~n", [Bidi])
    end,
    Bidi.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_decomposition("") ->
    [];
parse_decomposition(Str) when hd(Str) =:= $< ->
    {Tag,Str1} = parse_decomp_tag(Str),
    [Tag|parse_char_sequence(Str1)];
parse_decomposition(Str) ->
    parse_char_sequence(Str).

parse_decomp_tag([$<|Str]) ->
    parse_decomp_tag([$<],Str).

parse_decomp_tag(Acc,[$>,$\s|Rest]) ->
    Tag = list_to_atom(lists:reverse(Acc,[$>])),
    case Tag of
	'<font>' -> ok;
	'<noBreak>' -> ok;
	'<initial>' -> ok;
	'<medial>' -> ok;
	'<final>' -> ok;
	'<isolated>' -> ok;
	'<circle>' -> ok;
	'<super>' -> ok;
	'<sub>' -> ok;
	'<vertical>' -> ok;
	'<wide>' -> ok;
	'<narrow>' -> ok;
	'<small>' -> ok;
	'<square>' -> ok;
	'<fraction>' -> ok;
	'<compat>' -> ok;
	_ ->
	    error_logger:info_msg(
	      "UCS DB WARNING: unknown decomposition tag: ~s~n", [Tag])
    end,
    {Tag,Rest};
parse_decomp_tag(Acc,[Chr|Rest]) ->
    parse_decomp_tag([Chr|Acc],Rest).

parse_char_sequence(Str) ->
    {Char,Len,Str1} = scan_hex(Str),
    true = (Len > 0),
    case Str1 of
	[$\s|Str2] ->
	    [Char|parse_char_sequence(Str2)];
	"" ->
	    [Char]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_numeric_values("","","") ->
    undefined;
parse_numeric_values("","",NumericValue) ->
    parse_numeric_value(NumericValue);
parse_numeric_values("",DigitValue,DigitValue) ->
    {N,_,""} = scan_integer(DigitValue),
    {digit,N};
parse_numeric_values([Digit],[Digit],[Digit])
  when integer(Digit), Digit >= $0, Digit =< $9 ->
    Digit - $0;
parse_numeric_values(DecimalDigitValue,DigitValue,NumericValue) ->
    error_logger:info_msg(
    "UCS DB WARNING: Numeric value fields don't match as expected: ~s;~s;~s~n",
      [DecimalDigitValue,DigitValue,NumericValue]),
    undefined.

parse_numeric_value([$-|Rest]) ->
    case parse_numeric_value_u(Rest) of
	{N} -> {-N};
	{N,D} -> {-N,D}
    end;
parse_numeric_value(Str) ->
    parse_numeric_value_u(Str).

parse_numeric_value_u(Str) ->
    {N,LenN,Rest} = scan_integer(Str),
    true = (LenN > 0),
    case Rest of
	"" -> {N};
	[$/|Denom] ->
	    {D,LenD,""} = scan_integer(Denom),
	    true = (LenD > 0),
	    {N,D}
    end.
%%% ----------------------------------------------------------------------------
%%% This is only a helper function to create large tuples from a character
%%% mapping table
-define(TUPLE_SIZE,255).

create_tuple([Path,Cs]) ->
    Charset="unicode-"++atom_to_list(Cs)++".txt",
    SrcPath=filename:join([atom_to_list(Path),Charset]),
    {ok,InputFile} = file:open(SrcPath,[read]),
    case ets:info(Cs) of
	undefined ->
	    ok;
	_ ->
	    io:format("Charset known: ~p ~p !~n",[Charset,ets:all()]),
	    ets:delete(Cs)
    end,
    CharsetTab = ets:new(Cs,[set,named_table]),
    ok = process_mapping_data_file(Cs,InputFile),
    ok = file:close(InputFile),
    {List,Rest}=create_tuple_list(ets:tab2list(CharsetTab),[]),
    io:format("create_tuple~n Tuple:~p~n Rest:~p~n",[list_to_tuple(List),Rest]),
    ok.

create_tuple_list([],Out) ->
    create_tuple_list2(0,lists:sort(Out),[]);
create_tuple_list([{{UniChar},Char}|Rest],Out) ->
    create_tuple_list(Rest,[{UniChar,Char}|Out]);
create_tuple_list([_|Rest],Out) ->
    create_tuple_list(Rest,Out).

create_tuple_list2(?TUPLE_SIZE,Rest,Out) ->
    {lists:reverse(Out),Rest};
create_tuple_list2(Num,[{Num,Val}|Rest],Out) ->
    create_tuple_list2(Num+1,Rest,[Val|Out]);
create_tuple_list2(Num,Rest,Out) ->
    create_tuple_list2(Num+1,Rest,[undefined|Out]).

