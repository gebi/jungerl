%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is rdbms_mailmerge-1.0.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION 
%%% ----------------------------------------------------------
%%% %CCaseFile:	rdbms_mailmerge.erl %
%%% Author:          Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description:     Imports/exports a tab-delimited ASCII file.
%%%
%%% Modules used:    lists, file, string, rdbms_import_server, rdbms
%%% ----------------------------------------------------------
-module(rdbms_mailmerge).
-vsn('1.0').
-date('99-01-08').
-author('ulf.wiger@ericsson.com').


%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([file/1, files/1, dir/1]).
-export([export_data/2]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("kernel/include/file.hrl").


% Default delimiters

-define(ROW_DELIM, 10).  % newline
-define(COL_DELIM,  9).  % tab

-define(REC_DELIM, $.).  % dot, e.g. "person.name"
-define(MULT_DELIM, $,). % comma, e.g. "person.office, office.name"
-define(BLOCK_DELIM, $.). % A single dot on one line separates blocks.



%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR THE EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #3.1.1           file(File_name)
%%% Input:           File_name = a string identifying the import file
%%% Output:          ok | {error, Reason}.
%%% Exceptions:      Result = {error, Reason} if
%%%                  - Records in header do not exist in dictionary
%%%                  - labels in header do not match record elements
%%%                  - input data violates integrity constraints
%%% Description:     rdbms_import_mailmerge:file/1 imports a tab-delimited 
%%%		     file into Mnesia. Each file is imported atomically as 
%%%                  one transaction, i.e. either all rows or none are
%%%                  imported.
%%%                  A file can contain multiple blocks of data, 
%%%                  separated by a line containing only a period.
%%%                  Each block must begin with a header line
%%%                  containing the column names, which will be matched
%%%                  against the definition of the given record.
%%% ----------------------------------------------------------

file(File) ->
    {ok, Pid} = rdbms_import_server:start_link(),
    case file(File, Pid) of
	ok ->
	    case rdbms_import_server:commit(Pid) of
		{aborted, Reason} ->
		    {error, Reason};
		ok ->
		    ok
	    end;
	{error, Reason} ->
	    rdbms_import_server:abort(Pid),
	    {error, Reason}
    end.


file(File, Pid) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    case read_file(Fd, Pid) of
		ok ->
		    file:close(Fd),
		    ok;
		Other ->
		    file:close(Fd),
		    {error, Other}
	    end;
	{error, Reason} ->
	    error_logger:format("Error reading file ~p.~n", [File]),
	    {error, Reason}
    end.



%%% ----------------------------------------------------------
%%% #3.1.2           files([string()])
%%% Input:           A list of file names
%%% Output:          A list of {File, Result} tuples.
%%% Exceptions:      Same as file/1.
%%% Description:     files/1 calls file/1 for each file in the
%%%                  list. Each file is imported atomically, and
%%%                  a summary is returned to the calling function.
%%% ----------------------------------------------------------


files([File|T]) ->
    Res = file(File),
    [{File, Res}|files(T)];
files([]) ->
    [].

%%% ----------------------------------------------------------
%%% #3.1.3           dir(string())
%%% Input:           A directory name
%%% Output:          A list of {File, Result} tuples.
%%% Exceptions:      Same as file/1.
%%% Description:     dir/1 calls files/1 with all files found
%%%                  in the given directory.
%%% ----------------------------------------------------------

dir(Directory) ->
    case file:file_info(Directory) of
	{ok, {_, directory, _, _, _, _, _}} ->
	    {ok, FileList} = file:list_dir(Directory),
	    files([Directory ++ "/" ++ F || F <- FileList]);
	Other ->
	    error_logger:format("Error reading directory ~p.~n", [Directory]),
	    {error, dir}
    end.
	    

%%% ----------------------------------------------------------
%%% #3.1.2           export_data(Dir : string(), Tabs : all | [atom()])
%%% Input:           DirectoryName, ListOfTableNames
%%% Output:          {atomic, [ok|...]}
%%% Exceptions:      {aborted, Reason} if anything goes wrong
%%%		     or 'EXIT' if Dir couldn't be verified/created
%%% Description:     Exports the given mnesia tables to text files in Dir.
%%%		     The text files have the same format as the import
%%%		     data files, and are named TabName.txt.
%%%		     This function takes a read lock on the given tables
%%%		     before exporting the data. If Tabs == all, all tables
%%%		     except the mnesia schema are exported.
%%% ----------------------------------------------------------

export_data(Dir, Tabs) ->
    verify_dir(Dir),
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    Res = export_data(Tabs),
    ok = file:set_cwd(CurDir),
    Res.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% none

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

export_data(all) ->
    Tabs = mnesia:system_info(tables) -- [schema],
    do_export_data(Tabs);
export_data(Tabs) when list(Tabs) ->
    do_export_data(Tabs).

do_export_data(Tabs) ->
    mnesia:transaction(fun() ->
			       [mnesia:read_lock_table(T) || T <- Tabs],
			       [export_tab(T) || T <- Tabs]
		       end).


export_tab(Tab) ->
    Attrs = mnesia:table_info(Tab, attributes),
    WildP = mnesia:table_info(Tab, wild_pattern),
    TabStr = atom_to_list(Tab),
    {ok, Fd} = file:open(TabStr ++ ".txt", [write]),
    export_header(Fd, TabStr, Attrs),
    export_data(Fd, Tab, mnesia:dirty_slot(Tab, 0), 0),
    file:close(Fd).

export_header(Fd, TabStr, Attrs) ->
    Str = header_string(TabStr, Attrs),
    ok = io:format(Fd, "~s~n", [Str]).

export_data(Fd, Tab, '$end_of_table', Slot) -> ok;
export_data(Fd, Tab, Objs, Slot) ->
    export_objects(Fd, Objs),
    Slot1 = Slot+1,
    export_data(Fd, Tab, mnesia:dirty_slot(Tab, Slot1), Slot1).

export_objects(Fd, [Obj|Objs]) ->
    [_|Flds] = tuple_to_list(Obj),
    Str = data_string(Flds),
    ok = io:format(Fd, "~s~n", [Str]),
    export_objects(Fd, Objs);
export_objects(Fd, []) -> ok.
    
data_string([Term]) ->
    io_lib:format("~w", [Term]);
data_string([H|T]) ->
    io_lib:format("~w\t", [H]) ++ data_string(T).
	

%% mnesia enforces the rule that each tab must have at least one 
%% non-key attribute. Thus, the following code will work.
header_string(TabStr, [Attr]) -> 
    TabStr ++ 
	("." ++ atom_to_list(Attr));
header_string(TabStr, [A|Attrs]) ->
    TabStr ++ 
	("." ++ 
	 (atom_to_list(A) ++ 
	  ("\t" ++ header_string(TabStr, Attrs))));
header_string(_, []) -> 
    exit(bad_attributes).



%%% ----------------------------------------------------------
%%% #3.3.1           store_blocks(Binary, File_name)
%%% Input:           File content, file name
%%% Output:          ok | {error, Reason}.
%%% Exceptions:      None, provided correct input values.
%%% Description:     store_blocks/2 tries to interpret the 
%%%                  binary object Bin as a bunch of data to
%%%                  import into Mnesia. If data is correct, it
%%%                  inserts it into Mnesia within a transaction.
%%%                  File_name is only used in the return value.
%%% ----------------------------------------------------------

read_file(Fd, Pid) ->
    read_block(Fd, Pid).

read_block(Fd, Pid) ->
    case io:get_line(Fd, []) of
	eof ->
	    ok;
	".\n" ->
	    read_block(Fd, Pid);
	[$%|_] ->
	    read_block(Fd, Pid);
	Str ->
	    {[B], []} = rows(Str, []),
	    {ok, Hdr}= detailed_columns(B),
	    rdbms_import_server:header(Pid, Hdr),
	    read_block_data(Fd, Pid)
    end.

read_block_data(Fd, Pid) ->
    case io:get_line(Fd, []) of
	eof ->
	    ok;
	".\n" ->
	    read_block(Fd, Pid);
	[$%|_] ->
	    read_block_data(Fd, Pid);
	Str ->
	    S = remove_eol(Str),
	    case columns(S) of
		[] ->
		    read_block_data(Fd, Pid);
		L ->
		    rdbms_import_server:data(Pid, list_to_tuple(L)),
		    read_block_data(Fd, Pid)
	    end
    end.


remove_eol([]) ->
    [];
remove_eol(Str) ->
    case lists:reverse(Str) of
	[?ROW_DELIM|Rest] ->
	    lists:reverse(Rest);
	S ->
	    Str
    end.
    

%%% ----------------------------------------------------------
%%% #3.3.5           rows(Bytes), read_to_eol/2
%%% Input:           A list of bytes
%%% Output:          {Block_rows, Rest}.
%%% Exceptions:      No error checking.
%%% Description:     rows/2 is a variant of string:tokens, which
%%%                  splits one block (ended by ?BLOCK_DELIM or eof)
%%%                  into rows (separated by ?ROW_DELIM,
%%%                  ignoring empty lines.
%%% ----------------------------------------------------------
rows([?BLOCK_DELIM,?ROW_DELIM|Rest], Rows) ->
    {lists:reverse(Rows), Rest};
rows([?ROW_DELIM|Rest], Rows) ->  %ignore empty lines
    rows(Rest, Rows);
rows([C|Bytes], Rows) ->
    {Row, Rest} = read_to_eol(Bytes, [C]),
    rows(Rest, [Row|Rows]);
rows([], Rows) ->
    {lists:reverse(Rows), []}.

read_to_eol([?ROW_DELIM|Rest], Line) ->
    {lists:reverse(Line), Rest};
read_to_eol([C|Rest], Line) ->
    read_to_eol(Rest, [C|Line]);
read_to_eol([], Line) ->
    {lists:reverse(Line), []}.

%%% ----------------------------------------------------------
%%% #3.3.7           columns(Row)
%%% Input:           A string signifying a row of data
%%% Output:          A list of strings, one for each column.
%%% Exceptions:      -
%%% Description:     This function divides a string into a list
%%%                  of strings, based on column separators.
%%% ----------------------------------------------------------
columns(Row) ->
    tokens(Row).

%%% ----------------------------------------------------------
%%% #3.3.8           detailed_columns(Bytes)
%%% Input:           A list of bytes representing one row of data
%%% Output:          {ok ,Cols} Cols = A list of strings, 
%%%                                    one for each column.
%%% Exceptions:      No error checking.
%%% Description:     The function uses as column delimiter whatever is
%%%                  defined as ?COL_DELIM.
%%%                  NOTE: This function uses a variant of string:tokens
%%%                  rewritten to accept empty columns
%%%                  detailed_columns/1 is used on the header row.
%%%                  header columns are checked for validity and joins.
%%% ----------------------------------------------------------
detailed_columns(Row) ->
    Cols1 = tokens(Row, ""),
    Cols2 = multiples(Cols1),
    Cols3 = names(Cols2),
    {ok, Cols3}.


%%% ----------------------------------------------------------
%%% #3.3.9           tokens(Bytes), tokens1/3, tokens2/4
%%% Input:           A list of bytes representing one row of data
%%% Output:          A list of strings, one for each column.
%%% Exceptions:      No error checking.
%%% Description:     scan a string and return a list of columns, 
%%%                  as separated by ?COL_DELIM (e.g. tab). 
%%%                  Several delimiters in a row are to
%%%                  be interpreted as empty columns (a default value 
%%%                  is inserted) and a trailing tab means a last empty 
%%%                  column.
%%%                  tokens(String) defaults to tokens(String, NULL), i.e.
%%%                  default value inserted is NULL.
%%% ----------------------------------------------------------
tokens(S) ->
    tokens(S, rdbms:null_value()).
tokens(S, Null) ->
    tokens1(S, [], Null).

tokens1([$ |S], Toks, Null) ->    % ignore spaces.
    tokens1(S, Toks, Null);
tokens1([?COL_DELIM|S], Toks, Null) ->
    tokens1(S, [Null|Toks], Null);
tokens1([C|S], Toks, Null) ->
    tokens2(S, Toks, [C], Null);
tokens1([], Toks, Null) ->
    lists:reverse([Null|Toks]).

tokens2([?COL_DELIM|S], Toks, Cs, Null) ->
    tokens1(S, [lists:reverse(Cs)|Toks], Null);
tokens2([C|S], Toks, Cs, Null) ->
    tokens2(S, Toks, [C|Cs], Null);
tokens2([], Toks, Cs, Null) ->
    lists:reverse([lists:reverse(Cs)|Toks]).



%%% ----------------------------------------------------------
%%% #3.3.10          multiples(Columns)
%%% Input:           A list of header columns
%%% Output:          [ [Attr_name] ] a list of lists of attribute names.
%%% Exceptions:      No error checking.
%%% Description:     multiples/1 uses string:tokens to separate attr
%%%                  names separated by ?MULT_DELIM (e.g. ',').
%%% ----------------------------------------------------------
multiples([Col|T]) ->
    [string:tokens(Col, [?MULT_DELIM, $ ])|multiples(T)];
multiples([]) ->
    [].


%%% ----------------------------------------------------------
%%% #3.3.11          names(Columns), names1(Column)
%%% Input:           A list of Columns
%%% Output:          [ [{Rec_name, Attr_name}] ].
%%% Exceptions:      Throws all errors.
%%% Description:     names/1 parses all attr names using ?REC_DELIM
%%%                 (e.g. '.')..
%%% ----------------------------------------------------------
names([Col|T]) ->
    [names1(Col)|names(T)];
names([]) ->
    [].

names1([N|T]) ->
    case string:tokens(N, [?REC_DELIM]) of
	[A,B] ->
	    [{list_to_atom(A),list_to_atom(B)}|names1(T)];
	Other ->
	    throw({error, {parse_error, Other}})
    end;
names1([]) ->
    [].



%%%----------------------------------------------------------------------
%%% -type verify_dir(Dir : string())->
%%%     ok.
%%% Input: Dir
%%% Output: ok
%%% Exceptions: 
%%% Description: This function verifies a directory name. If the directory
%%%              does not exist, this function tries to create it, and 
%%%              any parent directories that may be missing.
%%%              The function exits if:
%%%              - the directory is read-only  {'EXIT', {access, Dir}}
%%%              - the dir (or parent dir) could not be created
%%%                                            {'EXIT', {create, Dir}}
%%%----------------------------------------------------------------------
verify_dir(Dir) ->
    case file:read_file_info(Dir) of
        {ok, FI} when FI#file_info.type == directory,
                      FI#file_info.access == read_write ->
            ok;
        {ok, FI} when FI#file_info.type == directory ->
            exit({access, Dir});
        {error, enoent} ->
            try_create(Dir)
    end.
 

try_create("/") ->
    exit({create, "/"});
try_create(".") ->
    exit({create, "/"});
try_create(Dir) ->
    case file:make_dir(Dir) of
        {error, Reason} ->
            try_create(filename:dirname(Dir)),
            try_create(Dir);
        ok ->
            ok
    end.
 




