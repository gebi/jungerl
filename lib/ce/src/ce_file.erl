%%% BEGIN ce_file.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2003 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc File manipulation library.
%%
%% @end

-module(ce_file).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([exists/1, size/1, last_modified/1]).
-export([is_dir/1, is_world_readable/1, is_world_executable/1, is_binary/1]).
-export([dump/2, log/2, log/3, each_line/3, slurp/1]).
-export([create/2, create/3, find/3]).
-export([import_fields/5, import_columns/4, import/4]).
-export([app_name/1, app_version/1]).
-export([complete/1, complete/2]).

-include_lib("kernel/include/file.hrl").

%% @spec exists(filename()) -> true | false
%% @doc Checks whether a file or directory exists.  Unlike previous
%% incarnations, this literally checks whether the file exists, i.e.
%% whether it's file information can be read.

exists(FileName) ->
  case file:read_file_info(FileName) of
    {ok, Info} -> true;
    _          -> false
  end.

%% @spec is_dir(filename()) -> true | false
%% @doc Checks whether a file is a directory.  Thanks to James Hague.

is_dir(Filename) ->
  case file:read_file_info(Filename) of
    {ok, Info} ->
      case Info#file_info.type of
        directory -> true;
        _ -> false
      end;
    _ -> false
  end.

%% @spec is_world_readable(filename()) -> true | false
%% @doc Checks whether a file is readable by the entire world.

is_world_readable(Filename) ->
  case file:read_file_info(Filename) of
    {ok, Info} ->
      case Info#file_info.mode band 4 of
        4 -> true;
        _ -> false
      end;
    _ -> false
  end.

%% @spec is_world_executable(filename()) -> true | false
%% @doc Checks whether a file is executable by the entire world.

is_world_executable(Filename) ->
  case file:read_file_info(Filename) of
    {ok, Info} ->
      case Info#file_info.mode band 1 of
        1 -> true;
        _ -> false
      end;
    _ -> false
  end.

%% @spec is_binary(filename()) -> true | false | {error, Reason}
%% @doc Makes an educated guess as to whether the file is binary (as
%% opposed to plain ASCII text).  The heuristic used is similar to that
%% of <code>grep</code> and Perl's <code>-B</code> operator.
%% The first 32K of the
%% file is examined for odd characters such as strange control codes or
%% characters with the high bit set.  If too many strange characters
%% (>30%) are found, or if any zero bytes (nulls) are encountered,
%% the file is considered binary.

is_binary(Filename) ->
  case file:open(Filename, [read]) of
    {ok, IoDevice} ->
      case file:read(IoDevice, 32768) of
        {ok, Block} ->
          Result = is_binary_block(Block, 0),
          file:close(IoDevice),
          Result;
        Other ->
          Other
      end;
    Other ->
      Other
  end.
is_binary_block([], A) -> false;
is_binary_block([0 | T], A) -> true;
is_binary_block(_, A) when A > 9830 -> true;
is_binary_block([9  | T], A) -> is_binary_block(T, A);
is_binary_block([10 | T], A) -> is_binary_block(T, A);
is_binary_block([12 | T], A) -> is_binary_block(T, A);
is_binary_block([13 | T], A) -> is_binary_block(T, A);
is_binary_block([H  | T], A) when H >= 32; H =< 126 -> is_binary_block(T, A);
is_binary_block([H  | T], A) -> is_binary_block(T, A+1).

%% @spec size(filename()) -> integer()
%% @doc Returns the size of a file, in bytes.

size(Filename) ->
  case file:read_file_info(Filename) of
    {ok, Info} -> Info#file_info.size;
    _ -> 0
  end.

%% @spec last_modified(filename()) -> {date(), time()}
%% @doc Returns the local date and time the file was last modified.

last_modified(Filename) ->
  case file:read_file_info(Filename) of
    {ok, Info} -> Info#file_info.mtime;
    _ -> {{0, 0, 0}, {0, 0, 0}}
  end.

%% @spec dump(filename(), [term()]) -> {ok, [term()]} | {error, Reason}
%% @doc Writes all terms to a file.  Complements file:consult/1.

dump(Filename, List) ->
  case file:open(Filename, [write]) of
    {ok, Device} ->
      lists:foreach(fun(Term) ->
                      io:fwrite(Device, "~p.~n", [Term])
		    end, List),
      file:close(Device),
      {ok, List};
    Other ->
      Other
  end.

%% @spec log(atom(), string()) -> {ok, string()} | {error, Reason}
%% @doc Naively logs a line both to a text file and to the console.

log(App, Text) ->
  log0(App, Text).
log(App, Fmt, List) ->
  log0(App, io_lib:format(Fmt, List)).
  
log0(App, Text) ->
  Filename = filename:join(code:priv_dir(App), "log.txt"),
  Str = lists:flatten(io_lib:format(
    "~w ~s ~s", [App, ce_calendar:logfile_datetime(), Text])),
  case file:open(Filename, [write, append]) of
    {ok, Device} ->
      io:fwrite(Device, "~s\r\n", [Str]),     % TODO: check operating system
      file:close(Device),
      io:fwrite("~s~n", [Str]),   % on console
      {ok, Str};
    Other0 ->
      io:fwrite("~s~n", [Str]),   % on console
      Other0
  end.

%% @spec each_line(fun(), term(), filename()) -> term()
%% @doc Iterates over a text file.  Thanks to Klacke for the general idea.

each_line(Fun, Acc, FileName) ->
  case file:open(FileName, [read]) of
    {ok, IoDevice} ->
      L = each_line_tail(Fun, Acc, IoDevice),
      file:close(IoDevice),
      L;
    N -> N
  end.
each_line_tail(Fun, Acc, Io) ->
  Line = io:get_line(Io, ''),
  case Line of
    X when list(X) ->
      each_line_tail(Fun, Fun(Line, Acc), Io);
    _ -> Acc
  end.

%% @spec slurp(filename()) -> [string()]
%% @doc Returns the entire text file as a list of strings.
%% This is not a particularly scaleable solution,
%% and is not recommended on large files.

slurp(FileName) ->
  lists:reverse(each_line(fun(L, Acc) ->
    [L | Acc]
  end, [], FileName)).

%% @spec import_fields(fun(), term(), filename(), string(), string()) -> term()
%% @doc Imports fields from a CSV-like file.

import_fields(Fun, Acc, FileName, Delim, Quote) ->
  ce_file:each_line(fun(L, FAcc) ->
    C = ce_string:fields(ce_string:chomp(L), Delim, Quote),
    Fun(C, FAcc)
  end, Acc, FileName).

%% @spec import_columns(fun(), term(), filename(), lineschema()) -> term()
%% @doc Imports fields from a columnar text file.
%% Schema is a list of {Column,Width} tuples.

import_columns(Fun, Acc, FileName, Schema) ->
  lists:reverse(ce_file:each_line(fun(L, FAcc) ->
    C = ce_string:columns(L, Schema),
    Fun(C, FAcc)
  end, Acc, FileName)).

%% @spec import(fun(), term(), filename(), fileschema()) -> term()
%%         fileschema() = [{tag(), regexp(), fields | columns, lineschema()}]
%%         lineschema() = [{column(), width()}]
%%         column() = integer()
%%         width() = integer()
%% @doc Provides a very generic interface for importing fields from files.

import(Fun, Acc, FileName, FileSchema) ->
  lists:reverse(ce_file:each_line(fun(L, FAcc) ->
    L0 = ce_string:chomp(L),
    case find_line_schema(L0, FileSchema) of
      {Tag, Pattern, fields, [{delimiter, D}, {quote, Q} | Rest]} ->
        C = ce_string:fields(L0, D, Q),
        Fun(Tag, C, FAcc);
      {Tag, Pattern, columns, Schema} ->
        C = ce_string:columns(L0, Schema),
        Fun(Tag, C, FAcc);
      _ ->
        FAcc   % ignore this line
    end
  end, Acc, FileName)).

find_line_schema(Line, []) -> nil;
find_line_schema(Line, [{Tag, Pattern, Type, Schema}=H | T]) ->
  case regexp:match(Line, Pattern) of
    {match, Start, Length} -> H;
    _ -> find_line_schema(Line, T)
  end.

%% @spec find(dir(), pred(), action()) -> [term()] | {error, {File, Reason}}
%% @doc Finds files, filters them, and executes a callback for each one.
%% dir() is the top directory where everything starts.
%% pred() is a fun/1 which takes a filename and returns true
%% if it is to be listed.  action() is a fun/1 to apply to each
%% found file which matches the predicate.
%% Both funs are passed the full file name
%% (up to but not including the top) as parameter.
%% Return value is a list of the return values from the 
%% action() fun.  Thanks to Klacke for providing the origin of this function.

find(Dir, Pred, Action) ->
  case file:list_dir(Dir) of
    {ok, Files} ->
      find(Dir, Files, Pred, Action, []);
    {error, Reason}  ->
      {error, {Dir, Reason}}
  end.

find(Dir, [F|Tail], Pred, Action, Acc) ->
  F2 = filename:join([Dir, F]),
  case ce_file:is_dir(F2) of
    true ->
      case find(F2, Pred, Action) of
	{error, Reason} ->
          {error, Reason};
	List ->
	  find(Dir, Tail, Pred, Action, List ++ Acc)
      end;
    false ->
      case Pred(F2) of
        true ->
          find(Dir, Tail, Pred, Action, [Action(F2) | Acc]);
        false ->
	  find(Dir, Tail, Pred, Action, Acc);
	{error, Reason} ->
	  {error, {F2, {pred, Reason}}}
      end
  end;
find(_Dir, [], _Pred, _Action, Acc) -> Acc.

%% @spec create(filename(), length()) -> ok | {error, Reason}
%% @equiv create(filename(), length(), 0)

create(FileName, Length) -> create(FileName, Length, 0).

%% @spec create(filename(), length(), Pad::byte()) -> ok | {error, Reason}
%% @doc Creates an empty file of a given length, padded with a given value.

create(FileName, Length, Byte) ->
  {ok, IoDevice} = file:open(FileName, [raw, write, delayed_write, binary]),
  create0(IoDevice, Length, Byte),
  file:close(IoDevice),
  ok.
create0(IoDevice, 0, Byte) ->
  ok;
create0(IoDevice, Length, Byte) when Length >= 64 ->
  List = lists:duplicate(64, Byte),
  file:write(IoDevice, list_to_binary(List)),
  create0(IoDevice, Length-64, Byte);
create0(IoDevice, Length, Byte) ->
  file:write(IoDevice, <<Byte:8/unsigned>>),
  create0(IoDevice, Length-1, Byte).

%% @spec app_name(filename()) -> string()
%% @doc Returns the application portion of an application directory name.
%% Everything after the last hyphen is stripped.
%% e.g. <code>ce_file:app_name("/usr/local/lib/erlang/lib/foo-2.3") -> "foo"</code>

app_name(FileName) ->
  FileName0 = filename:basename(FileName),
  case string:rchr(FileName0, $-) of
    0 -> FileName0;
    N -> string:left(FileName0, N-1)
  end.

%% @spec app_version(filename()) -> string()
%% @doc Returns the version portion of an application directory name.
%% Everything before the last hyphen is stripped.
%% e.g. <code>ce_file:app_name("/usr/local/lib/erlang/lib/foo-1.9") -> "1.9"</code>

app_version(FileName) ->
  FileName0 = filename:basename(FileName),
  case string:rchr(FileName0, $-) of
    0 -> FileName0;
    N -> string:substr(FileName0, N+1)
  end.

%% @spec complete(string()) -> {ok, filename()} | {ambiguous, [filename()]} | {error, Reason}
%% @equiv complete(filename:dirname(string()), filename:basename(string()))

complete(PartialFileName) ->
  DirName = filename:dirname(PartialFileName),
  BaseName = filename:basename(PartialFileName),
  complete(DirName, BaseName).

%% @spec complete(dirname(), string()) -> {ok, filename()} | {ambiguous, [filename()]} | {error, Reason}
%% @doc Given the name of a directory and a prefix (of any length) of
%% a file or files in that directory, returns the file or files that
%% the prefix matches, if any.
%% e.g. <code>ce_file:complete("/usr/local/", "e") -> {ok, "/usr/local/etc"}</code>

complete(DirName, PartialFileName) ->
  {ok, Dir} = file:list_dir(DirName),
  Dir0 = lists:reverse(lists:sort(Dir)),
  complete(DirName, Dir0, PartialFileName, length(PartialFileName), []).
complete(DirName, [], PartialFileName, Length, []) -> {error, no_match};
complete(DirName, [], PartialFileName, Length, [FileName]) -> {ok, FileName};
complete(DirName, [], PartialFileName, Length, Acc) -> {ambiguous, Acc};
complete(DirName, [FileName | Tail], PartialFileName, Length, Acc) ->
  case string:left(FileName, Length) of
    PartialFileName ->
      FullFileName = filename:join([DirName, FileName]),
      complete(DirName, Tail, PartialFileName, Length, [FullFileName | Acc]);
    _ ->
      complete(DirName, Tail, PartialFileName, Length, Acc)
  end.

%%% END of ce_file.erl %%%
