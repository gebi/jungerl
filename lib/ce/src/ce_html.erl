%%% BEGIN ce_html.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2002 Cat's Eye Technologies.  All rights reserved.
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

%% @doc Library of utilities for HTML.
%%
%% @end

-module(ce_html).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([escape_html/1, table/4]).

%% @spec escape_html(string()) -> string()
%% @doc Embeds HTML in HTML by escaping HTML metacharacters.
%% (Mainly &lt;, &gt; and &amp;).

escape_html(H) -> lists:reverse(escape_html(H, [])).
escape_html([], Acc) -> Acc;
escape_html([H | T], Acc) when list(H) ->
  escape_html(T, [escape_html(H) | Acc]);
escape_html([$< | T], Acc) ->
  escape_html(T, ["&lt;" | Acc]);
escape_html([$> | T], Acc) ->
  escape_html(T, ["&gt;" | Acc]);
escape_html([$& | T], Acc) ->
  escape_html(T, ["&amp;" | Acc]);
escape_html([H | T], Acc) ->
  escape_html(T, [H | Acc]).

%% @spec table([term()], [align()], [[term()]], [option()]) -> string()
%%         align() = left | center | right
%%         option() = {atom(), term()}
%% @doc Generates an HTML table from a list of lists of terms.
%% If the table would be empty, returns an empty string.
%% Options are any options that can be inserted into the table tag
%% (width, border, cellpadding, etc.)  The first option may be the
%% tuple <code>{compressed, true}</code>, in which case empty columns will be
%% elided from the table.  Note that the string returned by this function
%% is <b>not</b> flattened.

table(Headings, _, [], Options) -> "";
table(Headings, Align, Contents, [{compressed, true} | NewOptions]) ->
  {NewHeadings, NewAlign, NewContents} =
    compress_table(Headings, Align, Contents),
  table(NewHeadings, NewAlign, NewContents, NewOptions);
table(Headings, Align, Contents, Options) ->
  HeadingString = ["<tr>", lists:foldl(fun({Align, Heading}, A) ->
    [A, "<td align=\"", ce_lib:to_string(Align), "\"><b>",
      ce_lib:to_string(Heading), "</b></td>"]
  end, "", ce_lists:zip(Align, Headings)), "</tr>"],
  BodyString = lists:foldl(fun(Row, A) ->
    [A, table_row(Row, Align)]
  end, "", Contents),
  TableString = ["<table ", lists:foldl(fun({Option, Value}, A) ->
    [A, ce_lib:to_string(Option), "=\"", ce_lib:to_string(Value), "\" "]
  end, "", Options), ">"],
  [TableString, HeadingString, BodyString, "</table>"].

table_row(Row, Align) ->
  ["<tr>", lists:foldl(fun({Align, Item}, A) ->
    [A, "<td align=\"", ce_lib:to_string(Align), "\">",
        ce_lib:to_string(Item), "</td>"]
  end, "", ce_lists:zip(Align, Row)), "</tr>"].

compress_table(Headings, Align, Contents) ->
  Columns = ce_lists:zipn(Contents),
  Compressed = lists:reverse(lists:foldl(fun(X, A) ->
    [is_empty_column(X) | A]
  end, [], Columns)),
  NewHeadings = lists:reverse(lists:foldl(fun({Heading, false}, A) ->
    [Heading | A];
   ({Heading, true}, A) ->
    A
  end, [], ce_lists:zip(Headings, Compressed))),
  NewAlign = lists:reverse(lists:foldl(fun({Al, false}, A) ->
    [Al | A];
   ({Al, true}, A) ->
    A
  end, [], ce_lists:zip(Align, Compressed))),
  NewContents = lists:reverse(lists:foldl(fun(Row, A) ->
    [lists:reverse(lists:foldl(fun({Cell, false}, Acc) ->
                                 [Cell | Acc];
                                  ({Cell, true}, Acc) ->
                                 Acc
                               end, [], ce_lists:zip(Row, Compressed))) | A]
  end, [], Contents)),
  {NewHeadings, NewAlign, NewContents}.

is_empty_column(Column) when tuple(Column) ->
  is_empty_column(tuple_to_list(Column));
is_empty_column([]) -> true;
is_empty_column(["" | T]) -> is_empty_column(T);
is_empty_column([H | T]) -> false.

%%% END of ce_html.erl %%%
