%% =====================================================================
%% epp_dodger - bypasses the Erlang preprocessor.
%%
%% Copyright (C) 2001 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Author contact: richardc@csd.uu.se
%%
%% $Id$
%%
%% =====================================================================
%%
%% @doc <code>epp_dodger</code> - bypasses the Erlang preprocessor.
%%
%% <p>This module tokenises and parses most Erlang source code without
%% expanding preprocessor directives and macro applications, as long as
%% these are syntactically "well-behaved". Because the normal parse
%% trees of the <code>erl_parse</code> module cannot represent these
%% things (normally, they are expanded by the Erlang preprocessor
%% "<code>epp</code>" before the parser sees them), an extended syntax
%% tree is created, using the <code>erl_syntax</code> module.</p>
%% 
%% @end
%% =====================================================================

-module(epp_dodger).

-export([parse_file/1, parse/1, parse/2, parse_form/2, format_error/1]).


%% =====================================================================
%% @spec parse_file(File) -> {ok, Forms} | {error, ErrorInfo}
%%       File = file:filename()
%%       Forms = [erl_syntax:syntaxTree()]
%%       ErrorInfo = term()
%%
%% @doc Reads and parses a file. If successful, <code>{ok, Forms}</code>
%% is returned, where <code>Forms</code> is a list of abstract syntax
%% trees representing the "program forms" of the file (cf.
%% <code>erl_syntax:is_form/1</code>). Otherwise, <code>{error,
%% ErrorInfo}</code> is returned, where <code>ErrorInfo</code> is an
%% Erlang I/O ErrorInfo structure (see module <code>io</code>.)
%%
%% @see erl_syntax:is_form/1
%% @see io

parse_file(File) ->
    case file:open(File, [read]) of
        {ok, Dev} ->
            V = parse(Dev),
            file:close(Dev),
            V;
        Other ->
            Other
    end.


%% =====================================================================
%% @spec parse(IODevice) -> {ok, Forms} | {error, ErrorInfo}
%% @equiv parse(IODevice, 1)

parse(Dev) ->
    parse(Dev, 1).


%% =====================================================================
%% @spec parse(IODevice, StartLine) -> {ok, Forms} | {error, ErrorInfo}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Forms = [erl_syntax:syntaxTree()]
%%       ErrorInfo = term()
%%
%% @doc Reads and parses program text from an I/O stream. Characters are
%% read from <code>IODevice</code> until end-of-file; apart from this,
%% the behaviour is the same as for <code>parse_file/1</code>.
%% <code>StartLine</code> is the initial line number, which should be a
%% positive integer.
%%
%% @see parse_file/1

parse(Dev, L0) ->
    parse(Dev, L0, []).

parse(Dev, L0, Fs) ->
    case parse_form(Dev, L0) of
        {ok, F, L1} ->
            parse(Dev, L1, [F | Fs]);
        {error, R, L1} ->
            parse(Dev, L1, [{error, R} | Fs]);
        {eof, L1} ->
            {ok, lists:reverse(Fs)}
    end.


%% =====================================================================
%% @spec parse_form(IODevice, StartLine) -> {ok, Forms, LineNo}
%%                                        | {eof, LineNo}
%%                                        | {error, ErrorInfo, LineNo}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Forms = [erl_syntax:syntaxTree()]
%%       ErrorInfo = term()
%%       LineNo = integer()
%%
%% @doc Reads and parses a single program form from an I/O stream.
%% Characters are read from <code>IODevice</code> until an end-of-form
%% marker is found (a period character followed by whitespace), or until
%% end-of-file; apart from this, the behaviour is similar to that of
%% <code>parse/2</code>, except that the return values also contain the
%% final line number, given that <code>StartLine</code> is the initial
%% line number, and that <code>{eof, LineNo}</code> may be returned.
%%
%% @see parse/2

parse_form(Dev, L0) ->
    case io:scan_erl_form(Dev, "", L0) of
        {ok, Ts, L1} ->
            case catch rewrite_form(parse_tokens(scan_form(Ts))) of
                {'EXIT', _} ->
                    {error, {L1, ?MODULE, unknown}, L1};
                {error, R} ->
                    {error, R, L1};
                F ->
                    {ok, F, L1}
            end;
        Other ->
            Other
    end.

parse_tokens(Ts) ->
    case erl_parse:parse_form(Ts) of
        {ok, Form} ->
            Form;
        {error, R} ->
            throw({error, R})
    end.

scan_form([{'-', L}, {atom, La, define} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, define} | scan_macros(Ts)];
scan_form([{'-', L}, {atom, La, undef} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, undef} | scan_macros(Ts)];
scan_form([{'-', L}, {atom, La, include} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, include} | scan_macros(Ts)];
scan_form([{'-', L}, {atom, La, include_lib} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, include_lib} | scan_macros(Ts)];
scan_form([{'-', L}, {atom, La, ifdef} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, ifdef} | scan_macros(Ts)];
scan_form([{'-', L}, {atom, La, ifndef} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, ifndef} | scan_macros(Ts)];
scan_form([{'-', L}, {atom, La, else} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, else} | scan_macros(Ts)];
scan_form([{'-', L}, {atom, La, endif} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, endif} | scan_macros(Ts)];
scan_form(Ts) ->
    scan_macros(Ts).

scan_macros([{'?', L}, {atom, La, _} = A, {'(', _}, {')', _} | Ts]) ->
    [{atom, L, '?macro_call'}, {'(', La}, A, {')', La}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {atom, La, _} = A, {'(', _} | Ts]) ->
    [{atom, L, '?macro_call'}, {'(', La}, A, {',', La}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {atom, La, _} = A | Ts]) ->
    [{atom, L, '?macro'}, {'(', La}, A, {')', La} | scan_macros(Ts)];
scan_macros([{'?', L}, {var, Lv, _} = V, {'(', _}, {')', _} | Ts]) ->
    [{atom, L, '?macro_call'}, {'(', Lv}, V, {')', Lv}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {var, Lv, _} = V, {'(', _} | Ts]) ->
    [{atom, L, '?macro_call'}, {'(', Lv}, V, {',', Lv}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {var, Lv, _} = V | Ts]) ->
    [{'(', L}, {atom, L, '?macro'}, {'(', L}, V, {')', Lv}, {')', Lv}
     | scan_macros(Ts)];
scan_macros([T | Ts]) ->
    [T | scan_macros(Ts)];
scan_macros([]) ->
    [].

rewrite_form({function, L, '?pp', _,
              [{clause, _, [], [], [{call, _, A, As}]}]}) ->
    erl_syntax:set_pos(erl_syntax:attribute(A, rewrite_list(As)), L);
rewrite_form({function, L, '?pp', _, [{clause, _, [], [], [A]}]}) ->
    erl_syntax:set_pos(erl_syntax:attribute(A), L);
rewrite_form(T) ->
    rewrite(T).

rewrite_list([T | Ts]) ->
    [rewrite(T) | rewrite_list(Ts)];
rewrite_list([]) ->
    [].

rewrite({call, _, {atom, L, '?macro'}, [A]}) ->
    erl_syntax:set_pos(erl_syntax:macro(A), L);
rewrite({call, _, {atom, L, '?macro_call'}, [A | As]}) ->
    erl_syntax:set_pos(erl_syntax:macro(A, rewrite_list(As)), L);
rewrite(Node) ->
    case erl_syntax:subtrees(Node) of
        [] ->
            Node;
        Gs ->
            Node1 = erl_syntax:make_tree(erl_syntax:type(Node),
                                         [[rewrite(T) || T <- Ts]
                                          || Ts <- Gs]),
            erl_syntax:copy_pos(Node, Node1)
    end.


%% @doc Callback function for formatting error descriptors.
%% @spec (term()) -> string()

format_error(unknown) -> "epp_dodger: unknown error".


%% =====================================================================
