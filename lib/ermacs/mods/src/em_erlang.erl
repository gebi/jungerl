%%%----------------------------------------------------------------------
%%% File    : em_erlang.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Erlang "Major Mode"
%%% Created : 10 Mar 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(em_erlang).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").
-import(edit_lib, [buffer/1]).
-export([mod_init/0, erlang_mode/1, reindent_cmd/1]).

-define(keymap, erlang_mode_map).

%% Called by the editor when this module is load/require'd
%%
%% NB: may be called several times due to explicit "load" calls.
mod_init() ->
    catch edit_keymap:delete(?keymap),
    init_map(),
    edit_keymap:global_set_key("C-x e", {?MODULE, erlang_mode, []}),
    edit_var:add_to_list(auto_mode_alist,
			 {"\.erl$$", {em_erlang, erlang_mode}}),
    ok.

erlang_mode(State) ->
    Mode = #mode{name="Erlang",
		 id=erlang,
		 keymaps=[?keymap]},
    Buf = buffer(State),
    edit_buf:set_mode(Buf, Mode),
    State.

init_map() ->
    edit_keymap:new(?keymap),
    edit_keymap:bind_each(?keymap, bindings()).

bindings() ->
    [{"C-i", {?MODULE, reindent_cmd, []}}
    ].

reindent_cmd(State) ->
    case scan_fun(State) of
	no_previous_line ->
	    State;
	Scan ->
	    B = buffer(State),
	    Indent = max(0,
			 idt(Scan)), %% + indent_cur_line_adjust(B)),
	    reindent(B, edit_lib:beginning_of_line_pos(B), Indent)
    end.

indent_cur_line_adjust(B) ->
    case cur_line_first_token(B) of
        nomatch ->
            0;
        {match, Tok} ->
            adjust_value(Tok)
    end.

adjust_value('when') -> 2;
adjust_value(')')    -> -1;
adjust_value('}')    -> -1;
adjust_value(']')    -> -1;
adjust_value('>>')   -> -2;
adjust_value(_)      -> 0.

strip_scan(X) -> lists:flatten(strip_scan1(X)).

strip_scan1([])            -> [];
strip_scan1([{lit,_}|T])   -> ["lit "|strip_scan1(T)];
strip_scan1([{X,Y}|T])     -> [atom_to_list(X) ++ " "|strip_scan1(T)].

reindent(Buf, Pos, Lvl) ->
    Pred = fun(C) -> (C /= $ ) and (C /= $\t) end,
    End = max(Pos, edit_lib:find_char_forward(Buf, Pred, Pos, 1)),
    edit_buf:replace(Buf, lists:duplicate(Lvl, $ ), Pos, End).

max(X, Y) when X > Y -> X;
max(X, Y)            -> Y.

min(X, Y) when X < Y -> X;
min(X, Y)            -> Y.


beginning_of_function(State) ->
    B = buffer(State),
    Pos = beginning_of_function_pos(B),
    edit_buf:move_mark(B, point, Pos).

beginning_of_function_pos(B) ->
    Point = min(edit_buf:point_max(B) - 1,
		max(1, edit_lib:beginning_of_line_pos(B) - 1)),
    Cord = edit_buf:get_cord(B),
    case cord_regexp:first_match("^[a-z'$-?]", Cord, Point, backward) of
	nomatch ->
	    1;
	{match, Start, End} ->
	    Start
    end.

%%% ----------------------------------------------------------------------
%%% Indentation
%%% ----------------------------------------------------------------------

%% Calculate the indentation level for the next line of a
%% function. `Scan' is the result of tokenising from the start of the
%% function.
idt(Scan) ->
    idt(Scan, [], [], 0).

%% Stack item
-record(si,
	{id,				% icr | '->' | open
	 tok,				% token
	 col,				% column number of `tok'
	 old_indent			% indent level when `tok' was found
	}).

%% icr means "if/case/receive(/begin/fun)" - really anything that's
%% matched by an 'end'

%% Variable names: C = Column, O = OldToken, S = Stack, I = IndentLevel

%% if case receive begin
idt([{Icr, C} | T], O, S, I)
  when Icr == 'if'; Icr == 'case'; Icr == 'receive'; Icr == 'begin'->
    CI = calc_clause_indent({Icr, C}, T, C),
    idt(T, Icr, push_icr(Icr, C, I, S), CI);

%% fun
idt([{'fun', C}, {'(', OpenC} | T], O, S, I) ->
    Rest = [{'(', OpenC} | T],
    CI = calc_clause_indent({'fun', C}, Rest, C),
    idt(Rest, 'fun', push_icr('fun', C, I, S), CI);

%% end
idt([{'end', C} | T], O, S, I) ->
    case unwind_icr(S) of
	nomatch ->
	    idt(T, 'end', [], 0);
	{SI, S2} ->
	    idt(T, 'end', S2, SI#si.old_indent)
    end;

%% ( [ { <<
idt([{Open, C} | T], O, S, I)
  when Open == '('; Open == '['; Open == '{'; Open == '<<' ->
    Width = length(atom_to_list(Open)),
    idt(T, Open, push_open(Open, C, I, S), C+Width);

%% ) ] } >>
idt([{Close, C} | T], O, S, I)
  when Close == ')'; Close == ']'; Close == '}'; Close == '>>' ->
    case unwind_close(matching(Close), S) of
	nomatch ->
	    idt(T, Close, [], 0);
	{SI, S2} ->
	    idt(T, Close, S2, SI#si.old_indent)
    end;

%% ->
idt([{'->', C} | T], O, S, I) ->
    NewIndent = case unwind_icr(S) of
		    nomatch ->
			4;
		    {ICR_SI, ICR_S2} ->
			ICR_SI#si.col + 8
		end,
    case unwind_when(S) of
	{SI, S2} ->
	    idt(T, '->', push_arrow(C, SI#si.old_indent, S2), NewIndent);
	nomatch ->
	    idt(T, '->', push_arrow(C, I, S), NewIndent)
    end;

%% when
idt([{'when', C}, {Next, NextC} | T], Old, S, I) when Next /= '->' ->
    idt(T, 'when', push_when(C, I, S), NextC);


%% ;
idt([{';', C} | T], O, S = [SI = #si{id='when'}|_], I) ->
    idt(T, ';', S, I);

idt([{',', C} | T], O, S = [SI = #si{id='when'}|_], I) ->
    idt(T, ';', S, I);

idt([{';', C} | T], O, S, I) ->
    case unwind_arrow(S) of
	nomatch ->
	    idt(T, ';', [], 0);
	{SI, S2} ->
	    idt(T, ';', S2, SI#si.old_indent)
    end;

idt([{'when', C} | T], '\n', S, I) ->
    idt(T, 'when', S, I+2);

%% Period: end of function
idt([{'.', C} | T], O, S, I) ->
    idt(T, start, [], 0);

%% Skip blank lines
idt([{'\n', C}, {'\n', _} | T], O, S, I) ->
    idt([{'\n', C} | T], O, S, I);

idt([{'\n', C} | T], O, S = [#si{id=open}|_], I) ->
    idt(T, '\n', S, I);

%% End of line. The preceeding token indicates whether we need to be unwinding
idt([{'\n', C} | T], Old, S, I)
%%when member(Old, ?ICR_TOKENS) ->
  when Old == '.'; Old == ';'; Old == 'of'; Old == 'begin';
       Old == 'receive'; Old == 'if'; Old == '->'; Old == ',';
       Old == '|' ->
    idt(T, '\n', S, I);

idt([{'\n', C} | T], O, S = [#si{id=open, old_indent=OldIndent}|_], I) ->
    idt(T, '\n', S, OldIndent);

idt([{'\n', C} | T], O, S, I) ->
    case unwind_icr(S) of
	nomatch ->
	    idt(T, '\n', [], 0);
	{SI, _S2} ->
	    idt(T, '\n', S, SI#si.col)
    end;

idt([{'||', C} | T], O, S, I) ->
    idt(T, '||', S, C + 3);

%% Quotes

idt([{Quote, C} | T], O, S, I) when Quote == '\'';
				    Quote == '"' ->
    match_quote(Quote, T, S, I);

%% Boring stuff
idt([{Tok,_}|T], O, S, I) ->
    idt(T, Tok, S, I);

idt([], O, S, I) ->
    I.

push_icr(Atom, Col, Indent, Stack) ->
    [#si{id=icr, tok=Atom, col=Col, old_indent=Indent} | Stack].

push_open(Atom, Col, Indent, Stack) ->
    [#si{id=open, tok=Atom, col=Col, old_indent=Indent} | Stack].

push_arrow(Col, Indent, Stack) ->
    [#si{id=arrow, tok='->', col=Col, old_indent=Indent} | Stack].

push_when(Col, Indent, Stack) ->
    [#si{id='when', tok='when', col=Col, old_indent=Indent} | Stack].

%% Unwind the stack to find a match for a closing bracket. For a
%% syntactically correct function, the head of the stack should always
%% match, but otherwise I just keep unwinding to be "forgiving" -
%% i.e. just give the wrong answer instead of an error :-)

unwind_close(Tok, [SI = #si{id=open, tok=Tok} | T]) ->
    {SI, T};
unwind_close(Tok, [H|T]) ->
    unwind_close(Tok, T);
unwind_close(Tok, []) ->
    nomatch.

unwind_icr([SI = #si{id=icr} | T]) ->
    {SI, T};
unwind_icr([H|T]) ->
    unwind_icr(T);
unwind_icr([]) ->
    nomatch.

%% Returns: {SI, ICR_SI, Stack}
unwind_arrow([SI = #si{id=arrow} | T]) ->
    {SI, T};
unwind_arrow([H|T]) ->
    unwind_arrow(T);
unwind_arrow([]) ->
    nomatch.

unwind_when([SI = #si{id='when'}|T]) ->
    {SI, T};
unwind_when([H|T]) ->
    unwind_when(T);
unwind_when([]) ->
    nomatch.

match_quote(Quote, [{'\\', _}, _ | T], S, I) ->
    %% Something is being escaped, hop over it
    match_quote(Quote, T, S, I);
match_quote(Quote, [Tok = {Quote, _} | T], S, I) ->
    idt(T, Quote, S, I);
match_quote(Quote, [Tok|T], S, I) ->
    match_quote(Quote, T, S, I);
match_quote(Quote, [], S, I) ->
    %% Out of tokens while inside a quote
    0.

calc_clause_indent({IRF, X}, [{Next, Y}|T], I)
  when IRF == 'if'; IRF == 'receive'; IRF == 'fun' ->
    if
	Next == '\n' ->
	    I + 4;
	true ->
	    Y
    end;
calc_clause_indent(_, _, I) ->
    I + 4.

matching(')')  -> '(';
matching(']')  -> '[';
matching('}')  -> '{';
matching('>>') -> '<<'.

%%% ----------------------------------------------------------------------
%%% Scanner
%%% ----------------------------------------------------------------------

% make_erlang_scanner() ->
%     em_scan:make_scanner(em_erlang_scan:yystate(),
%                          {em_erlang_scan, yystate},
%                          {em_erlang_scan, yyaction}).

% scan_fun(State) ->
%     B = buffer(State),
%     RStart = beginning_of_function_pos(B),
%     REnd = min(edit_lib:beginning_of_line_pos(B),
% 	       edit_buf:point_max(B) - 1),
%     Region = if
% 		 REnd > RStart ->
% 		     edit_buf:get_region(B, RStart, REnd);
% 		 true ->
% 		     ""
% 	     end,
%     scan(cord:new(Region)).

% scan(Cord) ->
%     Scanner = make_erlang_scanner(),
%     Walker = cord:walker(Cord),
%     case em_scan:edit_scan(Scanner, Walker) of
%         {error, Rsn} ->
%             {error, Rsn};
%         {ok, Toks} ->
%             %% This module expects {Class, Column}
%             [{Type, Col} || {Type, Col, Line} <- Toks]
%     end.

%%%%%%% OLD (current) SCANNER

%% Token = {Class, Column}

-define(MAX_TOKEN_LENGTH, 64).

first_token(Str) ->
    case scan(Str) of
	[] ->
	    nomatch;
	[{Tok,_}|_] ->
	    {match, Tok}
    end.

%% Returns: nomatch | {match, Token}
cur_line_first_token(Buf) ->
    Max = edit_buf:point_max(Buf),
    Start = edit_lib:beginning_of_line_pos(Buf),
    End = min(Max - 1,
	      min(Start + ?MAX_TOKEN_LENGTH,
		  edit_lib:end_of_line_pos(Buf))),
    if
	End > Start ->
	    Str = edit_buf:get_region(Buf, Start, End),
	    first_token(Str);
	true ->
	    nomatch
    end.

scan_fun(State) ->
    B = buffer(State),
    RStart = beginning_of_function_pos(B),
    REnd = min(edit_lib:beginning_of_line_pos(B),
               edit_buf:point_max(B) - 1),
    Region = if
                 REnd > RStart ->
                     edit_buf:get_region(B, RStart, REnd);
                 true ->
                     ""
             end,
    scan(Region).

strip(X) -> [element(1, E) || E <- X].

scan(Str) ->
    S = merge_literals(scan1(Str, true, 0)),
    S.

merge_literals([])                  -> [];
merge_literals([{lit,P},{lit,_}|T]) -> merge_literals([{lit,P}|T]);
merge_literals([H|T])               -> [H|merge_literals(T)].

scan1([], _, P) ->
    [];
scan1([$%|T], _, P) ->
    skip_comments(T, P);
scan1([$$,_|T], _, P) ->
    [{lit,P}|scan1(T, true, P+2)];
scan1([$.,X|T], _, P) ->
    case lists:member(X, ws()) of
	true ->
	    [{'.',P} | scan1([X|T], true, P)];
	false ->
	    [{lit,P}|scan1([X|T], true, P)]
    end;
scan1(Str, Fresh, P) ->
    case match(Str, Fresh) of
	nomatch when hd(Str) /= $ , hd(Str) /= $\t ->
	    [{lit,P}|scan1(tl(Str), not symchar(hd(Str)), P+1)];
	nomatch ->
	    scan1(tl(Str), not symchar(hd(Str)), P+1);
	{match, "\n"} ->
	    [{'\n',P} |
	     scan1(tl(Str), true, 0)];
	{match, M} ->
	    Len = length(M),
	    Rest = lists:nthtail(Len, Str),
	    [{list_to_atom(M), P} |
	     scan1(Rest, true, P+Len)]
    end.

match(Str, true) ->
    case match_alone(Str) of
	nomatch ->
	    match(Str, false);
	X ->
	    X
    end;
match(Str, false) ->
    match_anywhere(Str).

match_alone(Str) ->
    match_alone(Str, alone()).

match_alone(Str, [Str|_]) ->
    {match, Str};
match_alone(Str, [X|T]) ->
    case lists:prefix(X, Str) of
	true ->
	    case not symchar(lists:nth(length(X)+1, Str)) of
		true ->
		    {match, X};
		false ->
		    match_alone(Str, T)
	    end;
	false ->
	    match_alone(Str, T)
    end;
match_alone(Str, [_|T]) ->
    match_alone(Str, T);
match_alone(Str, []) ->
    nomatch.

match_anywhere(Str) ->
    match_anywhere(Str, anywhere()).

match_anywhere(Str, [X|T]) ->
    case lists:prefix(X, Str) of
	true  -> {match, X};
	false -> match_anywhere(Str, T)
    end;
match_anywhere(Str, []) ->
    nomatch.

skip_comments([$\n|T], P) ->
    scan1(T, true, P);
skip_comments([_|T], P) ->
    skip_comments(T, 0);
skip_comments([], P) ->
    [].

%% Tokens that can appear anywhere
anywhere() ->
    ["{","}","<<",">>","(",")","[","]",";",".","->","||","|",";",",","\n",
     "\"", "'", "\\"].

%% Tokens that must appear "alone"
alone() ->
    ["begin", "end", "case", "if", "receive", "fun", "of", "when"].

ws() -> "\r\t\n ".

symchar(C) when C >= $a, C =< $z -> true;
symchar(C) when C >= $A, C =< $Z -> true;
symchar(C) when C >= $0, C =< $9 -> true;
symchar($_)                      -> true;
symchar(_)                       -> false.

