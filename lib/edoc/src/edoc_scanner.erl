%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings
%% AB. Portions created by Ericsson are Copyright 1999, Ericsson
%% Utvecklings AB. All Rights Reserved.''
%%
%% $Id$

%% Tokeniser for EDoc, based on the standard library `erl_scan.erl'.

-module(edoc_scanner).

%% NOTE: the interface to this module is ancient and should be updated.
%% Please do not regard these exported functions as stable. Their
%% behaviour is described in the documentation of the module `erl_scan'.
%%
%% Since there are no `full stop' tokens in EDoc specifications, the
%% `tokens' function *always* returns `{more, Continuation}' unless an
%% error occurs.

-export([string/1,string/2,tokens/3,format_error/1]).

-import(lists, [reverse/1]).

%% tokens(Continuation, CharList, StartPos) ->
%%	{done, {ok, [Tok], EndPos}, Rest} |
%%	{done, {error,{ErrorPos,cerl_scanner,What}, EndPos}, Rest} |
%%	{more, Continuation'}
%%  This is the main function into the re-entrant scanner. It calls the
%%  re-entrant pre-scanner until this says done, then calls scan/1 on
%%  the result

tokens([], Chars, Pos) ->
    %% First call
    tokens({[],[],{Pos,Pos}}, Chars, Pos);
tokens({Chars,SoFar,{SPos,CPos}}, MoreChars, _) ->
    In = Chars ++ MoreChars,
    R = pre_scan(In, SoFar, CPos),
    case R of
	{chars,TChars,Rest,EndPos} ->
	    case scan(TChars, SPos) of
		{ok,Toks} -> {done,{ok,Toks,EndPos},Rest};
		{error,E} -> {done,{error,E,EndPos},Rest}
	    end;
	{more,Rest,SoFar1,CPos1} ->
	    {more,{Rest,SoFar1,{SPos,CPos1}}};
	Other ->
	    %% An error has occurred
	    {done,Other,[]}
    end.

string(Cs) -> string(Cs, 1).

string(Cs, StartPos) ->
    case scan(Cs, StartPos) of
	{ok,Toks} -> {ok,Toks,StartPos};
	{error,E} -> {error,E,StartPos}
    end.

%% format_error(Error)
%%  Return a string describing the error.

format_error({string,Quote,Head}) ->
    ["unterminated string starting with " ++ io_lib:write_string(Head,Quote)];
format_error({illegal,Type}) -> io_lib:fwrite("illegal ~w", [Type]);
format_error(char) -> "unterminated character";
format_error(scan) -> "premature end";
format_error({base,Base}) -> io_lib:fwrite("illegal base '~w'", [Base]);
format_error(float) -> "bad float";

format_error(Other) -> io_lib:write(Other).

%% Re-entrant pre-scanner.
%%
%% If the input list of characters is insufficient to build a term
%% the scanner returns a request for more characters and a
%% continuation to be used when trying to build a term with more
%% characters. To indicate end-of-file the input character list
%% should be replaced with 'eof' as an empty list has meaning. Pos
%% holds the current line number.
%%
%% The call into it is:
%%
%%	Result = pre_scan(Characters, ScannedChars, Pos)
%%
%% where Result is:
%%	{chars,TermCharList,LeftOverList,NewPos}
%%	{more,StartCharList,OutputCharList,NewPos}
%%	{eof,NewPos}
%%	{error,{ErrorPos,cerl_scanner,Description},NewPos}
%%
%% Internal details:
%%
%% The output character list is built in reverse order to save
%% appending and then reversed when all the characters have been
%% collected.
%%
%% The start of each top-level "character sequence" is passed as an
%% argument saving rebuilding input sequences when forming a
%% continuation. Sometimes, however, when an input sequence can be long
%% (quoted strings) a special re-enter token is added to the
%% continuation. The token is usually the start characters as an atom,
%% e.g. '\''.

%% pre_scan(Characters, CharStack)
%%  Main pre-scan function. It has been split into 2 functions
%%  because of efficiency, with a good indexing compiler it would
%%  be unnecessary.

pre_scan([C|Cs], SoFar, Pos) ->
    pre_scan(C, Cs, SoFar, Pos);
pre_scan([], SoFar, Pos) ->
    {more,[],SoFar, Pos};
pre_scan(eof, SoFar, Pos) ->
    {eof, Pos}.

%% pre_scan(Char, RestChars, CharStack, Pos)

pre_scan($$, Cs0, SoFar0, Pos) ->
    case pre_scan_char(Cs0, [$$|SoFar0]) of
	{Cs,SoFar} ->
	    pre_scan(Cs, SoFar, Pos);
	more ->
	    {more,[$$|Cs0],SoFar0, Pos};
	error ->
	    pre_scan_error(char, Pos)
    end;
pre_scan($', Cs, SoFar, Pos) ->
    pre_scan_string(Cs, $', '\'', [$'|SoFar], Pos);
pre_scan('\'', Cs, SoFar, Pos) ->		% Re-entering atom
    pre_scan_string(Cs, $', '\'', SoFar, Pos);
pre_scan($", Cs, SoFar, Pos) ->
    pre_scan_string(Cs, $", '"', [$"|SoFar], Pos);
pre_scan('"', Cs, SoFar, Pos) ->		% Re-entering string
    pre_scan_string(Cs, $", '"', SoFar, Pos);
pre_scan($\n, Cs, SoFar, Pos) ->
    pre_scan(Cs, [$\n|SoFar], Pos+1);
pre_scan(C, Cs, SoFar, Pos) ->
    pre_scan(Cs, [C|SoFar], Pos).

pre_scan_string([Quote|Cs], Quote, Reent, SoFar, Pos) ->
    pre_scan(Cs, [Quote|SoFar], Pos);
pre_scan_string([C|Cs0], Quote, Reent, SoFar0, Pos) ->
    case pre_scan_char([C|Cs0], SoFar0) of
	{Cs,SoFar} ->
	    pre_scan_string(Cs, Quote, Reent, SoFar, Pos);
	more ->
	    {more,[Reent,C|Cs0],SoFar0,Pos};
	error ->
	    S = reverse(string:substr(SoFar0, 1, string:chr(SoFar0, Quote)-1)),
	    pre_scan_error({string,Quote,string:substr(S, 1, 16)}, Pos)
    end;
pre_scan_string([], Quote, Reent, SoFar, Pos) ->
    {more,[Reent],SoFar,Pos};
pre_scan_string(eof, Quote, _, SoFar, Pos) ->
    S = reverse(string:substr(SoFar, 1, string:chr(SoFar, Quote)-1)),
    pre_scan_error({string,Quote,string:substr(S, 1, 16)}, Pos).

pre_scan_char([$\\|Cs0], SoFar) ->
    case Cs0 of
	[$^,C3|Cs] ->
	    {Cs,[C3,$^,$\\|SoFar]};
	[$^] ->
	    more;
	[$^|eof] ->
	    error;
	[C2|Cs] ->
	    {Cs,[C2,$\\|SoFar]};
	[] ->
	    more;
	eof ->
	    error
    end;
pre_scan_char([C|Cs], SoFar) ->
    {Cs,[C|SoFar]};
pre_scan_char([], _) ->
    more;
pre_scan_char(eof, _) ->
    error.

pre_scan_error(In, Pos) ->
    {error,{Pos,cerl_scanner,In}, Pos}.


%% scan(CharList, StartPos)
%%  This takes a list of characters and tries to tokenise them.
%%
%%  The token list is built in reverse order (in a stack) to save appending
%%  and then reversed when all the tokens have been collected. Most tokens
%%  are built in the same way.
%%
%%  Returns:
%%	{ok,[Tok]}
%%	{error,{ErrorPos,cerl_scanner,What}}

scan(Cs, Pos) ->
    scan1(Cs, [], Pos).

%% scan1(Characters, TokenStack, Position)
%%  Scan a list of characters into tokens.

scan1([$\n|Cs], Toks, Pos) ->            	        % Newline
    scan1(Cs, Toks, Pos+1);
scan1([C|Cs], Toks, Pos) when C >= 0, C =< $  -> 	% Skip blanks
    scan1(Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $a, C =< $z ->	% Unquoted atom
    scan_atom(C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $0, C =< $9 ->	% Numbers
    scan_number(C, Cs, Toks, Pos);
scan1([$-,C| Cs], Toks, Pos) when C >= $0, C =< $9 ->	% Signed numbers
    scan_signed_number($-, C, Cs, Toks, Pos);
scan1([$+,C| Cs], Toks, Pos) when C >= $0, C =< $9 ->	% Signed numbers
    scan_signed_number($+, C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $A, C =< $Z ->	% Variables
    scan_variable(C, Cs, Toks, Pos);
scan1([$_|Cs], Toks, Pos) ->				% Variables
    scan_variable($_, Cs, Toks, Pos);
scan1([$$|Cs], Toks, Pos) ->			% Character constant
    case scan_char_const(Cs, Toks, Pos) of
	{ok, Result} ->
	    {ok, Result};
	{error, truncated_char} ->
	    scan_error(char, Pos);
	{error, illegal_character} ->
	    scan_error({illegal, char}, Pos)
    end;
scan1([$'|Cs0], Toks, Pos) ->				% Quoted atom
    case scan_string(Cs0, $', Pos) of
	{S,Cs1,Pos1} ->
	    case catch list_to_atom(S) of
		A when atom(A) ->
		    scan1(Cs1, [{atom,Pos,A}|Toks], Pos1);
		Error -> scan_error({illegal,atom}, Pos)
	    end;
	{error, premature_end} ->
	    scan_error({string,$',Cs0}, Pos);
	{error, truncated_char} ->
	    scan_error(char, Pos);
	{error, illegal_character} ->
	    scan_error({illegal, atom}, Pos)
    end;
scan1([$"|Cs0], Toks, Pos) ->				% String
    case scan_string(Cs0, $", Pos) of
	{S,Cs1,Pos1} ->
	    case Toks of
		[{string, Pos0, S0} | Toks1] ->
		    scan1(Cs1, [{string, Pos0, S0 ++ S} | Toks1],
			  Pos1);
		_ ->
		    scan1(Cs1, [{string,Pos,S}|Toks], Pos1)
	    end;
	{error, premature_end} ->
	    scan_error({string,$",Cs0}, Pos);
	{error, truncated_char} ->
	    scan_error(char, Pos);
	{error, illegal_character} ->
	    scan_error({illegal, string}, Pos)
    end;
%% Punctuation characters and operators, first recognise multiples.
scan1([$-,$>|Cs], Toks, Pos) ->
    scan1(Cs, [{'->',Pos}|Toks], Pos);
scan1([$:,$:|Cs], Toks, Pos) ->
    scan1(Cs, [{'::',Pos}|Toks], Pos);
scan1([C|Cs], Toks, Pos) ->    % Punctuation character
    P = list_to_atom([C]),
    scan1(Cs, [{P,Pos}|Toks], Pos);
scan1([], Toks0, Pos) ->
    Toks = reverse(Toks0),
    {ok,Toks}.

%% Note that `_' is not accepted as a variable token.
scan_variable(C, Cs, Toks, Pos) ->
    {Wcs,Cs1} = scan_name(Cs, []),
    W = [C|reverse(Wcs)],
    case W of
	"_" ->
	    scan_error({illegal,token}, Pos);
	_ ->
	    case catch list_to_atom(W) of
		A when atom(A) ->
		    scan1(Cs1, [{var,Pos,A}|Toks], Pos);
		_ ->
		    scan_error({illegal,variable}, Pos)
	    end
    end.

scan_atom(C, Cs, Toks, Pos) ->
    {Wcs,Cs1} = scan_name(Cs, []),
    W = [C|reverse(Wcs)],
    case catch list_to_atom(W) of
	A when atom(A) ->
	    scan1(Cs1, [{atom,Pos,A}|Toks], Pos);
	_ ->
	    scan_error({illegal,token}, Pos)
    end.

%% scan_name(Cs) -> lists:splitwith(fun (C) -> name_char(C) end, Cs).

scan_name([C|Cs], Ncs) ->
    case name_char(C) of
	true ->
	    scan_name(Cs, [C|Ncs]);
	false ->
	    {Ncs,[C|Cs]}		% Must rebuild here, sigh!
    end;
scan_name([], Ncs) ->
    {Ncs,[]}.

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $\337, C =< $\377, C /= $\367 -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $\300, C =< $\336, C /= $\327 -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.

%% scan_string(CharList, QuoteChar, Pos) ->
%%	{StringChars,RestChars, NewPos}

scan_string(Cs, Quote, Pos) ->
    scan_string(Cs, [], Quote, Pos).

scan_string([Quote|Cs], Scs, Quote, Pos) ->
    {reverse(Scs),Cs,Pos};
scan_string([], Scs, Quote, Pos) ->
    {error, premature_end};
scan_string(Cs0, Scs, Quote, Pos) ->
    case scan_char(Cs0, Pos) of
	{C,Cs,Pos1} ->
	    %% Only build the string here
	    scan_string(Cs, [C|Scs], Quote, Pos1);
	Error ->
	    Error
    end.

%% Note that space characters are not allowed
scan_char_const([$\040 | Cs0], Toks, Pos) ->
    {error, illegal_character};
scan_char_const(Cs0, Toks, Pos) ->
    case scan_char(Cs0, Pos) of
	{C,Cs,Pos1} ->
	    scan1(Cs, [{char,Pos,C}|Toks], Pos1);
	Error ->
	    Error
    end.

%% {Character,RestChars,NewPos} = scan_char(Chars, Pos)
%% Read a single character from a string or character constant. The
%% pre-scan phase has checked for errors here.
%% Note that control characters are not allowed.

scan_char([$\\|Cs], Pos) ->
    scan_escape(Cs, Pos);
scan_char([C | Cs], Pos) when C =< 16#1f ->
    {error, illegal_character};
scan_char([C|Cs], Pos) ->
    {C,Cs,Pos};
scan_char([], Pos) ->
    {error, truncated_char}.

%% The following conforms to Standard Erlang escape sequences.

scan_escape([O1, O2, O3 | Cs], Pos) when        % \<1-3> octal digits
  O1 >= $0, O1 =< $3, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    Val = (O1*8 + O2)*8 + O3 - 73*$0,
    {Val,Cs,Pos};
scan_escape([O1, O2 | Cs], Pos) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    Val = (O1*8 + O2) - 9*$0,
    {Val,Cs,Pos};
scan_escape([O1 | Cs], Pos) when
  O1 >= $0, O1 =< $7 ->
    {O1 - $0,Cs,Pos};
scan_escape([$^, C | Cs], Pos) ->    % \^X -> CTL-X
    if C >= $\100, C =< $\137 ->
	    {C - $\100,Cs,Pos};
       true -> {error, illegal_control_character}
    end;
scan_escape([C | Cs], Pos) ->
    case escape_char(C) of
	C1 when C1 > $\000 -> {C1,Cs,Pos};
	_ -> {error, undefined_escape_sequence}
    end;
scan_escape([],Pos) ->
    {error, truncated_char}.

%% Note that we return $\000 for undefined escapes.
escape_char($b) -> $\010;		% \b = BS
escape_char($d) -> $\177;		% \d = DEL
escape_char($e) -> $\033;		% \e = ESC
escape_char($f) -> $\014;		% \f = FF
escape_char($n) -> $\012;		% \n = LF
escape_char($r) -> $\015;		% \r = CR
escape_char($s) -> $\040;		% \s = SPC
escape_char($t) -> $\011;		% \t = HT
escape_char($v) -> $\013;		% \v = VT
escape_char($\\) -> $\134;		% \\ = \
escape_char($') -> $\047;		% \' = '
escape_char($") -> $\042;		% \" = "
escape_char(C) -> $\000.

%% scan_number(Char, CharList, TokenStack, Pos)
%%  We handle sign and radix notation:
%%    [+-]<digits>		- the digits in base [+-]10
%%    [+-]<digits>.<digits>
%%    [+-]<digits>.<digits>E+-<digits>
%%    [+-]<digits>#<digits>	- the digits read in base [+-]B
%%
%%  Except for explicitly based integers we build a list of all the
%%  characters and then use list_to_integer/1 or list_to_float/1 to
%%  generate the value.

%%  SPos == Start position
%%  CPos == Current position

scan_number(C, Cs0, Toks, Pos) ->
    {Ncs,Cs,Pos1} = scan_integer(Cs0, [C], Pos),
    scan_after_int(Cs, Ncs, Toks, Pos, Pos1).

scan_signed_number(S, C, Cs0, Toks, Pos) ->
    {Ncs,Cs,Pos1} = scan_integer(Cs0, [C, S], Pos),
    scan_after_int(Cs, Ncs, Toks, Pos, Pos1).

scan_integer([C|Cs], Stack, Pos) when C >= $0, C =< $9 ->
    scan_integer(Cs, [C|Stack], Pos);
scan_integer(Cs, Stack, Pos) ->
    {Stack,Cs,Pos}.

scan_after_int([$.,C|Cs0], Ncs0, Toks, SPos, CPos) when C >= $0, C =< $9 ->
    {Ncs,Cs,CPos1} = scan_integer(Cs0, [C,$.|Ncs0], CPos),
    scan_after_fraction(Cs, Ncs, Toks, SPos, CPos1);	
scan_after_int(Cs, Ncs, Toks, SPos, CPos) ->
    N = list_to_integer(reverse(Ncs)),
    scan1(Cs, [{integer,SPos,N}|Toks], CPos).

scan_after_fraction([$E|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent(Cs, [$E|Ncs], Toks, SPos, CPos);
scan_after_fraction([$e|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent(Cs, [$e|Ncs], Toks, SPos, CPos);
scan_after_fraction(Cs, Ncs, Toks, SPos, CPos) ->
    case catch list_to_float(reverse(Ncs)) of
	N when float(N) ->
	    scan1(Cs, [{float,SPos,N}|Toks], CPos);
	Error -> scan_error({illegal,float}, SPos)
    end.

%% scan_exponent(CharList, NumberCharStack, TokenStack, StartPos, CurPos)
%%  Generate an error here if E{+|-} not followed by any digits.

scan_exponent([$+|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent1(Cs, [$+|Ncs], Toks, SPos, CPos);
scan_exponent([$-|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent1(Cs, [$-|Ncs], Toks, SPos, CPos);
scan_exponent(Cs, Ncs, Toks, SPos, CPos) ->
    scan_exponent1(Cs, Ncs, Toks, SPos, CPos).

scan_exponent1([C|Cs0], Ncs0, Toks, SPos, CPos) when C >= $0, C =< $9 ->
    {Ncs,Cs,CPos1} = scan_integer(Cs0, [C|Ncs0], CPos),
    case catch list_to_float(reverse(Ncs)) of
	N when float(N) ->
	    scan1(Cs, [{float,SPos,N}|Toks], CPos1);
	Error -> scan_error({illegal,float}, SPos)
    end;
scan_exponent1(_, _, _, _, CPos) ->
    scan_error(float, CPos).

scan_error(In, Pos) ->
    {error,{Pos,cerl_scanner,In}}.
