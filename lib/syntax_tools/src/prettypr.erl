%% =====================================================================
%% Generic pretty printer: a strict-style context passing implementation
%% of John Hughes algorithm, described in ``The design of a
%% Pretty-printing Library''. The "paragraph-style" formatting, empty
%% documents and "floating" documents are my own additions to the
%% algorithm. A note on hacking: it's fairly easy to break this thing
%% (in particular, to muck up the complexity) if you don't understand
%% how it works.
%%
%% $Id$
%% ---------------------------------------------------------------------
%% Copyright (C) 2000-2001 Richard Carlsson
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
%% =====================================================================

%% TODO: can floats be moved in/out of sep:s without too much pain?

-module(prettypr).

-export([above/2, beside/2, best/3, break/1, empty/0, floating/1,
	 floating/3, follow/2, follow/3, format/1, format/2,
	 format/3, nest/2, par/1, par/2, sep/1, text/1]).

-record(text, {s}).
-record(nest, {n, d}).
-record(beside, {d1, d2}).
-record(above, {d1, d2}).
-record(sep, {ds, i = 0, p = false}).


%% =====================================================================
%% text(string()) -> document()
%%
%%	Yields a document representing a fixed sequence of characters.
%%	The string should contain only *printable* characters (tabs
%%	allowed but not recommended), and not newline, line feed,
%%	vertical tab, etc. A tab character (`$\t') is interpreted as
%%	padding of 1-8 space characters to the next column of 8
%%	characters *within the string*.

text(S) ->
    mktext(string(S)).	  % convert to internal representation

%% This function is used internally only, and expects a string on
%% the internal representation:

mktext(S) ->
    #text{s = S}.


%% =====================================================================
%% empty() -> document()
%% break(Doc) -> Doc1
%%
%%	    Doc = Doc1 = document()
%%
%%	`empty' yields the empty document, which has neither height nor
%%	width (it is thus different from an empty `text' string, which
%%	has zero width but height 1). Empty documents are occasionally
%%	useful; in particular, they have the property that |above(X,
%%	empty())| will force a new line after `X' without leaving an
%%	empty line below it; since this is a common idiom, the utility
%%	function `break' will place a given document in such a context.

empty() ->
    null.

break(D) ->
    above(D, empty()).


%% =====================================================================
%% nest(N, Doc) -> Doc1
%%
%%	    N = integer()
%%	    Doc = Doc1 = document()
%%
%%	`Doc1' is the document `Doc' with an additional indentation of
%%	`N' character positions to the right. Note that N may be
%%	negative, shifting the text to the left, or zero, in which case
%%	D is not affected.

nest(N, D) ->
    if N == 0 ->
	    D;
       true ->
	    #nest{n = N, d = D}
    end.


%% =====================================================================
%% beside(Doc1, Doc2) -> Doc
%%
%%	    Doc1 = Doc2 = Doc = document()
%%
%%	`Doc' is a document representing the horizontal concatenation of
%%	the documents `Doc1' and `Doc2' such that the last character of
%%	`Doc1' is directly adjacent to the first character of `Doc2', in
%%	all possible layouts of `Doc'.
%%
%%	Note: any indentation of `Doc2' by `nest' is dropped.

beside(D1, D2) ->
    #beside{d1 = D1, d2 = D2}.


%% =====================================================================
%% above(Doc1, Doc2) -> Doc
%%
%%	    Doc1 = Doc2 = Doc = document()
%%
%%	`Doc' is a document representing the vertical concatenation of
%%	the documents `Doc1' and `Doc2' such that the first line of
%%	`Doc2' follows directly below the last line of `Doc1', and the
%%	first character of `Doc1' is in the same horizontal column as
%%	the first character of `Doc2', in all possible layouts of `Doc'.

above(D1, D2) ->
    #above{d1 = D1, d2 = D2}.


%% =====================================================================
%% sep(Docs) -> Doc
%%
%%	    Docs = [document()] \ []
%%	    Doc = document()
%%
%%	`Doc' is a document representing two alternative layouts of the
%%	(nonempty) sequence `Docs' of documents, such that either all
%%	elements in `Docs' are concatenated horizontally, and separated
%%	by a space character, or all elements are concatenated
%%	vertically (without extra separation).
%%
%%	Note: If some document in `Docs' contains a line break, the
%%	vertical layout will always be selected.

sep(Ds) ->
    #sep{ds = Ds}.


%% =====================================================================
%% par(Docs) -> Doc
%% par(Docs, Offset) -> Doc
%% follow(Doc1, Doc2) -> Doc
%% follow(Doc1, Doc2, Offset) -> Doc
%%
%%	    Docs = [document()] \ []
%%	    Doc = Doc1 = Doc2 = document()
%%	    Offset = integer()
%%
%%	`Doc' is a document representing all possible alternative
%%	left-aligned "paragraph-style" layouts of the (nonempty)
%%	sequence `Docs' of documents. Elements in `Docs' are separated
%%	horizontally by a single space character and vertically with a
%%	single line break. All new lines are indented to the same left
%%	column. The optional `Offset' parameter specifies the
%%	indentation of the following lines (if any) relative to the
%%	position of the first element in `Docs'. For example, with an
%%	offset of -4, the following layout can be produced:
%%
%%		    1 2 3 4
%%		5 6 7 8 9 10
%%		11 12 13 14
%%		15 16 17
%%
%%	Note: whenever a document in `Docs' contains a line break, it
%%	will be placed on a separate line. Thus, neither
%%
%%		(foo) (bar
%%		       baz)
%%	nor
%%		(bar
%%		 baz) (foo)
%%
%%	will be generated. However, a useful idiom for making the former
%%	variant possible for two documents `D1' and `D2' is:
%%	|beside(par([D1, text("")], N), D2)|. This will break the line
%%	between `D1' and `D2' if `D1' contains a line break, or
%%	otherwise if necessary, and (optionally) further indent `D2' by
%%	`N' character positions; i.e.:
%%
%%		D1 D2
%%	or
%%		D1
%%		--N-->D2
%%
%%	The utility function `follow' creates this context for two
%%	documents `D1' and `D2', and an optional integer `N'.

par(Ds) ->
    par(Ds, 0).

par(Ds, N) ->
    mksep(Ds, N, true).

%% Used internally only:

mksep(Ds, N, P) ->
    #sep{ds = Ds, i = N, p = P}.

follow(D1, D2) ->
    follow(D1, D2, 0).

follow(D1, D2, N) ->
    beside(par([D1, nil()], N), D2).


%% =====================================================================
%% floating(Doc) -> Doc1
%% floating(Doc, HPri, VPri) -> Doc1
%%
%%	    Doc = Doc1 = document()
%%	    HPri = VPri = integer()
%%
%%	`Doc1' is a "floating" document representing the same set of
%%	layouts as `Doc'. A floating document may be moved relative to
%%	other floating documents immediately beside or above it,
%%	according to their relative horizontal and vertical priorities.
%%	These priorities are set with the `HPri' and `VPri' parameters;
%%	if left out, both default to zero.
%%
%%	Notes: Floating documents seem to be working well, but are less
%%	general than one could wish, losing effect when embedded in
%%	certain contexts. It is possible to nest "floating" operators
%%	(even with different priorities), but the effects may be
%%	difficult to predict. In any case, note that the way the
%%	algorithm reorders floating documents amounts to a "bubblesort",
%%	so don't expect it to be able to sort large sequences of
%%	floating documents quickly.

-record(float, {d, h, v}).

floating(D) ->
    floating(D, 0, 0).

floating(D, H, V) ->
    #float{d = D, h = H, v = V}.


%% =====================================================================
%% format(Document) -> string()
%% format(Document, PaperWidth) -> string()
%% format(Document, PaperWidth, LineWidth) -> string()
%%
%%	    Document = document()
%%	    PaperWidth = LineWidth = int()
%%
%%	`Document' specifies a set of possible layouts of a text; see
%%	the corresponding constructor functions for details. `format'
%%	selects a "best" layout for the document (cf. `best') and
%%	returns the corresponding text.
%%
%%	`PaperWidth' specifies the width (in number of characters) of
%%	the field for which the text is to be laid out. `LineWidth'
%%	specifies the desired maximum width (in number of characters) of
%%	the text printed on any single line, disregarding leading and
%%	trailing white space. For instance, formatting with a
%%	`PaperWidth' of 120, it could be desirable to use a `LineWidth'
%%	of no greater than 70 in order to avoid getting too much text on
%%	individual lines. By default, `PaperWidth' is 80 and `LineWidth'
%%	is 65.

format(D) ->
    format(D, 80).

format(D, W) ->
    format(D, W, 65).

format(D, W, R) ->
    case best(D, W, R) of
	empty ->
	    exit(no_layout);
	L -> layout(L)
    end.

%% Representation:
%%
%%	document() = #text{s = string()},
%%		   | #nest{n = int(), d = document()}
%%		   | #beside{d1 = document(), d2 = document()}
%%		   | #above{d1 = document(), d2 = document()}
%%		   | #sep{ds = [document()], i = int(),
%%			  p = bool()}
%%		   | null
%%
%% A `text' node simply represents a string (which should not contain
%% linefeed or carriage return characters). A `nest' node specifies a
%% relative indentation (in number of character positions) of a
%% document. The indentation could be a negative number. A `beside' node
%% specifies a horizontal composition of two documents, and an `above'
%% node a vertical composition. A `sep' node specifies a list of
%% alternative documents; the `i' field holds the extra indentation of
%% all documents but the first in `ds', and if the `p' field is `true'
%% then the list is typeset in paragraph mode.
%%
%% The function `best/3' yields a representation of a "best layout",
%% suitable for direct conversion to text, having the following
%% restricted form:
%%
%%	layout() = {text, string()}
%%		 | {above, {text, string()}, layout()}
%%		 | {nest, int(), layout()}
%%		 | null
%%
%% the function `layout/1' performs the final transformation to a single
%% flat string.

layout(L) ->
    lists:flatten(layout(0, L)).

layout(N, #above{d1 = #text{s = S}, d2 = L}) ->
    [indent(N, string_chars(S)) | layout(N, L)];
layout(N, #nest{n = N1, d = L}) ->
    layout(N + N1, L);
layout(N, #text{s = S}) ->
    indent(N, string_chars(S));
layout(N, null) ->
    "".

indent(N, S) when N >= 8 ->
    [$\t | indent(N - 8, S)];
indent(N, S) when N > 0 ->
    [$\s | indent(N - 1, S)];
indent(N, S) ->
    [S, $\n].


%% =====================================================================
%% best(Doc, PaperWidth, LineWidth) -> empty | Doc1
%%
%%	    Doc = Doc1 = document()
%%	    PaperWidth = LineWidth = int()
%%
%%	`Doc' specifies a set of possible layouts of a text; see
%%	`format' for more information. This function selects a "best"
%%	layout for the document and returns the corresponding `Doc1'
%%	representing that single layout, or the atom `empty' if no
%%	layout could be produced. For information on `PaperWidth' and
%%	`LineWidth', see `format'.
%%
%%	This function is useful for computing a fixed layout for a
%%	document, which can then be included as part of a larger
%%	document. For example:
%%
%%	    best( ...above(text("Example:"),
%%			   nest(8, best(D, W - 12, L - 6))) ...,
%%		 W, L)
%%
%%	will include `D' as a displayed-text example indented by 8 and
%%	with a right margin indentation of 4, the maximum length of
%%	individual lines shorter by 6 than in the surrounding document.

%% Recall that a document represents a set of possible layouts. `best'
%% selects the "best" layout of a document, returning a simplified
%% representation that can be given directly to `layout', unless the
%% returned value is `empty', signaling that no layout could be
%% produced. In addition, documents on the form `#union{d1 = D1, d2 =
%% D2}' and `#fit{d = D}' are used internally.
%%
%% Note: It is vital for this algorithm to maintain the invariant on
%% unions that the left argument has a longer first line than the right
%% argument!

%% Contexts:
%%
%%	#c_best_nest{w = int(), r = int(), i = int()}
%%	#c_above_nest{d = doc(), i = int(), c = ctxt()}
%%	#c_beside{d = doc(), c = ctxt()}
%%	#c_text_beside{s = string(), c = ctxt()}
%%	#c_sep_nest{ds = [doc()], i = int(), p = bool(),
%%		    c = ctxt()}
%%	#c_best_nest_or{w = int(), r = int(), i = int(),
%%			d = doc()}
%%	#c_fit{c = ctxt()}

-record(c_best_nest, {w, r, i}).	%% best(w, r, nest(i, *))

-record(c_above_nest, {d, i = 0, c}).	%% above(*, nest(i, d))

-record(c_beside, {d, c}).		%% beside(*, d)

-record(c_text_beside, {s, c}).		%% beside(text(s), *)

%% p = false	=>	sep([* | map(nest i, ds)])
%% p = true	=>	par([* | map(nest i, ds)])

-record(c_sep_nest, {ds, i, p, c}).

-record(c_best_nest_or, {w, r, i, d}).	%% nicest(
					%%   best(w, r,
					%%	  nest(i, *)),
					%%   best(w, r, d))

-record(c_fit, {c}).			%% fit(*)

-record(c_float_beside, {d, h, v, c}).		%% beside(
						%%   float(d, h,
						%%         v),
						%%   *)
-record(c_float_above_nest, {d, h, v, i, c}).	%% above(
						%%   float(d, h,
						%%         v),
						%%   nest(i, *))

%% Contexts introduced:		In case:
%%
%%	c_best_nest		top-level call
%%	c_above_nest		above (c_best_nest)
%%	c_beside		beside (c_best_nest)
%%	c_text_beside		text (c_beside)
%%	c_sep_nest		sep (c_best_nest)
%%	c_best_nest_or		union (c_best_nest)
%%	c_fit			fit
%%	c_float_beside		float (c_beside)
%%	c_float_above_nest	float (c_above_nest)

%% Document structures not available to the user:

-record(union, {d1, d2}).
-record(fit, {d}).

%% Entry point for the layout algorithm:

best(D, W, R) ->
    rewrite(D, #c_best_nest{w = W, r = R, i = 0}).

rewrite(#text{s = S}, C) ->
    case C of
	#c_best_nest{i = N} ->
	    nest(N, mktext(S));		% finish
	#c_above_nest{d = D1, i = N1, c = C1} ->
	    case C1 of
		#c_best_nest{w = W, r = R, i = N} ->
		    %% Move out completed line.
		    %% (Note new indentation N1.)
		    nest(N,
			 above(mktext(S),
			       rewrite(D1,
				       #c_best_nest{w = W - N,
						    r = R,
						    i = N1})));
		#c_beside{d = D2, c = C2} ->
		    %% Associativity (not symmetric)
		    rewrite(above(mktext(S),
				  nest(N1, beside(D1, D2))), C2);
		#c_text_beside{s = S1, c = C2} ->
		    %% Join segments (note the indentation!)
		    rewrite(above(mktext(concat(S1, S)),
				  nest(N1 + width(S1), D1)),
			    C2);
		#c_sep_nest{ds = Ds, i = N, c = C2} ->
		    case is_empty_string(S) of
			false ->
			    %% Move out the prefix (note the
			    %% indentation!)
			    W = width(S),
			    rewrite(beside(
				      mktext(S),
				      mksep([above(nil(),
						   nest(N1 - W,
							D1))
					     | Ds],
					    N - W,
					    C1#c_sep_nest.p)),
				    C2);
			true ->
			    %% Like when we have just an empty
			    %% string and nothing else, this
			    %% forces us to expand the `sep'. The
			    %% line break will then force a normal
			    %% `sep' to select the vertical
			    %% alternative, but for a `par', we
			    %% need to force a line break before
			    %% the remaining elements are laid
			    %% out. (Note the indentation!)
			    case C1#c_sep_nest.p of
				false ->
				    rewrite(expand_sep(
					      above(nil(),
						    nest(N1, D1)),
					      Ds, N),
					    C2);
				true ->
				    rewrite(expand_par(
					      above(nil(),
						    nest(N1, D1)),
					      Ds, N),
					    C2)
			    end
		    end;
		#c_best_nest_or{w = W, r = R, i = N, d = D} ->
		    L = width(S),
		    case ((L + N) > W) or (L > R) of
			true ->
			    %% The first line of the LHS layout is
			    %% not nice, so select the RHS.
			    rewrite(D, #c_best_nest{w = W, r = R,
						    i = N});
			false ->
			    %% Select the LHS. (Note the
			    %% indentation!)
			    rewrite(above(mktext(S),
					  nest(N1, D1)),
				    #c_best_nest{w = W, r = R,
						 i = N})
		    end;
		#c_float_beside{d = D2, c = C2} ->
		    rewrite(beside(D2, above(mktext(S),
					     nest(N1, D1))),
			    C2);
		#c_float_above_nest{d = D2, i = N2, c = C2} ->
		    rewrite(above(D2,
				  nest(N2, above(mktext(S),
						 nest(N1, D1)))),
			    C2);
		#c_above_nest{} ->
		    exit(badarg);	% this can't happen
		#c_fit{} ->
		    exit(badarg)	% this can't happen
	    end;
	#c_beside{d = D1, c = C1} ->
	    case C1 of
		#c_above_nest{d = D2, i = N, c = C2} ->
		    case is_empty_string(S) of
			false ->
			    %% Move out the prefix (note the
			    %% indentation!)
			    W = width(S),
			    rewrite(beside(mktext(S),
					   above(
					     beside(nil(), D1),
					     nest(N - W, D2))),
				    C2);
			true ->
			    %% Pass on
			    rewrite(D1, #c_text_beside{s = S,
						       c = C1})
		    end;
		#c_text_beside{s = S1, c = C2} ->
		    %% Associativity (we simplify early)
		    rewrite(beside(mktext(concat(S1, S)), D1),
			    C2);
		#c_sep_nest{ds = Ds, i = N, c = C2} ->
		    case is_empty_string(S) of
			false ->
			    %% Move out the prefix (note the
			    %% indentation!)
			    W = width(S),
			    rewrite(beside(mktext(S),
					   mksep(
					     [beside(nil(), D1)
					      | Ds],
					     N - W,
					     C1#c_sep_nest.p)),
				    C2);
			true ->
			    %% Pass on
			    rewrite(D1, #c_text_beside{s = S,
						       c = C1})
		    end;
		#c_best_nest_or{w = W, r = R, i = N, d = D} ->
		    L = width(S),
		    case ((L + N) > W) or (L > R) of
			true ->
			    %% The first line of the LHS layout is
			    %% not nice, so select the RHS.
			    rewrite(D, #c_best_nest{w = W, r = R,
						    i = N});
			false ->
			    %% Pass on
			    rewrite(D1, #c_text_beside{s = S,
						       c = C1})
		    end;
		#c_float_beside{d = D2, c = C2} ->
		    rewrite(beside(D2, beside(mktext(S), D1)),
			    C2);
		#c_float_above_nest{d = D2, i = N, c = C2} ->
		    rewrite(above(D2,
				  nest(N, beside(mktext(S), D1))),
			    C2);
		_ ->
		    %% Pass on
		    rewrite(D1, #c_text_beside{s = S, c = C1})
	    end;
	#c_text_beside{s = S1, c = C1} ->
	    rewrite(mktext(concat(S1, S)), C1);	% join segments
	#c_sep_nest{ds = Ds, i = N, c = C1} ->
	    case is_empty_string(S) of
		false ->
		    %% Move out the prefix (note the indentation!)
		    rewrite(beside(mktext(S),
				   mksep([nil() | Ds],
					 N - width(S),
					 C#c_sep_nest.p)),
			    C1);
		true ->
		    %% This is the only place where we are forced to
		    %% introduce a union. Recall the invariant that the
		    %% left argument must have a longer first line than
		    %% the right argument; also recall that `Ds' is
		    %% always nonempty here. Now, since [D | Ds]
		    %% contains at least two elements, the first line of
		    %% the horizontal layout will always contain at
		    %% least one space character more than the first
		    %% line of the vertical layout.
		    case C#c_sep_nest.p of
			false ->
			    rewrite(expand_sep(nil(), Ds, N), C1);
			true ->
			    rewrite(expand_par(nil(), Ds, N), C1)
		    end
	    end;
	#c_best_nest_or{w = W, r = R, i = N, d = D} ->
	    L = width(S),
	    case ((L + N) > W) or (L > R) of
		true ->
		    %% The first line of the LHS layout is not
		    %% nice, so select the RHS (which contains
		    %% at least two lines).
		    rewrite(D, #c_best_nest{w = W, r = R, i = N});
		false ->
		    nest(N, mktext(S))	  % finish
	    end;
	#c_fit{c = C1} ->
	    %% Identity:
	    rewrite(mktext(S), C1);
	#c_float_beside{d = D1, c = C1} ->
	    rewrite(beside(D1, mktext(S)), C1);
	#c_float_above_nest{d = D1, i = N, c = C1} ->
	    rewrite(above(D1, nest(N, mktext(S))), C1)
    end;
rewrite(#nest{n = N, d = D}, C) ->
    case C of
	#c_best_nest{w = W, r = R, i = N1} ->
	    %% Note that we simplify by not creating an actual `nest'
	    %% node, but instead just modifying the context:
	    %% rewrite(nest(N1, nest(N, D))) = rewrite(nest(N1 + N, D)).
	    rewrite(D, #c_best_nest{w = W, r = R, i = N + N1});
	#c_above_nest{d = D1, i = N1, c = C1} ->
	    %% Distributivity
	    %% (Note the indentation!)
	    rewrite(nest(N, above(D, nest(N1 - N, D1))), C1);
	#c_beside{d = D1, c = C1} ->
	    %% Associativity (not symmetric):
	    rewrite(nest(N, beside(D, D1)), C1);
	#c_text_beside{} ->
	    rewrite(D, C);   % (`beside' kills RHS indentation)
	#c_sep_nest{ds = Ds, i = N1, c = C1} ->
	    %% Distributivity (in the vertical form, the RHS
	    %% indentation is killed)
	    rewrite(nest(N, mksep([D | Ds],
				  N1 - N,
				  C#c_sep_nest.p)),
		    C1);
	#c_fit{c = C1} ->
	    %% Distributivity:
	    rewrite(nest(N, fit(D)), C1);
	#c_float_beside{} ->
	    rewrite(D, C);    % (`beside' kills RHS indentation)
	#c_float_above_nest{d = D1, h = H, v = V, i = N1,
			    c = C1} ->
	    rewrite(D, #c_float_above_nest{d = D1, h = H, v = V,
					   i = N + N1, c = C1});
	#c_best_nest_or{} ->
	    exit(undefined)    % this can't happen
    end;
rewrite(#above{d1 = D1, d2 = D2}, C) ->
    case C of
	#c_above_nest{d = D3, i = N, c = C1} ->
	    %% Associativity:
	    %% (Note the indentation!)
	    rewrite(D1, #c_above_nest{d = above(D2, nest(N, D3)),
				      c = C1});
	#c_beside{d = D3, c = C1} ->
	    %% Associativity (not symmetric):
	    rewrite(above(D1, beside(D2, D3)), C1);
	#c_fit{c = C1} ->
	    rewrite(empty, C1);	% this is the whole point of `fit'
	_ ->
	    rewrite(D1, #c_above_nest{d = D2, c = C})	% pass on
    end;
rewrite(#beside{d1 = D1, d2 = D2}, C) ->
    case C of
	#c_beside{d = D3, c = C1} ->
	    %% Associativity:
	    rewrite(D1, #c_beside{d = beside(D2, D3), c = C1});
	#c_fit{c = C1} ->
	    %% Distributivity:
	    rewrite(beside(fit(D1), fit(D2)), C1);
	_ ->
	    rewrite(D1, #c_beside{d = D2, c = C})	% pass on
    end;
rewrite(#sep{ds = Ds, i = N, p = P}, C) ->
    case C of
	#c_fit{c = C1} ->
	    %% The vertical layout is thus impossible, and the
	    %% extra indentation has no effect.
	    rewrite(fit(horizontal(Ds)), C);
	#c_float_beside{d = D1, c = C1} ->
	    %% Floats are not moved in or out of sep's
	    rewrite(beside(D1, mksep(Ds, N, P)), C1);
	#c_float_above_nest{d = D1, i = N1, c = C1} ->
	    %% Floats are not moved in or out of sep's
	    rewrite(above(D1, nest(N1, mksep(Ds, N, P))), C1);
	_ ->
	    enter_sep(Ds, N, P, C)		% pass on
    end;
rewrite(#union{d1 = D1, d2 = D2}, C) ->
    %% Introduced by the occurrence of an empty `text' string in a
    %% `sep' context. See the note above about the invariant for
    %% unions!
    case C of
	#c_best_nest{w = W, r = R, i = N} ->
	    %% Pass on
	    rewrite(D1, #c_best_nest_or{w = W, r = R, i = N,
					d = D2});
	#c_above_nest{d = D3, i = N, c = C1} ->
	    %% Distributivity:
	    %% (Note the indentation!)
	    rewrite(union(above(D1, nest(N, D3)),
			  above(D2, nest(N, D3))),
		    C1);
	#c_beside{d = D3, c = C1} ->
	    %% Distributivity:
	    rewrite(union(beside(D1, D3), beside(D2, D3)), C1);
	#c_text_beside{s = S, c = C1} ->
	    %% Distributivity:
	    rewrite(union(beside(mktext(S), D1),
			  beside(mktext(S), D2)),
		    C1);
	#c_sep_nest{ds = Ds, i = N, c = C1} ->
	    %% Distributivity:
	    rewrite(union(mksep([D1 | Ds], N, C#c_sep_nest.p),
			  mksep([D2 | Ds], N, C#c_sep_nest.p)),
		    C1);
	#c_best_nest_or{w = W, r = R, i = N, d = D3} ->
	    %% Associativity:
	    rewrite(D1, #c_best_nest_or{w = W, r = R, i = N,
					d = union(D2, D3)});
	#c_fit{c = C1} ->
	    %% Distributivity:
	    rewrite(union(fit(D1), fit(D2)), C1);
	#c_float_beside{d = D3, h = H, v = V, c = C1} ->
	    %% Distributivity:
	    rewrite(union(beside(floating(D3, H, V), D1),
			  beside(floating(D3, H, V), D2)),
		    C1);
	#c_float_above_nest{d = D3, h = H, v = V, i = N, c = C1} ->
	    %% Distributivity:
	    rewrite(union(above(floating(D3, H, V), nest(N, D1)),
			  above(floating(D3, H, V), nest(N, D2))),
		    C1)
    end;
rewrite(empty, C) ->
    %% Introduced by `sep'.
    case C of
	#c_best_nest{} ->
	    empty;		% preserve `empty'
	#c_above_nest{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_beside{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_text_beside{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_sep_nest{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_best_nest_or{w = W, r = R, i = N, d = D} ->
	    %% Try the other layout
	    rewrite(D, #c_best_nest{w = W, r = R, i = N});
	#c_fit{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_float_beside{c = C1} ->
	    rewrite(empty, C1);	% preserve `empty'
	#c_float_above_nest{c = C1} ->
	    rewrite(empty, C1)	% preserve `empty'
	end;
rewrite(#fit{d = D}, C) ->
    %% Introduced by the occurrence of an empty `text' string in a
    %% `sep' context.
    case C of
	#c_fit{} ->
	    %% Idempotency:
	    rewrite(D, C);
	_ ->
	    rewrite(D, #c_fit{c = C})	% pass on
    end;
rewrite(#float{d = D, h = H, v = V}, C) ->
    case C of
	#c_beside{d = D1, c = C1} ->
	    case C1 of
		#c_float_beside{d = D2, h = H1, v = V1, c = C2}
		when H1 > H ->
		    %% Move left
		    rewrite(beside(floating(D, H, V),
				   beside(floating(D2, H1, V1),
					  D1)),
			    C2);
		#c_float_beside{d = D2, h = H1, v = V1, c = C2}
		when V1 /= V ->
		    %% Align vertically
		    rewrite(above(floating(D2, H1, V1),
				  beside(floating(D, H, V), D1)),
			    C2);
		#c_float_above_nest{d = D2, h = H1, v = V1,
				    i = N1, c = C2}
		when V1 > V ->
		    %% Move up (note the indentation, and note
		    %% that all three become aligned vertically)
		    rewrite(above(nest(N1, floating(D, H, V)),
				  above(floating(D2, H1, V1),
					D1)),
			    C2);
		#c_float_above_nest{d = D2, h = H1, v = V1,
				    i = N1, c = C2}
		when V1 == V, H1 /= H ->
		    %% Align horizontally
		    rewrite(beside(floating(D2, H1, V1),
				   beside(floating(D, H, V),
					  D1)),
			    C2);
		_ ->
		    rewrite(D1, #c_float_beside{d = D, h = H,
						v = V, c = C1})
	    end;
	#c_above_nest{d = D1, i = N, c = C1} ->
	    case C1 of
		#c_float_beside{d = D2, h = H1, v = V1, c = C2}
		when H1 > H ->
		    %% Move left (indentation is lost; note that
		    %% all three become aligned horizontally)
		    rewrite(beside(floating(D, H, V),
				   beside(floating(D2, H1, V1),
					  D1)),
			    C2);
		#c_float_beside{d = D2, h = H1, v = V1, c = C2}
		when V1 /= V ->
		    %% Align vertically
		    rewrite(above(floating(D2, H1, V1),
				  above(floating(D, H, V),
					nest(N, D1))),
			    C2);
		#c_float_above_nest{d = D2, h = H1, v = V1,
				    i = N1, c = C2}
		when V1 > V ->
		    %% Move up (note the indentation)
		    rewrite(above(nest(N1, floating(D, H, V)),
				  above(floating(D2, H1, V1),
					nest(N + N1, D1))),
			    C2);
		#c_float_above_nest{d = D2, h = H1, v = V1,
				    i = N1, c = C2}
		when V1 == V, H1 /= H ->
		    %% Align horizontally
		    rewrite(beside(
			      floating(D2, H1, V1),
			      above(floating(D, H, V),
				    nest(N, D1))),
			    C2);
		_ ->
		    rewrite(D1, #c_float_above_nest{d = D, h = H,
						    v = V, i = N,
						    c = C1})
	    end;
	#c_fit{c = C1} ->
	    rewrite(floating(fit(D), H, V), C1);
	#c_float_beside{d = D1, h = H1, v = V1, c = C1} ->
	    if H1 > H ->
		    %% Swap
		    rewrite(beside(floating(D, H, V),
				   floating(D1, H1, V1)),
			    C1);
	       V1 /= V ->
		    %% Align vertically
		    rewrite(above(floating(D, H, V),
				  floating(D1, H1, V1)),
			    C1);
	       true ->
		    %% Drop the 'float' wrapper of the rightmost.
		    rewrite(beside(floating(D1, H1, V1), D), C1)
	    end;
	#c_float_above_nest{d = D1, h = H1, v = V1, i = N,
			    c = C1} ->
	    if V1 > V ->
		    %% Swap (note the indentation)
		    rewrite(above(nest(N, floating(D, H, V)),
				  floating(D1, H1, V1)),
			    C1);
	       V1 == V, H1 /= H ->
		    %% Align horizontally
		    rewrite(beside(floating(D, H, V),
				   floating(D1, H1, V1)),
			    C1);
	       true ->
		    %% Drop the 'float' wrapper of the lower.
		    rewrite(above(floating(D1, H1, V1),
				  nest(N, D)),
			    C1)
	    end;
	_ ->
	    %% All other cases simply drop the `float' wrapper.
	    rewrite(D, C)
    end;
rewrite(null, C) ->
    case C of
	#c_best_nest{} ->
	    null;    % done
	#c_above_nest{d = D, i = N, c = C1} ->
	    rewrite(nest(N, D), C1);
	#c_beside{d = D, c = C1} ->
	    rewrite(D, C1);
	#c_text_beside{s = S, c = C1} ->
	    rewrite(mktext(S), C1);
	#c_sep_nest{} ->
	    %% In a `nest' context, an empty document behaves like
	    %% the empty string.
	    rewrite(nil(), C);
	#c_best_nest_or{w = W, r = R, i = N} ->
	    %% An empty document as "nice" as it can be, so we
	    %% discard the alternative.
	    rewrite(null, #c_best_nest{w = W, r = R, i = N});
	#c_fit{c = C1} ->
	    rewrite(null, C1);    % identity
	#c_float_beside{d = D, h = H, v = V, c = C1} ->
	    %% We just remove the float wrapper; cf. below.
	    rewrite(beside(D, null), C1);
	#c_float_above_nest{d = D, h = H, v = V, i = N, c = C1} ->
	    %% It is important that this case just removes the
	    %% float wrapper; the empty document must be preserved
	    %% until later, or it will not be useful for forcing
	    %% line breaks.
	    rewrite(above(D, nest(N, null)), C1)
    end.

%% Both `null' and `empty' are already in use, so what do you do?

nil() ->
    text("").

hspace() ->
    text([$\s]).

union(D1, D2) ->
    #union{d1 = D1, d2 = D2}.

fit(D) ->
    #fit{d = D}.

enter_sep(Ds, N, P, C) ->
    case Ds of
	[D] ->
	    rewrite(D, C);    % Handle this case separately
	[D | Ds1] ->
	    %% Note that we never build a `sep'-context with an
	    %% empty "tail" list! `Ds1' is nonempty here!
	    rewrite(D, #c_sep_nest{ds = Ds1, i = N, p = P, c = C})
    end.

%% When we expand a `sep', the extra indentation appears as `nest'
%% operators, but not until then.

expand_sep(D, Ds, N) ->
    union(fit(horizontal([D | Ds])),
	  vertical([D | [nest(N, D1) || D1 <- Ds]])).

expand_par(D, [D1 | Ds], N) ->
    union(beside(fit(D),
		 beside(hspace(),
			mksep([fit(D1) | Ds], N - 1, true))),
	  above(D, nest(N, par([D1 | Ds])))).

horizontal(Ds) ->
    foldr1(fun (D1, D2) ->
		   beside(D1, beside(hspace(), D2))
	   end, Ds).

vertical(Ds) ->
    foldr1(fun above/2, Ds).

foldr1(F, [H]) ->
    H;
foldr1(F, [H | T]) ->
    F(H, foldr1(F, T)).

%% Internal representation of strings; stores the field width and does
%% not perform list concatenation until the text is requested. Strings
%% are thus deep lists whose first element is the length of the string.

string(S) ->
    [strwidth(S) | S].

concat([L1 | S1], [L2 | S2]) ->
    [L1 + L2 | [S1 | S2]].

string_chars([_ | S]) ->
    S.

width(S) ->
    hd(S).

is_empty_string(S) ->
    width(S) == 0.

%% We need to use `strwidth' instead of list `length', to properly
%% handle Tab characters in the text segments. Note that the width of
%% tabs is hard-coded as 8 character positions, and that strings are
%% individually considered to be aligned at column 0; Tab characters are
%% not really nice to give to a prettyprinter, and this seems to be the
%% best interpretation.

strwidth(S) ->
    strwidth(S, 0).

strwidth([$\t | Cs], N) ->
    strwidth(Cs, N - (N rem 8) + 8);
strwidth([_ | Cs], N) ->
    strwidth(Cs, N + 1);
strwidth([], N) ->
    N.


%% =====================================================================
