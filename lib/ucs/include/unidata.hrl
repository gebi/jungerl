%%% -*- Erlang -*-
%%% $Id$

%%% Format of the records in ucsdata.dets
%%% Pretty close to an exact duplicate of the UnicodeData.txt fields.

-record(unidata,{
	  code,			% [0] Code value, integer
	  name,			% [1] Character name, string
	  category='Cn',	% [2] General Category, atom
	  combining=0,		% [3] Canonical Combining Classes, 0 .. 255
	  bidi='L',		% [4] Bidirectional Category, atom
	  decomp=[],		% [5] Character Decomposition Mapping, list
	  numeric,		% [6..8]: numeric value(s), see below
	  mirrored=false,	% [9] Mirrored, bool
	  old_name="",		% [10] Unicode 1.0 Name, string
	  comment="",		% [11] 10646 comment field, string
	  case_mapping=[]	% [12-14] Case mapping, tuple-list
	 }).

%%% Fields 6-8 ("Decimal digit value", "Digit value", and
%%% "Numeric value") are combined into a single field in the
%%% record ('numeric'), which contains:
%%%	undefined -- all 3 fields are undefined
%%%	0 .. 9 -- all 3 fields have the specified value
%%%	{digit,N} -- field 6 is undefined, fields 7 and 8 have
%%%		the non-negative integral value N.
%%%	{N} -- fields 6 and 7 are undefined, field 8 has
%%%		the integral value N.
%%%	{N,D} -- fields 6 and 7 are undefined, field 8 has
%%%		the rational value N/D.

%%% Fields 12-14 ("Uppercase Mapping", "Lowercase Mapping", and "Titlecase
%%% Mapping") are combined into a list of pairs: {upper,Char}, {lower,Char}
%%% and {title,Char} (with undefined elements missing).
