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
%%% The Original Code is rdbms_types-1.0.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:	rdbms_types.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Type conversion (from strings) library for rdbms
%%% 
%%% Modules used : rdbms, erl_scan, erl_parse
%%% 
%%%----------------------------------------------------------------------

-module(rdbms_types).
-vsn('1.0').
-date('99-01-08').
-author('ulf.wiger@ericsson.com').


%%%----------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%%----------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%%----------------------------------------------------------------------
-export([default/1]).
-export([check/2]).

%%%----------------------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% #3.    CODE
%%%----------------------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% -type default(Attr : {atom(), atom()})->
%%%     undefined | term().
%%% Input: {Tab, Attr}
%%% Output: Default_value
%%% Exceptions: 
%%% Description: This function returns the specified default value
%%%              for attr Attr in table Tab.
%%%----------------------------------------------------------------------
default(Attr) ->
    rdbms:default(Attr).

%%%----------------------------------------------------------------------
%%% -type check(Val : string(), Attr : {atom(), atom()})->
%%%     term()  | error().
%%% Input: String representing an imported value
%%% Output: CorrectValue
%%% Exceptions: Non-local return if input is invalid
%%% Description: 
%%%----------------------------------------------------------------------
check(Val, Attr) ->
    Val1 = check_type(Val, Attr),
    rdbms:verify_attribute(Val1, Attr),
    Val1;
check(Val, _) ->
    Val.

%%%----------------------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%%	#internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%%	#check_type(Value, Attr)
%%% Input: Value = string(), Attr = {TabName, AttrName}
%%% Output: NewValue | {error, Reason}
%%% Exceptions: 
%%% Description: This function tries to enforce the specified 
%%%              type for Attr.
%%%----------------------------------------------------------------------
check_type(Val, Attr) ->
    case rdbms:type(Attr) of
	string ->
	    Val;
	number ->
	    force_number(Val);
	atom ->
	    list_to_atom(Val);
	integer ->
	    force_integer(Val);
	float ->
	    force_float(Val);
	list ->
	    case force_term(Val ++ ".\n") of
		T when list(T) ->
		    T;
		_ ->
		    exit({expected_list, Val})
	    end;
	tuple ->
	    case force_term(Val ++ ". ") of
		T when tuple(T) ->
		    T;
		_ ->
		    exit({expected_tuple, Val})
	    end;
	term ->
	    force_term(Val ++ ". ");
	any ->
	    force_any(Val);
	{record, Rec} ->
	    force_record(force_term(Val ++ ". "), Rec);
	Other ->
	    Val
    end.



%%%----------------------------------------------------------------------
%%%	#force_number(Value)
%%% Input: ValueAsString
%%% Output: ValueAsNumber
%%% Exceptions: non-local return if operation fails
%%% Description: This function converts Value to a number, 
%%%              if possible.
%%%----------------------------------------------------------------------
force_number(Val) when list(Val) ->
    case (catch force_integer(Val)) of
	{'EXIT',_} ->
	    case (catch force_float(Val)) of
		{'EXIT', _} ->
		    exit({error, {expected_number, Val}});
		Float ->
		    Float
	    end;
	Int ->
	    Int
    end;
force_number(Val) ->
    exit({error, {expected_number, Val}}).

%%%----------------------------------------------------------------------
%%%	#force_integer(Value)
%%% Input: ValueAsString
%%% Output: ValueAsInteger
%%% Exceptions: non-local return if operation fails
%%% Description: This function converts Value to an integer, 
%%%              if possible.
%%%----------------------------------------------------------------------
force_integer(Val) when list(Val) ->
    case catch list_to_integer(Val) of
	Int when integer(Int) ->
	    Int;
	{'EXIT', _} ->
	    case catch force_term(Val++". ") of
		Int when integer(Int) ->
		    Int;
		_ ->
		    exit({error, {expected_integer, Val}})
	    end
    end;
force_integer(Val) ->
    exit({error, {expected_integer, Val}}).



%%%----------------------------------------------------------------------
%%%	#force_float(Value)
%%% Input: ValueAsString
%%% Output: ValueAsFloat
%%% Exceptions: non-local return if operation fails
%%% Description: This function converts Value to a floating-point number, 
%%%              if possible.
%%%----------------------------------------------------------------------
force_float(Val) when list(Val) ->
    case catch list_to_float(Val) of
	{'EXIT', _} ->
	    case catch force_term(Val ++ ". ") of
		F when float(F) ->
		    F;
		I when integer(I) ->
		    float(I);
		_ ->
		    exit({error, {expected_float, Val}})
	    end;
	Float ->
	    Float
    end;
force_float(Val) ->
    exit({error, {expected_float, Val}}).



%%%----------------------------------------------------------------------
%%%	#force_any(String)
%%% Input: String
%%% Output: NewValue
%%% Exceptions: non-local return if operation fails
%%% Description: This function tries to convert to the most
%%%              reasonable type. It does not try to verify records.
%%%----------------------------------------------------------------------
force_any(String) ->
    case catch force_term(String ++ ". ") of
	{'EXIT', _} ->
	    String;
	Other ->
	    Other
    end.

%%%----------------------------------------------------------------------
%%%	#force_term(String)
%%% Input: String
%%% Output: NewValue
%%% Exceptions: non-local return if operation fails
%%% Description: This function converts String to an Erlang term, 
%%%              if possible.
%%%----------------------------------------------------------------------
force_term(String) ->
    {ok, Toks, _} = erl_scan:string(String),
    {ok, Term} = erl_parse:parse_term(Toks),
    Term.


%%%----------------------------------------------------------------------
%%%	#force_record(Term, RecordName)
%%% Input: Term, RecordName
%%% Output: Record
%%% Exceptions: non-local return if operation fails
%%% Description: This function converts Term to a record as specified, 
%%%              in rdbms.
%%%----------------------------------------------------------------------
force_record(Term, Rec) when tuple(Term), element(1, Term) == Rec ->
    case rdbms:attributes(Rec) of
	undefined ->
	    exit({undefined_record, Term});
	Attrs ->
	    ExpectedSize = length(Attrs)+1,
	    if ExpectedSize == size(Term) ->
		    Vals = verify_values(Attrs, tl(tuple_to_list(Term)), Rec),
		    list_to_tuple([Rec|Vals]);
	       true ->
		    exit({invalid_record, Term})
	    end
    end;
force_record(Term, Rec) ->
    exit({not_a_record, Term}).

verify_values([Attr|Attrs], [Val|Vals], Rec) ->
    [check(Val, {Rec, Attr})|verify_values(Attrs, Vals, Rec)];
verify_values([], [], _) ->
    [].


%%%----------------------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%%----------------------------------------------------------------------







