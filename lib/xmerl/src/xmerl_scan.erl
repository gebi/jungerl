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
%%% The Original Code is xmerl-0.15
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s):
%%%    Mickael Remond <mickael.remond@IDEALX.com>:
%%%    Johan Blom <johan.blom@mobilearts.se>
%%%    Richard Carlsson
%%%        
%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:       xmerl_scan.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Simgle-pass XML scanner. See xmerl.hrl for data defs.
%%% 
%%% Modules used : ets, file, filename, io, lists
%%% 
%%%----------------------------------------------------------------------

-module(xmerl_scan).
-vsn('0.14').
-date('01-03-03').
-author('ulf.wiger@ericsson.com').


%% main API
-export([string/1, string/2,
	 file/1, file/2]).

%% exported helper functions
-export([is_letter/1,
	 is_namechar/1,
	 accumulate_whitespace/4,
	 detect_charset/1,detect_charset/2]).

%% access functions for various states
-export([user_state/1, user_state/2,
	 event_state/1, event_state/2,
	 hook_state/1, hook_state/2,
	 rules_state/1, rules_state/2,
	 fetch_state/1, fetch_state/2,
	 cont_state/1, cont_state/2]).

%-define(debug, 1).
-include("xmerl.hrl").		% record def, macros
-include_lib("kernel/include/file.hrl").


-define(fatal(Reason, S),
	ok=io:format("~p- fatal: ~p~n", [?LINE, Reason]),
	fatal(Reason, S)).

-define(ustate(U, S), S#xmerl_scanner{user_state = U}).


%%% FIXME! Whatabout aliases etc? Shouldn't transforming with ucs be optional?
%%% detect_charset(ExtCharset,Content)
%%% where
%%%   ExtCharset - Any externally declared character set (e.g. in Content-Type)
%%%   Content - XML Document
%%% Returns {auto,'iso-10646-utf-1',Content} |
%%%         {external,'iso-10646-utf-1',Content} |
%%%         {undefined,undefined,Content} |
%%%         {external,ExtCharset,Content}
%%% 
detect_charset(Content) ->
    detect_charset(undefined,Content).

detect_charset(ExtCharset,Content) ->
    case autodetect(ExtCharset,Content) of	    
	{auto,Content1} ->
	    {auto,'iso-10646-utf-1',Content};
	{external,Content1} ->
	    {external,'iso-10646-utf-1',Content};
	{undefined,_} ->
	    {undefined,undefined,Content};
	{ExtCharset, Content} ->
	    {external,ExtCharset,Content}
    end.

%%------------------------------------------------------------------------------
%% Auto detect what kind of character set we are dealing with and transform
%% to Erlang integer Unicode format if found.
%% Appendix F, Page 56-57, XML 1.0 W3C Recommendation 6 October 2000
%% (http://www.w3.org/TR/REC-xml)
%% 00 00 00 3C ( "<" in UCS-4 big-endian)
%% 3C 00 00 00 ( "<" in UCS-4 little-endian)
%% FE FF (UTF-16 - big-endian Mark)
%% FF FE (UTF-16 - little-endian Mark)
%% 00 3C 00 3F ( "<?" in UTF-16 big-endian)
%% 3C 00 3F 00 ( "<?" in UTF-16 big-endian)
%% 3C 3F (7-bit,8-bit or mixed width encoding)
%% 4C 6F A7 94 (EBCDIC) - Not Implemented!!!!

%% Check byte-order mark and transform to Unicode, Erlang integer
%%% --- With byte-order mark
autodetect(undefined,[0,0,16#fe,16#ff | Input]) ->
    {auto, ucs:from_ucs4be(Input)};
autodetect('iso-10646-utf-1',[0,0,16#fe,16#ff | Input]) ->
    {external, ucs:from_ucs4be(Input)};
autodetect(undefined,[16#ff,16#fe,0,0 | Input]) ->
    {auto, ucs:from_ucs4le(Input)};
autodetect('iso-10646-utf-1',[16#ff,16#fe,0,0 | Input]) ->
    {external, ucs:from_ucs4le(Input)};

autodetect(undefined,[16#fe,16#ff | Input]) ->
    {auto, ucs:from_utf16be(Input)};
autodetect('utf-16be',[16#fe,16#ff | Input]) ->
    {external, ucs:from_utf16be(Input)};
autodetect(undefined,[16#ff,16#fe | Input]) ->
    {auto, ucs:from_utf16le(Input)};
autodetect('utf-16le',[16#ff,16#fe | Input]) ->
    {external, ucs:from_utf16le(Input)};

autodetect(undefined,[16#ef,16#bb,16#bf | Input]) ->
    {auto, ucs:from_utf8(Input)};
autodetect('utf-8',[16#ef,16#bb,16#bf | Input]) ->
    {external, ucs:from_utf8(Input)};

%%% --- Without byte-order mark
autodetect(undefined,[0,0,0,16#3c|Input]) ->
    {auto, ucs:from_ucs4be([0,0,0,16#3c|Input])};
autodetect('iso-10646-utf-1',[0,0,0,16#3c|Input]) ->
    {external, ucs:from_ucs4be([0,0,0,16#3c|Input])};
autodetect(undefined,[16#3c,0,0,0|Input]) ->
    {auto, ucs:from_ucs4le([16#3c,0,0,0|Input])};
autodetect('iso-10646-utf-1',[16#3c,0,0,0|Input]) ->
    {external, ucs:from_ucs4le([16#3c,0,0,0|Input])};

autodetect(undefined,[0,16#3c,0,16#3f | Input]) ->
    {auto, ucs:from_utf16be([0,16#3c,0,16#3f|Input])};
autodetect('utf-16be',[0,16#3c,0,16#3f | Input]) ->
    {external, ucs:from_utf16be([0,16#3c,0,16#3f|Input])};
autodetect(undefined,[16#3c,0,16#3f,0 | Input]) ->
    {auto, ucs:from_utf16le([16#3c,0,16#3f,0|Input])};
autodetect('utf-16le',[16#3c,0,16#3f,0 | Input]) ->
    {external, ucs:from_utf16le([16#3c,0,16#3f,0|Input])};

autodetect(ExtCharset,Content) ->
    {ExtCharset, Content}.



%% Functions to access the various states

user_state(#xmerl_scanner{user_state = S}) -> S.

event_state(#xmerl_scanner{fun_states = #xmerl_fun_states{event = S}}) -> S.
hook_state(#xmerl_scanner{fun_states = #xmerl_fun_states{hook = S}}) -> S.
rules_state(#xmerl_scanner{fun_states = #xmerl_fun_states{rules = S}}) -> S.
fetch_state(#xmerl_scanner{fun_states = #xmerl_fun_states{fetch = S}}) -> S.
cont_state(#xmerl_scanner{fun_states = #xmerl_fun_states{cont = S}}) -> S.


%% Functions to modify the various states

user_state(X, S) ->
    S#xmerl_scanner{user_state = X}.

event_state(X, S = #xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{event = X},
    S#xmerl_scanner{fun_states = FS1}.
hook_state(X, S = #xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{hook = X},
    S#xmerl_scanner{fun_states = FS1}.
rules_state(X, S = #xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{rules = X},
    S#xmerl_scanner{fun_states = FS1}.
fetch_state(X, S = #xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{fetch = X},
    S#xmerl_scanner{fun_states = FS1}.
cont_state(X, S = #xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{cont = X},
    S#xmerl_scanner{fun_states = FS1}.


file(F) ->
    file(F, []).

file(F, Options) ->
    case int_file(F, Options) of
	{Res, Tail, S = #xmerl_scanner{}} ->
	    {Res,Tail};
	{error, Reason} ->
	    {error, Reason};
	Other ->
	    {error, Other}
    end.

int_file(F, Options) ->
    case file:read_file(F) of
	{ok, Bin} ->
	    Options1 = 
		case lists:keymember(directory, 1, Options) of
		    false ->
			[{directory, filename:dirname(F)}|Options];
		    true ->
			Options
		end,
	    int_string(binary_to_list(Bin), Options1);
	Error ->
	    Error
    end.


string(Str) ->  
    string(Str, []).

string(Str, Options) ->
    case int_string(Str, Options) of
	{Res, Tail, S = #xmerl_scanner{close_fun = Close}} ->
	    Close(S),
	    {Res,Tail};
	{error, Reason} ->
	    {error, Reason};
	Other ->
	    {error, Other}
    end.

int_string(Str, Options) ->
    S = initial_state(Options),
    Res = scan_prolog(Str, S).
    


initial_state(Options) ->
    {ok, Cwd} = file:get_cwd(),
    initial_state(Options, #xmerl_scanner{
					  event_fun = fun event/2,
					  hook_fun = fun hook/2,
					  acc_fun = fun acc/3,
					  fetch_fun = fun fetch/2,
					  close_fun = fun close/1,
					  continuation_fun = fun cont/3,
					  rules_read_fun = fun rules_read/3,
					  rules_write_fun = fun rules_write/4,
					  directory = Cwd
					 }).

initial_state([{event_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{event_fun = F});
initial_state([{event_fun, F, ES}|T], S) ->
    S1 = event_state(ES, S#xmerl_scanner{event_fun = F}),
    initial_state(T, S1);
initial_state([{acc_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{acc_fun = F});
initial_state([{hook_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{hook_fun = F});
initial_state([{hook_fun, F, HS}|T], S) ->
    S1 = hook_state(HS, S#xmerl_scanner{hook_fun = F}),
    initial_state(T, S1);
initial_state([{close_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{close_fun = F});
initial_state([{fetch_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{fetch_fun = F});
initial_state([{fetch_fun, F, FS}|T], S) ->
    S1 = fetch_state(FS, S#xmerl_scanner{fetch_fun = F}),
    initial_state(T, S1);
initial_state([{fetch_path, P}|T], S) ->
    initial_state(T, S#xmerl_scanner{fetch_path = P});
initial_state([{continuation_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{continuation_fun = F});
initial_state([{continuation_fun, F, CS}|T], S) ->
    S1 = cont_state(CS, S#xmerl_scanner{continuation_fun = F}),
    initial_state(T, S1);
initial_state([{rules, R}|T], S) ->
    initial_state(T, S#xmerl_scanner{rules = R,
				     keep_rules = true});
initial_state([{rules, Read, Write, RS}|T], S) ->
    S1 = rules_state(RS, S#xmerl_scanner{rules_read_fun = Read,
					 rules_write_fun = Write,
					 keep_rules = true}),
    initial_state(T, S1);
initial_state([{user_state, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{user_state = F});
initial_state([{directory, D}|T], S) ->
    initial_state(T, S#xmerl_scanner{directory = D});
initial_state([{prolog, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{prolog = F});
initial_state([{space, L}|T], S) ->
    initial_state(T, S#xmerl_scanner{space = L});
initial_state([{line, L}|T], S) ->
    initial_state(T, S#xmerl_scanner{line = L});
initial_state([], S = #xmerl_scanner{rules = undefined}) ->
    Tab = ets:new(rules, [set, public]),
    S#xmerl_scanner{rules = Tab};
initial_state([], S) ->
    S.


%%% -----------------------------------------------------
%%% Default modifier functions

%%% Hooks:
%%% - {element, Line, Name, Attrs, Content}
%%% - {processing_instruction, Line, Data}

hook(X, State) ->
    {X, State}.

%%% Events:
%%%
%%% #xmerl_event{event : started | ended,
%%%              line  : integer(),
%%%		 col   : integer(),
%%%              data}
%%%
%%% Data		Events
%%% document		started, ended
%%% #xmlElement		started, ended
%%% #xmlAttribute	ended
%%% #xmlPI		ended
%%% #xmlComment		ended
%%% #xmlText		ended
event(X, S) ->
    S.

%% The acc/3 function must return either {[X'|Acc], S'} or {Acc, S'}.
%% It is not allowed to make significant changes to X, such as altering
%% the object type.
%% Below is an example of an acceptable operation
acc(X = #xmlText{value = Text}, Acc, S) ->
    {[X#xmlText{value = lists:flatten(Text)}|Acc], S};
acc(X, Acc, S) ->
    {[X|Acc], S}.

fetch({system, URI}, S) ->
    fetch_URI(URI, S);
fetch({public, PublicID, URI}, S) ->
    fetch_URI(URI, S).

fetch_URI(URI, S) ->
    %% assume URI is a filename
    Split = filename:split(URI),
    Filename = lists:last(Split),
    Fullname = 
	case Split of
	    ["/"|Rest] when Rest /= [] ->
		%% absolute path name
		URI;
	    _ ->
		filename:join(S#xmerl_scanner.directory, URI)
	end,
    File = path_locate(S#xmerl_scanner.fetch_path, Filename, Fullname),
    ?dbg("fetch(~p) -> {file, ~p}.~n", [URI, File]),
    {ok, {file, File}, S}.

path_locate([Dir|Dirs], FN, FullName) ->
    F = filename:join(Dir, FN),
    case file:read_file_info(F) of
	{ok, #file_info{type = regular}} ->
	    F;
	_ ->
	    path_locate(Dirs, FN, FullName)
    end;
path_locate([], FN, FullName) ->
    FullName.


cont(F, Exception, US) ->
    Exception(US).

close(S) ->
    S.

%%%
%%% -----------------------------------------------------


%%% -----------------------------------------------------
%%% Scanner

%%%%%%% [22] Prolog
scan_prolog(Str, S = #xmerl_scanner{event_fun = Event,
				    line = L, col = C}) ->
    S1 = Event(#xmerl_event{event = started,
			    line = L,
			    col = C,
			    data = document}, S),

    {Res, Tail, S2} = scan_prolog(Str, S1, StartPos = 1),
    
    S3 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S2#xmerl_scanner.line,
					       col = S2#xmerl_scanner.col,
					       data = document}, S2),
    S4 = cleanup(S3),

    {Res, Tail, S4}.


scan_prolog([], S = #xmerl_scanner{continuation_fun = F}, Pos) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_prolog(MoreBytes, S1, Pos)
      end, fun(S1) -> {[], [], S1} end, S);
scan_prolog("<?xml" ++ T, S0, Pos) ->
    ?dbg("prolog(\"<?xml\")~n", []),
    ?bump_col(5),
    {_, T1, S1} = scan_xml_decl(T, S),
    scan_prolog(T1, S1, Pos);
scan_prolog(T = "<!--" ++ _, S, Pos) ->
    {_, T1, S1} = scan_comment(T, S, Pos, Parents = [], Lang = []),
    scan_prolog(T1, S1, Pos+1);
scan_prolog("<?" ++ T, S0, Pos) ->
    ?dbg("prolog(\"<?\")~n", []),
    ?bump_col(5),
    {PI, T1, S1} = scan_pi(T, S, Pos),
    scan_prolog(T1, S1, Pos+1);
scan_prolog("<!DOCTYPE" ++ T, S0, Pos) ->
    ?dbg("prolog(\"<!DOCTYPE\")~n", []),
    ?bump_col(9),
    {T1, S1} = scan_doctype(T, S),
    scan_prolog(T1, S1, Pos);
scan_prolog(Str = "<!" ++ _, S, Pos) ->
    ?dbg("prolog(\"<!\")~n", []),
    %% In e.g. a DTD, we jump directly to markup declarations
    scan_ext_subset(Str, S);
scan_prolog("<" ++ T, S0 = #xmerl_scanner{prolog=Mode,user_state=US}, Pos) ->
    ?dbg("prolog(\"<\")~n", []),
    case Mode of
	stop ->
	    {{US,"<" ++ T},[],S0};
	_ ->
	    %% OK, assume we're going directly for the data.
	    ?bump_col(1),
	    scan_element(T, S, Pos)
    end;
scan_prolog(T = [H|_], S, Pos) when ?whitespace(H) ->
    ?dbg("prolog(whitespace)~n", []),
    ?strip1,
    scan_prolog(T1, S1, Pos).


cleanup(S = #xmerl_scanner{keep_rules = false,
			   rules = Rules}) ->
    ets:delete(Rules),
    S#xmerl_scanner{rules = undefined};
cleanup(S) ->
    S.



%%%%%%% [23] XMLDecl

scan_xml_decl(T, S) ->
    %% VersionInfo [24] is mandatory
    ?strip1,
    "version" ++ T2 = T1, S2=S1,
    {T3, S3} = scan_eq(T2, S2),
    {Vsn, T4, S4} = scan_xml_vsn(T3, S3),
    ?strip5,
    Attr = #xmlAttribute{name = version,
			 parents = [{xml, XMLPos = 1}],
			 value = Vsn},
    scan_xml_decl(T5, S5, #xmlDecl{attributes = [Attr]}).

scan_xml_decl([], S = #xmerl_scanner{continuation_fun = F}, Decl) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_xml_decl(MoreBytes, S1, Decl)
      end,
      fun(S1) ->
	      {[], [], S1}
      end, S);
scan_xml_decl("?>" ++ T, S0 = #xmerl_scanner{hook_fun = Hook,
					     event_fun = Event}, 
	      Decl0 = #xmlDecl{attributes = Attrs}) ->
    ?bump_col(2),
    ?strip1,
    Decl = Decl0#xmlDecl{attributes = lists:reverse(Attrs)},
    S2 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S0#xmerl_scanner.line,
					       col = S0#xmerl_scanner.col,
					       data = Decl}, S1),
    {Ret, S3} = Hook(Decl, S2),
    {[], T1, S3};
scan_xml_decl("encoding" ++ T, S0 = #xmerl_scanner{event_fun = Event},
	      Decl0 = #xmlDecl{attributes = Attrs}) ->
    %% [80] EncodingDecl
    ?bump_col(8),
    {T1, S1} = scan_eq(T, S),
    {EncName, T2, S2} = scan_enc_name(T1, S1),
    Attr = #xmlAttribute{name = encoding,
			 parents = [{xml, XMLPos = 1}],
			 value = EncName},
    Decl = Decl0#xmlDecl{encoding = EncName,
			 attributes = [Attr|Attrs]},
    S3 = #xmerl_scanner{} = Event(#xmerl_event{event = ended, 
					       line = S0#xmerl_scanner.line, 
					       col = S0#xmerl_scanner.col,
					       data = Attr}, S2),
    scan_xml_decl(T2, S3, Decl).

%%%%%%% [81] EncName


scan_enc_name([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_enc_name(MoreBytes, S1)
      end, fun(S1) -> ?fatal(expected_encoding_name, S1) end, S);
scan_enc_name([H|T], S0) when H >= $"; H =< $' -> 
    ?bump_col(1),
    scan_enc_name(T, S, H, []).


scan_enc_name([], S = #xmerl_scanner{continuation_fun = F}, Delim, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_enc_name(MoreBytes, S1, Delim, Acc)
      end, fun(S1) -> ?fatal(expected_encoding_name, S1) end, S);
scan_enc_name([H|T], S0, H, Acc) ->
    ?bump_col(1),
    ?strip1,
    {lists:reverse(Acc), T1, S1};
scan_enc_name([H|T], S0, Delim, Acc) when H >= $a, H =< $z ->
    ?bump_col(1),
    scan_enc_name(T, S, Delim, [H|Acc]);
scan_enc_name([H|T], S0, Delim, Acc) when H >= $A, H =< $Z ->
    ?bump_col(1),
    scan_enc_name(T, S, Delim, [H|Acc]);
scan_enc_name([H|T], S0, Delim, Acc) when H >= $0, H =< $9 ->
    ?bump_col(1),
    scan_enc_name(T, S, Delim, [H|Acc]);
scan_enc_name([H|T], S0, Delim, Acc) when H == $.; H == $_; H == $- ->
    ?bump_col(1),
    scan_enc_name(T, S, Delim, [H|Acc]).



scan_xml_vsn([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_xml_vsn(MoreBytes, S1)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_xml_vsn([H|T], S) when H==$"; H==$'->
    xml_vsn(T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}, H, []).

xml_vsn([], S = #xmerl_scanner{continuation_fun = F}, Delim, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      xml_vsn(MoreBytes, S1, Delim, Acc)
      end, 
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
xml_vsn([H|T], S = #xmerl_scanner{col = C}, H, Acc) ->
    {lists:reverse(Acc), T, S#xmerl_scanner{col = C+1}};
xml_vsn([H|T], S = #xmerl_scanner{col = C}, 
	Delim, Acc) when H >= $a, H =< $z ->
    xml_vsn(T, S#xmerl_scanner{col = C+1}, Delim, [H|Acc]);
xml_vsn([H|T], S = #xmerl_scanner{col = C}, 
	Delim, Acc) when H >= $A, H =< $Z ->
    xml_vsn(T, S#xmerl_scanner{col = C+1}, Delim, [H|Acc]);
xml_vsn([H|T], S = #xmerl_scanner{col = C}, 
	Delim, Acc) when H >= $0, H =< $9 ->
    xml_vsn(T, S#xmerl_scanner{col = C+1}, Delim, [H|Acc]);
xml_vsn([H|T], S = #xmerl_scanner{col = C}, Delim, Acc) ->
    case lists:member(H, "_.:-") of
	true ->
	    xml_vsn(T, S#xmerl_scanner{col = C+1}, Delim, [H|Acc]);
	false ->
	    ?fatal({invalid_vsn_char, H}, S)
    end.


%%%%%%% [16] PI

scan_pi([], S = #xmerl_scanner{continuation_fun = F}, Pos) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
		 scan_pi(MoreBytes, S1, Pos)
	 end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_pi(Str = [H1,H2,H3 | T], 
	S = #xmerl_scanner{line = L, col = C}, Pos) when H1 == $x; H1 == $X ->
    %% names beginning with [xX][mM][lL] are reserved for future use.
    if H2 == $m; H2 == $M ->
	    if H3 == $l; H3 == $L ->
		    ?fatal({invalid_target_name, lists:sublist(Str, 1, 6)}, S);
	       true ->
		    ok
	    end;
       true ->
	    ok
    end,
    {Target, NamespaceInfo, T1, S1} = scan_name_no_colons(Str, S),
    scan_pi(T1, S1, Target, L, C, Pos, []);
scan_pi(Str, S = #xmerl_scanner{line = L, col = C}, Pos) ->
    {Target, NamespaceInfo, T1, S1} = scan_name(Str, S),
    scan_pi(T1, S1, Target, L, C, Pos, []).


scan_pi([], S = #xmerl_scanner{continuation_fun = F}, Target, 
	L, C, Pos, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_pi(MoreBytes, S1, Target, L, C, Pos, Acc)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_pi("?>" ++ T, S0 = #xmerl_scanner{hook_fun = Hook,
				       event_fun = Event}, 
	Target, L, C, Pos, Acc) ->
    ?bump_col(2),
    PI = #xmlPI{name = Target,
		pos = Pos,
		value = lists:reverse(Acc)},
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = L,
					       col = C,
					       data = PI}, S),
    {Ret, S2} = Hook(PI, S1),
    {Ret, T, S2};
scan_pi([H|T], S0, Target, L, C, Pos, Acc) ->
    ?bump_col(1),
    scan_pi(T, S, Target, L, C, Pos, [H|Acc]).



scan_doctype([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_doctype(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_doctype(T, S) ->
    ?strip1,
    {DTName, NamespaceInfo, T2, S2} = scan_name(T1, S1),
    ?strip3,
    scan_doctype1(T3, S3#xmerl_scanner{doctype_name =  DTName}).


scan_doctype1([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_doctype1(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_doctype1("PUBLIC" ++ T, S0) ->
    ?bump_col(6),
    ?strip1,
    {PIDL, T2, S2} = scan_pubid_literal(T1, S1),
    ?strip3,
    {SL, T4, S4} = scan_system_literal(T3, S3),
    ?strip5,
    S6 = fetch_DTD({public, PIDL, SL}, S5),
    scan_doctype2(T5, S6);
scan_doctype1("SYSTEM" ++ T, S0) ->
    ?bump_col(6),
    ?strip1,
    {SL, T2, S2} = scan_system_literal(T1, S1),
    ?strip3,
    S4 = fetch_DTD({system, SL}, S3),
    scan_doctype2(T3, S4);
scan_doctype1(T, S) ->
    scan_doctype2(T, S).


scan_doctype2([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_doctype2(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_doctype2("[" ++ T, S0) ->
    ?bump_col(1),
    ?strip1,
    scan_doctype3(T1, S1);
scan_doctype2(">" ++ T, S0) ->
    ?bump_col(1),
    ?strip1,
    {T1, S1}.

scan_doctype3([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_doctype3(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_doctype3("%" ++ T, S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1),
    scan_doctype3(" "++ExpRef+" "++T1, S1); % Add spaces, see Section 4.4.8
scan_doctype3("]" ++ T, S0) ->
    ?bump_col(1),
    ?strip1,
    ">" ++ T2 = T1,
    {T2, S1};
scan_doctype3(T, S) ->
    {_, T1, S1} = scan_markup_decl(T, S),
    scan_doctype3(T1, S1).


fetch_DTD(DTDSpec, S = #xmerl_scanner{fetch_fun = Fetch,
				      rules = Rules}) ->
    case Fetch(DTDSpec, S) of
	{ok, NewS} ->
	    NewS;
	{ok, DataRet, NewS = #xmerl_scanner{user_state = UState,
					    event_fun = Event,
					    hook_fun = Hook,
					    fetch_fun = Fetch1,
					    close_fun = Close1,
					    continuation_fun = Cont,
					    acc_fun = Acc}} ->
	    EvS = event_state(NewS),
	    HoS = hook_state(NewS),
	    FeS = fetch_state(NewS),
	    CoS = cont_state(NewS),
	    Options = [{user_state, UState},
		       {rules, Rules},
		       {event_fun, Event, EvS},
		       {hook_fun, Hook, HoS},
		       {fetch_fun, Fetch1, FeS},
		       {close_fun, Close1},
		       {continuation_fun, Cont, CoS},
		       {acc_fun, Acc}],
	    case DataRet of
		{file, F} ->
		    {Res, Tail, Sx} =
			int_file(F, Options),
		    NewSx = #xmerl_scanner{} = Close1(Sx);
		{string, String} ->
		    {Res, Tail, Sx} = 
			int_string(String, Options),
		    ?dbg("DTD string -> US = ~p~n", [String]),
		    NewSx = #xmerl_scanner{} = Close1(Sx)
	    end;
	Error ->
	    ?fatal({error_fetching_DTD, {DTDSpec, Error}}, S)
    end.


%%%%%%% [30] extSubSet

scan_ext_subset([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_ext_subset(MoreBytes, S1)
      end, fun(S1) -> {[], [], S1} end, S);
scan_ext_subset("%" ++ T, S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S0),
    S2 = expand_external_pe_reference(PERefName, S1),
    {_, T2, S3} = strip(T1,S2),
    scan_ext_subset(T2, S3);
scan_ext_subset("<![" ++ T, S0) ->
    ?bump_col(3),
    ?strip1,
    {_, T2, S2} = scan_conditional_sect(T1, S1);
scan_ext_subset(T, S) ->
    {_, T1, S1} = scan_markup_decl(T, S),
    scan_ext_subset(T1, S1).


%%%%%%% [61] ConditionalSect

scan_conditional_sect([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_conditional_sect(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_conditional_sect("IGNORE" ++ T, S0) ->
    ?bump_col(6),
    scan_ignore(T, S);
scan_conditional_sect("INCLUDE" ++ T, S0) ->
    ?bump_col(7),
    ?strip1,
    scan_include(T1, S1).


%%%%%%% [63] IgnoreSect

scan_ignore([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_ignore(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_ignore("<![" ++ T, S0) ->
    %% nested conditional section. Topmost condition is ignore, though
    ?bump_col(3),
    {_, T1, S1} = scan_ignore(T, S),
    scan_ignore(T1, S1);
scan_ignore("]]>" ++ T, S0) ->
    ?bump_col(3),
    {[], T, S}.


%%%%%%% [62] IncludeSect

scan_include([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_include(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_include("]]>" ++ T, S0) ->
    ?bump_col(3),
    {[], T, S};
scan_include("%" ++ T, S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1),
    scan_include(ExpRef ++ T1, S1);
scan_include("<![" ++ T, S0) ->
    ?bump_col(3),
    ?strip1,
    {_, T2, S2} = scan_conditional_sect(T1, S1);
scan_include(T, S) ->
    {_, T1, S1} = scan_markup_decl(T, S),
    scan_include(T1, S1).


%%%%%%% [29] markupdecl
%%%%%%% [45] elementdecl

%% Validity constraint: Unique Type Declaration: No element type may be
%% declared more than once.
%%
scan_markup_decl([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_markup_decl(MoreBytes, S1)
      end, fun(S1) -> {[], [], S1} end, S);
scan_markup_decl(T = "<!--" ++ _, S) ->
    {_, T1, S1} = scan_comment(T, S),
    ?strip2;
scan_markup_decl("<!ELEMENT" ++ T, 
		 #xmerl_scanner{rules_read_fun = Read,
				rules_write_fun = Write} = S0) ->
    ?bump_col(9),
    ?strip1,
    {Ename, NamespaceInfo, T2, S2} = scan_name(T1, S1),
    case Read(elem_def, Ename, S2) of
	PrevDef when PrevDef /= undefined ->
	    ?fatal({already_defined, Ename}, S2);
	_ ->
	    ok
    end,
    ?strip3,
    {Edef, T4, S4} = scan_contentspec(T3, S3),
    ?strip5,
    ">" ++ T6 = T5,
    S6 = Write(elem_def, Ename, #xmlElement{name = Ename,
					    content = Edef}, S5),
    ?strip7;
scan_markup_decl("<!ENTITY" ++ T, S0) ->
    %% <!ENTITY [%] entity.name NDATA notation.name>
    %% <!ENTITY [%] entity.name "replacement text">
    %% <!ENTITY [%] entity.name SYSTEM "system.identifier">
    %% <!ENTITY [%] entity.name PUBLIC public.identifier "system.identifier">
    ?bump_col(8),
    ?strip1,
    {T2, S2} = scan_entity(T1, S1),
    ?strip3;
scan_markup_decl("<!NOTATION" ++ T, S0) ->
    %% <!NOTATION notation.name "public.identifier" "helper.application">
    ?bump_col(10),
    ?strip1,
    {T2, S2} = scan_notation_decl(T1, S1),
    strip3;
scan_markup_decl("<!ATTLIST" ++ T, 
		 #xmerl_scanner{rules_read_fun = Read,
				rules_write_fun = Write} = S0) ->
    %% <!ATTLIST Ename ( AttrName Type Value )*>
    ?bump_col(9),
    ?strip1,
    {Ename, NamespaceInfo, T2, S2} = scan_name(T1, S1),
    case Read(elem_def, Ename, S2) of
	undefined ->
	    ?fatal({unknown_element, Ename}, S2);
	Edef = #xmlElement{attributes = OldAttrs} ->
	    ?strip3,
	    {Attributes, T4, S4} = scan_attdef(T3, S3),
	    NewAttrs = update_attributes(Attributes, OldAttrs),
	    NewEdef = Edef#xmlElement{attributes = NewAttrs},
	    S5 = Write(elem_def, Ename, NewEdef, S4),
	    T5 = T4,
	    ?strip6
    end.

update_attributes(NewAttrs, OldAttrs) ->
    update_attributes(NewAttrs, OldAttrs, lists:reverse(OldAttrs)).

update_attributes([A = {Name, Type, Default}|Attrs], OldAttrs, Acc) ->
    case lists:keymember(Name, 1, OldAttrs) of
	true ->
	    update_attributes(Attrs, lists:keydelete(Name, 1, OldAttrs),
			      Acc);
	false ->
	    update_attributes(Attrs, lists:keydelete(Name, 1, OldAttrs), 
			      [A|Acc])
    end;
update_attributes([], _, Acc) ->
    lists:reverse(Acc).


%%%%%%% [53] AttDef

scan_attdef([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_attdef(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_attdef(T, S) ->
    scan_attdef(T, S, AttrAcc = []).


scan_attdef([], S = #xmerl_scanner{continuation_fun = F}, Attrs) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_attdef(MoreBytes, S1, Attrs)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_attdef(">" ++ T, S0, Attrs) ->
    ?bump_col(1),
    {lists:reverse(Attrs), T, S};
scan_attdef("%" ++ T, S0, Attrs) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S0),
    ExpRef = expand_pe_reference(PERefName, S1),
    scan_attdef(ExpRef ++ T1, S1, Attrs);
scan_attdef(T, S, Attrs) ->
    {AttName, NamespaceInfo, T1, S1} = scan_name(T, S),
    ?strip2,
    {AttType, T3, S3} = scan_att_type(T2, S2),
    ?strip4,
    {DefaultDecl, T5, S5} = scan_default_decl(T4, S4, AttType),
    ?strip6,
    scan_attdef(T6, S6, [{AttName, AttType, DefaultDecl}|Attrs]).


%%% [54] StringType
scan_att_type([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_att_type(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_att_type("CDATA" ++ T, S0) ->
    ?bump_col(5),
    {'CDATA', T, S};
%%% [55] TokenizedType
scan_att_type("ID" ++ T, S0) ->
    ?bump_col(2),
    {'ID', T, S};
scan_att_type("IDREF" ++ T, S0) ->
    ?bump_col(5),
    {'IDREF', T, S};
scan_att_type("IDREFS" ++ T, S0) ->
    ?bump_col(6),
    {'IDREFS', T, S};
scan_att_type("ENTITY" ++ T, S0) ->
    ?bump_col(6),
    {'ENTITY', T, S};
scan_att_type("ENTITIES" ++ T, S0) ->
    ?bump_col(8),
    {'ENTITIES', T, S};
scan_att_type("NMTOKEN" ++ T, S0) ->
    ?bump_col(7),
    {'NMTOKEN', T, S};
scan_att_type("NMTOKENS" ++ T, S0) ->
    ?bump_col(8),
    {'NMTOKENS', T, S};
%%% [57] EnumeratedType
scan_att_type("NOTATION" ++ T, S0) ->
    ?bump_col(8),
    ?strip1,
    "(" ++ T2 = T1,
    S2 = S1,
    ?strip3,
    {Name, NamespaceInfo, T4, S4} = scan_name(T3, S3),
    notation_exists(Name, S4),
    scan_notation_type(T2, S2, [Name]);
scan_att_type("(" ++ T, S0) ->
    ?bump_col(1),
    ?strip1,
    {NmToken, NamespaceInfo, T2, S2} = scan_nmtoken(T1, S1),
    ?strip3,
    scan_enumeration(T3, S3, [NmToken]);
scan_att_type("%" ++ T, S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S0),
    ExpRef = expand_pe_reference(PERefName, S1),
    {ExpRef,T1,S1}.

%%% [58] NotationType

scan_notation_type([], S = #xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_notation_type(MoreBytes, S1, Acc)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_notation_type(")" ++ T, S0, Acc) ->
    ?bump_col(1),
    {{notation, lists:reverse(Acc)}, T, S};
scan_notation_type("|" ++ T, S0, Acc) ->
    ?bump_col(1),
    ?strip1,
    {Name, NamespaceInfo, T2, S2} = scan_name(T1, S1),
    notation_exists(Name, S2),
    scan_notation_type(T2, S2, [Name | Acc]).

%%% Validity constraint for NotationType: 
%%% The notation names must have been declared.
notation_exists(Name, #xmerl_scanner{rules_read_fun = Read} = S) ->
    case Read(notation, Name, S) of
	undefined ->
	    ?fatal({unknown_notation, Name}, S);
	_Value ->
	    ok
    end.

%%% [59] Enumeration

scan_enumeration([], S = #xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_enumeration(MoreBytes, S1, Acc)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_enumeration(")" ++ T, S0, Acc) ->
    ?bump_col(1),
    {{enumeration, lists:reverse(Acc)}, T, S};
scan_enumeration("|" ++ T, S0, Acc) ->
    ?bump_col(1),
    ?strip1,
    {NmToken, NamespaceInfo, T2, S2} = scan_nmtoken(T1, S1),
    ?strip3,
    scan_enumeration(T3, S3, [NmToken|Acc]).


%%%%%%% [60] DefaultDecl

scan_default_decl([], S = #xmerl_scanner{continuation_fun = F}, Type) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_default_decl(MoreBytes, S1, Type)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_default_decl("#REQUIRED" ++ T, S0, Type) ->
    ?bump_col(9),
    {'#REQUIRED', T, S};
scan_default_decl("#IMPLIED" ++ T, S0, Type) ->
    ?bump_col(8),
    {'#IMPLIED', T, S};
scan_default_decl("#FIXED" ++ T, S0, Type) ->
    ?bump_col(6),
    ?strip1,
    default_value(T1, S1, Type);
scan_default_decl(Str, S, Type) ->
    default_value(Str, S, Type).


%% There is room here to validate against Type, but we don't do it at
%% the moment.
default_value(T, S, Type) ->
    {Val, T1, S1} = scan_att_value(T, S).


%%%%%%% [71] EntityDef

scan_entity([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_entity(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_entity("%" ++ T, #xmerl_scanner{rules_write_fun = Write} = S0) ->
    %% parameter entity
    ?bump_col(1),
    ?strip1,
    {PEName, NamespaceInfo, T2, S2} = scan_name_no_colons(T1, S1),
    ?strip3,
    {PEDef, T4, S4} = scan_pe_def(T3, S3, PEName),
    ?strip5,
    ">" ++ T6 = T5,
    S6 = Write(parameter_entity, PEName, PEDef, S5),
    {T6, S6};
scan_entity(T, #xmerl_scanner{rules_write_fun = Write} = S) ->
    %% generic entity
    {EName, NamespaceInfo, T1, S1} = scan_name_no_colons(T, S),
    ?strip2,
    {EDef, T3, S3} = scan_entity_def(T2, S2, EName),
    ?strip4,
    ">" ++ T5 = T4,
    S5 = Write(entity, EName, EDef, S4),
    {T5, S5}.


%%%%%%% [73] EntityDef

scan_entity_def([], S = #xmerl_scanner{continuation_fun = F}, EName) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_entity_def(MoreBytes, S1, EName)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_entity_def("'" ++ T, S0, EName) ->
    ?bump_col(1),
    scan_entity_value(T, S, $', EName);
scan_entity_def("\"" ++ T, S0, EName) ->
    ?bump_col(1),
    scan_entity_value(T, S, $", EName);
scan_entity_def(Str, S, EName) ->
    {ExtID, T1, S1} = scan_external_id(Str, S),
    ?strip2,
    {NData, T3, S3} = scan_ndata_decl(T2, S2),
    {{ExtID, NData}, T3, S3}.


scan_ndata_decl([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_ndata_decl(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_ndata_decl(Str = ">", S) ->
    {[], Str, S};
scan_ndata_decl("NDATA" ++ T, S0 = #xmerl_scanner{rules_read_fun = Read}) ->
    ?bump_col(5),
    ?strip1,
    {Name, NamespaceInfo, T2, S2} = scan_name(T1, S1),
    case Read(notation, Name, S2) of
	undefined ->
	    ?fatal({unknown_notation, Name}, S2);
	_Value ->
	    {{ndata, Name}, T2, S2}
    end.

%%%%%%% [39] element

scan_element(T, S, Pos) ->
    scan_element(T, S, Pos, S#xmerl_scanner.space,
		 Lang = [], Parents = [], #xmlNamespace{}).

scan_element(T, S = #xmerl_scanner{line = L, col = C}, Pos, SpaceDefault, 
	     Lang, Parents, NS) ->
    {Name, NamespaceInfo, T1, S1} = scan_name(T, S),
    ?strip2,
    scan_element(T2, S2, Pos, Name, L, C, Attrs = [], 
		 Lang, Parents, NamespaceInfo, NS, 
		 SpaceDefault).


scan_element("/", S = #xmerl_scanner{continuation_fun = F},
	     Pos, Name, StartL, StartC, Attrs, Lang, Parents, 
	     NSI, NS, SpaceDefault) ->
    ?dbg("trailing / detected~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_element("/" ++ MoreBytes, S1, 
			   Pos, Name, StartL, StartC, Attrs, 
			   Lang, Parents, NSI, NS, SpaceDefault)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_element([], S = #xmerl_scanner{continuation_fun = F}, 
	     Pos, Name, StartL, StartC, Attrs, Lang, Parents, 
	     NSI, NS, SpaceDefault) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_element(MoreBytes, S1, 
			   Pos, Name, StartL, StartC, Attrs, 
			   Lang, Parents, NSI, NS, SpaceDefault)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_element("/>" ++ T, S0 = #xmerl_scanner{hook_fun = Hook,
					    line = L, col = C,
					    event_fun = Event}, Pos,
	     Name, StartL, StartC, Attrs0, Lang, Parents, NSI, 
	     Namespace, SpaceDefault) ->
    ?bump_col(2),
    Attrs = lists:reverse(Attrs0),
    E=processed_whole_element(S, Pos, Name, Attrs, Lang, Parents,NSI,Namespace),
    
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = L,
					       col = C,
					       data = E}, S0),
    {Ret, S2} = Hook(E, S1),
    {Ret, T, S2};
scan_element(">" ++ T, S0 = #xmerl_scanner{event_fun = Event,
					   line = L, col = C,
					   hook_fun = Hook,
					   space = SpaceOption},
	     Pos, Name, StartL, StartC, Attrs0, Lang, Parents, 
	     NSI, Namespace, SpaceDefault) ->
    ?bump_col(1),
    Attrs = lists:reverse(Attrs0),
    XMLSpace = case lists:keysearch('xml:space', 1, Attrs) of
		   false ->			SpaceDefault;
		   {value, "default"} ->	SpaceOption;
		   {value, "preserve"} ->	preserve;
		   _ ->				SpaceDefault
	       end,
    
    E0=processed_whole_element(S, Pos, Name, Attrs,Lang,Parents,NSI,Namespace),
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = started,
					       line = StartL,
					       col = StartC,
					       data = E0}, S),
    {Content, T1, S2} = scan_content(T, S1, Name, Attrs, XMLSpace, 
				     Lang, [{Name, Pos}|Parents], Namespace),
    Element = E0#xmlElement{content = Content},
    S3 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = L,
					       col = C,
					       data = Element}, S2),
    {Ret, S4} = Hook(Element, S3),
    {Ret, T1, S4};
scan_element(T, S, Pos, Name, StartL, StartC, Attrs, Lang, Parents, 
	     NSI, NS, SpaceDefault) ->
    {AttName, NamespaceInfo, T1, S1} = scan_name(T, S),
    {T2, S2} = scan_eq(T1, S1),
    {AttValue, T3, S3} = scan_att_value(T2, S2),
    NewNS = check_namespace(AttName, NamespaceInfo, AttValue, NS),
    ?strip4,
    AttrPos = case Attrs of
		  [] ->
		      1;
		  [#xmlAttribute{pos = P}|_] ->
		      P+1
	      end,
    scan_element(T4, S4, Pos, Name, StartL, StartC, 
		 [#xmlAttribute{name = AttName, 
				pos = AttrPos,
				language = Lang,
				namespace = NamespaceInfo,
				value = AttValue}|Attrs], 
		 Lang, Parents, NSI, NewNS, SpaceDefault).


processed_whole_element(S = #xmerl_scanner{hook_fun = Hook,
					   line = L, col = C,
					   event_fun = Event}, 
			Pos, Name, Attrs, Lang, Parents, NSI, Namespace) ->
    Language = check_language(Attrs, Lang),

    {ExpName, ExpAttrs} = 
	case S#xmerl_scanner.namespace_conformant of
	    true ->
		%% expand attribute names. We need to do this after having
		%% scanned all attributes of the element, since (as far as
		%% I can tell), XML Names only specifies that namespace attrs
		%% are valid within the whole scope of the element in which
		%% they are declared, which should also mean that even if they
		%% are declared after some other attributes, the namespace
		%% should apply to those attributes as well.
		%% Note that the default URI does not apply to attrbute names.
		TempNamespace = Namespace#xmlNamespace{default = []},
		ExpAttrsX = 
		    [A#xmlAttribute{expanded_name = expanded_name(
						      A#xmlAttribute.name, 
						      NSI,
						      TempNamespace, S)} ||
			A <- Attrs],
		{expanded_name(Name, NSI, Namespace, S), ExpAttrsX};
	    false ->
		{Name, Attrs}
	end,

    #xmlElement{name = Name,
		pos = Pos,
		parents = Parents,
		attributes = ExpAttrs,
		language = Language,
		expanded_name = ExpName,
		nsinfo = NSI,
		namespace = Namespace}.


check_language([{'xml:lang', Lang}|_], _) ->
    Lang;
check_language([H|T], Lang) ->
    check_language(T, Lang);
check_language([], Lang) ->
    Lang.


check_namespace(xmlns, _, Value, NS) ->
    NS#xmlNamespace{default = list_to_atom(Value)};
check_namespace(_, {"xmlns", Prefix}, Value, 
		NS = #xmlNamespace{nodes = Ns}) ->
    NS#xmlNamespace{nodes = keyreplaceadd(
			      Prefix, 1, Ns, {Prefix, list_to_atom(Value)})};
check_namespace(_, _, _, NS) ->
    NS.


expanded_name(Name, [], #xmlNamespace{default = []}, S) ->
    Name;
expanded_name(Name, [], #xmlNamespace{default = URI}, S) ->
    {URI, Name};
expanded_name(Name, {Prefix, Local}, #xmlNamespace{nodes = Ns}, S) ->
    case lists:keysearch(Prefix, 1, Ns) of
	{value, {_, URI}} ->
	    {URI, list_to_atom(Local)};
	false ->
	    %% A namespace constraint of XML Names is that the prefix
	    %% must be declared
	    ?fatal({namespace_prefix_not_declared, Prefix}, S)
    end.
		    



keyreplaceadd(K, Pos, [H|T], Obj) when K == element(Pos, H) ->
    [Obj|T];
keyreplaceadd(K, Pos, [H|T], Obj) ->
    [H|keyreplaceadd(K, Pos, T, Obj)];
keyreplaceadd(K, Pos, [], Obj) ->
    [Obj].

%%%%%%% [10] AttValue

scan_att_value([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_att_value(MoreBytes, S1)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_att_value([H|T], S0) when H == $"; H == $' ->
    ?bump_col(1),
    {Str, T1, S1} = scan_att_chars(T, S, H, []).

scan_att_chars([], S = #xmerl_scanner{continuation_fun = F}, H, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_att_chars(MoreBytes, S1, H, Acc)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_att_chars([H|T], S0, H, Acc) ->
    ?bump_col(1),
    {lists:reverse(Acc), T, S};
scan_att_chars("&" ++ T, S0, Delim, Acc) ->
    ?bump_col(1),
    {ExpRef, T1, S1} = scan_reference(T, S),
    scan_att_chars(ExpRef ++ T1, S1, Delim, Acc);
scan_att_chars([H|T], S0, Delim, Acc) ->
    ?bump_col(1),
    scan_att_chars(T, S, Delim, [H|Acc]).

%%%%%%% [43] content

scan_content(T, S, Name, Attrs, Space, Lang, Parents, NS) ->
    scan_content(T, S, Pos = 1, Name, Attrs, Space, 
		 Lang, Parents, NS, Acc = []).

scan_content("<", S= #xmerl_scanner{continuation_fun = F},
            Pos, Name, Attrs, Space, Lang, Parents, NS, Acc) ->
    ?dbg("trailing < detected~n", []),
    F(fun(MoreBytes, S1) ->
             scan_content("<" ++ MoreBytes, S1, 
                          Pos, Name, Attrs, 
                          Space, Lang, Parents, NS, Acc)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_content([], S = #xmerl_scanner{continuation_fun = F}, 
	     Pos, Name, Attrs, Space, Lang, Parents, NS, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_content(MoreBytes, S1, 
			   Pos, Name, Attrs, 
			   Space, Lang, Parents, NS, Acc)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_content("</" ++ T, S0, Pos, Name, Attrs, Space, Lang, Parents, NS, Acc) ->
    ?bump_col(2),
    {ETagName, NamespaceInfo, T1, S1} = scan_name(T, S),
    if ETagName == Name ->
	    ok;
       true ->
	    ?fatal({endtag_does_not_match, {ETagName, Name}}, S1)
    end,
    ?strip2,
    case T2 of
	">" ++ T3 ->
	    {lists:reverse(Acc), T3, S2};
	_ -> % Mal-formed XML! Behave friendly and just skip to end of end tag.
	    T3=tl(lists:dropwhile(fun(X) ->
					  if X==$> -> false;
					     true ->true
					  end
				  end,T2)),
	    {lists:reverse(Acc), T3, S2}
    end;
scan_content("&" ++ T, S0, Pos, Name, Attrs, Space, Lang, Parents, NS, Acc) ->
    ?bump_col(1),
    {ExpRef, T1, S1} = scan_reference(T, S),
    scan_content(ExpRef ++ T1, S1, Pos, Name, Attrs, 
		 Space, Lang, Parents, NS, Acc);
scan_content(T = "<!--" ++ _, S, Pos, Name, Attrs, Space, 
	     Lang, Parents, NS, Acc) ->
    {_, T1, S1} = scan_comment(T, S, Pos, Parents, Lang),
    scan_content(T1, S1, Pos+1, Name, Attrs, Space, Lang, Parents, NS, Acc);
scan_content("<" ++ T, S0, Pos, Name, Attrs, Space, Lang, Parents, NS, Acc) ->
    ?bump_col(1),
    {Markup, T1, S1} = 
	scan_content_markup(T, S, Pos, Name, Attrs, Space, Lang, Parents, NS),
    AccF = S1#xmerl_scanner.acc_fun,
    {NewAcc, S2} = AccF(Markup, Acc, S1),
    scan_content(T1, S2, Pos+1, Name, Attrs, Space, Lang, Parents, NS, NewAcc);
scan_content(T, S = #xmerl_scanner{acc_fun = F, 
				   event_fun = Event, 
				   line = L}, 
	     Pos, Name, Attrs, Space, Lang, Parents, NS, Acc) ->
    Text0 = #xmlText{pos = Pos,
		     parents = Parents},
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = started,
					       line = S#xmerl_scanner.line,
					       data = Text0}, S),
    {Data, T1, S2} =  scan_char_data(T, S1, Space),
    Text = Text0#xmlText{value = Data},
    S3 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S2#xmerl_scanner.line,
					       data = Text}, S2),
    {NewAcc, S4} = F(Text, Acc, S3),
    scan_content(T1, S4, Pos+1, Name, Attrs, Space, Lang, Parents, NS, NewAcc).


scan_content_markup([], S = #xmerl_scanner{continuation_fun = F}, 
		    Pos, Name, Attrs, Space, Lang, Parents, NS) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_content_markup(
		MoreBytes, S1, Pos, Name, Attrs, Space, Lang, Parents, NS)
      end, fun(S1) -> ?fatal(unexpected_end, S1) end, S);
scan_content_markup("[CDATA[" ++ T, S0, Pos, Name, Attrs, 
		    Space, Lang, Parents, NS) ->
    ?bump_col(7),
    scan_cdata(T, S, Pos, Parents);
scan_content_markup("?" ++ T, S0, Pos, Name, Attrs, 
		    Space, Lang, Parents, NS) ->
    ?bump_col(1),
    scan_pi(T, S, Pos);
scan_content_markup(T, S, Pos, Name, Attrs, 
		    Space, Lang, Parents, NS) ->
    scan_element(T, S, Pos, Space, Lang, Parents, NS).

scan_char_data(T, S, Space) ->
    scan_char_data(T, S, Space, Acc = []).

%%%%%%% [14] CharData

scan_char_data([], S = #xmerl_scanner{continuation_fun = F}, Space, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_char_data(MoreBytes, S1, Space, Acc)
      end, 
      fun(S1) -> 
	      ?fatal(unexpected_end, S1) 
      end, S);
scan_char_data(T = [$&|_], S, Space, Acc) ->
    {lists:reverse(Acc), T, S};
scan_char_data(T = [$<|_], S, Space, Acc) ->
    {lists:reverse(Acc), T, S};
scan_char_data(T = [H|_], S, Space, Acc) when ?whitespace(H) ->
    {NewAcc, T1, S1} = accumulate_whitespace(T, S, Space, Acc),
    scan_char_data(T1, S1, Space, NewAcc);
scan_char_data([H|T], S0, Space, Acc) ->
    ?bump_col(1),
    scan_char_data(T, S, Space, [H|Acc]).



%%%%%%% [18]-[21] CDATA

scan_cdata(Str, S, Pos, Parents) ->
    scan_cdata(Str, S, Pos, Parents, Acc = []).


scan_cdata([], S = #xmerl_scanner{continuation_fun = F}, Pos, Parents, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_cdata(MoreBytes, S1, Pos, Parents, Acc)
      end,
      fun(S1) -> 
	      ?fatal(unexpected_end, S1)
      end, S);
scan_cdata("]]>" ++ T, S0, Pos, Parents, Acc) ->
    ?bump_col(3),
    {#xmlText{pos = Pos,
	      parents = Parents,
	      value = lists:reverse(Acc)}, T, S};
scan_cdata([H|T], S0, Pos, Parents, Acc) ->
    ?bump_col(1),
    scan_cdata(T, S, Pos, Parents, [H|Acc]).


%%%%%%% [67] Reference


scan_reference([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_reference(MoreBytes, S1)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_reference("#x" ++ T, S0) ->
    %% [66] CharRef
    ?bump_col(1),
    if hd(T) /= $; ->
	    scan_char_ref_hex(T, S, 0);
       true ->
	    ?fatal(invalid_char_ref, S)
    end;
scan_reference("#" ++ T, S0) ->
    %% [66] CharRef
    ?bump_col(1),
    if hd(T) /= $; ->
	    scan_char_ref_dec(T, S, []);
       true ->
	    ?fatal(invalid_char_ref, S)
    end;
scan_reference(T, S) ->
    scan_entity_ref(T, S).


%% Chapter 4.4.2: ... the replacement text of entities used to escape
%% markup delimiters (the entities amp, lt, gt, apos, quot) is always treated
%% as data. (The string "AT&amp;T;" expands to "AT&T;" and the remaining
%% ampersand is not recognized as an entity-reference delimiter.)"
%%
%% How to achieve this? My current approach is to insert the *strings* "&",
%% "<", ">", "'", and "\"" instead of the characters. The processor will 
%% ignore them when performing multiple expansions. This means, for now, that
%% the character data output by the processor is (1-2 levels) deep.
%% At some suitable point, we should flatten these, so that application-level
%% processors should not have to be aware of this detail.

scan_entity_ref([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_entity_ref(MoreBytes, S1)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_entity_ref("amp;" ++ T, S0) ->
    ?bump_col(4),
    {["&"], T, S};
scan_entity_ref("lt;" ++ T, S0) ->
    ?bump_col(3),
    {["<"], T, S};
scan_entity_ref("gt;" ++ T, S0) ->
    ?bump_col(3),
    {[">"], T, S};
scan_entity_ref("apos;" ++ T, S0) ->
    ?bump_col(5),
    {["'"], T, S};
scan_entity_ref("quot;" ++ T, S0) ->
    ?bump_col(5),
    {["\""], T, S};
scan_entity_ref(T, S) ->
    {Name, NamespaceInfo, T1, S1} = scan_name(T, S),
    ";" ++ T2 = T1, 
    S2 = S1,
    Expansion = expand_reference(Name, S2),
    {Expansion, T2, S2}.


%%%%%%% [69] PEReference

scan_pe_reference(T, S) ->
    {Name, NamespaceInfo, T1, S1} = scan_name(T, S),
    ";" ++ T2 = T1, 
    {Name, T2, S1#xmerl_scanner{col = S1#xmerl_scanner.col+1}}.

expand_pe_reference(Name, #xmerl_scanner{rules_read_fun = Read} = S) ->
    case Read(parameter_entity, Name, S) of
	undefined ->
	    ?fatal({unknown_parameter_entity, Name}, S);
	Result ->
	    Result
    end.

expand_external_pe_reference(Name, #xmerl_scanner{rules_read_fun = Read} = S) ->
    case Read(parameter_entity, Name, S) of
	undefined ->
	    ?fatal({unknown_parameter_entity, Name}, S);
	Result ->
	    fetch_DTD(Result,S)
    end.
    

%%%%%%% [68] EntityReference

expand_reference(Name, #xmerl_scanner{rules_read_fun = Read} = S) ->
    case Read(entity_ref, Name, S) of
	undefined ->
	    ?fatal({unknown_entity_ref, Name}, S);
	Value ->
	    Value
    end.


%%%%%%% [66] CharRef

scan_char_ref_dec([], S = #xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_char_ref_dec(MoreBytes, S1, Acc)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_char_ref_dec([H|T], S0, Acc) when H >= $0, H =< $9 ->
    ?bump_col(1),
    scan_char_ref_dec(T, S, [H|Acc]);
scan_char_ref_dec(";" ++ T, S0, Acc) ->
    ?bump_col(1),
    Ref = list_to_integer(lists:reverse(Acc)),
    {[[Ref]], T, S}.


scan_char_ref_hex([], S = #xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_char_ref_hex(MoreBytes, S1, Acc)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_char_ref_hex([H|T], S0, Acc) when H >= $0, H =< $9 ->
    ?bump_col(1),
    Dec = H - $0,
    scan_char_ref_hex(T, S, (Dec bor (Acc bsl 4)));
scan_char_ref_hex([H|T], S0, Acc) when H >= $a, H =< $f ->
    ?bump_col(1),
    Dec = (H - $a) + 10,
    scan_char_ref_hex(T, S, (Dec bor (Acc bsl 4)));
scan_char_ref_hex([H|T], S0, Acc) when H >= $A, H =< $F ->
    ?bump_col(1),
    Dec = (H - $A) + 10,
    scan_char_ref_hex(T, S, (Dec bor (Acc bsl 4)));
scan_char_ref_hex(";" ++ T, S0, Acc) ->
    ?bump_col(1),
    {[[Acc]], T, S}.



%%%%%%% [25] Eq

scan_eq(T, S) ->
    ?strip1,
    "=" ++ T2 = T1, 
    S2 = S1,
    ?strip3,
    {T3, S3}.


%% scan_name/2
%%
%% We perform some checks here to make sure that the names conform to 
%% the "Namespaces in XML" specification. This is an option.
%% 
%% Qualified Name:
%% [6]      QName ::= (Prefix ':')? LocalPart
%% [7]     Prefix ::= NCName
%% [8]  LocalPart ::= NCName
%% [4]     NCName ::= (Letter | '_') (NCNameChar)*
%% [5] NCNameChar ::= Letter | Digit | '.' | '-' | '_' 
%%                    | CombiningChar | Extender


%% The effect of XML Names (namespace) conformance is that:
%% - All element types and attribute names contain either zero or one colon
%% - No entity names, PI targets, or notation names contain any colons.
%%
%% scan_name_no_colons/2 will ensure that the name contains no colons iff
%% the scanner has been told to be namespace conformant. Otherwise, it will
%% behave exactly like scan_name/2.
%%
scan_name_no_colons(Str, S) ->
    NSC = S#xmerl_scanner.namespace_conformant,
    case NSC of 
	true ->
	    {Target, NSI, T1, S1} = 
		scan_name(Str, S#xmerl_scanner{namespace_conformant = 
					       no_colons}),
	    {Target, NSI, T1, S1#xmerl_scanner{namespace_conformant = 
					       NSC}};
	false ->
	    scan_name(Str, S)
    end.



%% [5] Name ::= (Letter | '_' | ':') (NameChar)*
scan_name([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_name(MoreBytes, S1)
      end, 
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_name(Str = [$:|T], S0 = #xmerl_scanner{namespace_conformant = NSC}) ->
    if NSC == false ->
	    ?bump_col(1),
	    scan_nmtoken(T, S, [$:], NSC);
       NSC == no_colons ->
	    ?fatal({invalid_NCName, lists:sublist(Str, 1, 6)}, S0);
       true ->
	    %% In order to conform with the "Namespaces in XML" spec,
	    %% we cannot allow names to begin with ":"
	    ?fatal({invalid_NCName, lists:sublist(Str, 1, 6)}, S0)
    end;
scan_name([$_|T], S0 = #xmerl_scanner{namespace_conformant = NSC}) ->
    ?bump_col(1),
    scan_nmtoken(T, S, [$_], NSC);
scan_name([H|T], S0 = #xmerl_scanner{namespace_conformant = NSC}) ->
    case is_letter(H) of
	true ->
	    ?bump_col(1),
	    scan_nmtoken(T, S, [H], NSC);
	false ->
	    ?fatal({invalid_name, lists:sublist([H|T], 1, 6)}, S0)
    end;
scan_name(Str, S) ->
    ?fatal({invalid_name, Str}, S).






scan_nmtoken(Str, S, Acc, NSC) ->
    scan_nmtoken(Str, S, Acc, Prefix = [], Local = Acc, NSC).

%% scan_nmtoken/2
%% [7] NmToken ::= (NameChar)+
scan_nmtoken([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_nmtoken(MoreBytes, S1)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_nmtoken(Str = [H|T], S) ->
    case is_namechar(H) of
	true ->
	    scan_nmtoken(T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}, 
			 Acc = [H], Prefix = [], Local = [H], 
			 NamespaceConformant = false);
	false ->
	    ?fatal({invalid_nmtoken, lists:sublist(Str, 1, 6)}, S)
    end.


scan_nmtoken([], S = #xmerl_scanner{continuation_fun = F}, 
	     Acc, Prefix, Local, NSC) ->
    ?dbg("cont()...~n", []),
    ContF = fun(MoreBytes, S1) ->
		    scan_nmtoken(MoreBytes, S1,
				 Acc, Prefix, Local, NSC)
	    end,
    ReturnF = 
	fun(S1) ->
		NmString = lists:reverse(Acc),
		{list_to_atom(NmString), 
		 namespace_info(Prefix, Local), 
		 [], S1}
	end,
    F(ContF, ReturnF, S);
%% whitespace marks the end of a name
scan_nmtoken(Str = [H|_], S, Acc, Prefix, Local, NSC) when ?whitespace(H) ->
    %% we don't strip here because the occurrence of whitespace may be an error
    %% e.g. <!ELEMENT spec (front, body, back ?)>
    NmString = lists:reverse(Acc),
    {list_to_atom(NmString), namespace_info(Prefix, Local), Str, S};

scan_nmtoken(Str = [$:|_], S, Acc, [], Local, no_colons) ->
    ?fatal({invalid_NCName, 
	    lists:sublist(lists:reverse(Acc) ++ Str, 1, 6)}, S);
scan_nmtoken([$:|T], S0, Acc, [], Local, NSC) ->
    ?bump_col(1),
    scan_nmtoken(T, S, [$:|Acc], lists:reverse(Local), [], NSC);
scan_nmtoken(Str = [$:|T], S, Acc, Prefix, Local, NSC = true) ->
    %% non-empty Prefix means that we've encountered a ":" already.
    %% Conformity with "Namespaces in XML" requires 
    %% at most one colon in a name
    ?fatal({invalid_NCName, 
	    lists:sublist(lists:reverse(Acc) ++ Str, 1, 6)}, S);

%% non-namechar also marks the end of a name
scan_nmtoken(Str = [H|T], S0, Acc, Prefix, Local, NSC) ->
    ?bump_col(1),
    case is_namechar(H) of
	true ->
	    scan_nmtoken(T, S, [H|Acc], Prefix, [H|Local], NSC);
	false ->
	    NmStr = lists:reverse(Acc),
	    {list_to_atom(NmStr), namespace_info(Prefix, Local), Str, S}
    end.

namespace_info([], _) ->
    [];
namespace_info(Prefix, Local) ->
    {Prefix, lists:reverse(Local)}.

%%%%%%% [11] SystemLiteral

scan_system_literal([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_system_literal(MoreBytes, S1)
      end, 
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_system_literal("\"" ++ T, S) ->
    scan_system_literal(T, S, $", []);
scan_system_literal("'" ++ T, S) ->
    scan_system_literal(T, S, $', []).


scan_system_literal([], S = #xmerl_scanner{continuation_fun = F}, 
		    Delimiter, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_system_literal(MoreBytes, S1, Delimiter, Acc)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_system_literal([H|T], S, H, Acc) ->
    {lists:reverse(Acc), T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}};
scan_system_literal([H|T], S, Delimiter, Acc) ->
    scan_system_literal(T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}, 
			Delimiter, [H|Acc]).


%%%%%%% [12] PubidLiteral

scan_pubid_literal([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_pubid_literal(MoreBytes, S1)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_pubid_literal([H|T], S) when H == $"; H == $' ->
    scan_pubid_literal(T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}, H, []);
scan_pubid_literal([H|T], S) ->
    ?fatal({invalid_pubid_char, H}, S).


scan_pubid_literal([], S = #xmerl_scanner{continuation_fun = F}, 
		   Delimiter, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_pubid_literal(MoreBytes, S1, Delimiter, Acc)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_pubid_literal([H|T], S, H, Acc) ->
    {lists:reverse(Acc), T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}};
scan_pubid_literal(Str = [H|_], S, Delimiter, Acc) when ?whitespace(H) ->
    %% Before matching public identifiers, all whitespace must be normalized,
    %% so we do that here
    {_, T, S1} = strip(Str, S),
    scan_pubid_literal(T, S1, Delimiter, [16#20|Acc]);
scan_pubid_literal([H|T], S, Delimiter, Acc) ->
    case is_pubid_char(H) of
	true ->
	    scan_pubid_literal(
	      T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}, 
	      Delimiter, [H|Acc]);
	false ->
	    ?fatal({invalid_pubid_char, H}, S)
    end.

%% We do not match whitespace here, even though they're allowed in public
%% identifiers. This is because we normalize this whitespace as we scan
%% (see above in scan_pubid_literal())
%%
is_pubid_char(X) when X >= $a, X =< $z -> true;
is_pubid_char(X) when X >= $A, X =< $Z -> true;
is_pubid_char(X) when X >= $0, X =< $9 -> true;
is_pubid_char(X) ->
    lists:member(X, "-'()+,./:=?;!*#@$_%").


%%%%%%% [46] contentspec

scan_contentspec([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_contentspec(MoreBytes, S1)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_contentspec("EMPTY" ++ T, S0) ->
    ?bump_col(5),
    {empty, T, S};
scan_contentspec("ANY" ++ T, S0) ->
    ?bump_col(3),
    {any, T, S};
scan_contentspec("%" ++ T, S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1),
    scan_contentspec(ExpRef ++ T1, S1);
scan_contentspec("(" ++ T, S0) ->
    ?bump_col(1),
    ?strip1,
    scan_elem_content(T1, S1).


%%%%%%% [47] children
%%%%%%% [51] Mixed

scan_elem_content(T, S) ->
    scan_elem_content(T, S, Context = children, Mode = unknown, Acc = []).

scan_elem_content([], S = #xmerl_scanner{continuation_fun = F}, 
		  Context, Mode0, Acc0) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_elem_content(MoreBytes, S1, Context, Mode0, Acc0)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_elem_content(")" ++ T, S0, Context, Mode0, Acc0) ->
    ?bump_col(1),
    {Mode, Acc} = case {Mode0, Acc0} of
		      {unknown, [X]} ->
			  {seq, Acc0};
		      {M, L} when M == seq; M == choice ->
			  {Mode0, lists:reverse(Acc0)}
		  end,
    {Occurrence, T1, S1} = scan_occurrence(T, S),
    case {Occurrence, Context} of
	{once, mixed} -> ok;
	{'*', mixed} -> ok;
	{Other, mixed} -> 
	    ?fatal({illegal_for_mixed_content, Other}, S1);
	_ ->
	    ok
    end,
    ?strip2,
    {format_elem_content({Occurrence, {Mode, Acc}}), T2, S2};
scan_elem_content("#PCDATA" ++ T, S0, Context, Mode, Acc) ->
    ?bump_col(7),
    ?strip1,
    scan_elem_content(T1, S1, mixed, Mode, ['#PCDATA'|Acc]);
scan_elem_content("(" ++ T, S0, Context, Mode, Acc) ->
    ?bump_col(1),
    ?strip1,
    {Inner, T2, S2} = scan_elem_content(T1, S1),
    scan_elem_content(T2, S2, Context, Mode, [Inner|Acc]);
scan_elem_content("," ++ T, S0, Context, Mode, Acc) ->
    ?bump_col(1),
    ?strip1,
    scan_elem_content(T1, S1, Context, seq, Acc);
scan_elem_content("|" ++ T, S0, Context, Mode, Acc) ->
    ?bump_col(1),
    ?strip1,
    scan_elem_content(T1, S1, Context, choice, Acc);
scan_elem_content("%" ++ T, S0, Context, Mode, Acc) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1),
    scan_elem_content(ExpRef ++ T1, S1, Context, Mode, Acc);
scan_elem_content(T, S, Context, Mode, Acc) ->
    {Name, NameStr, T1, S1} = scan_name(T, S),
    {Occurrence, T2, S2} = scan_occurrence(T1, S1),
    case {Occurrence, Context} of
	{once, mixed} -> ok;
	{Other, mixed} -> 
	    ?fatal({illegal_for_mixed_content, Other}, S1);
	_ ->
	    ok
    end,
    ?strip3,
    NewAcc = [format_elem_content({Occurrence, Name}) | Acc],
    scan_elem_content(T3, S3, Context, Mode, NewAcc).

format_elem_content({once, What}) -> What;
format_elem_content(Other) -> Other.



scan_occurrence([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_occurrence(MoreBytes, S1)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_occurrence([$?|T], S0) ->
    ?bump_col(1),
    {'?', T, S};
scan_occurrence([$+|T], S0) ->
    ?bump_col(1),
    {'+', T, S};
scan_occurrence([$*|T], S0) ->
    ?bump_col(1),
    {'*', T, S};
scan_occurrence(T, S) ->
    {once, T , S}.


%%%%%%% [74] PEDef


scan_pe_def([], S = #xmerl_scanner{continuation_fun = F}, PEName) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_pe_def(MoreBytes, S1, PEName)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_pe_def("'" ++ T, S0, PEName) ->
    ?bump_col(1),
    scan_entity_value(T, S, $', PEName);
scan_pe_def("\"" ++ T, S0, PEName) ->
    ?bump_col(1),
    scan_entity_value(T, S, $", PEName);
scan_pe_def(Str, S, PEName) ->
    scan_external_id(Str, S).


%%%%%%% [82] NotationDecl

scan_notation_decl(T, #xmerl_scanner{rules_write_fun = Write} = S) ->
    {Name, NameStr, T1, S1} = scan_name_no_colons(T, S),
    {Def, T2, S2} = scan_notation_decl1(T1, S1),
    ?strip3,
    ">" ++ T4 = T3,
    S4 = Write(notation, Name, Def, S3),
    {T4, S4}.

scan_notation_decl1([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_notation_decl1(MoreBytes, S1)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_notation_decl1("SYSTEM" ++ T, S0) ->
    ?bump_col(6),
    ?strip1,
    {SL, T2, S2} = scan_system_literal(T1, S1),
    {{system, SL}, T2, S2};
scan_notation_decl1("PUBLIC" ++ T, S0) ->
    ?bump_col(6),
    ?strip1,
    {PIDL, T2, S2} = scan_pubid_literal(T1, S1),
    ?strip3,
    case T3 of
	">" ++ T4 ->
	    {{public, PIDL}, T4, 
	     S3#xmerl_scanner{col = S3#xmerl_scanner.col+1}};
	_ ->
	    {SL, T4, S4} = scan_system_literal(T3, S3),
	    {{public, PIDL, SL}, T4, S4}
    end.

%%%%%%% [75] ExternalID

scan_external_id([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_external_id(MoreBytes, S1)
      end,
      fun(S1) -> ?fatal(unexpected_end, S1)
      end, S);
scan_external_id("SYSTEM" ++ T, S0) ->
    ?bump_col(6),
    ?strip1,
    {SL, T2, S2} = scan_system_literal(T1, S1),
    {{system, SL}, T2, S2};
scan_external_id("PUBLIC" ++ T, S0) ->
    ?bump_col(6),
    ?strip1,
    {PIDL, T2, S2} = scan_pubid_literal(T1, S1),
    ?strip3,
    {SL, T4, S4} = scan_system_literal(T3, S3),
    {{public, PIDL, SL}, T4, S4}.


%%%%%%% [9] EntityValue

%% Note that we have two different scan functions for EntityValue
%% They differ in that this one checks for recursive calls to the same
%% parameter entity. This should perhaps be done

scan_entity_value(Str, S, Delim, Name) ->
    scan_entity_value(Str, S, Delim, Acc = [], Name).


scan_entity_value([], S = #xmerl_scanner{continuation_fun = F}, 
		  H, Acc, PEName) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_entity_value(MoreBytes, S1, H, Acc, PEName)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_entity_value([H|T], S0, H, Acc, PEName) ->
    ?bump_col(1),
    {lists:reverse(Acc), T, S};
scan_entity_value("%" ++ T, S0, Delim, Acc, PEName) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    if PERefName == PEName ->
	    ?fatal({illegal_recursion_in_PE, PEName}, S1);
       true ->
	    ExpRef = expand_pe_reference(PERefName, S1),
	    scan_entity_value(ExpRef ++ T1, S1, Delim, Acc, PEName)
    end;
scan_entity_value("&" ++ T, S0, Delim, Acc, PEName) ->
    ?bump_col(1),
    {ExpRef, T1, S1} = scan_reference(T, S),
    scan_entity_value(ExpRef ++ T1, S1, Delim, Acc, PEName);
scan_entity_value([H|T], S0, Delim, Acc, PEName) ->
    ?bump_col(1),
    scan_entity_value(T, S, Delim, [H|Acc], PEName).


%%%%%%%



%%%%%%%



%%%%%%% [15] Comment
scan_comment(Str, S) ->
    scan_comment(Str, S, Pos = undefined, Parents = [], Lang = []).

scan_comment(Str, S = #xmerl_scanner{col = C,
				     event_fun = Event}, Pos, Parents, Lang) ->
    Comment = #xmlComment{pos = Pos,
			  parents = Parents,
			  language = Lang,
			  value = undefined},
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = started,
					       line = S#xmerl_scanner.line,
					       col = C,
					       pos = Pos,
					       data = Comment}, S),
    
    scan_comment1(Str, S, Pos, Comment, Acc = []).

scan_comment1([], S = #xmerl_scanner{continuation_fun = F}, 
	     Pos, Comment, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_comment1(MoreBytes, S1, Pos, Comment, Acc)
      end,
      fun(S1) ->
	      ?fatal(unexpected_end, S1)
      end, S);
scan_comment1("-->" ++ T, S0 = #xmerl_scanner{col = C,
					     event_fun = Event,
					     hook_fun = Hook}, 
	     Pos, Comment, Acc) ->
    ?bump_col(2),
    Comment1 = Comment#xmlComment{value = lists:reverse(Acc)},
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S#xmerl_scanner.line,
					       col = C,
					       data = Comment1}, S),
    {Ret, S2} = Hook(Comment1, S1),
    T2 = T,
    ?strip3,
    {Ret, T3, S3};
scan_comment1("\n" ++ T, S = #xmerl_scanner{line = L}, Pos, Cmt, Acc) ->
    scan_comment1(T, S#xmerl_scanner{line = L+1, col = 1}, 
		 Pos, Cmt, "\n" ++ Acc);
scan_comment1("\r\n" ++ T, S = #xmerl_scanner{line = L}, Pos, Cmt, Acc) ->
    %% CR followed by LF is read as a single LF
    scan_comment1(T, S#xmerl_scanner{line = L+1, col=1}, 
		  Pos, Cmt, "\n" ++ Acc);
scan_comment1("\r" ++ T, S = #xmerl_scanner{line = L}, Pos, Cmt, Acc) ->
    %% CR not followed by LF is read as a LF
    scan_comment1(T, S#xmerl_scanner{line = L+1, col = 1}, 
		 Pos, Cmt, "\n" ++ Acc);
scan_comment1([H|T], S = #xmerl_scanner{col = C}, Pos, Cmt, Acc) ->
    scan_comment1(T, S#xmerl_scanner{col = C+1}, Pos, Cmt, [H|Acc]).

%%%%%%%

strip([], S = #xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()... stripping whitespace~n", []),
    F(fun(MoreBytes, S1) ->
	      strip(MoreBytes, S1)
      end,
      fun(S1) ->
	      {[], [], S1}
      end, S);
strip("\s" ++ T, S = #xmerl_scanner{col = C}) ->
    strip(T, S#xmerl_scanner{col = C+1});
strip("\t" ++ T, S = #xmerl_scanner{col = C}) ->
    strip(T, S#xmerl_scanner{col = expand_tab(C)});
strip("\n" ++ T, S = #xmerl_scanner{line = L}) ->
    strip(T, S#xmerl_scanner{line = L+1, col = 1});
strip("\r\n" ++ T, S = #xmerl_scanner{line = L}) ->
    %% CR followed by LF is read as a single LF
    strip(T, S#xmerl_scanner{line = L+1, col = 1});
strip("\r" ++ T, S = #xmerl_scanner{line = L}) ->
    %% CR not followed by LF is read as a LF
    strip(T, S#xmerl_scanner{line = L+1, col = 1});
strip(Str, S) ->
    {[], Str, S}.


%%% Function to accumulate/normalize whitespace.

accumulate_whitespace(T, S, preserve, Acc) ->
    accumulate_whitespace(T, S, Acc);
accumulate_whitespace(T, S, normalize, Acc) ->
    {WsAcc, T1, S1} = accumulate_whitespace(T, S, []),
    {[$\s|Acc], T1, S1}.


accumulate_whitespace([], S = #xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      accumulate_whitespace(MoreBytes, S1, Acc)
      end,
      fun(S1) ->
	      {Acc, [], S1}
      end, S);
accumulate_whitespace("\s" ++ T, S = #xmerl_scanner{col = C}, Acc) ->
    accumulate_whitespace(T, S#xmerl_scanner{col = C+1}, [$\s|Acc]);
accumulate_whitespace("\t" ++ T, S = #xmerl_scanner{col = C}, Acc) ->
    accumulate_whitespace(T, S#xmerl_scanner{col = expand_tab(C)}, [$\t|Acc]);
accumulate_whitespace("\n" ++ T, S = #xmerl_scanner{line = L}, Acc) ->
    accumulate_whitespace(T, S#xmerl_scanner{line = L+1, col = 1}, [$\n|Acc]);
accumulate_whitespace("\r\n" ++ T, S = #xmerl_scanner{line = L}, Acc) ->
    %% CR followed by LF is read as a single LF
    accumulate_whitespace(T, S#xmerl_scanner{line = L+1, col=1}, [$\n|Acc]);
accumulate_whitespace("\r" ++ T, S = #xmerl_scanner{line = L}, Acc) ->
    %% CR not followed by LF is read as a LF
    accumulate_whitespace(T, S#xmerl_scanner{line = L+1, col = 1}, [$\n|Acc]);
accumulate_whitespace(Str, S, Acc) ->
    {Acc, Str, S}.


expand_tab(Col) ->
    Rem = (Col-1) rem 8,
    NewCol = Col + 8 - Rem.

%%%%%%%% [2] Char

%is_char(16#9) -> true;
%is_char(16#a) -> true;
%is_char(16#d) -> true;
%is_char(X) when X >= 16#20, X =< 16#d7ff -> true;
%is_char(X) when X >= 16#e000, X =< 16#fffd -> true;
%is_char(X) when X >= 16#10000, X =< 16#10ffff -> true;
%is_char(_) ->
%    false.


%%%%%%%% [3] Whitespace

%is_whitespace(16#20) -> true;
%is_whitespace(16#9) -> true;
%is_whitespace(16#d) -> true;
%is_whitespace(16#a) -> true;
%is_whitespace(_) ->
%    false.

%%%%%%% [4] Namechar

is_namechar(X) when X == $. ; X == $- ; X == $_ ; X == $: ->
    true;
is_namechar(X) ->
    case is_letter(X) of
	true -> true;
	false ->
	    case is_digit(X) of
		true -> true;
		false ->
		    case is_combining_char(X) of
			true -> true;
			false ->
			    is_extender(X)
		    end
	    end
    end.


%%% Helper functions

fatal(Reason, S) ->
    exit({fatal, {Reason, S#xmerl_scanner.line, S#xmerl_scanner.col}}).



rules_write(Context, Name, Value, #xmerl_scanner{rules = T} = S) ->
    ets:insert(T, {{Context, Name}, Value}),
    S.

rules_read(Context, Name, #xmerl_scanner{rules = T}) ->
    case ets:lookup(T, {Context, Name}) of
	[] ->
	    undefined;
	[{K, V}] ->
	    V
    end.

%%% UNICODE character definitions

is_letter(X) ->
    case is_base_char(X) of
	false ->
	    is_ideographic(X);
	true ->
	    true
    end.

is_base_char(X) when X >= 16#0041, X =< 16#005A -> true;
is_base_char(X) when X >= 16#0061, X =< 16#007A -> true;
is_base_char(X) when X >= 16#00C0, X =< 16#00D6 -> true;
is_base_char(X) when X >= 16#00D8, X =< 16#00F6 -> true;
is_base_char(X) when X >= 16#00F8, X =< 16#00FF -> true;
is_base_char(X) when X >= 16#0100, X =< 16#0131 -> true;
is_base_char(X) when X >= 16#0134, X =< 16#013E -> true;
is_base_char(X) when X >= 16#0141, X =< 16#0148 -> true;
is_base_char(X) when X >= 16#014A, X =< 16#017E -> true;
is_base_char(X) when X >= 16#0180, X =< 16#01C3 -> true;
is_base_char(X) when X >= 16#01CD, X =< 16#01F0 -> true;
is_base_char(X) when X >= 16#01F4, X =< 16#01F5 -> true;
is_base_char(X) when X >= 16#01FA, X =< 16#0217 -> true;
is_base_char(X) when X >= 16#0250, X =< 16#02A8 -> true;
is_base_char(X) when X >= 16#02BB, X =< 16#02C1 -> true;
is_base_char(16#0386) -> true;
is_base_char(X) when X >= 16#0388, X =< 16#038A -> true;
is_base_char(16#038C) -> true;
is_base_char(X) when X >= 16#038E, X =< 16#03A1 -> true;
is_base_char(X) when X >= 16#03A3, X =< 16#03CE -> true;
is_base_char(X) when X >= 16#03D0, X =< 16#03D6 -> true;
is_base_char(16#03DA) -> true;
is_base_char(16#03DC) -> true;
is_base_char(16#03DE) -> true;
is_base_char(16#03E0) -> true;
is_base_char(X) when X >= 16#03E2, X =< 16#03F3 -> true;
is_base_char(X) when X >= 16#0401, X =< 16#040C -> true;
is_base_char(X) when X >= 16#040E, X =< 16#044F -> true;
is_base_char(X) when X >= 16#0451, X =< 16#045C -> true;
is_base_char(X) when X >= 16#045E, X =< 16#0481 -> true;
is_base_char(X) when X >= 16#0490, X =< 16#04C4 -> true;
is_base_char(X) when X >= 16#04C7, X =< 16#04C8 -> true;
is_base_char(X) when X >= 16#04CB, X =< 16#04CC -> true;
is_base_char(X) when X >= 16#04D0, X =< 16#04EB -> true;
is_base_char(X) when X >= 16#04EE, X =< 16#04F5 -> true;
is_base_char(X) when X >= 16#04F8, X =< 16#04F9 -> true;
is_base_char(X) when X >= 16#0531, X =< 16#0556 -> true;
is_base_char(16#0559) -> true;
is_base_char(X) when X >= 16#0561, X =< 16#0586 -> true;
is_base_char(X) when X >= 16#05D0, X =< 16#05EA -> true;
is_base_char(X) when X >= 16#05F0, X =< 16#05F2 -> true;
is_base_char(X) when X >= 16#0621, X =< 16#063A -> true;
is_base_char(X) when X >= 16#0641, X =< 16#064A -> true;
is_base_char(X) when X >= 16#0671, X =< 16#06B7 -> true;
is_base_char(X) when X >= 16#06BA, X =< 16#06BE -> true;
is_base_char(X) when X >= 16#06C0, X =< 16#06CE -> true;
is_base_char(X) when X >= 16#06D0, X =< 16#06D3 -> true;
is_base_char(16#06D5) -> true;
is_base_char(X) when X >= 16#06E5, X =< 16#06E6 -> true;
is_base_char(X) when X >= 16#0905, X =< 16#0939 -> true;
is_base_char(16#093D) -> true;
is_base_char(X) when X >= 16#0958, X =< 16#0961 -> true;
is_base_char(X) when X >= 16#0985, X =< 16#098C -> true;
is_base_char(X) when X >= 16#098F, X =< 16#0990 -> true;
is_base_char(X) when X >= 16#0993, X =< 16#09A8 -> true;
is_base_char(X) when X >= 16#09AA, X =< 16#09B0 -> true;
is_base_char(16#09B2) -> true;
is_base_char(X) when X >= 16#09B6, X =< 16#09B9 -> true;
is_base_char(X) when X >= 16#09DC, X =< 16#09DD -> true;
is_base_char(X) when X >= 16#09DF, X =< 16#09E1 -> true;
is_base_char(X) when X >= 16#09F0, X =< 16#09F1 -> true;
is_base_char(X) when X >= 16#0A05, X =< 16#0A0A -> true;
is_base_char(X) when X >= 16#0A0F, X =< 16#0A10 -> true;
is_base_char(X) when X >= 16#0A13, X =< 16#0A28 -> true;
is_base_char(X) when X >= 16#0A2A, X =< 16#0A30 -> true;
is_base_char(X) when X >= 16#0A32, X =< 16#0A33 -> true;
is_base_char(X) when X >= 16#0A35, X =< 16#0A36 -> true;
is_base_char(X) when X >= 16#0A38, X =< 16#0A39 -> true;
is_base_char(X) when X >= 16#0A59, X =< 16#0A5C -> true;
is_base_char(16#0A5C) -> true;
is_base_char(X) when X >= 16#0A72, X =< 16#0A74 -> true;
is_base_char(X) when X >= 16#0A85, X =< 16#0A8B -> true;
is_base_char(16#0A8D) -> true;
is_base_char(X) when X >= 16#0A8F, X =< 16#0A91 -> true;
is_base_char(X) when X >= 16#0A93, X =< 16#0AA8 -> true;
is_base_char(X) when X >= 16#0AAA, X =< 16#0AB0 -> true;
is_base_char(X) when X >= 16#0AB2, X =< 16#0AB3 -> true;
is_base_char(X) when X >= 16#0AB5, X =< 16#0AB9 -> true;
is_base_char(16#0ABD) -> true;
is_base_char(16#0AE0) -> true;
is_base_char(X) when X >= 16#0B05, X =< 16#0B0C -> true;
is_base_char(X) when X >= 16#0B0F, X =< 16#0B10 -> true;
is_base_char(X) when X >= 16#0B13, X =< 16#0B28 -> true;
is_base_char(X) when X >= 16#0B2A, X =< 16#0B30 -> true;
is_base_char(X) when X >= 16#0B32, X =< 16#0B33 -> true;
is_base_char(X) when X >= 16#0B36, X =< 16#0B39 -> true;
is_base_char(16#0B3D) -> true;
is_base_char(X) when X >= 16#0B5C, X =< 16#0B5D -> true;
is_base_char(X) when X >= 16#0B5F, X =< 16#0B61 -> true;
is_base_char(X) when X >= 16#0B85, X =< 16#0B8A -> true;
is_base_char(X) when X >= 16#0B8E, X =< 16#0B90 -> true;
is_base_char(X) when X >= 16#0B92, X =< 16#0B95 -> true;
is_base_char(X) when X >= 16#0B99, X =< 16#0B9A -> true;
is_base_char(16#0B9C) -> true;
is_base_char(X) when X >= 16#0B9E, X =< 16#0B9F -> true;
is_base_char(X) when X >= 16#0BA3, X =< 16#0BA4 -> true;
is_base_char(X) when X >= 16#0BA8, X =< 16#0BAA -> true;
is_base_char(X) when X >= 16#0BAE, X =< 16#0BB5 -> true;
is_base_char(X) when X >= 16#0BB7, X =< 16#0BB9 -> true;
is_base_char(X) when X >= 16#0C05, X =< 16#0C0C -> true;
is_base_char(X) when X >= 16#0C0E, X =< 16#0C10 -> true;
is_base_char(X) when X >= 16#0C12, X =< 16#0C28 -> true;
is_base_char(X) when X >= 16#0C2A, X =< 16#0C33 -> true;
is_base_char(X) when X >= 16#0C35, X =< 16#0C39 -> true;
is_base_char(X) when X >= 16#0C60, X =< 16#0C61 -> true;
is_base_char(X) when X >= 16#0C85, X =< 16#0C8C -> true;
is_base_char(X) when X >= 16#0C8E, X =< 16#0C90 -> true;
is_base_char(X) when X >= 16#0C92, X =< 16#0CA8 -> true;
is_base_char(X) when X >= 16#0CAA, X =< 16#0CB3 -> true;
is_base_char(X) when X >= 16#0CB5, X =< 16#0CB9 -> true;
is_base_char(16#0CDE) -> true;
is_base_char(X) when X >= 16#0CE0, X =< 16#0CE1 -> true;
is_base_char(X) when X >= 16#0D05, X =< 16#0D0C -> true;
is_base_char(X) when X >= 16#0D0E, X =< 16#0D10 -> true;
is_base_char(X) when X >= 16#0D12, X =< 16#0D28 -> true;
is_base_char(X) when X >= 16#0D2A, X =< 16#0D39 -> true;
is_base_char(X) when X >= 16#0D60, X =< 16#0D61 -> true;
is_base_char(X) when X >= 16#0E01, X =< 16#0E2E -> true;
is_base_char(16#0E30) -> true;
is_base_char(X) when X >= 16#0E32, X =< 16#0E33 -> true;
is_base_char(X) when X >= 16#0E40, X =< 16#0E45 -> true;
is_base_char(X) when X >= 16#0E81, X =< 16#0E82 -> true;
is_base_char(16#0E84) -> true;
is_base_char(X) when X >= 16#0E87, X =< 16#0E88 -> true;
is_base_char(16#0E8A) -> true;
is_base_char(16#0E8D) -> true;
is_base_char(X) when X >= 16#0E94, X =< 16#0E97 -> true;
is_base_char(X) when X >= 16#0E99, X =< 16#0E9F -> true;
is_base_char(X) when X >= 16#0EA1, X =< 16#0EA3 -> true;
is_base_char(16#0EA5) -> true;
is_base_char(16#0EA7) -> true;
is_base_char(X) when X >= 16#0E99, X =< 16#0E9F -> true;
is_base_char(X) when X >= 16#0EA1, X =< 16#0EA3 -> true;
is_base_char(16#0EB0) -> true;
is_base_char(X) when X >= 16#0EB2, X =< 16#0EB3 -> true;
is_base_char(16#0EBD) -> true;
is_base_char(X) when X >= 16#0EC0, X =< 16#0EC4 -> true;
is_base_char(X) when X >= 16#0F40, X =< 16#0F47 -> true;
is_base_char(X) when X >= 16#0F49, X =< 16#0F69 -> true;
is_base_char(X) when X >= 16#10A0, X =< 16#10C5 -> true;
is_base_char(X) when X >= 16#10D0, X =< 16#10F6 -> true;
is_base_char(16#1100) -> true;
is_base_char(X) when X >= 16#1102, X =< 16#1103 -> true;
is_base_char(X) when X >= 16#1105, X =< 16#1107 -> true;
is_base_char(16#1109) -> true;
is_base_char(X) when X >= 16#110B, X =< 16#110C -> true;
is_base_char(X) when X >= 16#110E, X =< 16#1112 -> true;
is_base_char(16#113C) -> true;
is_base_char(16#113E) -> true;
is_base_char(16#1140) -> true;
is_base_char(16#114C) -> true;
is_base_char(16#114E) -> true;
is_base_char(16#1150) -> true;
is_base_char(X) when X >= 16#1154, X =< 16#1155 -> true;
is_base_char(16#1159) -> true;
is_base_char(X) when X >= 16#115F, X =< 16#1161 -> true;
is_base_char(16#1163) -> true;
is_base_char(16#1165) -> true;
is_base_char(16#1167) -> true;
is_base_char(16#1169) -> true;
is_base_char(X) when X >= 16#116D, X =< 16#116E -> true;
is_base_char(X) when X >= 16#1172, X =< 16#1173 -> true;
is_base_char(16#1175) -> true;
is_base_char(16#119E) -> true;
is_base_char(16#11A8) -> true;
is_base_char(16#11AB) -> true;
is_base_char(X) when X >= 16#11AE, X =< 16#11AF -> true;
is_base_char(X) when X >= 16#11B7, X =< 16#11B8 -> true;
is_base_char(16#11BA) -> true;
is_base_char(X) when X >= 16#11BC, X =< 16#11C2 -> true;
is_base_char(16#11EB) -> true;
is_base_char(16#11F0) -> true;
is_base_char(16#11F9) -> true;
is_base_char(X) when X >= 16#1E00, X =< 16#1E9B -> true;
is_base_char(X) when X >= 16#1EA0, X =< 16#1EF9 -> true;
is_base_char(X) when X >= 16#1F00, X =< 16#1F15 -> true;
is_base_char(X) when X >= 16#1F18, X =< 16#1F1D -> true;
is_base_char(X) when X >= 16#1F20, X =< 16#1F45 -> true;
is_base_char(X) when X >= 16#1F48, X =< 16#1F4D -> true;
is_base_char(X) when X >= 16#1F50, X =< 16#1F57 -> true;
is_base_char(16#1F59) -> true;
is_base_char(16#1F5B) -> true;
is_base_char(16#1F5D) -> true;
is_base_char(X) when X >= 16#1F5F, X =< 16#1F7D -> true;
is_base_char(X) when X >= 16#1F80, X =< 16#1FB4 -> true;
is_base_char(X) when X >= 16#1FB6, X =< 16#1FBC -> true;
is_base_char(16#1FBE) -> true;
is_base_char(X) when X >= 16#1FC2, X =< 16#1FC4 -> true;
is_base_char(X) when X >= 16#1FC6, X =< 16#1FCC -> true;
is_base_char(X) when X >= 16#1FD0, X =< 16#1FD3 -> true;
is_base_char(X) when X >= 16#1FD6, X =< 16#1FDB -> true;
is_base_char(X) when X >= 16#1FE0, X =< 16#1FEC -> true;
is_base_char(X) when X >= 16#1FF2, X =< 16#1FF4 -> true;
is_base_char(X) when X >= 16#1FF6, X =< 16#1FFC -> true;
is_base_char(16#2126) -> true;
is_base_char(X) when X >= 16#212A, X =< 16#212B -> true;
is_base_char(16#212E) -> true;
is_base_char(X) when X >= 16#2180, X =< 16#2182 -> true;
is_base_char(X) when X >= 16#3041, X =< 16#3094 -> true;
is_base_char(X) when X >= 16#30A1, X =< 16#30FA -> true;
is_base_char(X) when X >= 16#3105, X =< 16#312C -> true;
is_base_char(X) when X >= 16#ac00, X =< 16#d7a3 -> true;
is_base_char(_) ->
    false.


is_ideographic(X) when X >= 16#4e00, X =< 16#9fa5 -> true;
is_ideographic(16#3007) -> true;
is_ideographic(X) when X >= 16#3021, X =< 16#3029 -> true;
is_ideographic(_) ->
    false.


is_combining_char(X) when X >= 16#0300, X =< 16#0345 -> true;
is_combining_char(X) when X >= 16#0360, X =< 16#0361 -> true;
is_combining_char(X) when X >= 16#0483, X =< 16#0486 -> true;
is_combining_char(X) when X >= 16#0591, X =< 16#05a1 -> true;
is_combining_char(X) when X >= 16#05a3, X =< 16#05b9 -> true;
is_combining_char(X) when X >= 16#05bb, X =< 16#05bd -> true;
is_combining_char(16#05bf) -> true;
is_combining_char(X) when X >= 16#05c1, X =< 16#05c2 -> true;
is_combining_char(16#05c4) -> true;
is_combining_char(X) when X >= 16#064b, X =< 16#0652 -> true;
is_combining_char(16#0670) -> true;
is_combining_char(X) when X >= 16#06d6, X =< 16#06dc -> true;
is_combining_char(X) when X >= 16#06dd, X =< 16#06df -> true;
is_combining_char(X) when X >= 16#06e0, X =< 16#06e4 -> true;
is_combining_char(X) when X >= 16#06e7, X =< 16#06e8 -> true;
is_combining_char(X) when X >= 16#06ea, X =< 16#06ed -> true;
is_combining_char(X) when X >= 16#0901, X =< 16#0903 -> true;
is_combining_char(16#093c) -> true;
is_combining_char(X) when X >= 16#093e, X =< 16#094c -> true;
is_combining_char(16#094d) -> true;
is_combining_char(X) when X >= 16#0951, X =< 16#0954 -> true;
is_combining_char(X) when X >= 16#0962, X =< 16#0963 -> true;
is_combining_char(X) when X >= 16#0981, X =< 16#0983 -> true;
is_combining_char(16#09bc) -> true;
is_combining_char(16#09be) -> true;
is_combining_char(16#09bf) -> true;
is_combining_char(X) when X >= 16#09c0, X =< 16#09c4 -> true;
is_combining_char(X) when X >= 16#09c7, X =< 16#09c8 -> true;
is_combining_char(X) when X >= 16#09cb, X =< 16#09cd -> true;
is_combining_char(16#09d7) -> true;
is_combining_char(X) when X >= 16#09e2, X =< 16#09e3 -> true;
is_combining_char(16#0a02) -> true;
is_combining_char(16#0a3c) -> true;
is_combining_char(16#0a3e) -> true;
is_combining_char(16#0a3f) -> true;
is_combining_char(X) when X >= 16#0a40, X =< 16#0a42 -> true;
is_combining_char(X) when X >= 16#0a47, X =< 16#0a48 -> true;
is_combining_char(X) when X >= 16#0a4b, X =< 16#0a4d -> true;
is_combining_char(X) when X >= 16#0a70, X =< 16#0a71 -> true;
is_combining_char(X) when X >= 16#0a81, X =< 16#0a83 -> true;
is_combining_char(16#0abc) -> true;
is_combining_char(X) when X >= 16#0abe, X =< 16#0ac5 -> true;
is_combining_char(X) when X >= 16#0ac7, X =< 16#0ac9 -> true;
is_combining_char(X) when X >= 16#0acb, X =< 16#0acd -> true;
is_combining_char(X) when X >= 16#0b01, X =< 16#0b03 -> true;
is_combining_char(16#0b3c) -> true;
is_combining_char(X) when X >= 16#0b3e, X =< 16#0b43 -> true;
is_combining_char(X) when X >= 16#0b47, X =< 16#0b48 -> true;
is_combining_char(X) when X >= 16#0b4b, X =< 16#0b4d -> true;
is_combining_char(X) when X >= 16#0b56, X =< 16#0b57 -> true;
is_combining_char(X) when X >= 16#0b82, X =< 16#0b83 -> true;
is_combining_char(X) when X >= 16#0bbe, X =< 16#0bc2 -> true;
is_combining_char(X) when X >= 16#0bc6, X =< 16#0bc8 -> true;
is_combining_char(X) when X >= 16#0bca, X =< 16#0bcd -> true;
is_combining_char(16#0bd7) -> true;
is_combining_char(X) when X >= 16#0c01, X =< 16#0c03 -> true;
is_combining_char(X) when X >= 16#0c3e, X =< 16#0c44 -> true;
is_combining_char(X) when X >= 16#0c46, X =< 16#0c48 -> true;
is_combining_char(X) when X >= 16#0c4a, X =< 16#0c4d -> true;
is_combining_char(X) when X >= 16#0c55, X =< 16#0c56 -> true;
is_combining_char(X) when X >= 16#0c82, X =< 16#0c83 -> true;
is_combining_char(X) when X >= 16#0cbe, X =< 16#0cc4 -> true;
is_combining_char(X) when X >= 16#0cc6, X =< 16#0cc8 -> true;
is_combining_char(X) when X >= 16#0cca, X =< 16#0ccd -> true;
is_combining_char(X) when X >= 16#0cd5, X =< 16#0cd6 -> true;
is_combining_char(X) when X >= 16#0d02, X =< 16#0d03 -> true;
is_combining_char(X) when X >= 16#0d3e, X =< 16#0d43 -> true;
is_combining_char(X) when X >= 16#0d46, X =< 16#0d48 -> true;
is_combining_char(X) when X >= 16#0d4a, X =< 16#0d4d -> true;
is_combining_char(16#0d57) -> true;
is_combining_char(16#0e31) -> true;
is_combining_char(X) when X >= 16#0e34, X =< 16#0e3a -> true;
is_combining_char(X) when X >= 16#0e47, X =< 16#0e4e -> true;
is_combining_char(16#0eb1) -> true;
is_combining_char(X) when X >= 16#0eb4, X =< 16#0eb9 -> true;
is_combining_char(X) when X >= 16#0ebb, X =< 16#0ebc -> true;
is_combining_char(X) when X >= 16#0ec8, X =< 16#0ecd -> true;
is_combining_char(X) when X >= 16#0f18, X =< 16#0f19 -> true;
is_combining_char(16#0f35) -> true;
is_combining_char(16#0f37) -> true;
is_combining_char(16#0f39) -> true;
is_combining_char(16#0f3e) -> true;
is_combining_char(16#0f3f) -> true;
is_combining_char(X) when X >= 16#0f71, X =< 16#0f84 -> true;
is_combining_char(X) when X >= 16#0f86, X =< 16#0f8b -> true;
is_combining_char(X) when X >= 16#0f90, X =< 16#0f95 -> true;
is_combining_char(16#0f97) -> true;
is_combining_char(X) when X >= 16#0f99, X =< 16#0fad -> true;
is_combining_char(X) when X >= 16#0fb1, X =< 16#0fb7 -> true;
is_combining_char(16#0fb9) -> true;
is_combining_char(X) when X >= 16#20d0, X =< 16#20dc -> true;
is_combining_char(16#20e1) -> true;
is_combining_char(X) when X >= 16#302a, X =< 16#302f -> true;
is_combining_char(16#3099) -> true;
is_combining_char(16#309a) -> true;
is_combining_char(X) -> false.

is_digit(X) when X >= 16#0030, X =< 16#0039 -> true;
is_digit(X) when X >= 16#0660, X =< 16#0669 -> true;
is_digit(X) when X >= 16#0966, X =< 16#096f -> true;
is_digit(X) when X >= 16#09e6, X =< 16#09ef -> true;
is_digit(X) when X >= 16#0a66, X =< 16#0a6f -> true;
is_digit(X) when X >= 16#0ae6, X =< 16#0aef -> true;
is_digit(X) when X >= 16#0b66, X =< 16#0b6f -> true;
is_digit(X) when X >= 16#0be7, X =< 16#0bef -> true;
is_digit(X) when X >= 16#0c66, X =< 16#0c6f -> true;
is_digit(X) when X >= 16#0ce6, X =< 16#0cef -> true;
is_digit(X) when X >= 16#0d66, X =< 16#0d6f -> true;
is_digit(X) when X >= 16#0e50, X =< 16#0e59 -> true;
is_digit(X) when X >= 16#0ed0, X =< 16#0ed9 -> true;
is_digit(X) when X >= 16#0f20, X =< 16#0f29 -> true;
is_digit(X) -> false.

is_extender(16#00b7) -> true;
is_extender(16#02d0) -> true;
is_extender(16#02d1) -> true;
is_extender(16#0387) -> true;
is_extender(16#0640) -> true;
is_extender(16#0e46) -> true;
is_extender(16#0ec6) -> true;
is_extender(16#3005) -> true;
is_extender(X) when X >= 16#3031, X =< 16#3035 -> true;
is_extender(X) when X >= 16#309d, X =< 16#309e -> true;
is_extender(X) when X >= 16#30fc, X =< 16#30fe -> true;
is_extender(X) -> false.

