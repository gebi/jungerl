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
%%% The Original Code is xmerl-0.9
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:       xmerl_eventp.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Simple event-based processor (front-end to xmerl_scanner)
%%% 
%%% Modules used : 
%%% 
%%%----------------------------------------------------------------------

-module(xmerl_eventp).
-vsn('0.7').
-date('00-10-12').
-author('ulf.wiger@ericsson.com').

-export([file/2,
	 cont/3]).

-include("xmerl.hrl").

file(Fname, ScannerOptions) ->
    AccF = fun(X, Acc, S) ->
		   {Acc, S}
	   end,
    Dir = filename:dirname(Fname),
    case file:open(Fname, [read, raw, binary]) of
	{ok, Fd} ->
	    B0 = list_to_binary([]),
	    ContS = [{B0, Fname, Fd}],
	    Opts = scanner_options(ScannerOptions,
				   [{continuation_fun, fun cont/3, ContS},
				    {acc_fun, AccF},
				    {fetch_fun, fun fetch/2},
				    {close_fun, fun close/1},
				    {directory, Dir}]),
	    xmerl_scan:string([], Opts);
	Error ->
	    Error
    end.

scanner_options([H|T], Opts) ->
    case catch keyreplace(H, 1, Opts) of
	false ->
	    scanner_options(T, [H|Opts]);
	NewOpts ->
	    scanner_options(T, NewOpts)
    end;
scanner_options([], Opts) ->
    Opts.

keyreplace(X, Pos, [H|T]) when element(Pos, X) == element(Pos, H) ->
    [X|T];
keyreplace(X, Pos, [H|T]) ->
    [H|keyreplace(X, Pos, T)];
keyreplace(X, Pos, []) ->
    throw(false).


cont(F, Exception, S) ->
    case xmerl_scan:cont_state(S) of
	[{Fname, eof}|_] ->
	    Exception(S);
	[{Sofar, Fname, Fd}|T] ->
	    cont(F, Exception, Sofar, Fd, Fname, T, S)
    end.

cont(F, Exception, Sofar, Fd, Fname, T, S) ->
    case read_chunk(Fd, Fname, Sofar) of
	{ok, Bin} ->
	    find_good_split(list_to_binary([Sofar, Bin]), 
			    F, Exception, Fd, Fname, T, S);
	eof ->
	    file:close(Fd),
	    NewS = xmerl_scan:cont_state([{Fname, eof}|T], S),
	    F(binary_to_list(Sofar), NewS);
	Error ->
	    exit(Error)
    end.
    


read_chunk(Fd, Fname, Sofar) ->
    file:read(Fd, 512).



-ifndef(no_bitsyntax).

find_good_split(Bin, F, Exception, Fd, Fname, T, S) ->
    find_good_split(size(Bin)-1, Bin, F, Exception, Fd, Fname, T, S).

find_good_split(0, B, F, Exception, Fd, Fname, T, S) ->
    cont(F, Exception, B, Fd, Fname, T, S);
find_good_split(Size, B, F, Exception, Fd, Fname, T, S) ->
    case B of
	<<Bytes:Size/binary, H/integer, Tail/binary>> when ?whitespace(H) ->
	    {SubB,_} = split_binary(B, Size+1),
	    NewS = xmerl_scan:cont_state([{Tail, Fname, Fd}|T], S),
	    F(binary_to_list(SubB), NewS);
	_ ->
	    find_good_split(Size-1, B, F, Exception, Fd, Fname, T, S)
    end.

-else.

find_good_split(Bin, F, Exception, Fd, Fname, T, S) ->
    find_good_split(size(Bin), Bin, F, Exception, Fd, Fname, T, S).

find_good_split(0, B, F, Exception, Fd, Fname, T, S) ->
    cont(F, Exception, B, Fd, Fname, T, S);
find_good_split(Size, B, F, Exception, Fd, Fname, T, S) ->
    case binary_to_list(B, Size, Size) of
	[H] when ?whitespace(H) ->
	    {SubB,Tail} = split_binary(B, Size),
	    NewS = xmerl_scan:cont_state([{Tail, Fname, Fd}|T], S),
	    F(binary_to_list(SubB), NewS);
	_ ->
	    find_good_split(Size-1, B, F, Exception, Fd, Fname, T, S)
    end.

-endif.


fetch({system, URI}, S) ->
    fetch_URI(URI, S);
fetch({public, PublicID, URI}, S) ->
    fetch_URI(URI, S).

fetch_URI(URI, S) ->
    %% assume URI is a filename
    ContS = xmerl_scan:cont_state(S),
    F = 
	case filename:split(URI) of
	    ["/", _|_] ->
		%% absolute path name
		URI;
	    _ ->
		filename:join(S#xmerl_scanner.directory, URI)
	end,
    ?dbg("fetch(~p) -> {file, ~p}.~n", [URI, F]),
    case file:open(F, [read, raw, binary]) of
	{ok, Fd} ->
	    NewS = xmerl_scan:cont_state(
		     [{list_to_binary([]), F, Fd}|ContS], S),
	    {ok, {string, []}, NewS};
	Error ->
	    Error
    end.

close(S) ->
    ContS = xmerl_scan:cont_state(S),
    case ContS of
	[{Fname, eof}|T] ->
	    xmerl_scan:cont_state(T, S);
	[{Sofar, Fname, Fd}|T] ->
	    file:close(Fd),
	    xmerl_scan:cont_state(T, S)
    end.
