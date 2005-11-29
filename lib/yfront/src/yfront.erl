%%%-------------------------------------------------------------------
%%% Created : 15 Nov 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : Yfront - A Yaws toolbox
%%%
%%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%%% 
%%% @doc <b>yfront</b> is a toolbox for building Yaws/Mnesia web apps.
%%%
%%% @end
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(yfront).

%% External exports
-export([authorization_hdr/1,
	 auth_redirect/0, auth_redirect/1, auth_digest_redirect/0,
	 setup_sequence/0, init_sequence/1, init_sequence/2, 
	 sequence/1, sequence/2]).

-include("yaws_api.hrl").

%%% sequence table record
%%% (taken from http://www.erlang.org/ml-archive/erlang-questions/200508/msg00291.html)
-record(sequence, {key, index}).

%%% @doc Creates sequence table on local node.
setup_sequence() ->
    setup_sequence([node()]).

%%% @doc Creates sequence table on Nodes.
setup_sequence(Nodes) when list(Nodes) ->
    create_sequence(Nodes).

create_sequence(Nodes) ->
    mnesia:create_table(sequence, [{type, set},
				   {disc_copies, Nodes},
				   {attributes, record_info(fields, sequence)}]).

%%% @doc Initiate sequence counter to: 1
init_sequence(Name) ->
    init_sequence(Name, 1).

%%% @doc Initiate sequence counter to: Value
init_sequence(Name, Value) ->
    {atomic, ok} =
	mnesia:transaction(fun() ->
				   mnesia:write(#sequence{key=Name, index=Value})
			   end),
    ok.

%%% @doc Returns current value for sequence Name 
%%%      and increment. Sequence is created if it does not exists, 
%%%      and initial value 0 is returned.
sequence(Name) ->
     sequence(Name, 1).

%%% @doc Increment sequence with Inc
sequence(Name, Inc) ->
     mnesia:dirty_update_counter(sequence, Name, Inc).






%%% @doc Return a 401 Authorization-Required status code, require Basic-Authentication.
auth_redirect() ->
    auth_redirect("").

%%% @doc Works as {@link auth_redirect/0}, specify realm.
auth_redirect(Realm) ->
    auth_redirect0("realm=\""++Realm++"\"").

auth_redirect0(Realm) ->
    [{status, 401},
     {header, "WWW-Authenticate: Basic "++Realm}].

%%% FIXME !!
auth_digest_redirect() ->
    %% Realm = "  realm=\""++Realm0++"\","
    auth_digest_redirect0("").

auth_digest_redirect0(Realm) ->
    [{status, 401},
     {header, 
      "WWW-Authenticate: Digest "++Realm++
      "  nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\","
      "  qop=\"auth\","
      "  opaque=\"5ccc069c403ebaf9f0171e9517f40e41\""
     }].

    
%%% @private
authorization_hdr(A) ->
    %%io:format("~n+++ Arg=~p~n", [A]),
    (A#arg.headers)#headers.authorization.


