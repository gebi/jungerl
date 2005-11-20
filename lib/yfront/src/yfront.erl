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
	 auth_redirect/0, auth_redirect/1,
	 auth_digest_redirect/0]).

-include("yaws_api.hrl").


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


