%%% BEGIN ce_goat_demo.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2003 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc Demo of the <code>goat</code> subsystem.
%%
%% @end

-module(ce_goat_demo).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-behaviour(ce_goat).

-export([handle_call/3, handle_cast/3]).
-export([start/0]).

%% @spec handle_call(goat_id(), Message::term(), State::term()) ->
%%   {ok, Reply, NewState}

handle_call(GoatId, Message=inc, State) ->
  {ok, {got, Message}, State+1};
handle_call(GoatId, Message=dec, State) ->
  {ok, {got, Message}, State-1}.

%% @spec handle_cast(goat_id(), Message::term(), State::term()) ->
%%   {ok, NewState}

handle_cast(GoatId, Message, State) ->
  io:fwrite("goat ~p got cast ~p while in state ~p~n", [GoatId, Message, State]),
  {ok, State}.

%% @spec start() -> ok
%% @doc Demonstration of the <code>goat</code> subsystem.

start() ->
  ce_goat:start(),
  GoatId = ce_goat:new(?MODULE, 1),
  ce_goat:call(GoatId, inc),
  ce_goat:call(GoatId, inc),
  ce_goat:cast(GoatId, zook), % goat is active from here on, but we can't tell
  ce_goat:call(GoatId, inc),
  ce_goat:call(GoatId, dec),
  ce_goat:call(GoatId, dec),
  ce_goat:call(GoatId, dec),
  ce_goat:cast(GoatId, puppish),
  ce_goat:stop().

%%% END of ce_goat_demo.erl %%%
