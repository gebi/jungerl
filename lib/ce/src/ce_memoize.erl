%%% BEGIN ce_memoize.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2002 Cat's Eye Technologies.  All rights reserved.
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

%% @doc Memoization (cacheing) design pattern.
%%
%% <p>When functions are executed using <code>ce_memoize:apply/4</code>,
%% their results are saved for future reference (subsequent calls to
%% <code>apply/4</code>) in an ets table.  This can be useful for values
%% which are slow to initially calculate and rarely change thereafter,
%% and where speed is desired for every access.</p>
%%
%% @end

-module(ce_memoize).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([start/1]).
-export([apply/4]).
-export([flush/1]).

%% @spec start(memo_name()) -> ok
%%         memo_name() = atom()
%% @doc Starts the memoization service.

start(Name) ->
  ets:new(Name, [public, named_table]),
  ok.

%% @spec apply(memo_name(), module(), function(), args()) -> term()
%%         args() = [term()]
%% @doc Applies a function, memoizing (cacheing) the results.

apply(Name, Module, Function, Args) ->
  case ets:lookup(Name, {Module, Function, Args}) of
    [] ->
      Result = erlang:apply(Module, Function, Args),
      ets:insert(Name, {{Module, Function, Args}, Result}),
      Result;
    [{{Module, Function, Args}, Result}] ->
      Result;
    Else ->
      Else
  end.

%% @spec flush(memo_name()) -> ok
%% @doc Flushes the given memoization table.

flush(Name) ->
  ets:delete_all_objects(Name),
  ok.

%%% END of ce_memoize.erl %%%
