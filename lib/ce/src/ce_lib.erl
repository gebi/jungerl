%%% BEGIN ce_lib.erl %%%
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

%% @doc Miscellaneous spiffy stuff library.
%%
%% @end

-module(ce_lib).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([iterate/4, repeat/2]).
-export([breed/1]).
-export([eval/1]).
-export([clean/2, place/2]).
-export([attempt/2, attempt/3]).
-export([compose/1]).
-export([to_integer/2, to_float/2, to_string/1, to_string/2]).

%% @spec iterate(integer(), integer(), fun(), term()) -> term()
%% @doc Implements iteration with a counter variable.
%% Not dissimilar to 'for loops' in other languages.

iterate(I, Q, Fun, Acc) when Q < I -> Acc;
iterate(I, N, Fun, Acc) ->
  Acc0 = Fun(I, Acc),
  iterate(I+1, N, Fun, Acc0).

%% @spec repeat(integer(), fun()) -> [term()]
%% @doc Implements opaque finite repetition.
%% Not dissimilar to the 'repeat' loop in LOGO.
%% This version is tail-recursive and returns a list of all
%% evaluated results.

repeat(N, F) when integer(N) -> lists:reverse(repeat(N, F, [])).

repeat(0, F, A) -> A;
repeat(N, F, A) -> repeat(N - 1, F, [F() | A]).

%% @spec breed(term()) -> term()
%% @doc Returns a copy of a deep aggregate of data
%% with each Fun/0 object contained within replaced by
%% the result of running it.

breed([H|T]) -> [breed(H) | breed(T)];
breed(F) when function(F) -> apply(F, []);
breed(T) when tuple(T) -> breed_tuple(T, size(T));
breed(Q) -> Q.

breed_tuple(T, 0) -> {};
breed_tuple(T, N) ->
  erlang:append_element(breed_tuple(T, N-1), breed(element(N, T))).

%% @spec eval(string()) -> term()
%% @doc Evaluates a string as an Erlang expression.
%% This adds reflectivity to Erlang with a simple, Perl-like
%% interface.

eval(S) -> eval(S, []).
eval(S, PT) when list(S) ->
  {ok, T, _} = erl_scan:string(S),
  {ok, C} = erl_parse:parse_exprs(T),
  % execute all parse transforms in PT
  {value, Y, _} = erl_eval:exprs(C, erl_eval:bindings(erl_eval:new_bindings())),
  Y.

%% @spec place(term(), term()) -> ok
%% @doc Places the given value in the process dictionary iff the given key
%% does not yet exist.  Exclusive use of place/2 instead of erlang:put/2
%% imposes the restriction of single-assignment on the process dictionary.

place(Key, Value) ->
  case erlang:get(Key, Value) of
    undefined ->
      put(Key, Value),
      ok;
    _ ->
      throw({error, already_set})
  end.

%% @spec clean(fun(), [{key(), value()}]) -> term()
%% @doc Evaluate a given Fun with a new, temporary process dictionary.
%% The new process dictionary will contain only the values given to clean/2,
%% and will be reset to the old process dictionary after the Fun has been
%% evaluated.  Simulates local variables with dynamic scope as found in 
%% many non-single-assignment languages.

clean(F, L) ->
  ProcessDictionary = erase(),
  lists:foreach(fun({K,V}) -> put(K,V) end, L),
  Result = F(),
  erase(),
  lists:foreach(fun({K,V}) -> put(K,V) end, ProcessDictionary),
  Result.

%% @spec attempt(function(), [term()]) -> {ok, term()} | {error, term()}
%% @doc Catches errors from an error-throwing function.
%% Thanks to Thomas Arts for this code.

attempt(F, Args) ->
  case catch apply(F, Args) of
    {'EXIT', Why} -> {error, Why};
            Other -> {ok, Other}
  end.

%% @spec attempt(function(), [term()], term()) -> term()
%% @doc Catches errors and returns a default value if error thrown.
%% Also can be used with functions which return {ok, Result} | {error, Why}.

attempt(F, Args, Default) ->
  case catch apply(F, Args) of
    {'EXIT', _} -> Default;
     {error, _} -> Default;
    {ok, Value} -> Value;
          Other -> Other
  end.

%% @spec compose([function()]) -> function()
%% @doc Composes a function by chaining the results of a list of functions.
%% Thanks to Alex Peake for this code.

compose([Fn]) -> Fn;
compose([Fn | FnTail]) ->
  fun(Args) ->
    apply(Fn, [apply(compose(FnTail), [Args])])
  end.

%% @spec to_integer(term(), integer()) -> integer()
%% @doc Tries to convert any term into an integer, using default value if it
%% cannot be done.

to_integer(F, D) when is_float(F) ->
  trunc(F + 0.5);
to_integer(I, D) when is_integer(I) ->
  I;
to_integer(S, D) when is_list(S) ->
  case catch list_to_integer(S) of
    F when integer(F) -> F;
    {'EXIT', Reason} -> D
  end;
to_integer(X, D) -> D.

%% @spec to_float(term(), float()) -> float()
%% @doc Tries to convert any term into a float, using default value if it
%% cannot be done.

to_float(F, D) when is_float(F) ->
  F;
to_float(I, D) when is_integer(I) ->
  I * 1.0;
to_float(S, D) when is_list(S) ->
  case catch list_to_float(S) of
    F when number(F) -> F;
    {'EXIT', Reason} ->
      case catch list_to_integer(S) of
        G when integer(G) -> G * 1.0;
	{'EXIT', Reason0} -> D
      end
  end;
to_float(X, D) -> D.

to_string(X) -> to_string(X, "").

%% @spec to_string(term(), string()) -> string()
%% @doc Tries to convert any term into a string, using default value if it
%% cannot be done.

to_string(F, D) when is_float(F) ->
  float_to_list(F);
to_string(I, D) when is_integer(I) ->
  integer_to_list(I);
to_string(A, D) when is_atom(A) ->
  atom_to_list(A);
to_string(S, D) when is_list(S) -> S;
to_string(X, D) -> io_lib:format("~w", [X]).

%%% END of ce_lib.erl %%%
