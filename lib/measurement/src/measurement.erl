%%% BEGIN measurement.erl %%%
%%%
%%% measurement - Measurement ADT for Erlang
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

%% @doc Measurement ADT for Erlang.
%%
%% <p>This module implements an abstract data type in Erlang which
%% represents a <i>measurement</i> taken from the observed world.
%% These measurements have, as part of their make-up, both a <i>scalar</i>
%% component which determines their magnitude, and a <i>units of
%% measurement</i> (<i>UoM</i>) component which determines their
%% dimensionality (and may further determine their magnitude within
%% that dimensionality.)</p>
%%
%% <p>Simple arithmetic may be done on measurements, including all the
%% usual operations (addition, subtraction, multiplication, division,
%% and exponentiation,) and during these operations both the scalar
%% component and the UoM component are handled correctly.</p>
%%
%% <p>This module is by no means complete.  Many features must be added
%% to it in order to make this ADT reasonably versatile.  These include:
%% <ul>
%%   <li>converting between different types of compatible units:
%%   <ul>
%%     <li>explicit conversions, such as between feet and meters</li>
%%     <li>implicit conversions, such as when feet are added to meters</li>
%%     <li>UoM aliasing, such as between Joules and kg-m^2/sec^2</li>
%%   </ul></li>
%%   <li>tracking error (tolerance)</li>
%%   <li>parsing measurements from strings</li>
%% </ul></p>
%%
%% @end

-module(measurement).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([new/1, new/2]).
-export([scalar/1, unit/1]).
-export([equal/2, unequal/2, greater_than/2, less_than/2,
	 greater_than_or_equal/2, less_than_or_equal/2]).
-export([add/2, subtract/2, multiply/2, divide/2, exponent/2]).
-export([format/1, fwrite/2]).

-record(measurement,
{
  scalar,  % floating point value representing scalar portion
  uom      % dictionary where dimensions are keys and powers are values
}).

%%% CONSTRUCTORS %%%

%% @spec new(number() | unit()) -> measurement()
%% @doc Creates and returns a new measurement object, either a scalar
%% if a number is given, or a measurement of one unit if a unit is given.

new(S) when atom(S) -> new(1, [{S, 1}]);
new(S) -> new(S, []).

%% @spec new(number(), [{unit(), dimension()}]) -> measurement()
%%         unit() = atom()
%%         dimension() = integer()
%%         measurement() = measurement()
%% @doc Creates and returns a new measurement object with both a scalar
%% and a unit component.

new(S, L) when list(L) ->
  #measurement{
    scalar = S,
    uom = dict:from_list(L)
  }.

%%% PRIMITIVES %%%

%% @spec scalar(measurement()) -> number()
%% @doc Extracts the scalar portion of a measurement.

scalar(M) when record(M, measurement) ->
  M#measurement.scalar.

%% @spec unit(measurement()) -> measurement()
%% @doc Extracts the unit of a measurement (another measurement whose scalar
%% is one).

unit(M) when record(M, measurement) ->
  #measurement{scalar = 1, uom = M#measurement.uom}.

%%% COMPARISON OPERATIONS %%%

%% @spec equal(measurement(), measurement()) -> true | false | {error, Reason}
%% @doc Determines whether two measurements are equal.

equal(M1, M2) when record(M1, measurement), record(M2, measurement) ->
  case M1#measurement.uom == M2#measurement.uom of
    true ->
      M1#measurement.scalar == M2#measurement.scalar;
    false ->
      {error, incompatible_units}
  end;
equal(M1, M2) when record(M1, measurement) -> equal(M1, new(M2));
equal(M1, M2) when record(M2, measurement) -> equal(new(M1), M2);
equal(M1, M2) -> M1 == M2.

%% @spec unequal(measurement(), measurement()) ->
%%         true | false | {error, Reason}
%% @doc Determines whether two measurements are not equal.

unequal(M1, M2) ->
  case equal(M1, M2) of
    true -> false;
    false -> true;
    Else -> Else
  end.

%% @spec greater_than(measurement(), measurement()) ->
%%         true | false | {error, Reason}
%% @doc Determines whether the first measurement is greater than the second.

greater_than(M1, M2) when record(M1, measurement), record(M2, measurement) ->
  case M1#measurement.uom == M2#measurement.uom of
    true ->
      M1#measurement.scalar > M2#measurement.scalar;
    false ->
      {error, incompatible_units}
  end;
greater_than(M1, M2) when record(M1, measurement) -> greater_than(M1, new(M2));
greater_than(M1, M2) when record(M2, measurement) -> greater_than(new(M1), M2);
greater_than(M1, M2) -> M1 > M2.

%% @spec greater_than_or_equal(measurement(), measurement()) ->
%%         true | false | {error, Reason}
%% @doc Determines whether the first measurement is greater than
%% or equal to the second.

greater_than_or_equal(M1, M2) ->
  case greater_than(M1, M2) of
    true -> true;
    false -> equal(M1, M2);
    Else -> Else
  end.

%% @spec less_than(measurement(), measurement()) ->
%%         true | false | {error, Reason}
%% @doc Determines whether the first measurement is less than the second.

less_than(M1, M2) when record(M1, measurement), record(M2, measurement) ->
  case M1#measurement.uom == M2#measurement.uom of
    true ->
      M1#measurement.scalar < M2#measurement.scalar;
    false ->
      {error, incompatible_units}
  end;
less_than(M1, M2) when record(M1, measurement) -> less_than(M1, new(M2));
less_than(M1, M2) when record(M2, measurement) -> less_than(new(M1), M2);
less_than(M1, M2) -> M1 < M2.

%% @spec less_than_or_equal(measurement(), measurement()) ->
%%         true | false | {error, Reason}
%% @doc Determines whether the first measurement is less than
%% or equal to the second.

less_than_or_equal(M1, M2) ->
  case less_than(M1, M2) of
    true -> true;
    false -> equal(M1, M2);
    Else -> Else
  end.

%%% ORDER 0 ARITHMETIC OPERATIONS %%%

%% @spec add(measurement(), measurement()) ->
%%         measurement() | {error, Reason}
%% @doc Yields the sum of two measurements.

add(M1, M2) when record(M1, measurement), record(M2, measurement) ->
  case M1#measurement.uom == M2#measurement.uom of
    true ->
      #measurement{
        scalar = M1#measurement.scalar + M2#measurement.scalar,
        uom    = M1#measurement.uom
      };
    false ->
      {error, incompatible_units}
  end;
add(M1, M2) when record(M1, measurement) -> add(M1, new(M2));
add(M1, M2) when record(M2, measurement) -> add(new(M1), M2);
add(M1, M2) when atom(M1) -> add(new(M1), M2);
add(M1, M2) when atom(M2) -> add(M1, new(M2));
add(M1, M2) -> M1 + M2.

%% @spec subtract(measurement(), measurement()) ->
%%         measurement() | {error, Reason}
%% @doc Yields the difference between two measurements.

subtract(M1, M2) when record(M1, measurement), record(M2, measurement) ->
  case M1#measurement.uom == M2#measurement.uom of
    true ->
      #measurement{
        scalar = M1#measurement.scalar - M2#measurement.scalar,
        uom    = M1#measurement.uom
      };
    false ->
      {error, incompatible_units}
  end;
subtract(M1, M2) when record(M1, measurement) -> subtract(M1, new(M2));
subtract(M1, M2) when record(M2, measurement) -> subtract(new(M1), M2);
subtract(M1, M2) when atom(M1) -> subtract(new(M1), M2);
subtract(M1, M2) when atom(M2) -> subtract(M1, new(M2));
subtract(M1, M2) -> M1 - M2.

%%% ORDER 1 ARITHMETIC OPERATIONS %%%

%% @spec multiply(measurement(), measurement()) ->
%%         measurement() | {error, Reason}
%% @doc Yields the product of two measurements.

multiply(M1, M2) when record(M1, measurement), record(M2, measurement) ->
  #measurement{
    scalar = M1#measurement.scalar * M2#measurement.scalar,
    uom    = dict:filter(fun(_,V) -> V =/= 0 end,
               dict:merge(fun(_,A,B) -> A + B end,
	         M1#measurement.uom, M2#measurement.uom))
  };
multiply(M1, M2) when record(M1, measurement) -> multiply(M1, new(M2));
multiply(M1, M2) when record(M2, measurement) -> multiply(new(M1), M2);
multiply(M1, M2) when atom(M1) -> multiply(new(M1), M2);
multiply(M1, M2) when atom(M2) -> multiply(M1, new(M2));
multiply(M1, M2) -> M1 * M2.

%% @spec divide(measurement(), measurement()) ->
%%         measurement() | {error, Reason}
%% @doc Yields the ratio between two measurements.

divide(M1, M2) when record(M1, measurement), record(M2, measurement) ->
  #measurement{
    scalar = M1#measurement.scalar / M2#measurement.scalar,
    uom    = dict:filter(fun(_,V) -> V =/= 0 end,
               dict:merge(fun(_,A,B) -> A + B end, M1#measurement.uom,
	         dict:map(fun(K,V) -> -V end, M2#measurement.uom)))
  };
divide(M1, M2) when record(M1, measurement) -> divide(M1, new(M2));
divide(M1, M2) when record(M2, measurement) -> divide(new(M1), M2);
divide(M1, M2) when atom(M1) -> divide(new(M1), M2);
divide(M1, M2) when atom(M2) -> divide(M1, new(M2));
divide(M1, M2) -> M1 / M2.

%%% ORDER 2 ARITHMETIC OPERATIONS %%%

%% @spec exponent(measurement(), measurement()) ->
%%         measurement() | {error, Reason}
%% @doc Raises a measurement to the power of another measurement.
%% This only produces a sensible result if the second measurement
%% is a scalar and an integer.

exponent(M1, M2) when record(M1, measurement), record(M2, measurement) ->
  % count key/value pairs in M2's unit dictionary
  case dict:fold(fun(_,_,A) -> A + 1 end, 0, M2#measurement.uom) of
    0 ->
      case M2#measurement.scalar of
        I when is_integer(I) ->
          #measurement{
            scalar = math:pow(M1#measurement.scalar, M2#measurement.scalar),
            uom    = dict:filter(fun(_,V) -> V =/= 0 end,
                       dict:map(fun(_,V) -> V * M2#measurement.scalar end,
                         M1#measurement.uom))
          };
	_ ->
	  {error, exponent_must_be_integer}
      end;
    _ ->
      {error, exponent_must_be_scalar}
  end;
exponent(M1, M2) when record(M1, measurement) -> exponent(M1, new(M2));
exponent(M1, M2) when record(M2, measurement) -> exponent(new(M1), M2);
exponent(M1, M2) when atom(M1) -> exponent(new(M1), M2);
exponent(M1, M2) when atom(M2) -> exponent(M1, new(M2));
exponent(M1, M2) -> math:pow(M1, M2).

%% @spec format(measurement()) -> string()
%% @doc Render a measurement in human-readable form.

format(M) when record(M, measurement) ->
  NumerUom = dict:filter(fun(K, V) when V > 0 -> true; (K, V) -> false end,
                         M#measurement.uom),
  DenomUom = dict:map(fun(K,V) -> -V end,
               dict:filter(fun(K, V) when V < 0 -> true; (K, V) -> false end,
                           M#measurement.uom)),
  NumerCount = length(dict:fetch_keys(NumerUom)),
  DenomCount = length(dict:fetch_keys(DenomUom)),
  Fun = fun(K, V, A) ->
	  case V of
            1 -> A ++ io_lib:fwrite("~w ", [K]);
            _ -> A ++ io_lib:fwrite("~w^~w ", [K, V])
	  end
        end,
  UnitString = case {NumerCount, DenomCount} of
    {0,  0} -> "";
    {NC, 0} -> dict:fold(Fun, [], NumerUom);       
    {0, DC} -> "/" ++ dict:fold(Fun, [], DenomUom);
    {NC,DC} -> dict:fold(Fun, [], NumerUom) ++ "/" ++
               dict:fold(Fun, [], DenomUom)
  end,
  io_lib:fwrite("~w ~s", [scalar(M), UnitString]);
format(M) -> io_lib:fwrite("~w", [M]).

%% @spec fwrite(string(), [term()]) -> ok
%% @doc Wrapper for <code>io:fwrite</code> which is sensitive to measurements.

fwrite(Format, List) ->
  io:fwrite(Format, lists:map(fun(M) when record(M, measurement) ->
                                   format(M);
				 (M) -> M
                              end, List)).

%%% END of measurement.erl %%%
