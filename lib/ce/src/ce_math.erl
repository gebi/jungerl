%%% BEGIN ce_math.erl %%%
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

%% @doc Mathematics and arithmetic library.
%%
%% @end

-module(ce_math).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([ceil/1, ceil/2, max/2, min/2, sgn/1, fix/1, floor/1]).
-export([gcf/2, lcm/2]).
-export([deg_to_rad/1, rad_to_deg/1]).
-export([base/2]).
-export([fib/1, fact/1]).

%% @spec ceil(number()) -> integer()
%% @doc Returns the next highest integer of a value.
%% Complements <code>trunc</code>.

ceil(X) when integer(X) -> X;
ceil(X) -> trunc(X) + 1.

%% @spec ceil(number(), number()) -> number()
%% @doc Returns the next highest multiple of N.

ceil(X,N) -> ceil(X/N)*N.

%% @spec max(term(), term()) -> term()
%% @doc Returns the greater of two values.

max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

%% @spec min(term(), term()) -> term()
%% @doc Returns the lesser of two values.

min(X, Y) when X > Y -> Y;
min(X, Y) -> X.

%% @spec sgn(number()) -> integer()
%% @doc Returns +1, 0, or -1, depending on the argument's sign.

sgn(N) when N > 0 -> 1;
sgn(N) when N < 0 -> -1;
sgn(_) -> 0.

%% @spec fix(number()) -> integer()
%% @doc Returns the next lowest integer to the given number.

fix(X) when X > 0 -> trunc(X);
fix(X) when X < 0 -> trunc(X)-1;
fix(X) when number(X) -> 0.

%% @spec floor(number()) -> integer()
%% @equiv fix(number())

floor(X) -> fix(X).

%% @spec gcf(integer(), integer()) -> integer()
%% @doc Computes the greatest common factor of two integers.
%% This code was borrowed from
%% <a href="http://www.mmi.ee/~enn/node117.html">Scott Gasch</a>
%% (who borrowed it from Euclid) and translated into Erlang.

gcf(M, N) when integer(M), integer(N) ->
  R = M rem N,
  case R of
    0 -> N;
    _ -> gcf(N, R)
  end.

%% @spec lcm(integer(), integer()) -> integer()
%% @doc Computes the least common multiple of two integers.

lcm(I, J) when integer(I), integer(J) ->
  abs((I * J) div gcf(I, J)).

%% @spec base(Base::integer(), Value::integer() | string()) -> string() | integer()
%% @doc Formats an integer into a string in base N, or
%% parses a string in base N into an integer.  Throws an error if the
%% given string is not a well-formed number in base N.

base(Base, Number) when integer(Number)->
  base_format(Number, Base, []);
base(Base, String) when list(String)->
  base_parse(ce_string:uc(String), Base, 0).

base_format(0, Base, Acc) -> Acc;
base_format(Number, Base, Acc) ->
  Digit = Number rem Base,
  Char = digit_char(Digit),
  base_format(Number div Base, Base, [Char | Acc]).

digit_char(Digit) when Digit >= 0, Digit =< 9 -> Digit + $0;
digit_char(Digit) when Digit > 9 -> Digit - 10 + $A.

base_parse([], Base, Acc) -> Acc;
base_parse([H | T], Base, Acc) ->
  Digit = char_digit(H),
  Acc0 = Acc * Base + Digit,
  base_parse(T, Base, Acc0).

char_digit(Char) when Char >= $0, Char =< $9 -> Char - $0;
char_digit(Char) when Char >= $A, Char =< $F -> Char - $A + 10;
char_digit(Char) when Char >= $a, Char =< $f -> Char - $a + 10.

%% @spec deg_to_rad(number()) -> number()
%% @doc Converts degrees to radians.

deg_to_rad(X) when number(X) -> (X / 360) * (2 * math:pi()).

%% @spec rad_to_deg(number()) -> number()
%% @doc Converts radians to degrees.

rad_to_deg(X) when number(X) -> (X / (2 * math:pi())) * 360.

%% @spec fib(integer()) -> integer()
%% @doc Returns the nth term in the Fibonacci squence
%% <code>[1,1,2,3,5,8,13...]</code>

fib(1) -> 1;
fib(2) -> 1;
fib(N) when N > 2 -> fib(N - 1) + fib(N - 2).

%% @spec fact(integer()) -> integer()
%% @doc Returns the factorial of the given integer.

fact(1) -> 1;
fact(N) when N > 1 -> N * fact(N - 1).

%%% END of ce_math.erl %%%
