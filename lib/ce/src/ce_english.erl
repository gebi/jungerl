%%% BEGIN ce_english.erl %%%
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

%% @doc English grammar and syntax library.
%%
%% @end

-module(ce_english).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([is_vowel/1]).
-export([def_art/1, indef_art/1, plural/1, possessive/1]).

%% @spec is_vowel(char()) -> true | false
%% @doc Determines whether the given character is an English vowel or not.
%% Since this is intended mainly for testing for vowels at the beginning
%% and end of a word, "<b>y</b>" is not considered a vowel by this heuristic.

is_vowel($a) -> true;
is_vowel($e) -> true;
is_vowel($i) -> true;
is_vowel($o) -> true;
is_vowel($u) -> true;
is_vowel($A) -> true;
is_vowel($E) -> true;
is_vowel($I) -> true;
is_vowel($O) -> true;
is_vowel($U) -> true;
is_vowel([]) -> false;
is_vowel([H | T]) -> is_vowel(H);
is_vowel(_) -> false.

%% @spec def_art(string()) -> string() | {error, Reason}
%% @doc Prepends the appropriate definate article to the given string.
%% The string is assumed to be a single word or phrase.

def_art("") ->
  {error, not_a_word};
def_art(Word) ->                        % --- example ---
  "the " ++ Word.                       % cathedral -> the cathedral

%% @spec indef_art(string()) -> string() | {error, Reason}
%% @doc Prepends the appropriate indefinate article to the given string.
%% The string is assumed to be a single word or phrase.

indef_art("") ->
  {error, not_a_word};
indef_art(Word) ->
  case is_vowel(hd(Word)) of            % --- example ---
    true ->
      "an " ++ Word;                    % apple -> an apple
    false ->
      "a " ++ Word                      % tournament -> a tournament
  end.

%% @spec plural(string()) -> string() | {error, Reason}
%% @doc Returns the plural form of the given string.
%% The string is assumed to be a single word or phrase.

plural("") ->
  {error, not_a_word};
plural(Word) ->
  case ce_string:lc(Word) of
    "mouse"      ->   "mice";
    "ox"         ->   "oxen";
    "phenomenon" ->   "phenomena";
    "succubus"   ->   "succubi";
    "incubus"    ->   "incubi";
    "us"         ->   "us";             % degenerate case
    _            ->   plural0(Word)
  end.
plural0(Word) ->
  case lists:reverse(Word) of           % --- example ---
    "su" ++ Tail ->
      case is_vowel(hd(Tail)) of
        true ->
          lists:reverse(Tail) ++ "i";   % radius -> radii
        false ->
          Word ++ "es"                  % bus -> buses
      end;
    "xir" ++ Tail ->
      lists:reverse(Tail) ++ "rices";   % matrix -> matrices
    "y" ++ Tail ->
      lists:reverse(Tail) ++ "ies";     % hobby -> hobbies
    "s" ++ _ ->
      Word ++ "es";                     % carcass -> carcasses
    "x" ++ _ ->
      Word ++ "es";                     % sex -> sexes
    _ ->
      Word ++ "s"                       % yacht -> yachts
  end.

%% @spec possessive(string()) -> string() | {error, Reason}
%% @doc Returns the possessive form of the given string.
%% The string is assumed to be a single word or phrase.

possessive("") ->
  {error, not_a_word};
possessive(Word) ->
  case ce_string:lc(Word) of
    "him"        ->   "his";
    "he"         ->   "his";
    "her"        ->   "her";
    "she"        ->   "her";
    "them"       ->   "their";
    "they"       ->   "their";
    "it"         ->   "its";
    "us"         ->   "our";
    _            ->   possessive0(Word)
  end.
possessive0(Word) ->
  case lists:reverse(Word) of
    "s" ++ _ ->
       Word ++ "'es";
    "x" ++ _ ->
       Word ++ "'es";
    _ ->
       Word ++ "'s"
  end.

%%% END of ce_english.erl %%%
