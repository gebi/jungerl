%%% BEGIN ce_calendar.erl %%%
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

%% @doc Calendar library.  This module provides functions for
%% generating date/time strings in different formats.
%%
%% @end

-module(ce_calendar).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([rfc_1123_datetime/0, rfc_1123_datetime/1]).
-export([logfile_datetime/0]).
-export([timestamp/0]).

%% @spec rfc_1123_datetime() -> string()
%% @equiv rfc_1123_datetime(calendar:universal_time())

rfc_1123_datetime() -> rfc_1123_datetime(calendar:universal_time()).

%% @spec rfc_1123_datetime({date(), time()}) -> string()
%% @doc Returns a date/time string formatted in accordance to RFC 1123.
%% This sort of date/time looks like "Sun, 06 Nov 1994 08:23:19 GMT".

rfc_1123_datetime({{Y, M, D}, {H, I, S}}) ->
  Dow = case calendar:day_of_the_week(Y, M, D) of
    1 -> "Mon";
    2 -> "Tue";
    3 -> "Wed";
    4 -> "Thu";
    5 -> "Fri";
    6 -> "Sat";
    7 -> "Sun"
  end,
  Month = case M of
    1 -> "Jan";
    2 -> "Feb";
    3 -> "Mar";
    4 -> "Apr";
    5 -> "May";
    6 -> "Jun";
    7 -> "Jul";
    8 -> "Aug";
    9 -> "Sep";
   10 -> "Oct";
   11 -> "Nov";
   12 -> "Dec"
  end,
  lists:flatten(io_lib:format("~s, ~2.2.0w ~s ~w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
                              [Dow, D, Month, Y, H, I, S])).

%% @spec logfile_datetime() -> string()
%% @doc Returns a date/time string which is suitable for a log file.

logfile_datetime() ->
  {{Y, M, D}, {H, I, S}} = calendar:local_time(),
  lists:flatten(io_lib:format("~w.~2.2.0w.~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                             [Y, M, D, H, I, S])).

%% @spec timestamp() -> string()
%% @doc Returns a timestamp string which is both human-readable and unique.
%% This sort of date/time looks like "20020814.184025.148330".

timestamp() ->
  {{Y, M, D}, {H, I, S}} = calendar:local_time(),
  {_, _, Us} = erlang:now(),
  lists:flatten(io_lib:format("~w~2.2.0w~2.2.0w.~2.2.0w~2.2.0w~2.2.0w.~6.6.0w",
                             [Y, M, D, H, I, S, Us])).

%%% END of ce_calendar.erl %%%
