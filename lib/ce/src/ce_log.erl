%%% BEGIN ce_log.erl %%%
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

%% @doc Wrapper around <code>disk_log</code> to simplify logging.
%%
%% <p>This module adds datestamping and terminal output to the
%% usual <code>disk_log</code>.  It also provides a simpler,
%% less flexible interface (e.g. only asynchronous logging is
%% allowed.)</p>
%% 
%% @end

-module(ce_log).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([open/1, write/1, write/2]).

%% @spec open(filename()) -> ok | {error, Reason}
%% @doc Opens a logfile.  This step is optional; if no logfile is
%% opened, messages will only be logged to the terminal.

open(LogFile) ->
  disk_log:open([{name, ?MODULE}, {file, LogFile}]).

%% @spec write(string()) -> ok
%% @doc Logs a message.

write(String) ->
  {Date, Time} = calendar:local_time(),
  DateString = ce_calendar:logfile_datetime({Date, Time}),
  {Module, Function, Arity} = case tl(ce_lib:call_stack()) of
    [] ->
      {'?', '?', '?'};
    [Head | _] ->
      Head
  end,
  Term = {{Date, Time}, {Module, Function, Arity}, String},
  case disk_log:info(?MODULE) of
    List when is_list(List) ->
      disk_log:alog(?MODULE, Term);
    {error, Reason} ->
      ok
  end,
  io:fwrite("~s [~p:~p/~p] ~s~n",
    [DateString, Module, Function, Arity, String]),
  ok.

%% @spec write(string(), [term()]) -> ok
%% @doc Logs a formatted message.

write(Fmt, Args) ->
  write(lists:flatten(io_lib:format(Fmt, Args))).

%%% END of ce_log.erl %%%
