%%% BEGIN ce_event.erl %%%
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

%% @doc Super-simple generic synchronous event handler.
%%
%% <p>This module differs from <code>gen_event</code> in several significant
%% ways:
%% <ul>
%% <li>each event handler is a <code>fun/2</code></li> and is passed the
%% arguments and state given to <code>notify</code>
%% <li>each event (class, not instance) must have a unique name (event id)</li>
%% <li>fully synchronous (every event handler is executed in the
%%     process that causes the event)</li>
%% <li>each event is executed in order of its priority</li>
%% <li>each event handler can cancel all subsequent events and
%%     immediately return a value to the cause of the event</li>
%% </ul>
%% </p>
%%
%% <p>Event handlers are passed the state, initially given by the cause of
%% the event.  Each handler may return:
%% <ul>
%% <li><code>{ok, NewState}</code> to change the state and continue processing
%% this event with other handlers;</li>
%% <li><code>{done, NewState}</code> to change the state and cancel the processing
%% of the other handlers; or</li>
%% <li><code>{error, Error}</code> to indicate an error occurred.</li>
%% </ul></p>
%%
%% @end

-module(ce_event).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([new_manager/1, add_handler/4, add_handlers/2, notify/4]).
-export([test/0]).

new_manager(Options) ->
  dict:new().

add_handler(Manager, EventName, Priority, Handler) ->
  NewEventList = case dict:find(EventName, Manager) of
    {ok, EventList} ->
      case Priority of
        first ->
          [Handler] ++ EventList;
        last ->
          EventList ++ [Handler]
      end;
    error ->
      [Handler]
  end,
  NewManager = dict:store(EventName, NewEventList, Manager),
  {ok, NewManager}.

add_handlers(Manager, Handlers) ->
  NewManager = lists:foldl(fun({EventName, Priority, Handler}, InnerManager) ->
    case add_handler(InnerManager, EventName, Priority, Handler) of
      {ok, NewManager} -> NewManager;
      _ -> InnerManager
    end
  end, Manager, Handlers),
  {ok, NewManager}.

notify(Manager, EventName, Args, State) ->
  fold_handlers(dict:fetch(EventName, Manager), Args, State).

fold_handlers([], Args, State) ->
  {ok, State};
fold_handlers([Handler | Tail], Args, State) ->
  case Handler(Args, State) of
    {ok, HandlerState} ->
      fold_handlers(Tail, Args, HandlerState);
    {done, HandlerState} ->
      {ok, HandlerState};
    {error, Reason} ->
      {error, Reason}
  end.

test() ->
  ce_random:seed(),
  {ok, Manager} = ce_event:add_handlers(ce_event:new_manager([]), [
  {http, last,
    fun(Args, State) ->
      io:fwrite("authenticating ~p~n", [State]),
      case random:uniform(3) of
        1 ->
          {error, no_auth};
        _ ->
          {ok, State}
      end
    end},
  {http, last,
    fun(Args, State) ->
      io:fwrite("sending http resource ~p~n", [State]),
      case random:uniform(3) of
        1 ->
          {error, not_found};
        _ ->
          {ok, State}
      end
    end},
  {http, last,
    fun(Args, State) ->
      io:fwrite("cleaning up after http ~p~n", [State]),
      {ok, State}
    end}
  ]),
  loop(Manager).

loop(Manager) ->
  timer:sleep(1000),

  Result = notify(Manager, http, [arg1, arg2], {get, "/images/funky.jpg"}),

  io:fwrite("got ~p~n", [Result]),
  loop(Manager).

%%% END of ce_event.erl %%%
