%%% BEGIN ce_goat.erl %%%
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

%% @doc Subsystem for <code>goat</code>s.
%%
%% <p>A <code>goat</code> represents a middleground between a (dynamic) server
%% and a (static) data structure.  Code that accesses a <code>goat</code> doesn't need
%% to know or care whether the goat is dynamic or static, and indeed any
%% given goat may switch from one state to the other throughout its lifetime.</p>
%%
%% <p>The intent of this module is to allow for an (experimental) coding style
%% which can be described as "Everything is a process, except what isn't, except
%% everything is treated the same regardless of that."</p>
%%
%% <p>This approach has also been described (unflatteringly) as
%% "bastard semi-threaded / semi-not-threaded" programming.
%% If it's not your cup of tea, don't use it: the Erlang runtime system
%% is quite adequate for models along the lines of
%% "everything is always really a process."</p>
%%
%% <p><code>goat</code>s have a <code>gen_server</code>ish feel.  They can be
%% sent (synchronous) calls and (asynchronous) casts.  When a <code>goat</code>
%% is active, it will be assigned a process which will react to these requests.
%% When it is not active, it will react to calls, but it will become active
%% before reacting to any casts.  It can also deactivate itself after a
%% timeout.  It, and anything that accesses it, is otherwise oblivious to
%% whether it is active or not.</p>
%%
%% <p>See also Ulf Wiger's <code>mdisp</code> package.</p>
%%
%% <p>The name <code>goat</code> was chosen because goats are cute and have
%% square pupils.</p>
%%
%% @end

-module(ce_goat).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([behaviour_info/1]).

-export([start/0, stop/0]).
-export([new/2, new/3, delete/1]).
-export([call/2, cast/2]).
-export([goat_handler/2]).

-record(goat,
{
  id      = undefined,     % the unique identifier for the goat
  pid     = undefined,     % the process of the goat, or undefined when static
  handler = undefined,     % module which contains callbacks for the goat
  state   = undefined,     % the internal state of the goat
  timeout = infinity       % how long this goat will stay active after a cast
}).

%% @spec behaviour_info(callbacks) -> [{atom(), integer()}]
%% @doc Defines the callback interface.

behaviour_info(callbacks) ->
  [
    {handle_call, 3},
    {handle_cast, 3}
  ].

%% @spec start() -> ok
%% @doc Starts the <code>goat</code> subsystem.  Should be called before any
%% other functions in this module are used.

start() ->
  ets:new(?MODULE, [public, named_table, {keypos, 2}]),
  ets:insert(?MODULE, {goat, new_id, 0}),
  ok.

%% @spec stop() -> ok
%% @doc Stops the <code>goat</code> subsystem.

stop() ->
  ets:delete(?MODULE),
  ok.

%% @spec new(module(), state()) -> goat_id()
%% @equiv new(module(), state(), infinity)

new(Handler, State) when is_atom(Handler) ->
  GoatId = new_id(),
  Goat = #goat{ id = GoatId, handler = Handler, state = State },
  ets:insert(?MODULE, Goat),
  GoatId.

%% @spec new(module(), state(), timeout()) -> goat_id()
%%         module() = atom()
%%         state() = term()
%%         timeout() = integer()
%%         goat_id() = goat_id()
%% @doc Creates a new <code>goat</code> with the given handler module.  The handler
%% will be used to react to all calls and casts sent to the <code>goat</code>.

new(Handler, State, Timeout) when is_atom(Handler), is_integer(Timeout) ->
  GoatId = new_id(),
  Goat = #goat{
    id      = GoatId,
    handler = Handler,
    state   = State,
    timeout = Timeout
  },
  ets:insert(?MODULE, Goat),
  GoatId.

%% @spec new_id() -> goat_id()
%% @doc Generates a new, unique identifier for a <code>goat</code>.

new_id() ->
  ets:update_counter(?MODULE, new_id, 1).

%% @spec delete(goat_id()) -> ok | {error, Reason}
%% @doc Removes a <code>goat</code> from existence.

delete(GoatId) ->
  deactivate(GoatId),
  ets:delete(?MODULE, GoatId).

%% @spec retrieve(goat_id()) -> {ok, goat()} | {error, Reason}
%%         goat() = goat()
%% @doc Retrieves a <code>goat</code> based on it's identifier.

retrieve(GoatId) ->
  case ets:lookup(?MODULE, GoatId) of
    [] -> {error, not_found};
    [Goat] -> {ok, Goat}
  end.

%% @spec activate(goat_id()) -> ok | {error, Reason}
%% @doc Causes a <code>goat</code> to become active.  The <code>goat</code>
%% is assigned a process which will react to calls and casts.

activate(GoatId) ->
  {ok, #goat{ pid = Pid, handler = Handler, state = State } = Goat} = retrieve(GoatId),
  case Pid of
    undefined ->
      NewPid = spawn_link(?MODULE, goat_handler, [Handler, GoatId]),
      NewGoat = Goat#goat{ pid = NewPid },
      ets:insert(?MODULE, NewGoat),
      NewPid ! synchronize,
      ok;
    P when is_pid(P) ->
      {error, already_active}
  end.

%% @spec deactivate(goat_id()) -> ok | {error, Reason}
%% @doc Causes a <code>goat</code> to become deactivated.  Any process
%% assigned to the <code>goat</code> will be killed.

deactivate(GoatId) ->
  {ok, #goat{ pid = Pid } = Goat} = retrieve(GoatId),
  case Pid of
    undefined ->
      {error, already_inactive};
    P when is_pid(P) ->
      exit(Pid, deactivated),
      NewGoat = Goat#goat{ pid = undefined },
      ets:insert(?MODULE, NewGoat),
      ok
  end.

%% @spec call(goat_id(), term()) -> {ok, Reply} | {error, Reason}
%% @doc Executes a synchronous call to a <code>goat</code>.
%% This does not necessarily require the <code>goat</code> be active.

call(GoatId, Message) ->
  {ok, #goat{ pid = Pid } = Goat} = retrieve(GoatId),
  case Pid of
    undefined ->
      static_call(Goat, Message);
    _ ->
      Ref = erlang:make_ref(),
      Pid ! {{self(), call, Ref}, Message},
      receive
        {{Pid, call, Ref}, Reply} ->
          {ok, Reply}
      end
  end.
static_call(#goat{ id = GoatId, handler = Handler, state = State } = Goat, Message) ->
  case Handler:handle_call(GoatId, Message, State) of
    {ok, Reply, NewState} ->
      NewGoat = Goat#goat{ state = NewState },
      ets:insert(?MODULE, NewGoat),
      {ok, Reply};
    {deactivate, Reply, NewState} ->
      NewGoat = Goat#goat{ state = NewState },
      ets:insert(?MODULE, NewGoat),
      {ok, Reply}
  end.

%% @spec cast(goat_id(), term()) -> ok | {error, Reason}
%% @doc Executes an asynchronous cast to a <code>goat</code>.
%% The <code>goat</code> will be activated as necessary.

cast(GoatId, Message) ->
  activate(GoatId),
  {ok, #goat{ pid = Pid } = Goat} = retrieve(GoatId),
  Pid ! {{self(), cast}, Message}.

%% @spec goat_handler(module(), goat_id()) -> never_returns
%% @doc Handles the dynamic aspect of a <code>goat</code>, as required.
%% This function should not be called by user code.

goat_handler(Handler, GoatId) ->
  Self = self(),
  receive
    synchronize ->
      {ok, #goat{ pid = Self, state = State, timeout = Timeout }} = retrieve(GoatId),
      goat_handler(Handler, GoatId, State, Timeout)
  end.
goat_handler(Handler, GoatId, State, Timeout) ->
  receive
    {{Parent, call, Ref}, Message} ->
      case Handler:handle_call(GoatId, Message, State) of
        {ok, Reply, NewState} ->
          Parent ! {{self(), call, Ref}, Reply},
          goat_handler(Handler, GoatId, NewState, Timeout);
        {deactivate, Reply, NewState} ->
          Parent ! {{self(), call, Ref}, Reply},
	  leave_goat_handler(GoatId, NewState)
      end;
    {{Parent, cast}, Message} ->
      case Handler:handle_cast(GoatId, Message, State) of
        {ok, NewState} ->
          goat_handler(Handler, GoatId, NewState, Timeout);
        {deactivate, NewState} ->
          leave_goat_handler(GoatId, NewState)
      end
    after Timeout ->
      leave_goat_handler(GoatId, State)
  end.
leave_goat_handler(GoatId, State) ->
  Goat = retrieve(GoatId),
  NewGoat = Goat#goat{ state = State },
  ets:insert(?MODULE, NewGoat).

%%% END of ce_goat.erl %%%
