%%% BEGIN wumpus.erl %%%
%%%
%%% wumpus - Erlang Wumpus
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

%% @doc Cat's Eye Technologies' Erlang Wumpus.
%% 
%% <p>This is an implementation of Gregory Yob's classic game of
%% Hunt the Wumpus.</p>
%%
%% <p>This version is not wholly faithful to the original. It is primarily
%% intended for example purposes.  Notably, the Wumpus does not move at all.
%% Also, messages may be less than thrilling.</p>
%%
%% <p>As an example program, Erlang Wumpus demonstrates:</p>
%% <ul>
%% <li> using the <code>random</code> module </li>
%% <li> using the <code>digraph</code> module </li>
%% <li> using list comprehensions </li>
%% <li> using <code>case catch N</code> to catch errors </li>
%% <li> using <b>edoc</b> to document source code </li>
%% </ul>
%% @end

-module(wumpus).
-vsn('2002.0630').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([start/0, game/0]).

%% Various nasties in the game (superbats, bottomless pits, and the
%% Wumpus itself) are encapsulated into hazard records.

-record(hazard,
{
  type,     % wumpus, bat, or pit
  location  % a label
}).

%% The state of almost the entire game is encapsulated into one record,
%% so it can more easily be passed between functions.

-record(game,
{
  map,       % a digraph representing the cave system
  location,  % a vertex representing the player's current location
  arrows,    % an integer representing the number of arrows left
  nasty      % a list of hazard records representing the nasties
}).

%%% UTILITY FUNCTIONS %%%

%% @spec random_vertex(graph()) -> vertex()
%%         graph() = digraph:graph()
%%         vertex() = digraph:vertex()
%% @doc Picks a random vertex from the given graph.

random_vertex(G) ->
  Vs = digraph:vertices(G),
  lists:nth(random:uniform(length(Vs)), Vs).

%% @spec random_vertex(graph(), vertex()) -> vertex()
%% @doc Picks any random vertex except the given one from the given graph.

random_vertex(G, V) ->
  Vs = digraph:vertices(G),
  Q = lists:nth(random:uniform(length(Vs)), Vs),
  case Q of
    V -> random_vertex(G, V);
    _ -> Q
  end.

%% @spec vertex_ids(graph(), [vertex()]) -> [integer()]
%% @doc Returns a list of vertex ID's from a list of vertices in a graph().

vertex_ids(G, []) -> [];
vertex_ids(G, [H|T]) ->
  {['$v' | L], _} = digraph:vertex(G, H),
  [L | vertex_ids(G, T)].

%% @spec vertex(integer()) -> vertex()
%% @doc Returns a vertex given a vertex ID.

% Given an id, return a vertex.

vertex(Id) when integer(Id) -> ['$v' | Id].

% Add a cave to the cave system.

add_cave(G, L) ->
  V = digraph:add_vertex(G),
  digraph:add_vertex(G, V, L).

% Add a directed tunnel from one cave to the next.

add_tunnel(G, V1, V2) ->
  digraph:add_edge(G, V1, V2).

%%% MAIN %%%

%% @spec start() -> quit
%% @equiv game()

start() -> game().

%% @spec game() -> quit
%% @doc Provides the main user interface to the Hunt the Wumpus game.
%% game/0 initializes everything for the main loop, game/1.

game() ->
  {H,M,S} = time(),                                % this seems to be an adequate way to
  random:seed(S,M,H),                              % seed the random number generator

  G = digraph:new([cyclic, private]),              % create the cave system

  V = lists:foldl(fun(X,T) ->
        erlang:append_element(T, add_cave(G, X))
      end, {}, lists:seq(1, 20)),                  % create 20 new caves in it

  E  = lists:foldl(fun(X,T) -> 
         add_tunnel(G, X, random_vertex(G,X)),
         add_tunnel(G, X, random_vertex(G,X)),
         add_tunnel(G, X, random_vertex(G,X)),
         T
       end, {}, tuple_to_list(V)),                 % link each cave up to random neighbours

  L = random_vertex(G),                            % choose player's start location
  W = #game{
             map = G,
             location = L,
             arrows = 3,
             nasty =
             [
               #hazard{type = bat,    location = random_vertex(G,L)},   % distribute
               #hazard{type = bat,    location = random_vertex(G,L)},   % nasties
               #hazard{type = pit,    location = random_vertex(G,L)},   % randomly
               #hazard{type = pit,    location = random_vertex(G,L)},   % through
               #hazard{type = wumpus, location = random_vertex(G,L)}    % caves
             ]
           },                                      % create initial game-state
  game(W),                                         % initiate main game loop
  digraph:delete(G),
  ok.                                              % clean up and return

%% @spec game(quit | game_state()) -> quit | game_state()
%%         game_state() = game_state()
%% @doc Provides the main loop for the Hunt the Wumpus game.

game(quit) ->
  io:fwrite("*** Game Over ***~n"),
  ok;                                              % exit the loop on 'quit' atom
game(W) ->                                         % otherwise, examine game-state
  G = W#game.map,                                  % retrieve parts of game-state
  L = hd(vertex_ids(G, [W#game.location])),

  Neighbours = digraph:out_neighbours(G, W#game.location), % find neighbouring caves
  Exits = vertex_ids(G, Neighbours),
  Hazards = lists:map(fun(X) -> X#hazard.type end,         % all hazards here
                      [X || X <- W#game.nasty,
		       X#hazard.location == W#game.location]),
  Nearby  = lists:map(fun(X) -> X#hazard.type end,         % all hazards in
                      [X || X <- W#game.nasty,             % adjacent caves
		       lists:member(X#hazard.location, Neighbours)]),

  io:fwrite("You are in cave ~w with ~w arrows.~n", [L, W#game.arrows]),
  [ExitA, ExitB, ExitC] = Exits,
  io:fwrite("Tunnels lead to caves ~w, ~w, and ~w.~n", [ExitA, ExitB, ExitC]),

  % Notify the player of hazards in adjacent rooms.
  
  lists:foreach(fun(bat)    -> io:fwrite("Bats nearby!~n");
                   (pit)    -> io:fwrite("I feel a draft!~n");
		   (wumpus) -> io:fwrite("I smell a Wumpus!~n") end, Nearby),

  case Hazards of                                  % execute effects of hazards
    [bat    | T]  -> io:fwrite("ZAP! Super bat snatch... elsewheresville for you!~n"),
                     game(W#game{location = random_vertex(G)});
    [pit    | T]  -> io:fwrite("YIEEEEE!!! Fell in a pit!~n"), game(quit);
    [wumpus | T]  -> io:fwrite("The Wumpus ATE YOU UP!!!~n"), game(quit);
    _             -> game(move(W))                 % allow player to make move, then loop
  end.

%% @spec move(game_state()) -> game_state()
%% @doc Provides the player's move-or-shoot interface as part of the
%% game loop whenever no hazard is interfering with game-play.

move(W) ->
  G = W#game.map,                                  % retrieve parts of game-state
  Exits = vertex_ids(G, digraph:out_neighbours(G, W#game.location)),

  C = lib:nonl(io:get_line('wumpus> ')),           % listen to user's choice in the matter
  case C of
    "s" ++ _ -> shoot(W);                          % s = begin arrow-shooting sequence
    "m" ++ _ ->                                    % m = redundant
       io:fwrite("Just type the cave number to which you wish to move.~n"), W;
    "q" ++ _ -> quit;
     _  -> case catch list_to_integer(C) of        % we catch in case it's not an integer
             N when integer(N) ->
               case lists:member(N, Exits) of      % check if move is legal
                 true ->
		   W#game{location = vertex(N)};   % if so, move there
                 _ ->
		   io:fwrite("*BOOF!*  Ran into the cave wall!~n"),
		   W                               % if not try again
               end;
             _ ->
               io:fwrite("I don't acknowledge '~s'.~n"
                  "Your entry must be the number of a cave, "
		  "(s)hoot, or (q)uit.~n", [C]),
               W                                   % try again
           end
  end.

%% @spec shoot(game_state()) -> game_state()
%% @doc Provides the player's shoot sequence.  This function uses get_shot/0
%% to ask for a list of three caves into which the Crooked Arrow(tm) will
%% be shot, and then uses shoot/3 to determine the outcome.

shoot(W) ->
  case shoot(W, get_shot(), W#game.location) of
    miss when W#game.arrows == 1 ->             % ran out of arrows!
      io:fwrite("Out of ammunition! How humiliating.~n"),
      quit;
    miss ->
      io:fwrite("So much for that arrow.~n"),
      W#game{ arrows = W#game.arrows - 1 }; % reduce the player's inventory
    wall ->
      io:fwrite("*Klakk!*  Hit the cave wall!~n"),
      W#game{ arrows = W#game.arrows - 1 };
    {hit, player} ->
      io:fwrite("*OWCH!*  Caught by your own arrow!~n"),
      quit;
    {hit, wumpus} ->
      io:fwrite("You got the Wumpus!!!!!~n"),
      quit
  end.  

%% @spec shoot(game_state(), [integer()], integer()) -> shoot_outcome()
%%         shoot_outcome() = {hit, player} | {hit, wumpus} | miss | wall
%% @doc Actually shoots the arrow.
%% Returns an atom describing the consequences.

shoot(W, [], ArrowLoc) ->
  miss;
shoot(W, L, ArrowLoc) when length(L) < 3, ArrowLoc == W#game.location ->
  {hit, player};
shoot(W, Itinerary, ArrowLoc) ->
  G = W#game.map,                           % retrieve parts of game-state
  Exits = vertex_ids(G, digraph:out_neighbours(G, ArrowLoc)),

  D = hd(Itinerary),                        % D is vertex id of cave to go to
  io:fwrite("Twang...!~n"),

  case lists:member(D, Exits) of            % is the arrow's exit allowable?
    true -> case [X || X <- W#game.nasty,   % yes, but did it hit anything?
                       X#hazard.location == vertex(D),
                       X#hazard.type == wumpus] of
              [H] -> {hit, wumpus};         % yes, we hit the wumpus
                _ -> shoot(W, tl(Itinerary), vertex(D)) % no, move arrow
            end;
    false -> wall                           % no exit, it hit a wall
  end.

%% @spec get_shot() -> [integer()]
%% @doc Asks the player for three caves to shoot through. Returns the list.

get_shot() ->
  io:fwrite("Please enter the numbers of up to three caves~n"
            "through which you wish the arrow to travel.~n"),
  get_shot([], 3).
get_shot(L, 0) ->                      % no more caves to ask for, so fire!
  L;
get_shot(Itinerary, N) ->
  io:fwrite("shoot ~w", [4-N]),        % prompt the player for cave to shoot to
  C = lib:nonl(io:get_line('> ')),
  case catch list_to_integer(C) of     % again, catch for safety
    I when integer(I) ->
      get_shot(Itinerary ++ [I], N-1); % accumulate destination and recurse
    I ->
      io:fwrite("I don't acknowledge '~s'.~n", [C]),
      get_shot(Itinerary, N)
  end.

%%% END of wumpus.erl %%%
