%%% Copyright (C) 2004 Mikael Karlsson <mikael.karlsson@creado.com>.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met: 
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer. 
%%% 2. Redistributions in binary form must reproduce the above
%%%    copyright notice, this list of conditions and the following
%%%    disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%%-------------------------------------------------------------------
%%% File    : erlspect.erl
%%% Author  : Mikael Karlsson <mikael.karlsson@creado.com>
%%% Purpose : Expect like scripting in Erlang
%%%
%%% Created : 13 April 2004
%%%-------------------------------------------------------------------
-module(erlspect).
-author('mikael.karlsson@creado.com').

%% External exports
-export([exp_spawn/1, exp_spawn/2, exp_spawn/3, 
         close/1, 
         send/2, expect/2, expect/3,
         buffered/1]).

%% Internal exports
-export([init/2, init/3]).

exp_spawn(CmdStr) ->
    spawn_link(erlspect, init, [self(),CmdStr]).
exp_spawn(CmdStr, PortOpts) when list(CmdStr) -> 
    spawn_link(erlspect, init, [self(),CmdStr, PortOpts]);

exp_spawn(Node, CmdStr) when atom(Node) -> 
    spawn_link(Node, erlspect, init, [self(),CmdStr]).
exp_spawn(Node, CmdStr,PortOpts) when atom(Node) -> 
    spawn_link(Node, erlspect, init, [self(), CmdStr, PortOpts]).

close(PiD) -> PiD ! {close}.

init(PiD,CmdStr) ->
    process_flag(trap_exit,true),
    Port = open_port({spawn, CmdStr},
		     [stream, use_stdio, stderr_to_stdout]),
    loop(PiD,Port,[],[], false).
init(PiD,CmdStr,PortOpts) ->
    process_flag(trap_exit,true),
    Port = open_port({spawn, CmdStr},PortOpts),
    loop(PiD,Port,[],[], false).

-ifdef(DEBUG).
elg(Str) -> elg(Str,[]).
elg(Str,Args) ->
    io:format("erlspect: ~p " ++ Str, [self()] ++ Args),
    ok.
-else.
elg(_) -> ok.
elg(_,_) -> ok.
-endif.

loop(PiD, Port, EB, RB, EOF ) ->
    receive

	{Port, {data, Bytes}} ->
	    elg("got bytes ~s~n", [Bytes]),    
	    {EBuf, RBuf} = check_buffer(PiD, EB, RB ++ Bytes),
	    loop(PiD, Port, EBuf, RBuf, EOF);

	{send, Message} when EOF == false ->
	    port_command(Port, Message),
	    loop(PiD, Port, EB,  RB, EOF);

	{expect, {P, ExpBuf}} ->
	    {EBuf, RBuf} = check_buffer(P, ExpBuf, RB),
	    case {EOF,EBuf} of
		{false,_} ->
		    loop(P, Port, EBuf, RBuf, EOF);
		{true,[]} ->
		    loop(P, Port, EBuf, RBuf, EOF);
		{true,_} ->
		    P ! {self(), eof},
		    self() ! {close},
		    loop(P, Port, [], RBuf, EOF)
	    end;

        {buffered} ->
            PiD ! {self(), RB},
	    loop(PiD, Port, EB,  RB, EOF);

	{'EXIT', Port, A} ->
	    elg("Port exited, status ~s~n", [A]),
            case EB of
		[] -> 
		    loop(PiD, Port, EB, RB, true);
		_ ->
		    PiD ! {self(), eof},
		    self() ! {close},
		    loop(PiD, Port, [], RB, true)
	    end;

	{'EXIT', PiD, _} ->
	    elg("Controller process exited, closing~n"),
	    self() ! {close},
	    loop(PiD, Port, [],  RB, EOF);

	{close} when EOF == true ->
	    elg("Closing erlspect proc ~p~n", [self()]),
	    ok;

	{close} when EOF == false ->
	    port_close(Port),
	   ok;

	Any ->
	    elg("Unexpected message ~p~n", [Any]),
	    loop(PiD, Port, EB,  RB, EOF)
	    
    end.
	

send(P, Message) ->  P ! {send, Message}. 

%% expect with defaults to a  timeout of 60 seconds if nothing
%% else is stated.
expect(P, ExpBuf) -> expect(P, ExpBuf, 60).

%% expect eof from erlang Port
expect(P, eof, T) ->
   expect(P,"ThiswillNeverMatchatallHoHoHo",T);

%% expect(P, [{regexp1,return_val1},{regexp2,returnval2},..], Timeout)
%% returns {return_val,{ Regexp_match, Upto_and_including_Regexp_match}}
%% | timeout | eof
expect(P, ExpBuf, T) when list (ExpBuf), tuple(hd(ExpBuf)) ->
    ok = check_expbuf(ExpBuf),
    P ! {expect, {self(),ExpBuf} },
    receive
       {P, Res, Exp_Out} ->
           {Res, Exp_Out};
       {P, eof} -> 
           eof
       after T * 1000 ->
           P ! {expect,{self(),[]}}, %% OK, clearing the exp buf from calling
           timeout          %% process will cause a race condition, but it
    end;                    %% is not a big problem if timeouts are long.

%% The simple case expect(P,"expected",T)
%% reurns match | timeout | eof 
expect(P, ExpBuf, T) ->  
    case expect(P, [{ExpBuf, match}], T) of
	{match,_} ->
	    match;
	Any ->
	    Any
    end.

buffered(P) -> 
   P ! {buffered},
   receive
       {P, R} -> R
   after 1000 -> timeout
   end.

check_expbuf([])            -> ok;
check_expbuf([{RE,Ret}|T])  -> 
    {ok,_} = regexp:parse(RE), 
    check_expbuf(T).

check_buffer(PiD, [], RB)->
    {[],RB};
check_buffer(PiD, EB, RB)->
    case check_buffer(EB, RB) of
      nomatch -> 
           {EB, RB};
      {match, Ret,{NewRB, ExpectOut}} ->
          PiD ! {self(),Ret,ExpectOut},
          {[], NewRB}        % Clear ExpectBuf, Return new ReceiveBuf
      end.

check_buffer([], RB) ->
    nomatch;
check_buffer([{H,Ret}|T], RB) ->
    case check_reg(H,RB) of
        {match,A} -> {match,Ret,A};
        nomatch   -> check_buffer(T,RB)
    end.

check_reg(H, RB ) ->
    case regexp:first_match(RB,H) of
	{match, Start, Length} -> 
           A = {string:substr(RB,Start+Length),        % What is left of RBuf
                {string:substr(RB,Start,Length),       % What matches RegExp
                 string:substr(RB,1,Start+Length-1)}}, % Up to and incl RegExp
            {match,A};
        nomatch -> 
             nomatch
    end.      


