%%% File    : ssh_io.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Interaction module
%%% Created : 23 Sep 2004 by Tony Rogvall <tony@PBook.local>

-module(ssh_io).

-vsn("$Revision$ ").

-rcsid("$Id$\n").

-export([read_password/1, read_line/1]).
-export([verify_host/1]).
-export([verification_error/3]).
-import(lists, [reverse/1]).


read_line(Prompt) when list(Prompt) ->
    io:get_line(list_to_atom(Prompt));
read_line(Prompt) when atom(Prompt) ->
    io:get_line(Prompt).

read_ln(Prompt) ->
    trim(read_line(Prompt)).

yes_no(Prompt) ->
    io:format("~s [y/n]?", [Prompt]),
    case read_ln('') of
	"y" -> yes;
	"n" -> no;
	"Y" -> yes;
	"N" -> no;
	_ ->
	    io:format("please answer y or n\n"),
	    yes_no(Prompt)
    end.

%% Hook for handling verification error
verification_error(Host,Key,Reason) ->
    io:format("VERIFY FAILED: ~p\n", [Reason]),
    {error, bad_signature}.

verify_host(Host) ->
    case yes_no("New Host "++Host++" accept") of
	yes -> ok;
	no  -> {error, rejected}
    end.

%% FIXME: no echo!
read_password(Prompt) ->
    case read_ln(Prompt) of
	"" ->
	    read_password(Prompt);
	Pass -> Pass
    end.

trim(Line) when list(Line) ->
    reverse(trim1(reverse(trim1(Line))));
trim(Other) -> Other.

trim1([$\s|Cs]) -> trim(Cs);
trim1([$\r|Cs]) -> trim(Cs);
trim1([$\n|Cs]) -> trim(Cs);
trim1([$\t|Cs]) -> trim(Cs);
trim1(Cs) -> Cs.
    


