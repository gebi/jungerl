-module(ex11_utils).
-author('tnt@home.se').
%%%---------------------------------------------------------------------
%%% Created : 15 Feb 1999 by tnt@home.se
%%% Function: Misc. utility routines.
%%% ====================================================================
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.0, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is x11-0-1
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1999, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%---------------------------------------------------------------------
-export([first/2,i16/1,i16/2,i32/1,i32/4,split_list/2,get_display/0,
	 b16/2,b32/2,xalloc_id/1]).

-import(lists,[reverse/1]).

-include("ex11.hrl").

%% ---------------
%% Misc. routines

%% Que...? See XlibInt.c l.1492
xalloc_id(Dpy) when ?IS_DISPLAY(Dpy) ->    
    ResId = Dpy#display.resource_id,
    ResShift = Dpy#display.resource_shift,
    Id = ResId bsl ResShift,
    ResMask = Dpy#display.resource_mask,
    Dpy1 = if (Id >= ResMask) -> Dpy#display{resource_mask=ResMask+1};
	      true -> Dpy
	   end,
    if (Id =< ResMask) -> 
	    {Dpy1#display.resource_base+Id,
	     Dpy1#display{resource_id=ResId+1}};
       true ->
	    if (Id =/= 16#10000000) ->
		    io:format("Xlib: resource ID allocation space exhausted!\n"),
		    {16#10000000,
		     Dpy#display{resource_id = 16#10000000 bsr ResShift}};
	       true -> {Id,Dpy1}
	    end
    end.
	

%% ---------------------------------------------------------
%% Fetch the first element in list which satisfy F(Element),
%% where F(Element) => {true,Value] | false

first(F,[H|T]) ->
    case F(H) of
	{true,Value} -> {true,Value};
	_ -> first(F,T)
    end;
first(_,[]) -> false.
			
%% ----------------------------------
%% Encode/Decode 16/32 bits integers

i16(Int) when binary(Int) ->
    i16(binary_to_list(Int));
i16(Int)  when integer(Int) -> 
    [(Int bsr  8) band 255,Int band 255];
i16([X1,X0]) ->
    i16(X1,X0).

i16(X1,X0) ->
    (X1 bsl 8) bor X0.

i32(Int) when binary(Int) ->
    i32(binary_to_list(Int));
i32(Int)  when integer(Int) -> 
    [(Int bsr 24) band 255,
     (Int bsr 16) band 255,
     (Int bsr  8) band 255,
     Int band 255];
i32([X1,X2,X3,X4]) ->
    i32(X1,X2,X3,X4).

i32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

%% Deal with the byte order
    
b16(?LSB_BYTEORDER,I) when integer(I) -> reverse(i16(I));
b16(?LSB_BYTEORDER,L) when list(L)    -> i16(reverse(L));
b16(?MSB_BYTEORDER,I)                 -> i16(I).

b32(?LSB_BYTEORDER,I) when integer(I) -> reverse(i32(I));
b32(?LSB_BYTEORDER,L) when list(L)    -> i32(reverse(L));
b32(?MSB_BYTEORDER,I)                 -> i32(I).
    

%% --------------------------
%% Split a list in two parts

split_list(0,L) -> {[],L};
split_list(I,L) -> split_list(I,L,[]).

split_list(I,[H|T],Acc) when I>0 -> split_list(I-1,T,[H|Acc]);
split_list(0,L,Acc)              -> {reverse(Acc),L};
split_list(_,[],Acc)             -> {reverse(Acc),[]}.


get_display() ->
    case os:getenv("DISPLAY") of
	false   -> {error,"no DISPLAY variable set"};
	Display -> parse_display(Display)
    end.

parse_display(":0.0") -> ok. % NYI !!
