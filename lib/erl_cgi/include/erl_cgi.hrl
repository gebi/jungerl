%%% File    : erl_cgi.hrl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : cgi state structure
%%% Created : 24 Sep 2002 by Tony Rogvall <tony@bix.hemma.se>

-record(cgi,
	{
	  env,    %% environment table
	  send,   %% Send function   (REPLY)
	  recv    %% Recive function (POST/PUT)
	 }).
	  
	  




