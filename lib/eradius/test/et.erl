-module(et).
%%%----------------------------------------------------------------------
%%% File    : et.erl
%%% Author  : Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : eradius test code
%%% Created : 25 Sep 2003 by Torbjorn Tornkvist <tobbe@bluetail.com>
%%%----------------------------------------------------------------------
-export([local/0, duva/1, korp/0]).

-include("eradius.hrl").
-include("eradius_dict.hrl").

local() -> 
    go({127,0,0,1}, "tobbe", "qwe123", "qwe123", {127,0,0,1}).

duva(Passwd) ->
    go({192,168,128,1}, "support", Passwd, Passwd, {192,168,128,32}).

korp() ->
    go({192,168,128,47}, "tobbe", "qwe123", "mortuta42", {192,168,128,32}).


go(IP, User, Passwd, Shared, NasIP) ->
    TraceFun = fun(_E,Str,Args) ->
		       io:format(Str,Args),
		       io:nl()
	       end,
    E = #eradius{servers = [[IP, 1812, Shared]],
		 user = User,
		 passwd = Passwd,
		 tracefun = TraceFun,
		 nas_ip_address = NasIP},
    eradius:start(),
    eradius:load_tables(["dictionary",
			 "dictionary_alteon",
			 "dictionary_ascend"]),
    print_result(eradius:auth(E)).

print_result({accept, Attributes}) ->
    io:format("Got 'Accept' with attributes: ~p~n",[Attributes]),
    pa(Attributes);
print_result({reject, Attributes}) ->
    io:format("Got 'Reject' with attributes: ~p~n",[Attributes]),
    pa(Attributes);
print_result(Res) ->
    io:format("Got: ~p~n",[Res]).

pa([{K, V} | As]) ->
    case eradius_dict:lookup(K) of
	[A] ->
	    io:format("     ~s = ~p~n",[A#attribute.name, 
					to_list(V, A#attribute.type)]);
	_ -> 
	    io:format("  <not found in dictionary>: ~p~n", [{K,V}])
    end,
    pa(As);
pa([]) ->
    true.

to_list(B, string)  -> binary_to_list(B);
to_list(B, octets)  -> B;
to_list(B, integer) -> b2i(B);
to_list(B, ipaddr)  -> b2ip(B);
to_list(D, date)    -> D.  % FIXME !
		
b2i(<<I:32>>)          -> I;
b2i(<<I:16>>)          -> I;
b2i(<<I:8>>)           -> I;
b2i(I) when integer(I) -> I.

b2ip(<<A:8,B:8,C:8,D:8>>) -> {A,B,C,D};
b2ip({A,B,C,D})           -> {A,B,C,D}.
    
    
