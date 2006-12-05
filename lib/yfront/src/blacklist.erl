%%%-------------------------------------------------------------------
%%% Created :  4 Dec 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%
%%% @doc Blacklist functionality for DNS domains and specific IP's.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(blacklist).

-export([is_blacklisted/1,
	 ip_p/1, domain_p/1,
	 add_ip/1, add_domain/1,
	 rm_ip/1, rm_domain/1,
	 print/0
	]).

%%% When initializing the DB
-export([create_blacklist/0]).

-include_lib("kernel/include/inet.hrl").
-include_lib("stdlib/include/qlc.hrl").


%%%
%%% Internal table.
%%% Note: We can add more things to blacklist here as the key
%%%       is tag'ed (e.g a range of IP addresses).
%%%
-define(BLIST, blacklist).
-record(blacklist, {
	  key,                   % {ip, {A,B,C,D}} | {dns, <string>}
	  blacklisted = true     % Boolean
	 }).

-define(IP_KEY(IP),   {ip,  IP}).
-define(DNS_KEY(DNS), {dns, DNS}).


%%%
%%% @doc Print out blacklisted stuff
%%%
print() ->
    Qip  = qlc:q([Ip  || #blacklist{key = ?IP_KEY(Ip)}   <- mnesia:table(?BLIST)]),
    Qdom = qlc:q([Dom || #blacklist{key = ?DNS_KEY(Dom)} <- mnesia:table(?BLIST)]),
    print(qd(Qip), qd(Qdom)).

print(Qip, Qdom) ->
    Ips  = lists:flatten(implode([io_lib:format("~w",[Ip]) || Ip <- Qip], "\n")),
    Doms = lists:flatten(implode(Qdom, "\n")),
    io:format("~nBlacklisted IP addresses~n"
	      "------------------------~n"
	      "~s~n"
	      "~nBlacklisted Domains~n"
	      "------------------------~n"
	      "~s~n~n", [Ips, Doms]).

qd(Q) ->
    mnesia:async_dirty(fun() -> qlc:eval(Q) end).



%%%
%%% @doc Add Ip or Domain to be blacklisted
%%%
add_ip(Ip) when tuple(Ip),size(Ip)==4 ->
    db_add(?IP_KEY(Ip)).

add_domain(Domain) when list(Domain) ->
    db_add(?DNS_KEY(Domain)).


db_add(Key) ->
    F = fun() ->
		case mnesia:read({blacklist, Key}) of
		    [B] when B#blacklist.blacklisted == false ->
			mnesia:write(B#blacklist{blacklisted = true});
		    [B] when B#blacklist.blacklisted == true ->
			ok;
		    [] ->
			mnesia:write(#blacklist{key = Key,
						blacklisted = true})
		end
	end,
    tVALUE(kdb:transaction(F)).


%%%
%%% @doc Remove Ip or Domain from being blacklisted.
%%%
rm_ip(Ip) when tuple(Ip),size(Ip)==4 ->
    db_rm(?IP_KEY(Ip)).

rm_domain(Domain) when list(Domain) ->
    db_rm(?DNS_KEY(Domain)).


db_rm(Key) ->
    F = fun() -> mnesia:delete({blacklist, Key}) end,
    tVALUE(kdb:transaction(F)).


%%%
%%% @doc Check both the Ip address and the reverse looked up Domain name.
%%%
is_blacklisted(Ip) when tuple(Ip),size(Ip)==4 ->
    case ip_p(Ip) of
	true  -> true;
	false ->
	    case inet:gethostbyaddr(Ip) of
		{ok,#hostent{h_name = Hname}} ->
		    domain_p(Hname);
		_ ->
		    false
	    end
    end.


%%%
%%% @doc Returns true if the IP address is blacklisted
%%%
ip_p(Ip) when tuple(Ip),size(Ip)==4 -> 
    case mnesia:dirty_read({?BLIST, ?IP_KEY(Ip)}) of
	[#blacklist{blacklisted = true}] -> true;
	_                                -> false
    end.


%%%
%%% @doc Returns true if the DNS domain is blacklisted.
%%%
domain_p(Domain) when list(Domain) ->
    case string:tokens(Domain, ".") of
	[]    -> false;
	Parts -> match_domain(Parts)
    end.

match_domain([]   )         -> false;
match_domain([_|T] = Parts) ->
    D = implode(Parts, "."),
    case mnesia:dirty_read({?BLIST, ?DNS_KEY(D)}) of
	[#blacklist{blacklisted = true}] -> true;
	_                                -> match_domain(T)
    end.



%%%
%%% @doc Run this once on all participating nodes.
%%%
create_blacklist() ->
  case mnesia:create_table(blacklist,
			   [{disc_copies,[node()]},{type,set},
			    {attributes,record_info(fields,blacklist)},
			    {record_name,blacklist}
			   ]) of
    {atomic,ok} -> ok; {aborted,{already_exists,blacklist}} -> ok
  end.



%%%
%%% implode(["a","b","c"], ".") => "a.b.c"
%%%
implode(Data, Seperator) when is_list(Data) andalso is_list(Seperator) ->
    lists:foldr(fun(X,[]) -> X; (X,Acc) -> X++Seperator++Acc end, "", Data).


%%% transaction value                                                                                                     
tVALUE({atomic,Val}) -> Val;                                                                                              
tVALUE({aborted,Reason}) -> {error, {aborted, Reason}}.  
