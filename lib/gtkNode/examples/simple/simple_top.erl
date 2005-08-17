%%%-------------------------------------------------------------------
%%% File    : simple_top.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 11 Aug 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(simple_top).

-export([assert/1,stop/0]).
-import(lists,[flatten/1,sublist/2,sort/2]).

-record(st, {ref,client,old_data=[],
	     rows=20,tick=2000,key=reds,tags=[reg,mem,msgq,reds]}).

assert(ClientPid) ->
    case whereis(?MODULE) of
	undefined -> spawn_link(fun init/0) ! {start,ClientPid};
	Pid -> Pid ! {start,ClientPid}
    end.

stop() -> ?MODULE ! stop.

init() -> 
    register(?MODULE,self()),
    loop(#st{ref=timr(#st{})}).

loop(St) ->
    receive
	{timeout,_Ref,{}} -> loop(send_data(St#st{ref=timr(St)}));
	{start,Client}-> loop(St#st{client=Client,ref=timr(St)});
	stop -> loop(St#st{ref=untimr(St)})
    end.

untimr(St) ->
    (catch erlang:cancel_timer(St#st.ref)),[].

timr(St) ->
    case St#st.ref of 
	Ref when is_reference(Ref) -> timr(St#st{ref=untimr(St)});
	_ -> erlang:start_timer(St#st.tick,self(),{})
    end.

send_data(St = #st{old_data=[]}) ->
    St#st{old_data=data(St)};
send_data(St = #st{old_data=OldData}) ->
    Data = data(St),
    St#st.client ! {data,sort(St#st.key,St#st.rows,Data,OldData)},
    St#st{old_data = Data}.

data(#st{tags=Tags}) ->
    [{P,[pi(P,T) || T <- Tags]} || P <- processes()].

pi(P,reds) -> pin(P,reductions,0);
pi(P,mem) -> pin(P,heap_size,0)+pin(P,stack_size,0);
pi(P,msgq) -> pin(P,message_queue_len,0);
pi(P,reg) -> case sin(P,registered_name) of "[]"->sin(P,initial_call); X->X end.

pin(P,Tag,D) -> case (catch process_info(P,Tag)) of {Tag,V} -> V; _ -> D end.
sin(P,Tag) -> to_string(pin(P,Tag,"")).
    
to_string(Term) -> flatten(io_lib:format("~p",[Term])).

sort(Key,Rows,Data,OldData) -> 
    sublist(sort(fun(A,B)->cmp(Key,A,B) end, diff(Key,Data,OldData)),Rows).

cmp(reds,[_,A,_,X],[_,B,_,X]) -> B<A;
cmp(reds,[_,_,_,X],[_,_,_,Y]) -> Y<X.

diff(reds,Data,OldData) -> rdf(Data,OldData,[]).

rdf([],[],X) -> X;
rdf([{P,[A,B,C,R]}|Ds],[{P,[_,_,_,OR]}|Os],X) -> rdf(Ds,Os,[[A,B,C,R-OR]|X]);
rdf([{P1,_}|Ds],[{P2,_}|_]=Os,X) when P1<P2-> rdf(Ds,Os,X);
rdf([{P1,_}|_]=Ds,[{P2,_}|Os],X) when P2<P1-> rdf(Ds,Os,X);
rdf([],_,X) -> rdf([],[],X);
rdf(_,[],X) -> rdf([],[],X).
