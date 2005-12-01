%%%-------------------------------------------------------------------
%%% File    : gtkNode.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description : 
%%%
%%% Created : 22 Nov 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(gtkNode).
-export([start/1,stop/1]).
%%-export([recv/0]).
%%-export([glade/1,widget_get_attr/1,new_gvalue/2]).

-import(filename, [join/1,dirname/1]).

-record(st,{gtk_port=[],client_pid=[],handler_pid=[],gtk_pid=[],name=[]}).

-define(BORED, 5000).

%%%-------------------------------------------------------------------
%%% runs in client process
%%%-------------------------------------------------------------------
start(Name) -> 
    case whereis(Name) of
	undefined ->    
	    Self = self(),
	    Pid = spawn_link(fun() -> init(Self, Name) end),
	    receive 
		started -> Pid;
		quit -> ok
	    end;
	_ -> erlang:fault({already_started,Name})
    end.

stop(Pid) ->
    Pid ! quit.

%%%-------------------------------------------------------------------
%%% implements the gtkNode middleman process
%%% links to the gtkNode and to the signal handler
%%% the signal handler is the process from which the start function is called
%%%
%%% has a message API
%%% receives from the client;
%%%   {ClientPid, list({atom(Command),list(Args)})}
%%% receives from the port;
%%%   {port(Port),PortData}
%%% receives from the gtkNode;
%%%    {GtkPid, {atom(handshake|signal|reply|error),Data}
%%% from anywhere;
%%%    atom(quit) | EXIT signals
%%%-------------------------------------------------------------------
init(Client, Name) ->
    process_flag(trap_exit,true), 
    register(Name, self()),
    Client ! started,
    Port = start_gtkNode(Name),
    waiting_handshake(#st{handler_pid=Client, name=Name, gtk_port=Port}).

start_gtkNode(Name) ->
    open_port({spawn,make_cmd(Name)},[stderr_to_stdout,exit_status]).

make_cmd(Name) ->
    {_,OS} = os:type(),
    Path = join([dirname(dirname(code:which(?MODULE))),priv,bin]),
    Bin = "gtkNode-"++atom_to_list(OS),
    case os:find_executable(Bin,Path) of
	false -> erlang:fault({executable_no_found,Bin});
	Exe -> 
	    [Node,Host] = string:tokens(atom_to_list(node()),"@"),
	    RegName = atom_to_list(Name),
	    Cookie = atom_to_list(erlang:get_cookie()),
	    NN = atom_to_list(?MODULE)++"_"++atom_to_list(Name),
	    EDV = integer_to_list(erl_dist_vsn()),
	    string_join([Exe,Node,Host,RegName,Cookie,NN,EDV]," ")
    end.

erl_dist_vsn() ->
    case string:tokens(erlang:system_info(version),".") of
	[[X]|_] when X < $5-> throw({ancient_erl_version,[X]});
	["5","0"|_] -> 7;
	["5","1"|_] -> 7;
	["5","2"|_] -> 8;
	["5","3"|_] -> 9;
	_ -> 10
    end.

string_join([Pref|Toks], Sep) ->
    lists:foldl(fun(Tok,O) -> O++Sep++Tok end, Pref, Toks).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the states; waiting_handshake, idle, waiting_reply
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting_handshake(St = #st{gtk_port=Port}) ->
    receive
	{{GtkPid,handshake}, []} -> 
	    link(GtkPid),
	    idle(St#st{gtk_pid=GtkPid});
	{Port,PortData} ->			%from the port
	    waiting_handshake(handle_portdata(St, PortData));
	{'EXIT',Port,_Reason} ->		%port died, us too
	    die(St#st{gtk_port=[]});
	quit -> 
	    die(St)
    after 
	?BORED -> waiting_handshake(bored(waiting_handhake,St))
    end.

idle(St = #st{gtk_pid=GtkPid, gtk_port=Port, handler_pid=HandPid}) ->
    receive
	{{GtkPid, signal}, Sig} ->
	    %%from gtkNode
	    HandPid ! {St#st.name, {signal, Sig}},
	    idle(St);
	{Pid,CmdArgs} when pid(Pid) ->
	    %%from client
	    GtkPid ! CmdArgs,
	    waiting(St#st{client_pid = Pid});
	{Port,PortData} ->
	    %%from the port
	    idle(handle_portdata(St, PortData));
	{'EXIT',HandPid,_Reason} ->
	    %%handler died
	    die(St#st{handler_pid=[]});
	{'EXIT',Port,_Reason} ->
	    %%port died, us too
	    die(St#st{gtk_port=[]});
	quit -> 
	    die(St)
    end.

waiting(St = #st{gtk_pid=GtkPid, gtk_port=Port}) ->
    receive
	{{GtkPid,reply}, Ans}->			%from gtkNode
	    St#st.client_pid ! {St#st.name, {reply,Ans}},
	    idle(St#st{client_pid = []});
	{Port,{data,PortData}} ->		%from the port
	    waiting(handle_portdata(St, PortData));
	{'EXIT',Port,_Reason} ->		%port died, us too
	    die(St#st{gtk_port=[]});
	quit -> 
	    die(St)
    after 
	?BORED -> waiting(bored(waiting,St))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_portdata(St, PortData) ->
    case PortData of
	{data, Str} -> io:fwrite("~w - portdata - ~s~n", [?MODULE, Str]);
	_ -> io:fwrite("~w - portdata - ~p~n", [?MODULE, PortData])
    end,
    St.

bored(State,St) ->
    io:fwrite("~w - bored - ~p~n~p~n~p~n", 
	      [?MODULE, State, St, process_info(self(),messages)]),
    St.

die(_St) ->
    io:fwrite("~w - terminating~n", [?MODULE]),
    process_flag(trap_exit,false),     
    exit(dying).
