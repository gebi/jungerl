%%%-------------------------------------------------------------------
%%% File    : plain_fsm.erl
%%% @author Ulf Wiger, <ulf.wiger@ericsson.com>
%%% @end
%%% Created : 29 Jan 2004 by Ulf Wiger <ulf.wiger@ericsson.com>
%%%-------------------------------------------------------------------

%% @doc A behaviour/support library for writing plain Erlang FSMs.
%% 
%% <p>This module implements an OTP behaviour for writing plain Erlang FSMs,
%% alleviating a long-standing gripe of mine that the OTP behaviours, for all
%% their power, force programmers into a coding style that is very much 
%% different from that taught in the Basic Erlang Course (or the book, or
%% online tutorials, ...) -- the type of programming that made us want to 
%% use Erlang in the first place.</p>
%%
%% <p>Only in my old age have I begun to understand fully what a sacrifice
%% this is. See <a href="pots/index.html">
%% Programming Models for Concurrency </a> for a detailed discussion of 
%% the issues involved.</p>
%%
%% <p>The requirements that drove us away from plain Erlang programming
%% in the first place were:
%% <ul>
%%  <li><b>The need to support <i>system messages</i></b> to control upgrade,
%%    state inspection, shutdown, etc. The plain_fsm library solves this in a 
%%    reasonable way, I think.</li>
%%  <li><b>The need for debugging support</b>. The debugging support in
%%    e.g. gen_server is, I believe, rendered obsolete by the new powerful
%%    trace support (and dbg) in later versions of OTP.</li>
%%  <li>In the case of gen_server, <b>reducing the need to reinvent the 
%%    wheel</b>, a valid point, but more so for e.g. the client side of 
%%    gen_server:call(). In a protocol state machine, the only thing that 
%%    really needs reusing is the handling of system messages.</li>
%% </ul>
%% </p>
%%
%% <p>However, the behaviours provided by OTP for FSM programming, 
%% <code>gen_server</code> and <code>gen_fsm</code> (<code>gen_server</code>
%% is perhaps a more common choice than <code>gen_fsm</code>), both have the
%% distinct drawback that you cannot normally start with a classic Erlang design
%% and then migrate to a behaviour without significant code rewrite. In 
%% addition, the two behaviours are semantically different from the classic
%% Erlang design</p>
%%
%% <h2>Using plain_fsm</h2>
%%
%% <p>First, write your state machine without worrying about OTP system
%% messages. Once you're happy with it, figure out where you really want 
%% to handle system messages. Normally, it will suffice to do it in a fairly
%% stable state. </p>
%% 
%% <p>In the states where you want to handle system messages, you have 
%% two choices:</p>
%% 
%% <h3>(A) Insert the system messages in the receive clause:</h3>
%% 
%% <pre>
%% idle(S) ->
%%    Parent = plain_fsm:info(parent),
%%    receive
%%       {system, From, Req} ->
%%          plain_fsm:handle_system_msg(From, Req, S, fun(S1) -> idle(S1) end);
%%       {'EXIT', Parent, Reason} ->
%%          plain_fsm:parent_EXIT(Reason, S);
%%       ... %% your original code here
%%    end.
%% </pre>
%%
%% <p>This has the advantage that everyone can understand what's going on.
%% The part that plain_fsm.erl helps you with is the set of functions
%% <code>system_code_change()</code>, <code>system_continue()</code>,
%% <code>system_shutdown()</code>, <code>format_status()</code>, which
%% are required callbacks when you handle system messages directly.</p>
%%
%% <h3>(B) Write a pseudo wrapper function around your receive clause:</h3>
%%
%% <pre>
%% idle(S) ->
%%    plain_fsm:extended_receive(
%%       receive
%%          ... %% your original code
%%       end).
%% </pre>
%%
%% <p>The function <code>plain_fsm:extended_receive/1</code> is replaced 
%% in a <i>parse_transform</i> into something that looks very much like
%% the previous program (A). The code, as it reads, requires the reader to
%% know that the transformation takes place, otherwise the semantics
%% would be confusing (you cannot solve the problem using a real function
%% that way.) On the plus side, this is a fairly small violation of both
%% the original code and Erlang's semantics.</p>
%%
%% <h4>Example</h4>
%% <p>In the module <a href="../src/fsm_example.erl">fsm_example.erl</a>
%% (included in the plain_fsm package), we choose to handle system
%% messages in the idle state. The example code is runnable, and supports
%% suspend, resume, status inspection, and code change.</p>
%% <p>Imagine that the code initially looked like this:</p>
%% <pre>
%% idle(S) ->
%%     receive
%% 	a ->
%% 	    io:format("going to state a~n", []),
%% 	    a(S);
%% 	b ->
%% 	    io:format("going to state b~n", []),
%% 	    b(S)
%%     after 10000 ->
%% 	    io:format("timeout in idle~n", []),
%% 	    idle(S)
%%     end).
%% </pre>
%%
%% <p>The change required to handle system messages is as follows:</p>
%% <pre>
%% idle(S) ->
%%     {@link extended_receive/1. plain_fsm:extended_receive}(
%%       receive
%%           a ->
%%               io:format("going to state a~n", []),
%%               a(S);
%%           b ->
%%               io:format("going to state b~n", []),
%%               b(S)
%%       after 10000 ->
%%               io:format("timeout in idle~n", []),
%%               idle(S)
%%       end).
%% </pre>
%%
%% <p>In addition, we change the start function from, in this case:</p>
%% <pre>
%% spawn_link() ->
%%     spawn_link(fun() ->
%%                        process_flag(trap_exit, true),
%%                        idle(mystate)
%%                end).
%% </pre>
%% <p>Is changed into:</p>
%% <pre>
%% spawn_link() ->
%%     {@link spawn_link/2. plain_fsm:spawn_link}(?MODULE, fun() ->
%%                                           process_flag(trap_exit,true),
%%                                           idle(mystate)
%%                                   end).
%% </pre>
%% <p>See also {@link spawn/2. spawn/2} and {@link spawn_opt/3. spawn_opt/3}
%% for information on other possible start functions.</p>
%% <p>To be fully compliant, you also need to supply a code_change/3 function.
%% See {@link behaviour_info/1. behaviour_info/1} for details.</p>
%% @end



-module(plain_fsm).

%%% Functions to be used for starting the state machine:
-export([spawn/2,
	 spawn_link/2,
	 spawn_opt/3]).

%%% Functions to be called from within the state machine:
-export([extended_receive/1,
	 handle_system_msg/4,
	 parent_EXIT/2,
	 store_name/1,
	 info/1]).

%%% Used to transform the dummy function plain_fsm:recv() into something
%%% useful.
-export([parse_transform/2]).

%%% Linter callback
-export([behaviour_info/1]).

%%% Callbacks used by the sys.erl module
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 format_status/2]).

%%% Internal housekeeping records. The split into two records is used
%%% to separate the variables that are passed as explicit arguments in
%%% the sys API from the ones that are embedded in the 'state'.
%%% (the #sys{} record is the one being embedded...)
%%%
-record(sys, {cont,mod,name}).
-record(info, {parent = parent(),
	       debug = [],
	       sys = #sys{}}).



%%% ================ Internal functions ==================

%% @spec behaviour_info(atom()) -> term()
%% @doc Defines which functions this behaviour expects to be exported from
%% the user's callback module. plain_fsm requires only code_change/3 to
%% be present. The semantics of <code>Mod:code_change/3</code> are as follows:
%% <pre>
%%   code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% </pre>
%% <p>The above code is just like it would look like in a gen_server callback
%% module.</p>
%% <pre>
%%   code_change(OldVsn, State, Extra) -> {ok, NewState, Options}.
%% </pre>
%% <p>where <code>Options</code> may be any of </p>
%% <ul>
%%  <li><code>{mod, module()}</code>, allowing you to switch callback
%%   modules during a code change.</li>
%%  <li><code>{name, name()}</code>, allowing you to rename the process
%%   (note that you have to handle name registration yourself.)</li>
%%  <li><code>{cont, atom() | function(1)}</code>, allowing you to provide
%%   another continuation (point of entry into your own code after the 
%%   code change.)</li>
%% </ul>
%% @end
behaviour_info(callbacks) ->
    [{code_change, 3}];
behaviour_info(_Other) ->
    undefined.


%% @spec spawn(Mod::atom(), StartF::function()) -> pid()
%% @doc Equivalent to <code>proc_lib:spawn(StartF)</code>. This function also 
%% initializes the plain_fsm meta-data.
%% @end
spawn(Mod, StartF) ->
    ?MODULE:spawn_opt(Mod, StartF, []).

%% @spec spawn_link(Mod::atom(), StartF::function()) -> pid()
%% @doc Equivalent to <code>proc_lib:spawn_link(StartF)</code>. 
%% This function also initializes the plain_fsm meta-data.
%% @end
spawn_link(Mod, StartF) ->
    ?MODULE:spawn_opt(Mod, StartF, [link]).

%% @spec spawn_opt(Mod::atom(), StartF::function(), Opts::list()) -> pid()
%% @doc Equivalent to <code>proc_lib:spawn_opt(StartF, Opts)</code>. 
%% This function also initializes the plain_fsm meta-data.
%% @end
spawn_opt(Mod, StartF, Opts) when function(StartF) ->
    proc_lib:spawn_opt(fun() ->
			       init(Mod, StartF)
		       end, Opts).

%% @spec store_name(Name::term()) -> ok
%%
%% @doc stores an internal name for the FSM 
%%      (for <code>sys:get_status()</code>).
%% This can be used if the FSM were started as an anonymous process
%% (the only kind currently supported).
%% Note that this function does not register the name. The name stored
%% is the one that shows up in sys:get_status/1. No restriction is made
%% here regarding the data type.
%% @end
store_name(Name) ->
    #info{sys = Sys} = I = get({?MODULE, info}),
    put({?MODULE,info}, I#info{sys = Sys#sys{name = Name}}),
    ok.


%% @spec info(What::atom()) -> term()
%%  What = debug | name | mod | parent
%% @doc retrieves meta-data for the plain_fsm process.
%%  <p>Description of available meta-data:</p>
%%   <pre>
%%     debug : See the manual for sys.erl
%%     name  : Internal name, normally the same as the registered name.
%%             initially undefined, can be set via plain_fsm:store_name/1.
%%     mod   : Name of the callback module.
%%     parent: The pid() of the parent process.
%%   </pre>
%% @end
info(What) ->
    case get({?MODULE,info}) of
	undefined ->
	    exit(badarg);
	#info{} = I ->
	    case What of
		debug  -> I#info.debug;
		name   -> (I#info.sys)#sys.name;
		mod    -> (I#info.sys)#sys.mod;
		parent -> I#info.parent
	    end
    end.


%% @spec extended_receive(Expr) -> VOID
%%
%% @doc Virtual function used to wrap receive clauses.
%% <p>This function cannot be called directly, but is intended as a syntactic
%% wrapper around a receive clause. It will be transformed at compile time
%% to a set of receive patterns handling system messages and parent termination
%% according to the OTP rules. The transform requires that the surrounding
%% function has exactly one argument (the "State" or "Loop Data".)</p>
%% @end
extended_receive(_Expr) ->
    exit(cannot_be_called_directly).


%% @spec parent_EXIT(Reason, State) -> EXIT
%%
%% @doc Handles parent termination properly.
%% <p>This function is called when the parent of a plain_fsm instance dies.
%% The OTP rules state that the child should die with the same reason
%% as the parent (especially in the case of Reason='shutdown'.)</p>
%% @end
parent_EXIT(Reason, _State) ->
    %% no callback - don't know if there should be one...
    exit(Reason).


%% @spec handle_system_msg(Req, From, State, Cont::cont()) -> NEVER_RETURNS
%%
%% @doc Called when the process receives a system message.
%% <p>This function never returns. If the program handles system messages 
%% explicitly, this function can be called to handle them in the plain_fsm
%% way. Example:</p>
%% <pre>
%% idle(S) ->
%%   receive
%%      {system, From, Req} ->
%%          plain_fsm:handle_system_msg(From, Req, S, fun(S1) ->
%%                                                           idle(S1)
%%                                                    end);
%%      ...
%%   end.
%% </pre>
%% <p>The <code>Cont</code> argument should be either a fun with one argument
%% (the new state), which jumps back into the user code in the proper place,
%% or it can be the name of a function (in this case, 'idle'). In the latter
%% case, the function in question must be exported; in the former case, this
%% is not necessary.</p>
%% @end
handle_system_msg(Req, From, State, Cont) ->
    #info{parent = Parent, debug = Debug, sys = Sys} = I =
	get({?MODULE, info}),
    Sys1 = Sys#sys{cont = Cont},
    put({?MODULE,info}, I#info{sys = Sys1}),
    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
			  {Sys1, State}).


%% @hidden
%% @spec system_continue(Parent, Debug, IntState) -> USER_CODE
%%
%% @doc Internal export; handles the jump back into user code.
%%
system_continue(Parent, Debug, IntState) ->
    #info{} = I = get({?MODULE, info}),
    {#sys{mod = Mod, cont = Cont} = Sys, State} = IntState,
    put({?MODULE, info}, I#info{parent = Parent, debug = Debug,
				sys = Sys}),
    case Cont of
	_ when function(Cont) ->
	    Cont(State);
	_ when atom(Cont) ->
	    Mod:Cont(State)
    end.

%% @hidden
%% @spec system_terminate(Reason, Parent, Debug, IntState) -> EXIT
%%
%% @doc Internal export; called if the process is ordered to terminate e g 
%% during upgrade.
%%
system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).


%% @hidden
%% @spec system_code_change(IntState, Module, OldVsn, Extra) -> {ok,NewIntState}
%%
%% @doc Internal export; called in order to change into a newer version of 
%% the callback module.
%%
system_code_change(IntState, Module, OldVsn, Extra) ->
    {Sys,State} = IntState,
    case apply(Module, code_change, [OldVsn, State, Extra]) of
	{ok, NewState} ->
	    {ok, {Sys, NewState}};
	{ok, NewState, NewOptions} when list(NewOptions) ->
	    NewSys = process_options(NewOptions, Sys),
	    {ok, {NewSys, NewState}}
    end.


%% @hidden
%% @spec format_status(Opt, StatusData) -> term()
%%
%% @doc Internal export; called as a result of a call to sys:get_status(FSM).
%% <p>It is possible to provide a function, <code>format_status/2</code>,
%% in the callback module. If such a function is exported, it will be called
%% as <code>Mod:format_status(Opt, [ProcessDictionary, State])</code>, and
%% should return a <code>[{string(), term()}]</code> tuple list.</p>
%% <p>This behaviour is borrowed from gen_server.erl. Unfortunately, both
%% the Mod:format_status/2 callback required by sys.erl, and the optional
%% Mod:format_status/2 callback supported by gen_server.erl are undocumented.
%% </p>
%% @end
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, IntState] = StatusData,
    {#sys{mod = Mod, name = Name}, State} = IntState,
    NameTag = if pid(Name) ->
		      pid_to_list(Name);
		 atom(Name) ->
		      Name
	      end,
    Header = lists:concat(["Status for plain_fsm ", NameTag]),
    Log = sys:get_debug(log, Debug, []),
    Specific = 
        case erlang:function_exported(Mod, format_status, 2) of
            true ->
                case catch Mod:format_status(Opt, [PDict, State]) of
                    {'EXIT', _} -> [{data, [{"State", State}]}];
                    Else -> Else
                end;
            _ ->
                [{data, [{"State", State}]}]
        end,
    [{header, Header},
     {data, [{"Status", SysState},
             {"Module", Mod},
             {"Parent", Parent},
             {"Logged events", Log} |
	     Specific]}].


%%% ================ Internal functions ==================

init(Mod, StartF) ->
    I = #info{},
    Sys = I#info.sys,
    put({?MODULE, info}, I#info{sys = Sys#sys{mod = Mod}}),
    StartF().



process_options(Opts, Sys) ->
    lists:foldl(
      fun({cont, Cont}, S) ->
	      S#sys{cont = Cont};
	 ({mod, Mod}, S) ->
	      S#sys{mod = Mod};
	 ({name, Name}, S) ->
	      S#sys{name = Name}
      end, Sys, Opts).


parent() ->
    case get('$ancestors') of
        [Parent|_] ->
            Parent;
        _ ->
            []
    end.




%%% @hidden
%%% Here's the parse transform code

parse_transform(Forms, _Options) ->
    forms(Forms).


%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0|Fs0]) ->
    F1 = form(F0),
    Fs1 = forms(Fs0),
    [F1|Fs1];
forms([]) -> [].


%% -type form(Form) -> Form.
form({function,Line,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Line,Name,Arity,Clauses};
form(F) ->
    F.

%% -type function(atom(), integer(), [Clause]) -> {atom(),integer(),[Clause]}.
function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0, Name),
    {Name,Arity,Clauses1}.


%% -type clauses([Clause]) -> [Clause].
clauses([C0|Cs], Function) ->
    C1 = clause(C0, Function),
    [C1|clauses(Cs, Function)];
clauses([], _) -> [].


%% -type clause(Clause) -> Clause.
clause({clause,Line,H0,Guard,B0}, Function) ->
    {B1, H1} = exprs(B0, H0, Function),
    {clause,Line,H1,Guard,B1}.

exprs(Es, H, Function) ->
    exprs(Es, [], H, Function).

exprs([E|Es], EAcc, H, Function) ->
    case expr(E, H, Function) of
	{E1, H1} when is_tuple(E1) ->
	    exprs(Es, [E1|EAcc], H1, Function);
	{Es1, H1} when is_list(Es1) ->
	    exprs(Es, lists:reverse(Es1) ++ EAcc, H1, Function)
    end;
exprs([], EAcc, H, _Function) ->
    {lists:reverse(EAcc), H}.


expr({call,Line,{remote,_,{atom,_,?MODULE},{atom,_,extended_receive}},
      [{'receive',Line1,RecvClauses}]}, Head, Function) ->
    %% extended_receive/1 - no timeout
    ExtendedClauses = extend_recv(RecvClauses, Line1, Function),
    E1 = [get_parent_expr(Line1),
	  {'receive', Line, ExtendedClauses}],
    {E1, fix_head(Head)};
expr({call,Line,{remote,_,{atom,_,?MODULE},{atom,_,extended_receive}},
      [{'receive',Line1,RecvClauses, TO, TOExprs}]}, Head, Function) ->
    %% extended_receive/1 with timeout
    ExtendedClauses = extend_recv(RecvClauses, Line1, Function),
    E1 = [get_parent_expr(Line1),
	  {'receive', Line, ExtendedClauses, TO, TOExprs}],
    {E1, fix_head(Head)};
expr(E, H, _) ->
    {E, H}.

fix_head([{match,_L1,{var,_L2,'__FSM_State'},_}] = Head) ->
    Head;
fix_head([Pattern]) ->
    Line = element(2, Pattern),
    [{match, Line, {var,Line,'__FSM_State'}, Pattern}].

get_parent_expr(L) ->
    {match,L,
     {var,L,'__FSM_Parent'},
     {call,L,{remote,L,{atom,L,?MODULE},{atom,L,info}},[{atom,L,parent}]}}.

extend_recv(Cs, L, Function) ->
    [{clause,L,
      [{tuple,L,[{atom,L,'EXIT'},
		 {var, L, '__FSM_Parent'},
		 {var, L, '__FSM_Reason'}]}],
      [],
      [{call, L, {remote, L,
		  {atom, L, ?MODULE},
		  {atom, L, parent_EXIT}},
	[{var, L, '__FSM_Reason'},
	 {var, L, '__FSM_State'}]}]
     },
     {clause,L,
      [{tuple,L,[{atom,L,system},
                 {var, L,'__FSM_From'},
                 {var, L,'__FSM_Req'}]}],
      [],
      [{call, L, {remote, L,
                  {atom, L, ?MODULE},
                  {atom, L, handle_system_msg}},
        [{var, L, '__FSM_Req'},
         {var, L, '__FSM_From'},
         {var, L, '__FSM_State'},
         {'fun', L, {clauses, [{clause, L, [{var,L,'__FSM_Sx'}], [],
                                [{call, L, {atom,L,Function},
                                  [{var,L,'__FSM_Sx'}]}]}]}}
        ]}
      ]} | Cs].
