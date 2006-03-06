%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% %CCaseFile:	proc.erl %
%%% %CCaseRev:	/main/R1A/16 %
%%% %CCaseDate  %
%%% %CCaseDocNo:	102/190 55-CNA 113 33 Ux %
%%%
%%% @copyright Ericsson AB 2005 All rights reserved.  
%%% 
%%% The information in this document is the property of Ericsson. Except
%%% as specifically authorized in writing by Ericsson, the receiver of
%%% this document shall keep the information contained herein
%%% confidential and shall protect the same in whole or in part from
%%% disclosure and dissemination to third parties. Disclosure and
%%% disseminations to the receivers employees shall only be made on a
%%% strict need to know basis.
%%% @end
%%%----------------------------------------------------------------------

%%% @doc Flexible Local Process Registry 
%%%
%%%  <p>This application supports local registration of processes using any
%%%  erlang term. Furthermore, each process can register by several
%%%  different names at the same time.</p>
%%%
%%%  <h2>Semantics:</h2> <p>`proc' works with unique `names' and
%%%  non-unique `properties'.  </p><p>A name can be any erlang term
%%%  (except a pid or '_'), and each process can register itself under many
%%%  different unique names. 
%%%  A `property' can be any term (except '_'), and must be unique 
%%%  within a process, but several processes can register the same
%%%   property.</p>
%%%  <p>Furthermore, there are "fold" and "select"
%%%  functions to allow for efficient iteration over names or properties in the
%%%  process registry.</p>
%%%  <p>Many of the operations to access names and properties rely on
%%%  the semantics given by {@link //stdlib/ets}, and this is often 
%%%  mentioned in the interface documentation. This means that it is 
%%%  safe to assume that the functions conform to the search semantics
%%%  of ordered_set ets tables. However, it is <i>not</i> safe to assume
%%%  anything about whether the data in fact resides in ets tables, and
%%%  if so, what those tables are called, or indeed how the data is 
%%%  represented internally.</p>
%%%  @end


-module(proc).
-id('102/190 55-CNA 113 33 Ux').
-vsn('/main/R1A/16').
-date('2005-12-09').
-behaviour(gen_server).

%%%----------------------------------------------------------------------
%%% Template Id: ETX/B 00201 - 19055/1 Rev C
%%% 
%%% #Copyright (C) 1996
%%% by ERICSSON TELECOM AB
%%% S - 125 26  STOCKHOLM
%%% SWEDEN, tel int + 46 8 719 0000
%%% 
%%% The program may be used and/or copied only with the written permission from
%%% ERICSSON TELECOM AB, or in accordance with the terms and conditions
%%% stipulated in the agreement/contract under which the program has been
%%% supplied.
%%% 
%%% All rights reserved
%%% 
%%%----------------------------------------------------------------------
%%% #1.    REVISION LOG
%%%----------------------------------------------------------------------
%%% Rev      Date       Name        What
%%% -----    -------    --------    -------------------------------------
%%% R1A/1    20050630   ebcfsva     Checked Ulf Wiger's code into CC
%%% R1A/2    20050704   etxuwig     Renamed tables, use proc_tabs, etc.
%%% R1A/3    20050816   etxuwig     Fixed fold_keys, added await_reg()
%%% R1A/4    20050901   etxuwig     Added load regulation support, etc.
%%% R1A/5    20050902   etxuwig     Changed semantics of non-unique keys
%%% R1A/6    20050902   etxuwig     Changed names (API)
%%% R1A/7    20050905   etxuwig     update_property -> replace_property
%%%----------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%%----------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%%----------------------------------------------------------------------
-export([
	 %% Functions for registering, unregistering, sending,
	 reg/1,
	 unreg/1,
	 where/1,
	 send/2,
	 %% New functions for setting properies (what used to be (Key,Value))
	 add_property/1, add_property/2,
	 is_property/1, is_property/2,
	 replace_property/2, replace_property/3,
	 del_property/1, del_property/2,
	 set_access/1,
	 %% Counters
	 add_counter/2, add_counter/3,
	 read_counter/1, read_counter/2,
	 del_counter/1, del_counter/2,
	 update_counter/2, update_counter/3,
	 is_counter/1, is_counter/2,
	 guards/1,
	 %% Utility function allowing for notifications when someone registers
	 await_reg/1, clear_await_reg/1,
	 %% 
	 pids/1, pids/2,
	 select_pattern/1, select_pattern/2,
	 %% Functions for searching properties
	 select_properties/1, select_properties/2,
	 fold_properties/3, fold_properties/4, fold_properties/5, 
	 select_fold_properties/3, select_fold_properties/4,
	 select_fold_properties/5,
	 properties_by_pid/1, properties_by_pid/2, properties_by_pid/3,
	 %% Functions for searching unique names
	 select_names/1, select_names/2,
	 select/1,
	 fold_names/3, fold_names/4, fold_names/5,
	 first/1,
	 next/2,
	 previous/2,
	 last/1
	]).
-export([info/1,
	 info/2]).

-export([enable_i/0]).

%%% gen_server start function
-export([start_link/0]).

%%% utility functions
-export([i/1]).

%%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%-export([start/0]).

-include_lib("stdlib/include/ms_transform.hrl").
-define(REG, proc).
-define(REG_REV, procRev).
-define(PROPS, procProps).
-define(PROPS_REV, procPropsRev).
-define(PRIVS, procPrivs).
-define(MONITORS, procMonitors).

-define(BADARG, throw(badarg)).
-define(EACCESS, throw({badarg,access})).

%% start() ->
%%     application:start(proc).


%%% ===================== (unique) Names =========================



%%% @spec reg(Name) -> true
%%% @doc Register a unique `Name'. Returns `true' or exits.
%%% <p>This function differs from `erlang:register(Name,Pid)' in 
%%% the following ways:</p>
%%% <ul>
%%%  <li>Pid is implicit: always the current process.</li>
%%%  <li>Name can be any term (except a pid or '_'), not just an atom.</li>
%%%  <li>A process can be registered under several unique names.</li>
%%% </ul>
%%% @end
%%%
reg(Pid) when is_pid(Pid) ->
    erlang:error(badarg);
reg('_') ->
    erlang:error(badarg);
reg(Name) ->
    call({reg, Name}).

%%% @spec unreg(Name) -> true
%%% @doc Unregister `Name'
%%% <p>This function exits if `Name' isn't registered to the current
%%% process.</p>
%%% @end
%%%
unreg(Name) ->
    call({unreg, Name}).

%%% @spec where(Name) -> pid() | undefined
%%% @doc Analogous to `erlang:whereis(Name)'
%%%
where(Name) ->
    case ets:lookup(?REG, Name) of
	[{_, Pid}] ->
	    case erlang:is_process_alive(Pid) of
		true ->
		    Pid;
		false ->
		    undefined
	    end;
	[] ->
	    undefined
    end.

%%% @spec send(Name, Msg) -> Msg
%%% @doc Analogous to `Name ! Msg', except that send({Node,Dest}, Msg)
%%% will be interpreted as 'send to the local process registered as 
%%% {Node, Dest}'. If Name is a pid, this function will send the message
%%% to the process with process identifier Name (recall that it is not
%%% possible to register a pid as a unique name in proc.)
%%% @end
%%%
send(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg;
send(Name, Msg) ->
    case where(Name) of
	undefined ->
	    erlang:error(badarg);
	Pid when is_pid(Pid) ->
	    Pid ! Msg
    end.


%%% @spec await_reg(Name) -> {already_registered,pid()} | Reg
%%% @doc Request to be notified when someone registers as `Name'.
%%% <p>If `Name' is already registered, the function returns 
%%% with the value `{already_registered, Pid}'. Otherwise, it returns
%%% a "reference" (not necessarily of type `reference()') that can be used
%%% to recognize a future message.</p>
%%% <p>When some process `P' registers as `Name', a message on the form
%%% `{sysProgReg, Ref, reg, Name, Pid}' is sent to each waiting process
%%% (note that Ref will be different for each waiting process.)</p>
%%% @end
%%%
await_reg(Pid) when is_pid(Pid) ->
    erlang:error(badarg);
await_reg('_') ->
    erlang:error(badarg);
await_reg(Name) ->
    call({await_reg, Name}).

%%% @spec clear_await_reg(Ref) -> true
%%% @doc Cancel waiting for notification of process registration of a 
%%% given name.
%%% <p>This function exits with `badarg' if Ref does not appear to be 
%%% a valid reference returned by {@link await_reg/1}; otherwise returns
%%% `true'.</p>
%%% @end
%%%
clear_await_reg(Ref) ->
    call({clear_await_reg, Ref}).


%%% ================ (non-unique) Properties =====================

%%% @spec add_property(Property::term()) -> true
%%% @doc Publish a property of the current process.
%%% <p>If `Property' is already published for the current process,
%%% this function exits with badarg.</p>
%%%
%%% <p>This operation can be viewed as publishing the meta-data `Property'
%%% as part of the interface of the process. Several processes may publish
%%% the same property. The process can be found (as part of a group)
%%% through this property, using one of the functions {@link
%%% fold_properties/3}, {@link pids/1}, et al.</p>
%%% @end
%%%
add_property('_') ->
    erlang:error(badarg);
add_property(Property) ->
    call({add_property, self(), Property}).

%%% @spec add_property(Process, Property) -> true
%%% @doc Publish a property on behalf of another process.
%%% <p>Adding properties on behalf of another process is only 
%%% allowed if the other process has called {@link set_access/1},
%%% giving this process the proper rights to act as proxy.</p>
%%% <p>`Process' can be either a `pid()' or a registered name.</p>
%%% @end
%%%
add_property(_Process, '_') ->
    erlang:error(badarg);
add_property(Process, Property) ->
    call({add_property, Process, Property}).



%%% @spec set_access(Access::[{Action,Processes,Ops}]) -> true
%%%      Action = grant | revoke
%%%         Ops = [Op]
%%%         Op  = add_property | replace_property | del_property | properties |
%%%               add_counter | update_counter | del_counter | counters
%%% @doc Control the ability of Processes to perform certain functions
%%%  on behalf of the current process. 
%%% <p>`Op' must be one of</p>
%%% <ul>
%%%  <li>`add_property'</li>
%%%  <li>`replace_property'</li>
%%%  <li>`del_property'</li>
%%%  <li>`properties' (perform all operations on properties)</li>
%%%  <li>`add_counter'</li>
%%%  <li>`update_counter'</li>
%%%  <li>`del_counter'</li>
%%%  <li>`counters' (perform all operations on counters)</li>
%%% </ul>
%%% <p>`Processes' is a list of registered names and/or pids.</p>
%%% <p>Example:</p><pre>
%%% set_access([{grant, ["foo","bar"], [update_counter]},
%%%             {revoke, ["baz"], [properties]}])</pre>
%%% <p>Allows the processes registered as `"foo"' and `"bar"' to update
%%% counters, and makes sure that `"baz"' is no longer able to add, delete
%%% or replace properties.</p>
%%% @end
%%%
set_access(Access) when is_list(Access) ->
    call({set_access, Access}).

%%% @spec is_property(Property) -> true | false
%%% @doc Returns `true' if `Property' is a published property of the current
%%% process.
%%% @end
%%%
is_property(Property) ->
    is_property(self(), Property).

%%% @spec is_property(Process, Property) -> true | false
%%% @doc Check whether Property is in fact a property registered with Process.
%%% <p>`Process' can be either a pid or a registered name.</p>
%%% @end
%%%
is_property(Pid, Property) when is_pid(Pid) ->
    ets:member(?PROPS_REV, {Pid, Property});
is_property(Process, Property) ->
    Pid = on_pid(Process),
    ets:member(?PROPS_REV, {Pid, Property}).

%%% @spec is_counter(Property) -> true | false
%%% @doc Check whether `Property' is a counter property.
%%% <p>Counter properties are created through {@link add_counter/2}, and
%%% behave as normal properties with the following exceptions:</p>
%%% <ul>
%%% <li>A counter property has an integer value attached, which can be 
%%%     updated using {@link update_counter/2}</li>
%%% <li>Counter properties cannot be replaced using
%%%     {@link replace_property/2}</li>
%%% </ul>
%%% @end
%%%
is_counter(Property) ->
    is_counter(self(), Property).

%%% @spec is_counter(Process, Property) -> true | false
%%% @doc Check whether `Propery' registered under `Process' is a counter
%%% property.
%%% <p>`Process' can be either a pid or a registered name.</p>
%%% @end
%%%
is_counter(Pid, Property) when is_pid(Pid) ->
    (ets:select_count(?PROPS_REV, [{{{Pid,Property},counter},[],[true]}]) > 0);
is_counter(Process, Property) ->
    Pid = on_pid(Process),
    (ets:select_count(?PROPS_REV, [{{{Pid,Property},counter},[],[true]}]) > 0).


%%% @spec replace_property(OldProperty, NewProperty) -> true
%%% @doc Equivalent to del_property(OldProperty), add_property(NewProperty),
%%% but much more efficient.
%%% <p>If `OldProperty' is not a published property of the current process
%%% (see {@link add_property/1}), the function exits with `badarg'.</p>
%%% <p>One could use properties as a "published process dictionary",
%%% e.g. by replacing `put(Tag,Value)' with 
%%% `proc:add_property({Tag,Value})', and `get(Tag)' with (roughly)
%%% `proc:properties_by_pid(self(),Tag)'.
%%% While this would be somewhat less efficient than the built-in process
%%% dictionary, it has the advantage of allowing other processes to key
%%% on process meta-data in a much more efficient and disciplined way than
%%% `{_,Dict} = process_info(Pid,dictionary), lists:keysearch(Tag,1,Dict)'
%%% (Which makes no distinction between public and private data, and therefore
%%% is rightfully frowned upon, and hopefully never attempted.)"</p>
%%% <p><i>Note:</i> This function does not work on counter properties.</p>
%%% @end
%%%
replace_property(_OldProp, '_') ->
    erlang:error(badarg);
replace_property(OldProperty, NewProperty) ->
    call({replace_property, self(), OldProperty, NewProperty}).

%%% @spec replace_property(Process, OldProperty, NewProperty) -> true
%%% @doc Replace `OldProperty' with `NewProperty' on behalf of `Process'.
%%% <p>This function is allowed only if permission to replace properties
%%% has been given through `Process' calling {@link grant_access/2}.</p>
%%% <p>If permission to replace properties has not been given, this function
%%% exits with `access'.</p>
%%% @end
%%%
replace_property(_Process, _OldProperty, '_') ->
    erlang:error(badarg);
replace_property(Process, OldProperty, NewProperty) ->
    call({replace_property, Process, OldProperty, NewProperty}).


%%% @spec del_property(Property) -> true
%%% @doc Un-publishes the property `Property' for the current process.
%%% <p>If there is no published property `Property'
%%% (see {@link add_property/1}), for the current process, the function
%%%  exits with `badarg'.</p>
%%% @end
%%%
del_property(Property) ->
    call({del_property, self(), Property}).

%%% @spec del_property(Process, Property) -> true
%%% @doc Un-publish the property `Property' on behalf of `Process'.
%%% <p>This function is allowed only if permission to delete properties
%%% has been given through `Process' calling {@link grant_access/2}.</p>
%%% <p>If permission to delete properties has not been given, this function
%%% exits with `access'.</p>
%%% @end
%%%
del_property(Process, Property) ->
    call({del_property, Process, Property}).

%%% @spec add_counter(Name, InitialValue::integer()) -> true
%%% @doc Publish a counter property for the current process.
%%% <p>Counter properties behave as normal properties with
%%%    the following exceptions:</p>
%%% <ul>
%%% <li>A counter property has an integer value attached, which can be 
%%%     updated using {@link update_counter/2}</li>
%%% <li>Counter properties cannot be replaced using
%%%     {@link replace_property/2}</li>
%%% </ul>
%%% @end
%%%
add_counter('_', _Initial) ->
    erlang:error(badarg);
add_counter(Name, InitialValue) when is_integer(InitialValue) ->
    call({add_counter, self(), Name, InitialValue}).

%%% @spec add_counter(Process, Name, InitialValue::integer()) -> true
%%% @doc Publish a counter property on behalf of `Process'.
%%% <p>This function is allowed only if permission to add counters
%%% has been given through `Process' calling {@link grant_access/2}.</p>
%%% <p>If permission to add counters has not been given, this function
%%% exits with `access'.</p>
%%% @end
%%%
add_counter(_Process, '_', _Initial) ->
    erlang:error(badarg);
add_counter(Process, Name, InitialValue) when is_integer(InitialValue) ->
    call({add_counter, Process, Name, InitialValue}).

%%% @spec del_counter(Name) -> true
%%% @doc Un-publish a counter property for the current process.
%%%
del_counter(Name) ->
    call({del_counter, self(), Name}).

%%% @spec del_counter(Process, Name) -> true
%%% @doc Un-publish a counter property on behalf of `Process'.
%%% <p>This function is allowed only if permission to delete counters
%%% has been given through `Process' calling {@link grant_access/2}.</p>
%%% <p>If permission to delete counters has not been given, this function
%%% exits with `access'.</p>
%%% @end
%%%
del_counter(Process, Name) ->
    call({del_counter, Process, Name}).

%%% @spec read_counter(Name) -> integer()
%%% @doc Read the value of a counter property for the current process.
%%%
read_counter(Name) ->
    do_read_counter(self(), Name).

%%% @spec read_counter(Process, Name) -> integer()
%%% @doc Read the value of a counter property published by `Process'.
%%%
read_counter(Pid, Name) when is_pid(Pid) ->
    do_read_counter(Pid, Name);
read_counter(Process, Name) ->
    case where(Process) of
	Pid when is_pid(Pid) -> do_read_counter(Pid, Name);
	_ ->
	    erlang:error(badarg)
    end.

do_read_counter(Pid, Name) ->
    case ets:lookup_element(?PROPS, {Name, Pid}, 2) of
	I when is_integer(I) ->
	    I;
	_ ->
	    erlang:error(badarg)
    end.
   
%%% @spec update_counter(Name, Incr) -> integer()
%%% @doc Update the counter attribute of a counter property.
%%% <p>This function only works on counter properties. The `Incr' argument
%%% is passed as-is to {@link //stlib/ets:update_counter/3}.
%%% If a complex option, such as `{Pos, Incr}' is used,
%%% `Pos' must be equal to 2.</p>
%%% @end
%%%
update_counter(Name, Incr) ->
    ets:update_counter(?PROPS, {Name, self()}, Incr).

%%% @spec update_counter(Process, Name, Incr) -> integer()
%%% @doc Update counter attribute on behalf of `Process'.
%%% <p>This function is allowed only if permission to update counters
%%% has been given through `Process' calling {@link grant_access/2}.</p>
%%% <p>If permission to update counters has not been given, this function
%%% exits with `access'.</p>
%%% @end
%%%
update_counter(Process, Name, Value) ->
    try begin
	    Pid = on_pid(Process),
	    verify_access(Pid, update_counter, self()),
	    ets:update_counter(?PROPS, {Name, Pid}, Value)
	end
    catch 
	throw:Error ->
	    erlang:error(Error)
    end.

%%% @spec pids(Property) -> [pid()]
%%% @doc Returns all pids for which a property `Property' is published.
%%% <p>To be more precise, `Property' can be any pattern that would be
%%% accepted by {@link //stdlib/ets:select/2}, e.g. &apos;_&apos;</p>
%%% <p>If one imagines an `ordered_set ets' table `Tab' where all properties
%%% are stored on the form `{{Property,Pid},1}', then calling 
%%% this function is equivalent to calling
%%% <pre>ets:select(Tab, [{{{Property,'$1'},'_'}, [], ['$1']}]).</pre></p>
%%% <p>Note that this is also true as regards performance. If the `Key'
%%% pattern is unbound, the operation will be a linear search.</p>
%%% @end
%%%
pids(Property) ->
    pids(Property, []).

%%% @spec pids(Property, Guards::list()) -> [pid()]
%%% @doc Similar to {@link pids/1}, but also allowing select guards.
%%% <p>If one imagines an `ordered_set ets' table `Tab' where all non-unique
%%% keys are stored on the form `{{Property,Pid},1}', then calling 
%%% this function is equivalent to calling
%%% <pre>ets:select(Tab, [{{{Property,'$1'},'_'}, Guards, ['$1']}]).</pre>
%%% </p>
%%% <p>Note that this is also true as regards performance. If the `Property'
%%% pattern is unbound, the operation will be a linear search.</p>
%%% @end
%%%
pids(Property, Guards) ->
    ets:select(?PROPS, [{{{Property,'_'},'_'}, Guards, 
			[{element,2,{element,1,'$_'}}]}]).

%%% --

%%% @spec fold_properties(Fun::function(), Acc, Property) -> NewAcc
%%% @doc Similar to {@link //stdlib/lists:foldl/3}, but based on a
%%% select pattern on `Property', identifying a sub-set of
%%% published properties.
%%% <p>For each matching key, a call is made to 
%%% <pre>Fun({Property1,Pid1}, Acc1)</pre>, which is expected to 
%%% return `NewAcc'. Note that, as in {@link pids/1}, `Property'
%%% can be select a pattern. The following expression would return a 
%%% list of all published instances of `Property' together with the processes
%%% that registered them:</p>
%%% <pre>
%%% proc:fold_properties(fun(X,Acc) -> [X|Acc] end, [], '_')
%%% </pre>
%%% <p>This function is equivalent to `fold_properties(Fun,Acc,Property,1)',
%%% (see {@link fold_properties/4}.) See also {@link select_pattern/1}.</p>
%%% @end
%%%
fold_properties(Fun, Acc, Property) ->
    fold_properties(Fun, Acc, Property, 1, fun(_) -> true end).

%%% @spec fold_properties(Fun::function(), Acc, Key, Limit::integer()) -> 
%%% NewAcc
%%% @doc Like {@link fold_properties/3}, but works in batches of `Limit'
%%% objects at a time, in order to improve raw performance.
%%% <p>`fold_properties/3' uses a default limit of 1 object at a time.</p>
%%% <p>This function is equivalent to 
%%% `fold_properties(Fun,Acc,Property,Limit,fun(_) -> true end)',
%%% (see {@link fold_properties/5}.) See also {@link select_pattern/1}.</p>
%%% @end
%%%
fold_properties(Fun, Acc, Properties, Limit) ->
    fold_properties(Fun, Acc, Properties, Limit, fun(_) -> true end).

%%% @spec fold_properties(Fun::function(), Acc, Property,
%%%     Limit::integer(), Regulator::function()) -> NewAcc
%%% @doc Like {@link fold_properties/4}, but applies a "Regulator function"
%%% after each batch of objects before proceeding with the next one.
%%% <p>The `Regulator' function is expected to take the current accumulator
%%% as an argument and return either `true' (in which case processing 
%%% continues), or `false' (in which case the `fold_properties/5' function
%%% breaks and returns the current accumulator. See also 
%%% {@link select_pattern/1}.</p>
%%% @end
fold_properties(Fun, Acc, Property, Limit, Regulator) ->
    select_fold_properties(
      Fun, Acc, [{Property,[],[true]}], Limit, Regulator).


%%% @spec select_pattern(What) -> Patterns
%%% @doc Helper function to generate select patterns from the more
%%% intuitive patterns `Property' (or `Name', as it works equally for names).
%%% <p>The return value from this function can be used in e.g. 
%%% {@link select_properties/1}, {@link select_properties/2},
%%% {@link select_fold_properties/3},
%%% {@link select_fold_properties/4} or {@link select_fold_properties/5},
%%% or similarly, {@link select_names/1}, {@link select_names/2},
%%% {@link select_fold_names/3}, {@link select_fold_names/4}, or 
%%% {@link select_fold_names/5}.</p>
%%% <p>`Patterns' is a list of match specifications on the same form as 
%%% that used by {@link //stdlib/ets:select_delete/2} and
%%% {@link //stdlib/ets:select_count/2},
%%% that is:</p>
%%% <pre>
%%%   * Patterns = [MatchFunction] | '_'
%%%   * MatchFunction = {MatchHead, [Guard], [Result]}
%%%   * MatchHead = "Pattern as in ets:match"
%%%   * Guard = {"Guardtest name", ...}
%%%   * Result = true (if object is to be included)
%%% </pre>
%%% @end
select_pattern(What) ->
    [{What, [], [true]}].

%%% @spec select_pattern(What, Guards) -> Patterns
%%% @doc As {@link select_pattern/1}, but with the option to add Guard
%%% expressions. 
%%%
select_pattern(What, Guards) ->
    [{What, Guards, [true]}].


%%% @spec guards(List) -> NewList
%%% @doc Expand a list of `select' guards, allowing for the pseudo guard
%%% `is_counter'.
%%% <p>How counter properties can be distinguished from regular properties
%%% internally is not defined in the proc interface. Using this function,
%%% it is however possible to specify a guard test, `is_counter', which will
%%% expand to a legal guard. The `is_counter' test can be combined with the
%%% standard logical operators, `not', `and', `or', `orelse', `andalso',
%%% and `xor'.</p>
%%% <p>Example:</p>
%%% <pre>
%%% proc:select_properties(
%%%     [{'_', proc:guards([{'not', is_counter}]), [true]}]).
%%% </pre>
%%% <p>will list all published properties that are not counters.</p>
%%% @end
%%%
%%% Note that this function must work for both PROPS and PROPS_REV.
%%% For now, this affects counters. The second element of a counter 
%%% must be an integer, in both PROPS and PROPS_REV.
%%%
guards([is_counter|Gs]) ->
    [{is_integer, {element, 2, '$_'}} | guards(Gs)];
guards([{'not', G}|Gs]) ->
    [G1] = guards([G]),
    [{'not', G1} | guards(Gs)];
guards([G|Gs]) when is_tuple(G), size(G) > 1 ->
    case element(1,G) of
	Op when Op == 'and';
		Op == 'or';
		Op == 'andalso';
		Op == 'xor';
		Op == 'orelse' ->
	    Gs1 = tl(tuple_to_list(G)),
	    Gs2 = guards(Gs1),
	    [list_to_tuple([Op|Gs2]) | guards(Gs)];
	_ ->
	    [G | guards(Gs)]
    end;
guards([G|Gs]) ->
    [G | guards(Gs)];
guards([]) ->
    [].
	     
     

%%% @spec select_properties(Patterns) -> {Objs, Continuation} | '$end_of_table'
%%% @doc Returns `{Property,Pid}' objects one at a time based on 
%%% a given selection of all registered instances of `Key'.
%%% <p>`Patterns' can be written as follows:</p>
%%% <pre>
%%%   * Patterns = [MatchFunction] | '_'
%%%   * MatchFunction = {MatchHead, [Guard], [Result]}
%%%   * MatchHead = "Pattern as in ets:match"
%%%   * Guard = {"Guardtest name", ...}
%%%   * Result = true (if object is to be included)
%%% </pre>
%%% <p>or generated using {@link select_pattern/1} or 
%%% {@link select_pattern/2}.</p>
%%% <p>For convenience, the special pattern '_' is also accepted.
%%% It expands to <code>[{'_', [], [true]}]</code>.</p>
%%% <p>The `Continuation' returned can be used when calling {@link select/1}
%%% in order to continue processing.</p>
%%% @end
%%%
select_properties(Patterns) ->
    select_properties(Patterns, 1).



%%% @spec select_properties(Patterns, Limit::integer()) -> 
%%%   {Objs, Continuation} | '$end_of_table'
%%% @doc Like {@link select_properties/1} but works in batches of `Limit'
%%% objects at a time in order to improve raw performance.
%%%
select_properties(Patterns0, Limit) ->
    Patterns = if Patterns0 == '_' ->
		       [{'_',[],[true]}];
		  true ->
		       Patterns0
	       end,
    Patterns1 = [{{{P,'_'},'_'},G,[{{{element,1,{element,1,'$_'}},
 				     {element,2,{element,1,'$_'}}}}]} ||
 		    {P,G,[true]} <- Patterns],
    ets:select(?PROPS, Patterns1, Limit).



%%% @spec select_fold_properties(Fun, Acc, Patterns) -> NewAcc
%%% @doc Like {@link fold_properties/3} but more flexible as it allows
%%% for combining several patterns into one query.
%%% <p>`Patterns' is composed as for {@link select_properties/1}.</p>
%%% @end
%%%
select_fold_properties(Fun, Acc, Patterns) ->
    select_fold_properties(Fun, Acc, Patterns, 1).


%%% @spec select_fold_properties(Fun, Acc, Patterns, Limit) -> NewAcc
%%% @doc Like {@link select_fold_properties/3}, but works in batches of `Limit'
%%% objects at a time, in order to improve raw performance.
%%% <p>`select_fold_properties/3' uses a default limit of 1 object at a
%%% time.</p>
%%% <p>This function is equivalent to 
%%% `select_fold_properties(Fun,Acc,Patterns,Limit,fun(_) -> true end)',
%%% (see {@link select_fold_properties/5}.)</p>
%%% @end
select_fold_properties(Fun, Acc, Patterns, Limit) ->
    select_fold_properties(Fun, Acc, Patterns, Limit, fun(_) -> true end).


%%% @spec select_fold_properties(Fun::function(), Acc, Property,
%%%     Limit::integer(), Regulator::function()) -> NewAcc
%%% @doc Like {@link select_fold_properties/4}, but applies a
%%% "Regulator function"
%%% after each batch of objects before proceeding with the next one.
%%% <p>The `Regulator' function is expected to take the current accumulator
%%% as an argument and return either `true' (in which case processing 
%%% continues), or `false' (in which case the `select_fold_properties/5'
%%% function breaks and returns the current accumulator.</p>
%%% @end
select_fold_properties(Fun, Acc, Patterns, Limit, Regulator) ->
    Res = select_properties(Patterns, Limit),
    select_each_property(Res, Fun, Acc, Regulator).

select_each_property({Objs, Continuation}, Fun, Acc, Regulator) ->
    NewAcc = lists:foldl(
	       fun({_Prop,Pid} =X, Acc1) when is_pid(Pid) ->
		       Fun(X, Acc1)
	       end, Acc, Objs),
    case Regulator(NewAcc) of
	true ->
	    select_each_property(ets:select(Continuation),
				 Fun, NewAcc, Regulator);
	false ->
	    NewAcc
    end;
select_each_property('$end_of_table', _, Acc, _Regulator) ->
    Acc.


%%% @spec select_names(Patterns) -> {Objs, Continuation} | '$end_of_table'
%%% @doc Returns `{Name,Pid}' objects one at a time based on 
%%% a given selection of all registered unique names.
%%% <p>`Patterns' can be written as follows:</p>
%%% <pre>
%%%   * Patterns = [MatchFunction] | '_'
%%%   * MatchFunction = {MatchHead, [Guard], [Result]}
%%%   * MatchHead = "Pattern as in ets:match"
%%%   * Guard = {"Guardtest name", ...}
%%%   * Result = true (if object is to be included)
%%% </pre>
%%% <p>or generated using {@link select_pattern/1} or 
%%% {@link select_pattern/2}.</p>
%%% <p>The function is <i>almost</i> equivalent to
%%% `ets:select(Tab,Patterns, 100)' on an `ordered_set' table `Tab'
%%%  with the following representation:</p>
%%% <pre>{{Key, Value}, Pid}</pre>
%%% <p>The difference compared to `ets:select/3' is the `Result' expression.
%%% In this function, `Result' is expected to be &apos;true&apos; for those
%%% objects that are to be included.</p>
%%% <p>For convenience, the special pattern '_' is accepted, 
%%% and expanded to <code>[{'_', [], [true]}]</code>.</p>
%%% <p>Any other patterns are ignored.</p>
%%% <p>The `Continuation' returned can be used when calling {@link select/1}
%%% in order to continue processing.</p>
%%% @end
%%%
select_names(Patterns) ->    
    select_names(Patterns, 100).

%%% @spec select_names(Patterns, Limit) -> 
%%%    {Objs, Continuation} | '$end_of_table'
%%% @doc Like {@link select_names/1}, but works in batches of `Limit'
%%% objects at a time, in order to improve raw performance.
%%%
select_names(Patterns0, Limit) ->
    Patterns = if Patterns0 == '_' ->
		       [{'_',[],[true]}];
		   true ->
		       Patterns0
	       end,
    Patterns1 =
	[{P,[{is_pid,{element,2,'$_'}}|G],
	  [{{{element,1,'$_'},
	     {element,2,'$_'}}}]} ||
	    {P,G,[true]} <- Patterns],
    ets:select(?REG, Patterns1, Limit).


%%% @spec fold_names(Fun::function(), Acc, Patterns) -> NewAcc
%%% @doc Similar to {@link lists:foldl/3}, but based on a list of select
%%% patterns, identifying a sub-set of unique names.
%%% <p>For each matching name, a call is made to 
%%% <pre>Fun({Name1,Pid1}, Acc1)</pre>, which is expected to 
%%% return `NewAcc'.</p>
%%% <p>`Patterns' is a list of match specifications on the same form as 
%%% that used by {@link ets:select_delete/2} and {@link ets:select_count/2},
%%% that is:</p>
%%% <pre>
%%%   * Patterns = [MatchFunction] | '_'
%%%   * MatchFunction = {MatchHead, [Guard], [Result]}
%%%   * MatchHead = "Pattern as in ets:match"
%%%   * Guard = {"Guardtest name", ...}
%%%   * Result = true (if object is to be included)
%%% </pre>
%%% <p>The following expression would return a 
%%% list of all registered unique names together with the processes
%%% that registered them:</p>
%%% <pre>
%%% proc:fold_names(fun(X,Acc) -> [X|Acc] end, [], [{'_',[],[true]}])
%%% </pre>
%%% <p>For convenience, the special pattern '_' is also allowed. It is
%%% expanded to <code>[{'_',[],[true]}]</code>.</p>
%%% <p>This function is equivalent to `fold_names(Fun,Acc,Patterns,100)',
%%% (see {@link fold_names/5}.)</p>
%%% @end
%%%
fold_names(Fun, Acc, Patterns) ->
    fold_names(Fun, Acc, Patterns, 100).


%%% @spec fold_names(Fun, Acc, Patterns, Limit::integer()) -> NewAcc
%%% @doc Like {@link fold_names/3}, but works in batches of `Limit' objects
%%% at a time, in order to improve memory characteristics.
%%% <p>`fold_names/3' uses a default limit of 1 object at a time.</p>
%%% <p>This function is equivalent to 
%%% `fold_names(Fun,Acc,Patterns,Limit,fun(_) -> true end)',
%%% (see {@link fold_names/5}.)</p>
%%% @end
%%%
fold_names(Fun, Acc, Patterns, Limit) ->
    fold_names(Fun, Acc, Patterns, Limit, fun(_) -> true end).


%%% @spec fold_names(Fun::function(), Acc, Patterns,
%%%     Limit::integer(), Regulator::function()) -> 
%%% NewAcc
%%% @doc Like {@link fold_names/4}, but applies a "Regulator function"
%%% after each batch of objects before proceeding with the next one.
%%% <p>The `Regulator' function is expected to take the current accumulator
%%% as an argument and return either `true' (in which case processing 
%%% continues), or `false' (in which case the `fold_names/5' function breaks
%%% and returns the current accumulator.</p>
%%% @end
%%%
fold_names(Fun, Acc, Patterns, Limit, Regulator) ->
    Res = select_names(Patterns, Limit),
    select_each_name(Res, Fun, Acc, Regulator).
    
select_each_name({Objs, Continuation}, Fun, Acc, Regulator) ->
    NewAcc = lists:foldl(
	       fun(Key, Acc1) ->
		       Fun(Key, Acc1)
	       end, Acc, Objs),
    case Regulator(NewAcc) of
	true ->
	    select_each_name(ets:select(Continuation), Fun, NewAcc, Regulator);
	false ->
	    NewAcc
    end;
select_each_name('$end_of_table', _, Acc, _Regulator) ->
    Acc.


%%% @spec select(Continuation) -> {[Obj], NewContinuation} | '$end_of_table'
%%% @doc Analogous to {@link ets:select/1}.
%%% <p>This function is intended to be called with a `Continuation' 
%%% returned from {@link select_names/1} or {@link select_properties/2}.</p>
%%% @end
%%%
select(Continuation) ->
    ets:select(Continuation).


%%% @spec first(Type:: names | properties) -> Key | '$end_of_table'
%%% @doc Analogous to {@link ets:first/1}.
%%% <p>The tables corresponding to `names' and `properties' both have 
%%% `ordered_set' semantics.</p>
%%% @end
first(properties) ->
    ets:first(?PROPS);
first(names) ->
    ets:first(?REG).

%%% @spec next(Type:: names | properties, Key) -> Key | '$end_of_table'
%%% @doc Analogous to {@link ets:next/2}.
%%% <p>The tables corresponding to `names' and `properties' both have 
%%% `ordered_set' semantics. The key format of the `properties' table is
%%% `{Property, Pid}', while the key format of the `names' table is
%%% simply the name. Note that `names' and `properties' are not likely
%%% the physical names of the actual tables.</p>
%%% @end
next(properties, Prev) ->
    ets:next(?PROPS, Prev);
next(names, Prev) ->
    ets:next(?REG, Prev).

%%% @spec previous(Type:: names | properties, Key) -> Key | '$end_of_table'
%%% @doc Analogous to {@link ets:previous/2}.
%%% <p>The tables corresponding to `names' and `properties' both have 
%%% `ordered_set' semantics. The key format of the `properties' table is
%%% `{Property, Pid}', while the key format of the `names' table is
%%% simply the name. Note that `names' and `properties' are not likely
%%% the physical names of the actual tables.</p>
%%% @end
previous(properties, {_Prop, Pid} = Next) when is_pid(Pid) ->
    ets:prev(?PROPS, Next);
previous(names, Next) ->
    ets:prev(?REG, Next).


%%% @spec last(Type:: names | properties) -> Key | '$end_of_table'
%%% @doc Analogous to {@link ets:last/1}.
%%% <p>The tables corresponding to `names' and `properties' both have 
%%% `ordered_set' semantics.</p>
%%% @end
last(properties) ->
    ets:last(?PROPS);
last(names) ->
    ets:last(?REG).
     
    
    


%%% --



%%% @spec properties_by_pid(pid()) -> [Property]
%%% @doc Lists all properties for a given process.
%%%
properties_by_pid(P) when is_pid(P) ->
    properties_by_pid(P, '_', []).

%%% @spec properties_by_pid(P::pid(), KeyPat) ->
%%%   [Property]
%%% @doc Lists all properties for a given process that match the pattern
%%%  KeyPat.
%%% <p>Equivalend to `properties_by_pid(P, KeyPat, [])'.</p>
%%% @end
%%%
properties_by_pid(P, KeyPat) ->
    properties_by_pid(P, KeyPat, []).

%%% @spec properties_by_pid(P::pid(), KeyPat, Guards) -> [Property]
%%% @doc Like {@link properties_by_pid/2}, but with an added list of guards.
%%% <p>The Guards list may be one returned from the function {@link guards/1}.
%%% </p>
%%% @end
%%%
properties_by_pid(P, KeyPat, Guards) ->
    ets:select(?PROPS_REV, [{{{P,KeyPat},'_'}, Guards,
			    [{element,2,{element,1,'$_'}}]}]).



%%% @spec info(Process) -> [{Tag, Info}] | undefined
%%% @doc Like `process_info(PidOfProcess)', but with added
%%%      proc-related attributes.
%%% <p>This function calls `process_info(PidOfProcess)', and adds to the 
%%% result the following info items:</p>
%%% <ul>
%%%   <li>`{pid, pid()}' -- the pid of the process</li>
%%%   <li>`{names, Names}' -- all unique proc names
%%%                            registered by the process</li>
%%%   <li>`{properties, Props}' -- all non-unique properties</li>
%%% </ul>
%%% @end
%%%
info(NameOrPid) ->
    info(NameOrPid, all).



%%% @spec info(Process, Attr) -> {Attr, Info}
%%% @doc Like `process_info(PidOfProcess, Attr)' but accepts some 
%%%      additional attributes.
%%% <p>All attributes supported by {@link erlang:process_info/2} are 
%%% supported by this function. In addition, the following attributes are 
%%% handled:</p>
%%% <ul>
%%%   <li>`pid' -- the pid of the process (mainly here for symmetry)</li>
%%%   <li>`names' -- all unique names registered in proc by `Process'</li>
%%%   <li>`properties' -- all non-unique properties of `Process'</li>
%%% </ul>
%%% @end
info(NameOrPid, Attr) ->
    case NameOrPid of
        P when is_pid(P) -> pid_info(P, Attr);
        N -> case where(N) of
                 undefined ->
                     undefined;
                 P ->
                     pid_info(P, Attr)
             end
    end.

pid_info(P, all) when is_pid(P) ->
    Names = case pid_info(P, names) of
		undefined -> [];
		{names, Ns} -> Ns
	    end,
    Keys = properties_by_pid(P),
    [{pid, P},
     {names, Names},
     {properties, Keys} | process_info(P)];
pid_info(P, pid) when is_pid(P) ->
    {pid, P};
pid_info(P, names) when is_pid(P) ->
    {names,
     ets:select(?REG_REV, [{{{P,'$1'},'_'}, [], ['$1']}])};
pid_info(P, properties) when is_pid(P) ->
    {properties, properties_by_pid(P)};
pid_info(P, Attr) when is_pid(P), is_atom(Attr) ->
    process_info(P, Attr).




%%% @spec i(Option) -> ok
%%%   Option = help | [{Op, Type, Patterns}]
%%%       Op = i | x
%%%     Type = names | properties
%%% @doc Like {@link c:i/0}, but allows for filtering output with 
%%% `proc' names and properties.
%%% <p>Usage: `i([{i | x, names | properties, Patterns}])'.<br/>
%%% calls `c:i(Processes)' for a subset of all running processes.
%%% `{i, Type, Pattern}' specifies by name or property which processes
%%% to include. `{x, Type, Pattern}' specifies which processes to
%%% exclude from the listing. If no `{i,T,P}' tuples are specified,
%%% all processes are included per default, then reduced by the
%%% `{x,T,P}' patterns. Multiple tuples are allowed.
%%% `Pattern' is a match specification where the result head is `true'.</p>
%%% <p>Note that currently, using {@link ets:fun2ms/1} may have severe impact
%%% on performance if there is a large number of registered names or
%%% keys.</p>
%%% @end
%%%
i(help) ->	    
    io:format(
      "~s~n",
      ["------------------------------------------------------------------\n"
       "Usage: i([{i | x, names | properties, Patterns}]).\n\n"
       "calls c:i(Processes) for a subset of all running processes.\n"
       "{i, Type, Pattern} specifies by name or property which processes\n"
       "to include. {x, Type, Pattern} specifies which processes to\n"
       "exclude from the listing. If no {i,T,P} tuples are specified,\n"
       "all processes are included per default, then reduced by the\n"
       "{x,T,P} patterns. Multiple tuples are allowed.\n\n"
       "Pattern is a match specification where the result head is true.\n"
       "Note that currently, using ets:fun2ms/1 may have severe impact\n"
       "on performance if there is a large number of registered names or\n"
       "keys.\n\n"
       "(Note: 'name' uniquely identifies one process; 'property' is non-unique.)\n"
       "-------------------------------------------------------------------\n"]);
i(Options) when is_list(Options) ->
    Temp = ets:new(temp, [ordered_set]),
    case lists:keymember(i,1,Options) of
	true ->
	    i_names(Options, Temp),
	    i_props(Options, Temp);
	false ->
	    lists:foreach(
	      fun(Pid) -> 
		      ets:insert(Temp, {Pid, 1})
	      end, processes())
    end,
    x_names(Options, Temp),
    x_props(Options, Temp),
    Pids = ets:select(Temp, ets:fun2ms(fun({P,_}) ->
					       P
				       end)),
    ets:delete(Temp),
    c:i(Pids).
    

i_names(Opts, Temp) ->
    Pats = 
	case lists:member({i,names,'_'}, Opts) of
	    true ->
		[{'_',[],[true]}];
	    false ->
		lists:concat([P || {i, names, P} <- Opts])
	end,
    case Pats of
	[] ->
	    true;
	_ ->
	    fold_names(
	      fun({_Name, Pid}, _) ->
		      ets:insert(Temp, {Pid,1})
	      end, [], Pats)
    end.


i_props(Opts, Temp) ->
    Pats = 
	case lists:member({i,properties,'_'}, Opts) of
	    true ->
		[{'_',[],[true]}];
	    false ->
		lists:concat([P || {i, properties, P} <- Opts])
	end,
    case Pats of
	[] ->
	    true;
	_ ->
	    select_fold_properties(
	      fun({_Prop, Pid}, _) ->
		      ets:insert(Temp, {Pid, 1})
	      end, [], Pats)
    end.
    
    
x_names(Opts, Temp) ->
    case lists:concat([P || {x, names, P} <- Opts]) of
	[] ->
	    true;
	Pats ->
	    fold_names(
	      fun({_Name, Pid}, _) ->
		      ets:delete(Temp, Pid)
	      end, [], Pats)
    end.

x_props(Opts, Temp) ->
    case lists:concat([P || {x, properties, P} <- Opts]) of
	[] ->
	    true;
	Pats ->
	    select_fold_properties(
	      fun({_Prop, Pid}, _) ->
		      ets:delete(Temp, Pid)
	      end, [], Pats)
    end.



%%% Gen_server callbacks

%%% @spec start_link() -> {ok, pid()}
%%% @doc Starts the proc server.
%%% <p>`proc' assumes that the `proc_tabs' module is available, and that
%%% the `proc_tabs' process is running.</p>
%%% @end
%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%% @hidden
init([]) ->
    ensure_tabs_created(),
    set_monitors(),
    {ok, []}.

%%% @hidden
handle_call(Req, From, S) ->
    try handle_call1(Req, From, S)
    catch
	throw:Return ->
	    {reply, Return, S}
    end.


%%% @hidden
handle_cast(Msg, S) ->
    {stop, {unknown_cast, Msg}, S}.

%%% @hidden
handle_info({'DOWN', _MRef, process, Pid, _}, S) ->
    process_is_down(Pid),
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.


process_is_down(Pid) ->
    Pattern = {{Pid, '_'}, '_'},
    Regs = ets:match_object(?REG_REV, Pattern),
    lists:foreach(
      fun({{_, I}, 1}) ->
	      ets:delete(?REG, I);
	 ({{_, I}, w}) ->
	      [{_Name, {awaiting_reg, Ps}}] = ets:lookup(?REG, I),
	      case lists:keydelete(Pid, 1, Ps) of
		  [] ->
		      ets:delete(?REG, I);
		  [_|_] = PsLeft ->
		      ets:insert(?REG, {I, {awaiting_reg, PsLeft}})
	      end
      end, Regs),
    ets:match_delete(?REG_REV, Pattern),
    PPattern = {{Pid,'_'},'_'},
    Properties = ets:match_object(?PROPS_REV, PPattern),
    lists:foreach(
      fun({{_, Prop}, _}) ->
	      ets:delete(?PROPS, {Prop,Pid})
      end, Properties),
    ets:match_delete(?PROPS_REV, PPattern),
    ets:match_delete(?PRIVS, {{Pid,'_','_'}}),
    ets:delete(?MONITORS, Pid).


%%% @hidden
terminate(_Reason, _S) ->
    ok.

%%% @hidden
code_change(_FromVsn, State, _Extra) ->
    {ok, State}.


%%% broken out from handle_call/3
handle_call1({reg, Name}, {Pid,_} = From, S) ->
    CompleteReg = fun() ->
			  set_monitor(Pid),
			  ets:insert(?REG_REV, {{Pid, Name}, 1})
		  end,
    case ets:insert_new(?REG, {Name, Pid}) of
	false ->
	    case ets:lookup(?REG, Name) of
		[{_, {awaiting_reg, Pids}}] ->
		    ets:insert(?REG, {Name, Pid}),
		    CompleteReg(),
		    gen:reply(From, true),
		    lists:foreach(
		      fun({WaitingPid, Ref}) ->
			      clear_monitor(WaitingPid),
			      ets:delete(?REG_REV, {WaitingPid,Name}),
			      WaitingPid ! {?MODULE, Ref, reg, Name, Pid}
		      end, Pids),
		    {noreply, S};
		[{_,OtherPid}] ->
		    case is_process_alive(OtherPid) of
			true ->
			    ?BADARG;
			false ->
			    process_is_down(OtherPid),
			    ets:insert(?REG, {Name, Pid}),
			    CompleteReg(),
			    {reply, true, S}
		    end;
		[] ->
		    io:format("what the f...?~n", []),
		    ?BADARG
	    end;
	true ->
	    CompleteReg(),
	    {reply, true, S}
    end;
handle_call1({unreg, Name}, {Pid,_}, S) ->
    case ets:lookup(?REG, Name) of
	[{_, Pid}] ->
	    clear_monitor(Pid),
	    ets:delete(?REG_REV, {Pid, Name}),
	    ets:delete(?REG, Name),
	    {reply, true, S};
	_ ->
	    ?BADARG
    end;
handle_call1({await_reg, Name}, {Pid,_}, S) ->
    case ets:lookup(?REG, Name) of
	[] ->
	    set_monitor(Pid),
	    Ref = make_ref(),
	    ets:insert(?REG, {Name, {awaiting_reg, [{Pid, Ref}]}}),
	    ets:insert(?REG_REV, {{Pid, Name}, w}),
	    {reply, {Ref,Name}, S};
	[{_, RegPid}] when is_pid(RegPid) ->
	    {reply, {already_registered, RegPid}, S};
	[{_, {awaiting_reg, OtherPids}}] ->
	    set_monitor(Pid),
	    Ref = make_ref(),
	    ets:insert(?REG, {Name, {awaiting_reg, [{Pid, Ref}|OtherPids]}}),
	    ets:insert(?REG_REV, {{Pid, Name}, w}),
	    {reply, {Ref,Name}, S}
    end;
handle_call1({clear_await_reg, {Ref,Name}}, _From, S) when is_reference(Ref) ->
    case ets:lookup(?REG, Name) of
	[{Name, Pid}] when is_pid(Pid) ->
	    ignore;
	[{Name, {awaiting_reg, Ps}}] ->
	    case lists:keysearch(Ref, 2, Ps) of
		{value, {WPid, _}} when is_pid(WPid) ->
		    clear_monitor(WPid),
		    ets:delete(?REG_REV, {WPid, Name}),
		    case lists:keydelete(Ref, 2, Ps) of
			[] ->
			    ets:delete(?REG, Name);
			[_|_] = PsLeft ->
			    ets:insert(?REG, {Name, {awaiting_reg, PsLeft}})
		    end;
		false ->
		    ignore
	    end
    end,
    %% due to possible race conditions, we reply true as long as it's 
    %% reasonable that the reference given was valid once. It would be 
    %% unreasonable to require the client to verify that Name is not 
    %% registered just before the registry server handles this request.
    %% Therefore, whether the request would exit or return true could be 
    %% timing dependent, which would suck greatly. Therefore, don't worry,
    %% be happy, and return 'true' each time. If the argument is not 
    %% on the form {reference(), Name}, there _will_ be a BADARG.
    {reply, true, S};
handle_call1({add_property, Pid, Property}, {Pid,_}, S) ->
    do_add_property(Pid, Property),
    {reply, true, S};
handle_call1({add_property, Process, Property}, {Pid, _}, S) ->
    OnPid = on_pid(Process),
    verify_access(OnPid, add_property, Pid),
    do_add_property(OnPid, Property),
    {reply, true, S};
handle_call1({replace_property, Pid, OldProp, NewProp}, {Pid,_}, S) ->
    do_replace_property(Pid, OldProp, NewProp),
    {reply, true, S};
handle_call1({replace_property, Process, OldProp, NewProp}, {Pid,_}, S) ->
    OnPid = on_pid(Process),
    verify_access(OnPid, replace_property, Pid),
    do_replace_property(OnPid, OldProp, NewProp),
    {reply, true, S};
handle_call1({del_property, Pid, Property}, {Pid,_}, S) ->
    do_del_property(Pid, Property),
    {reply, true, S};
handle_call1({del_property, Process, Property}, {Pid, _}, S) ->
    OnPid = on_pid(Process),
    verify_access(OnPid, del_property, Pid),
    do_del_property(OnPid, Property),
    {reply, true, S};
handle_call1({add_counter, Pid, Name, Initial}, {Pid,_}, S) ->
    do_add_counter(Pid, Name, Initial),
    {reply, true, S};
handle_call1({add_counter, Process, Name, Initial}, {Pid,_}, S) ->
    OnPid = on_pid(Process),
    verify_access(OnPid, add_counter, Pid),
    do_add_counter(OnPid, Name, Initial),
    {reply, true, S};
handle_call1({del_counter, Pid, Name}, {Pid,_}, S) ->
    do_del_counter(Pid, Name),
    {reply, true, S};
handle_call1({del_counter, Process, Name}, {Pid,_}, S) ->
    OnPid = on_pid(Process),
    verify_access(OnPid, del_counter, Pid),
    do_del_counter(Process, Name),
    {reply, true, S};
handle_call1({set_access, Access}, {Pid,_}, S) ->
    Objs = 
	lists:foldr(
	  fun({Action, Procs, Ops}, Acc) ->
		  Pids = [on_pid(P) || P <- Procs],
		  Ops1 = expand_ops(Ops),
		  [[{Action, {Pid, Op, P}} || P <- Pids,
					      Op <- Ops1] | Acc]
	  end, [], Access),
    lists:foreach(
      fun({grant, Key}) ->
	      ets:insert(?PRIVS, {Key});
	 ({revoke, Key}) ->
	      ets:delete(?PRIVS, Key)
      end, lists:concat(Objs)),
    {reply, true, S};
	      
handle_call1(_Req, _From, _S) ->
    ?BADARG.


expand_ops(Ops) ->
    lists:foldl(
      fun(properties, Acc) ->
	      [add_property, del_property, replace_property] ++ Acc;
	 (counters, Acc) ->
	      [add_counter, del_counter, update_counter] ++ Acc;
	 (Op, Acc) when Op == add_property;
			Op == replace_property;
			Op == del_property;
			Op == add_counter;
			Op == del_counter;
			Op == update_counter ->
	      [Op|Acc]
      end, [], Ops).



verify_access(OnPid, Op, Pid) ->
    case ets:member(?PRIVS, {OnPid, Op, Pid}) of
	true ->
	    true;
	false ->
	    ?EACCESS
    end.



do_add_property(Pid, Property) ->
    case ets:member(?PROPS_REV, {Pid,Property}) of
	true ->
	    ?BADARG;
	false ->
	    set_monitor(Pid),
	    insert_property(Property, Pid)
    end.

do_replace_property(Pid, OldProp, NewProp) ->
    case ets:lookup(?PROPS_REV, {Pid, OldProp}) of
	[{_, property}] ->
	    delete_property(OldProp, Pid),
	    insert_property(NewProp, Pid);
	_ ->
	    ?BADARG
    end.

do_del_property(Pid, Property) ->
    case ets:lookup(?PROPS_REV, {Pid,Property}) of
	[{_, property}] ->
	    clear_monitor(Pid),
	    delete_property(Property, Pid);
	_ ->
	    ?BADARG
    end.


do_add_counter(Pid, Name, Initial) ->
    case ets:member(?PROPS_REV, {Pid, Name}) of
	true ->
	    ?BADARG;
	false ->
	    set_monitor(Pid),
	    insert_counter(Name, Initial, Pid)
    end.


do_del_counter(Pid, Counter) ->
    case ets:lookup(?PROPS_REV, {Pid,Counter}) of
	[{_, counter}] ->
	    clear_monitor(Pid),
	    delete_counter(Counter, Pid);
	Other ->
	    io:format(user, "ets:lookup(~p, ~p) -> ~p~n",
		      [?PROPS_REV, {Pid,Counter}, Other]),
	    ?BADARG
    end.

    
on_pid(Process) ->
    if is_pid(Process) -> Process;
       true -> 
	    case ets:lookup(?REG, Process) of
		[{_, P}] ->
		    P;
		_ ->
		    ?BADARG
	    end
    end.



%%% Helper functions

call(Req) ->
    case gen_server:call(?MODULE, Req) of
	badarg ->
	    erlang:error(badarg);
	{badarg, Error} ->
	    erlang:error(Error);
	Reply ->
	    Reply
    end.

ensure_tabs_created() ->
    lists:foreach(
      fun({T, Opts}) ->
	      case ets:info(T, size) of
		  undefined ->
		      proc_tabs:ets_new(T, Opts);
		  N when is_integer(N) ->
		      ok
	      end
      end, tabs()).

tabs() ->
    T = [public, named_table],
    [{?REG, [ordered_set|T]},
     {?REG_REV, [ordered_set|T]},
     {?PROPS, [ordered_set|T]},
     {?PROPS_REV, [ordered_set|T]},
     {?MONITORS, [set|T]},
     {?PRIVS, [ordered_set|T]}].

set_monitors() -> 
    ets:delete_all_objects(?REG_REV),
    ets:delete_all_objects(?PROPS_REV),
    ets:delete_all_objects(?MONITORS),
    ets:foldl(fun({Id, Pid}, Acc) -> 
		      set_monitor(Pid),
		      ets:insert(?REG_REV, {{Pid, Id}}),
                      Acc
              end, ok, ?REG),
    ets:foldl(fun({{Property,Pid}, _}, Acc) ->
		      set_monitor(Pid),
		      ets:insert(?PROPS_REV, {{Pid,Property},1}),
		      Acc
	      end, ok, ?PROPS).


			      
			  
set_monitor(Pid) ->
    case ets:member(?MONITORS, Pid) of
	false ->
	    MRef = erlang:monitor(process, Pid),
	    ets:insert(?MONITORS, {Pid, MRef, 1});
	true ->
	    ets:update_counter(?MONITORS, Pid, {3,1})
    end.

clear_monitor(Pid) ->
    case ets:update_counter(?MONITORS, Pid, {3,-1}) of
	0 ->
	    MRef = ets:lookup_element(?MONITORS, Pid, 2),
	    erlang:demonitor(MRef),
	    ets:delete(?MONITORS, Pid);
	_ ->
	    true
    end.


insert_property(Property, Pid) ->
    ets:insert(?PROPS, {{Property,Pid},property}),
    ets:insert(?PROPS_REV, {{Pid,Property},property}).

delete_property(Property, Pid) ->
    ets:delete(?PROPS, {Property,Pid}),
    ets:delete(?PROPS_REV, {Pid,Property}).

insert_counter(Name, Initial, Pid) ->
    ets:insert(?PROPS, {{Name, Pid}, Initial}),
    ets:insert(?PROPS_REV, {{Pid, Name}, counter}).

delete_counter(Name, Pid) ->
    delete_property(Name, Pid).



%%% dirty hack to get proc:i/1 to work:
%%% While waiting for a patch that makes c:i/1 exported,
%%% we force the export and recompile on the fly.
%%% Most of this code was copied from cover.erl.

enable_i() ->
    case lists:member({i,1},c:module_info(exports)) of
	true ->
	    already_enabled;
	false ->
	    Beam = code:which(c),
	    ok = code:unstick_dir(filename:dirname(Beam)),
	    case get_abstract_code(c, Beam) of
		no_abstract_code=E ->
		    {error, E};
		encrypted_abstract_code=E ->
		    {error, E};
		{Vsn,Code} ->
		    Forms = transform_c(Vsn, Code),
		    {ok, Module, Binary} = compile:forms(Forms, []),
		    case code:load_binary(Module, Beam, Binary) of
			{module, Module} ->
			    {ok, Module};
			_Error ->
			    error
		    end
	    end
    end.

get_abstract_code(Module, Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
	{ok, {Module, [{abstract_code, AbstractCode}]}} ->
	    AbstractCode;
	{error,beam_lib,{key_missing_or_invalid,_,_}} ->
	    encrypted_abstract_code;
	Error -> Error
    end.

transform_c(Vsn, Code) 
  when Vsn==abstract_v1; Vsn==raw_abstract_v1; Vsn==abstract_v2 ->
    Code1 = lists:filter(
	      fun({function,_,module_info,_,_}) ->
		      false;
		 (_) -> true
	      end, Code),
    lists:map(
      fun({attribute,L,export,Es} = Form) ->
	      case lists:member({i,0},Es) of
		  true ->
		      {attribute,L,export,[{i,1}|Es]};
		  false ->
		      Form
	      end;
	 (Form) ->
	      Form
      end, Code1).
