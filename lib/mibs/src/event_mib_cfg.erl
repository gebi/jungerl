%%%-------------------------------------------------------------------
%%% File    : event_mib_cfg.erl
%%% Created : 16 May 2005 by Martin Bjorklund <mbj@bluetail.com>
%%%
%%% @author Martin Björklund <mbj@bluetail.com>
%%% @version 1.0
%%% @doc Utility functions for simplified configuration of DISMAN-EVENT-MIB.
%%%
%%% The parse_* functions are available to simplify creation of
%%% triggers/events/notification from a text string.  This can be
%%% useful for config files and/or a command-line interface to
%%% a system that supports this mib.  An implementation of these
%%% functions may of course document a subset of the (optional) options
%%% in order to limit the complexity.
%%%
%%% These functions return {ok, Records, Key} or {error, Reason}.
%%% It is up to the caller to write the Records within a transaction
%%% to the database.  In order to ensure that all old instances of
%%% a modified entry is deleted, the caller has to to delete all
%%% records with key = Key = {Owner, Name} (e.g. mteTriggerTable and
%%% mteTriggerExistenceTable) and also all records in mteObjectsTable
%%% with key = {Owner, Name, *}. The functions monitor_delete_t(Key)
%%% and event_delete_t(Key) can be called to perform this task.
%%%
%%% When defining Object Identifiers that, according to the event mib,
%%% may be wildcarded, the code tries to be smart.  If the OID refers
%%% to an object w/o any instance identifiers, then the OID is treated as
%%% a wildcard.  Otherwise (i.e. if it has one or more instance identifiers)
%%% it's not treated as a wildcard.  This does not handle the case where
%%% some instance identifiers are given in the OID, but the rest are
%%% wildcarded.  An OID can be forced to be treated as wildcarded by prefixing
%%% it with "w:", e.g. w:ifRcvAddressStatus.1 is treated as wildcarded,
%%% but ifRcvAddressStatus.1 is not.
%%%
%%% The syntax of these items has been inspired by net-snmp.
%%%
%%% Syntax: "monitor [Opts] -b Name Oid Op Value"
%%%         "monitor [Opts] -t Name Oid LowVal FallingEvent HighVal RisingEvent
%%%                             [DeltaLowVal DeltaFallingEvent
%%%                              DeltaHighVal DeltaRisingEvent]"
%%%         "monitor [Opts] -x Name Oid [present | absent | changed]*"
%%%     -b (boolean, default)
%%%     -t (threshold)
%%%     -x (existence) (default is present | absent)
%%%     -f Frequency (in seconds, default is 600 (10 minutes))
%%%     -o OID (additional objects to send in the event)
%%%     -e EventName (the name of a notificationEvent) (only for bool/existence)
%%%     -d OID (delta discontinuity oid)
%%%     -D timeTicks | timeStamp | dateAndTime (delta discontinuity type,
%%%             default is timeticks)
%%%     -O OwnerString (default "sys")
%%%     -c CommentStr
%%%   Name is an arbitrary unique name for the monitor.
%%%   Oid is an object identifier to monitor. The following are equivalent
%%%             ifOperStatus.1
%%%             1.3.6.1.2.1.2.2.1.8.1
%%%             ifEntry.ifOperStatus.1
%%%             ifTable.1.ifOperStatus.1
%%%   Op is !=, ==, <=, >=, <, <, /=, >=, =<
%%%   Value = LowVal = HighVal = integer()
%%%
%%% NOTE: this syntax does not support multiple test types in the same
%%%       trigger.  (i.e. mteTriggerTest will always have one bit only set)
%%%
%%% Examples:
%%%   monitor -b link ifOperStatus.1 == 1
%%%   monitor -x newlink -e newlinkevent ifOperStatus present
%%%   monitor -x linkStatus ifOperStatus.1 changed
%%%
%%%
%%% Syntax: "event [Opts] Name Notification [Oid]*"
%%%     -O OwnerString (default "sys")
%%%     -c CommentStr
%%%   Name is an arbitrary unique name for the event.
%%%   Notification is the OID (or symbolic name) of a notification.
%%%
%%% Examples:
%%%   event newlinkevent mteTriggerFired ifDescr
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(event_mib_cfg).

-export([parse_monitor/1, delete_monitor_t/1, delete_monitor/1]).
-export([parse_event/1, delete_event_t/1, delete_event/1]).
-export([list_monitors/0, list_monitors/1,
	 list_monitors_t/0, list_monitors_t/1]).
-export([list_events/0, list_events/1,
	 list_events_t/0, list_events_t/1]).

-include("event_mib.hrl").
-incluce_lib("snmp/include/snmp_types.hrl").

%% TEST
%% {ok, R0, _} = event_mib_cfg:parse_monitor("-f 10 -e tstev -b tst sysServices == 2").
%% {ok, R1, _} = event_mib_cfg:parse_event("tstev mteTriggerFired sysName").    
%% mnesia:transaction(fun() -> lists:foreach(fun(X) -> mnesia:write(X) end, R0++R1) end). 
%%
%% {ok, R0, _} = event_mib_cfg:parse_monitor("-f 10 -t tst sysServices.0 30 evlo 100 evhi").
%% {ok, R1, _} = event_mib_cfg:parse_event("evlo mteTriggerFalling").
%% {ok, R2, _} = event_mib_cfg:parse_event("evhi mteTriggerRising"). 
%% mnesia:transaction(fun() -> lists:foreach(fun(X) -> mnesia:write(X) end, R0++R1++R2) end). 

-define(DEFAULT_OWNER, "sys").
-define(DEFAULT_FREQ, 600).

parse_monitor(Str) ->
    try 
        parse_monitor_str(Str)
    catch
	throw:Term -> {error, Term};
	exit:_ -> {error, syntax_error}
    end.

delete_monitor(Key) ->
    mnesia:transaction(fun() -> delete_monitor_t(Key) end).

delete_monitor_t(Key) ->
    {Owner, Name} = Key,
    mnesia:delete({mteTriggerTable, Key}),
    mnesia:delete({mteTriggerBooleanTable, Key}),
    mnesia:delete({mteTriggerExistenceTable, Key}),
    mnesia:delete({mteTriggerThresholdTable, Key}),
    mnesia:delete({mteTriggerDeltaTable, Key}),
    OName = monitor_objects_name(Name),
    delete_objects_t(Owner, OName, 1).

delete_objects_t(Owner, Name, Idx) ->
    TK = {mteObjectsTable, {Owner, Name, Idx}},
    case mnesia:read(TK) of
	[_] ->
	    mnesia:delete(TK),
	    delete_objects_t(Owner, Name, Idx+1);
	_ ->
	    ok
    end.

parse_event(Str) ->
    try 
        parse_event_str(Str)
    catch
	throw:Term -> {error, Term};
	exit:_ -> {error, syntax_error}
    end.

delete_event(Key) ->
    mnesia:transaction(fun() -> delete_event_t(Key) end).

delete_event_t(Key) ->
    {Owner, Name} = Key,
    mnesia:delete({mteEventTable, Key}),
    mnesia:delete({mteEventNotificationTable, Key}),
    OName = event_objects_name(Name),
    delete_objects_t(Owner, OName, 1).


parse_monitor_str(Str) ->
    {Opts, T0} = parse_opts(strip(Str), "btxf:o:e:d:D:O:c:"),
    {Name, T1} = scan_tok(T0, bad_name),
    {ValueIDStr, T2} = scan_tok(T1, bad_value_id),
    {ValueID, ValueIDWildP} = str2oid(ValueIDStr),
    Type = case lists:keymember($x, 1, Opts) of
	       true -> ?existence;
	       false ->
		   case lists:keymember($t, 1, Opts) of
		       true -> ?threshold;
		       false -> ?boolean
		   end
	   end,
    Owner = optval($O, Opts, ?DEFAULT_OWNER),
    Key = {Owner, Name},
    Freq = optval($f, Opts, ?DEFAULT_FREQ, fun(S) -> str2int(S) end),
    Comment = optval($c, Opts, ""),
    Event = optval($e, Opts, ""),
    {DeltaTabL, SampleType} =
	case lists:keysearch($d, 1, Opts) of
	    {value, {_, DiscOidStr}} ->
		DiscType =
		    case lists:keysearch($D, 1, Opts) of
			false -> ?timeTicks;
			{value, {_, "timeTicks"}} -> ?timeTicks;
			{value, {_, "timeStamp"}} -> ?timeStamp;
			{value, {_, "dateAndTime"}} -> ?dateAndTime;
			{value, {_, DT}} -> throw({bad_discontinuity_type, DT})
		    end,
		{DiscOid, DiscOidWildP} = str2oid(DiscOidStr),
		{[#mteTriggerDeltaTable{key = Key,
					discontinuityID = DiscOid,
					discontinuityIDWildcard = DiscOidWildP,
					discontinuityIDType = DiscType}],
		 ?deltaValue};
	    false ->
		{[], ?absoluteValue}
	end,
    OName = monitor_objects_name(Name),
    ObjectTabL = objects_tab(Owner, OName, [str2oid(OID) || {$o, OID} <- Opts]),
    TriggerTable =
	#mteTriggerTable{key = Key,
			 comment = Comment,
			 test = Type,
			 sampleType = SampleType,
			 valueID = ValueID,
			 valueIDWildcard = ValueIDWildP,
			 frequency = Freq,
			 objects = if ObjectTabL /= [] -> OName;
				      true -> ""
				   end,
			 objectsOwner = if ObjectTabL /= [] -> Owner;
					   true -> ""
					end,
			 enabled = ?TruthValue_true,
			 entryStatus = ?RowStatus_active},
    case Type of
	?boolean ->
	    {Op, Value} = parse_boolean_expr(T2),
	    {ok, [TriggerTable,
		  #mteTriggerBooleanTable{key = Key,
					  comparison = Op,
					  value = Value,
					  eventOwner = Owner,
					  event = Event} |
		  DeltaTabL ++ ObjectTabL], Key};
	?existence ->
	    TestBits = case parse_existence_expr(T2) of
			   0 -> ?absent bor ?present;
			   TB -> TB
		       end,
	    {ok, [TriggerTable,
		  #mteTriggerExistenceTable{key = Key,
					    test = TestBits,
					    eventOwner = Owner,
					    event = Event} |
		  DeltaTabL ++ ObjectTabL], Key};
	?threshold ->
	    {FallingV, FallingE, RisingV, RisingE,
	     DeltaFallingV, DeltaFallingE, DeltaRisingV, DeltaRisingE} =
		parse_threshold_expr(T2),
	    {ok, [TriggerTable,
		  #mteTriggerThresholdTable{key = Key,
					    rising = RisingV,
					    risingEvent = RisingE,
					    risingEventOwner = Owner,
					    falling = FallingV,
					    fallingEvent = FallingE,
					    fallingEventOwner = Owner,
					    deltaRising = DeltaRisingV,
					    deltaRisingEvent = DeltaRisingE,
					    deltaRisingEventOwner = Owner,
					    deltaFalling = DeltaFallingV,
					    deltaFallingEvent = DeltaFallingE,
					    deltaFallingEventOwner = Owner} |
		  DeltaTabL ++ ObjectTabL], Key}
    end.


parse_boolean_expr(Str) ->
    {Op, T1} = scan_tok(Str, bad_operator),
    {Val, _} = scan_tok(T1, bad_value),
    {str2op(Op), str2int(Val)}.

parse_threshold_expr(Str) ->
    {FallingVal, T0} = scan_tok(Str, bad_value),
    {FallingEvent, T1} = scan_tok(T0, bad_eventname),
    {RisingVal, T2} = scan_tok(T1, bad_value),
    {RisingEvent, T3} = scan_tok(T2, bad_eventname),
    case scan_tok_allow_empty(T3) of
	{[], []} ->
	    {str2int(FallingVal), FallingEvent,
	     str2int(RisingVal), RisingEvent,
	     0, "", 0, ""};
	{DeltaFallingVal, T4} ->
	    {DeltaFallingEvent, T5} = scan_tok(T4, bad_eventname),
	    {DeltaRisingVal, T6} = scan_tok(T5, bad_value),
	    {DeltaRisingEvent, _} = scan_tok(T6, bad_eventname),
	    {str2int(FallingVal), FallingEvent,
	     str2int(RisingVal), RisingEvent,
	     str2int(DeltaFallingVal), DeltaFallingEvent,
	     str2int(DeltaRisingVal), DeltaRisingEvent}
    end.

parse_existence_expr(Str) ->
    parse_existence_expr(Str, []).

parse_existence_expr(Str, Acc) ->
    case scan_tok_allow_empty(Str) of
	{[], []} ->
	    parse_existence_expr_test(Acc, 0);
	{TestStr, T} ->
	    parse_existence_expr(T, [TestStr | Acc])
    end.

parse_existence_expr_test(["present" | T], Bits) ->
    parse_existence_expr_test(strip(T), ?bit_set(?present, Bits));
parse_existence_expr_test(["absent" | T], Bits) ->
    parse_existence_expr_test(strip(T), ?bit_set(?absent, Bits));
parse_existence_expr_test(["changed" | T], Bits) ->
    parse_existence_expr_test(strip(T), ?bit_set(?changed, Bits));
parse_existence_expr_test([Unknown | _], _Bits) ->
    throw({unknown_existence_test, Unknown});
parse_existence_expr_test([], Bits) ->
    Bits.


parse_event_str(Str) ->
    {Opts, T0} = parse_opts(strip(Str), "O:c:"),
    {Name, T1} = scan_tok(T0, bad_name),
    {Notification, T2} = scan_tok(T1, bad_notification),
    Owner = optval($O, Opts, ?DEFAULT_OWNER),
    Comment = optval($c, Opts, ""),
    OName = event_objects_name(Name),
    ObjectTabL = objects_tab(Owner, OName,
			     [str2oid(OID) || OID <- string:tokens(T2, " \t")]),
    Key = {Owner, Name},
    {ok, [#mteEventTable{key = Key,
			 comment = Comment,
			 actions = ?notification,
			 enabled = ?TruthValue_true,
			 entryStatus = ?RowStatus_active},
	  #mteEventNotificationTable{
			  key = Key,
			  notification = str2oid2(Notification),
			  objects = if ObjectTabL /= [] -> OName;
				       true -> ""
				    end,
			  objectsOwner = if ObjectTabL /= [] -> Owner;
					    true -> ""
					 end} |
	  ObjectTabL], Key}.

monitor_objects_name(Name) -> "mo_"++Name.
event_objects_name(Name) -> "eo_"++Name.

objects_tab(Owner, Name, OIDs) ->
    objects_tab(Owner, Name, OIDs, 1).
 
objects_tab(Owner, Name, [{OID, WildP} | OIDs], Idx) ->
    [#mteObjectsTable{key = {Owner, Name, Idx},
		      iD = OID,
		      iDWildcard = WildP,
		      entryStatus = ?RowStatus_active} |
     objects_tab(Owner, Name, OIDs, Idx+1)];
objects_tab(_Owner, _Name, [], _Idx) ->
    [].


str2oid("w:" ++ Str) ->
    {str2oid2(Str), ?TruthValue_true};
str2oid(Str) ->
    OID = str2oid2(Str),
    {OID, is_oid_wild(OID)}.

str2oid2(Str) ->
    L = string:tokens(Str, "."),
    str2oid2(L, []).

str2oid2([H | T], RevOid) ->
    case catch list_to_integer(H) of
	Int when integer(Int) ->
	    str2oid2(T, [Int | RevOid]);
	_ ->
	    Alias = list_to_atom(H),
	    case snmpa_symbolic_store:aliasname_to_oid(Alias) of
		{value, Oid} when RevOid == [] ->
		    str2oid2(T, lists:reverse(Oid));
		{value, Oid} ->
		    case is_oid_prefix(lists:reverse(RevOid), Oid) of
			true ->
			    str2oid2(T, lists:reverse(Oid));
			false ->
			    throw({bad_oid, H})
		    end;
		_ ->
		    throw({bad_oid, H})
	    end
    end;
str2oid2([], RevOid) ->
    lists:reverse(RevOid).

is_oid_prefix([], [_]) -> true;
is_oid_prefix([H | T0], [H | T1]) -> is_oid_prefix(T0, T1);
is_oid_prefix(_, _) -> false.
    
oid2str(Oid, WildP) ->
    if WildP == ?TruthValue_true ->
	    oid2str2(Oid, [], "w:");
       WildP == ?TruthValue_false ->
	    oid2str2(Oid, [], "")
    end.

oid2str2([], PostOid, WildPrefix) ->
    WildPrefix ++ PostOid;
oid2str2(Oid, PostOid, WildPrefix) ->
    case snmpa_symbolic_store:oid_to_aliasname(Oid) of
	{value, Alias} when PostOid == [] ->
	    atom_to_list(Alias);
	{value, Alias} ->
	    WildPrefix ++ atom_to_list(Alias) ++ add_dot(PostOid);
	false ->
	    [Last | T] = lists:reverse(Oid),
	    oid2str2(lists:reverse(T),
		     integer_to_list(Last) ++ add_dot(PostOid),
		     WildPrefix)
    end.

add_dot("") -> "";
add_dot(Str) -> [$. | Str].


%% An OID is treated as a wildcarded OID if it does not have an
%% instance part as a suffix.  E.g.
%%   sysUpTime.0 is NOT wildcarded
%%   ifDescr.1 is NOT wildcarded
%%   ifDescr IS wildcarded
%% The way to check this is to see if the OID has an alias - this
%% only works if the MIB is loaded.
is_oid_wild(OID) ->
    case snmpa_symbolic_store:oid_to_aliasname(OID) of
	{value, _} -> ?TruthValue_true;
	_ -> ?TruthValue_false
    end.

str2int(Str) ->
    case catch list_to_integer(Str) of
	Int when integer(Int) ->
	    Int;
	_ ->
	    throw({bad_integer, Str})
    end.

parse_opts(Str, Spec) ->
    parse_opts(Str, Spec, []).
parse_opts(Str, Spec, Acc) ->
    case scan_tok_allow_empty(Str) of
	{[$-, OptCh] = Opt, T0} ->
	    case has_arg(OptCh, Spec) of
		true ->
		    {Val, T1} = scan_tok(T0, {bad_option, OptCh}),
		    parse_opts(T1, Spec, [{OptCh, Val} | Acc]);
		false ->
		    parse_opts(T0, Spec, [{OptCh, []} | Acc]);
		unknown ->
		    throw({unknown_option, Opt})
	    end;
	_ ->
	    {lists:reverse(Acc), Str}
    end.

optval(OptCh, Opts, DefVal) ->
    optval(OptCh, Opts, DefVal, fun(Str) -> Str end).
optval(OptCh, Opts, DefVal, TypeFun) ->
    case lists:keysearch(OptCh, 1, Opts) of
	{value, {_, Str}} -> TypeFun(Str);
	false -> DefVal
    end.

has_arg(Ch, [Ch, $: | _]) -> true;
has_arg(Ch, [Ch | _]) -> false;
has_arg(Ch, [_ | T]) -> has_arg(Ch, T);
has_arg(_, []) -> unknown.

scan_tok(Str, EmptyError) ->
    case scan_tok2(Str, []) of
	{[], _} ->
	    throw(EmptyError);
	Res ->
	    Res
    end.

scan_tok_allow_empty(Str) ->
    scan_tok2(Str, []).

scan_tok2([$\s | T], Acc) ->
    {lists:reverse(Acc), strip(T)};
scan_tok2([$\t | T], Acc) ->
    {lists:reverse(Acc), strip(T)};
scan_tok2([$" | T], []) ->
    scan_quoted_tok($", T, []);
scan_tok2([$' | T], []) ->
    scan_quoted_tok($', T, []);
scan_tok2([H | T], Acc) ->
    scan_tok2(T, [H | Acc]);
scan_tok2([], Acc) ->
    {lists:reverse(Acc), []}.

scan_quoted_tok(Q, [Q | T], Acc) ->
    {lists:reverse(Acc), strip(T)};
scan_quoted_tok(Q, [H | T], Acc) ->
    scan_quoted_tok(Q, T, [H | Acc]);
scan_quoted_tok(_Q, [], _Acc) ->
    throw(runaway_quotation).

strip([$\s | T]) ->
    strip(T);
strip([$\t | T]) ->
    strip(T);
strip(Str) ->
    Str.


%% These are "reverse" functions c/w the parse functions - they generate
%% a string from the db contents which can be passed to the parse functions.

list_monitors() ->
    list_monitors(?DEFAULT_OWNER).

list_monitors(Owner) ->
    mnesia:transaction(fun() -> list_monitors_t(Owner) end).

list_monitors_t() ->
    list_monitors_t(?DEFAULT_OWNER).

list_monitors_t(Owner) ->
    Keys = mnesia:all_keys(mteTriggerTable),
    lists:zf(fun({Owner0, _Name} = Key) when Owner0 == Owner ->
		     case catch mk_monitor_str_t(Key) of
			 {'EXIT', Error} ->
			     error_logger:format(
			       "event_mib_cfg: bad trigger ~p ~p\n",
			       [Key, Error]),
			     false;
			 Res ->
			     Res
		     end;
		 (_) ->
			false
		end, Keys).

mk_monitor_str_t({Owner, Name} = Key) ->
    [#mteTriggerTable{comment = Comment,
		      test = Type,
		      sampleType = SampleType,
		      valueID = ValueID,
		      valueIDWildcard = ValueIDWildP,
		      frequency = Freq,
		      objects = Objects}] =
	mnesia:read({mteTriggerTable, Key}),
    OidStr = oid2str(ValueID, ValueIDWildP),
    CStr = if Comment /= "" -> [" -c ", quote(Comment)];
	      true -> ""
	   end,
    FStr = if Freq /= ?DEFAULT_FREQ -> [" -f ", integer_to_list(Freq)];
	      true -> ""
	   end,
    DStr = if SampleType == ?absoluteValue -> "";
	      SampleType == ?deltaValue ->
		   [#mteTriggerDeltaTable{discontinuityID = DiscOid,
					  discontinuityIDWildcard =DiscOidWildP,
					  discontinuityIDType = DiscType}] =
		       mnesia:read({mteTriggerDeltaTable, Key}),
		   DDStr = if DiscType == ?timeTicks -> "";
			      DiscType == ?timeStamp -> " -D timeStamp";
			      DiscType == ?dateAndTime -> " -D dateAndTime"
			   end,
		   DiscOidStr = oid2str(DiscOid, DiscOidWildP),
		   [" -d ", DiscOidStr, DDStr]
	   end,
    OName = monitor_objects_name(Name),
    OStr = if Objects == "" -> "";
	      Objects == OName -> mk_object_str_t(Owner, OName, " -o ")
	   end,
    WStr = if Owner /= ?DEFAULT_OWNER -> [" -O ", quote(Owner)];
	      true -> ""
	   end,
    TStr = if Type == ?boolean ->
		   [#mteTriggerBooleanTable{comparison = Op,
					    value = Value,
					    eventOwner = Owner, % must match
					    event = Event}] =
		       mnesia:read({mteTriggerBooleanTable, Key}),
		   EStr = if Event /= "" -> [" -e ", quote(Event)];
			     true -> ""
			  end,
		   [EStr, " -b ", quote(Name), " ", OidStr, " ",
		    event_mib:op2str(Op), " ", integer_to_list(Value)];
	      Type == ?existence ->
		   [#mteTriggerExistenceTable{test = TestBits,
					      eventOwner = Owner, % must match
					      event = Event}] =
		       mnesia:read({mteTriggerExistenceTable, Key}),
		   BitsStr =
		       if ?bit_is_set(?present, TestBits) -> " present";
			  true -> ""
		       end ++
		       if ?bit_is_set(?absent, TestBits) -> " absent";
			  true -> ""
		       end ++
		       if ?bit_is_set(?changed, TestBits) -> " changed";
			  true -> ""
		       end,
		   EStr = if Event /= "" -> [" -e ", quote(Event)];
			     true -> ""
			  end,
		   [EStr, " -x ", quote(Name), " ", OidStr ++ BitsStr];
	      Type == ?threshold ->
		   [#mteTriggerThresholdTable{rising = RisingV,
					      risingEvent = RisingE,
					      risingEventOwner = Owner,
					      falling = FallingV,
					      fallingEvent = FallingE,
					      fallingEventOwner = Owner,
					      deltaRising = DeltaRisingV,
					      deltaRisingEvent = DeltaRisingE,
					      deltaRisingEventOwner = Owner,
					      deltaFalling = DeltaFallingV,
					      deltaFallingEvent = DeltaFallingE,
					      deltaFallingEventOwner = Owner}] =
		       mnesia:read({mteTriggerThresholdTable, Key}),
		   [" -t ", quote(Name), " ", OidStr,
		    threshold_str(FallingV, FallingE),
		    threshold_str(RisingV, RisingE),
		    threshold_str(DeltaFallingV, DeltaFallingE),
		    threshold_str(DeltaRisingV, DeltaRisingE)]
	   end,
    {true, {Key, lists:flatten([WStr, CStr, FStr, OStr, DStr, TStr])}}.

threshold_str(_, "") -> "";
threshold_str(Value, Event) -> [" ", integer_to_list(Value), " ", quote(Event)].

mk_object_str_t(Owner, Name, Opt) ->
    mk_object_str_t(Owner, Name, Opt, 1).

mk_object_str_t(Owner, Name, Opt, Idx) ->
    case mnesia:read({mteObjectsTable, {Owner, Name, Idx}}) of
	[#mteObjectsTable{iD = Oid, iDWildcard = WildP}] ->
	    [Opt, oid2str(Oid, WildP) |
	     mk_object_str_t(Owner, Name, Opt, Idx+1)];
	[] ->
	    []
    end.


list_events() ->
    list_events(?DEFAULT_OWNER).

list_events(Owner) ->
    mnesia:transaction(fun() -> list_events_t(Owner) end).

list_events_t() ->
    list_events_t(?DEFAULT_OWNER).

list_events_t(Owner) ->
    Keys = mnesia:all_keys(mteEventTable),
    lists:zf(fun({Owner0, _Name} = Key) when Owner0 == Owner ->
		     case catch mk_event_str_t(Key) of
			 {'EXIT', Error} ->
			     error_logger:format(
			       "event_mib_cfg: bad event ~p ~p\n",
			       [Key, Error]),
			     false;
			 Res ->
			     Res
		     end;
		 (_) ->
			false
		end, Keys).

mk_event_str_t({Owner, Name} = Key) ->
    [#mteEventTable{comment = Comment,
		    actions = ?notification}] = % must match
	mnesia:read({mteEventTable, Key}),
    [#mteEventNotificationTable{notification = Notification,
				objects = Objects}] =
	mnesia:read({mteEventNotificationTable, Key}),
    CStr = if Comment /= "" -> [" -c ", quote(Comment)];
	      true -> ""
	   end,
    WStr = if Owner /= ?DEFAULT_OWNER -> [" -O ", quote(Owner)];
	      true -> ""
	   end,
    OName = event_objects_name(Name),
    OStr = if Objects == "" -> "";
	      Objects == OName -> mk_object_str_t(Owner, OName, " ")
	   end,
    NStr = oid2str(Notification, ?TruthValue_false),
    {true,
     {Key,
      lists:flatten([WStr, CStr, " ", quote(Name), " ", NStr, OStr])}}.

quote(Str) ->
    HasSpace = lists:member($\s, Str) orelse
	       lists:member($\t, Str) orelse
	       lists:member($\n, Str),
    if HasSpace -> [$", Str, $"];
       true -> Str
    end.

str2op("==") -> ?equal;
str2op("/=") -> ?unequal;
str2op("!=") -> ?unequal;
str2op("<") -> ?less;
str2op("=<") -> ?lessOrEqual;
str2op("<=") -> ?lessOrEqual;
str2op(">") -> ?greater;
str2op("=.") -> ?greaterOrEqual;
str2op(">=") -> ?greaterOrEqual;
str2op(Op) -> throw({unknown_operator, Op}).
