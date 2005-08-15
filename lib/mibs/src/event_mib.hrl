-include("../include/DISMAN-EVENT-MIB.hrl").
-include_lib("snmp/include/SNMPv2-TC.hrl").

-define(bit_is_set(Fs, F), (Fs) band F =/= 0).
-define(bit_is_clr(Fs, F), (Fs) band F == 0).
-define(bit_clr(Fs,F), (Fs) band (bnot (F))).
-define(bit_set(Fs,F), (Fs) bor (F)).

-define(existence, 1).
-define(boolean,   2).
-define(threshold, 4).

-define(absoluteValue, 1).
-define(deltaValue, 2).

-record(mteTriggerTable, {
	  key, % {mteOwner, mteTriggerName}
	  comment = "",
	  test = ?boolean,
	  sampleType = ?absoluteValue,
	  valueID = [0,0],
	  valueIDWildcard = ?TruthValue_false,
	  targetTag = "",
	  contextName = "",
	  contextNameWildcard = ?TruthValue_false,
	  frequency = 600,
	  objectsOwner= "",
	  objects = "",
	  enabled = ?TruthValue_false,
	  entryStatus
	 }).  

-define(timeTicks, 1).
-define(timeStamp, 2).
-define(dateAndTime, 3).

-record(mteTriggerDeltaTable, {
	  key, % {mteOwner, mteTriggerName}
	  discontinuityID = ?sysUpTimeInstance,
	  discontinuityIDWildcard = ?TruthValue_false,
	  discontinuityIDType = ?timeTicks
	 }).

-define(present, 1).
-define(absent,  2).
-define(changed, 4).

-record(mteTriggerExistenceTable, {
	  key, % {mteOwner, mteTriggerName}
	  test = ?present bor ?absent,
	  startup = ?present bor ?absent,
	  objectsOwner = "",
	  objects = "",
	  eventOwner = "",
          event = ""
	 }).

-define(unequal, 1).
-define(equal, 2).
-define(less, 3).
-define(lessOrEqual, 4).
-define(greater, 5).
-define(greaterOrEqual, 6).

-record(mteTriggerBooleanTable, {
	  key, % {mteOwner, mteTriggerName}
	  comparison = ?unequal,
	  value = 0,
	  startup = ?TruthValue_true,
	  objectsOwner = "",
	  objects = "",
	  eventOwner = "",
	  event = ""
	 }).

-define(rising, 1).
-define(falling, 2).
-define(risingOrFalling, 3).

-record(mteTriggerThresholdTable, {
	  key, % {mteOwner, mteTriggerName}
	  startup = ?risingOrFalling,
	  rising = 0,
	  falling = 0,
	  deltaRising = 0,
	  deltaFalling = 0,
	  objectsOwner = "",
	  objects = "",
	  risingEventOwner = "",
	  risingEvent = "",
	  fallingEventOwner = "",
	  fallingEvent = "",
	  deltaRisingEventOwner = "",
	  deltaRisingEvent = "",
	  deltaFallingEventOwner = "",
	  deltaFallingEvent = ""
	 }).

-record(mteObjectsTable, {
	  key, % {mteOwner, mteObjectsName, mteObjectsIndex},
	  iD = [0,0],
	  iDWildcard = ?TruthValue_false,
	  entryStatus
	 }).

-define(notification, 1).
-define(set,          2).

-record(mteEventTable, {
	  key, % {mteOwner, mteEventName}
	  comment = "",
	  actions = 0,
	  enabled = ?TruthValue_false,
	  entryStatus
	 }).

-record(mteEventNotificationTable, {
	  key, % {mteOwner, mteEventName}
	  notification = [0,0],
	  objectsOwner = "",
	  objects = ""
	 }).

-record(mteEventSetTable, {
	  key, % {mteOwner, mteEventName}
	  object = [0,0],
	  objectWildcard = ?TruthValue_false,
	  value = 0,
	  targetTag = "",
	  contextName = "",
	  contextNameWildcard = ?TruthValue_false
	 }).

-record(mteVariables, {
	  name,
	  val
	 }).
