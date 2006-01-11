%%----------------------------------------------------------------------
%% File    : log_debug.hrl
%% Author  : Serge Aleynikov
%% Purpose : Generic log message macros
%% Created : 6/1/2005 1:43PM
%%----------------------------------------------------------------------

%% Write the report at the local node only.
-define(ALERT(Str_, Args_),   io:format(Str_, Args_)).
-define(ERROR(Str_, Args_),   io:format(Str_, Args_)).
-define(WARNING(Str_, Args_), io:format(Str_, Args_)).
-define(NOTICE(Str_, Args_),  io:format(Str_, Args_)).
-define(INFO(Str_, Args_),    io:format(Str_, Args_)).
-define(DEBUG(Str_, Args_),   io:format(Str_, Args_)).
