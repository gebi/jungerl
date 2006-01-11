%%----------------------------------------------------------------------
%% File    : logger.hrl
%% Author  : Serge Aleynikov
%% Purpose : Generic log message macros
%% Created : 6/1/2005 1:43PM
%%----------------------------------------------------------------------

%% Write the report at the local node only.
-define(ALERT(Str_, Args_),
    error_logger:error_report({lama,alert},
        {false,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(ERROR(Str_, Args_),
    error_logger:error_report({lama,error},
        {false,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(WARNING(Str_, Args_),
    error_logger:error_report({lama,warning},
        {false,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
%% NOTICE, INFO, DEBUG don't get sent to syslog, but get displayed on screen.
%% INFO, DEBUG - display plain formated text.
%% NOTICE - displays text with a standard SASL header
-define(NOTICE(Str_, Args_),
    error_logger:error_report({lama,notice},
        {false,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(INFO(Str_, Args_),
    error_logger:error_report({lama,info},
        {false,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(DEBUG(Str_, Args_),
    error_logger:error_report({lama,debug},
        {false,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).

%% Write the report at all known nodes.
-define(DIST_ALERT(Str_, Args_),
    error_logger:error_report({lama,alert},
        {true,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(DIST_ERROR(Str_, Args_),
    error_logger:error_report({lama,error},
        {true,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(DIST_WARNING(Str_, Args_),
    error_logger:error_report({lama,warning},
        {true,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(DIST_NOTICE(Str_, Args_),
    error_logger:error_report({lama,notice},
        {false,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(DIST_INFO(Str_, Args_),
    error_logger:error_report({lama,info},
        {true,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).
-define(DIST_DEBUG(Str_, Args_),
    error_logger:error_report({lama,debug},
        {true,{lama:get_app(),?MODULE,?LINE, Str_, Args_}})).

%% Format a string to be used by the macros above.
-define(FMT(Format_,Arguments_),
	lists:flatten(io_lib:format(Format_,Arguments_))).
-define(FLAT(String_), lists:flatten(String_)).
-define(IF(Condition_, Value_, True, False), case Condition_ of Value_ -> True; _ -> False end).
