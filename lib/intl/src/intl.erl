%%% File    : intl.erl
%%% Author  :  <tony@bit.hemma.se>
%%% Description : internationalization
%%% Created :  9 Sep 2003 by  <tony@bit.hemma.se>

-module(intl).

-export([
	 start/0,stop/0,
	 init/1,
	 gettext/1,
	 ngettext/3,
	 dgettext/2,
	 dngettext/4,
	 dcgettext/3,
	 dcngettext/5,
	 textdomain/1,
	 bindtextdomain/2,
	 bind_textdomain_codeset/2,
	 setlocale/2
	]).

%%
%% Message catalogs will be expected at the pathnames
%% <dirname>/<locale>/<category>/domainname.mo
%%

-define(INTL_GETTEXT,                 1).
-define(INTL_DGETTEXT,                2).
-define(INTL_DCGETTEXT,               3).
-define(INTL_NGETTEXT,                4).
-define(INTL_DNGETTEXT,               5).
-define(INTL_DCNGETTEXT,              6).
-define(INTL_TEXTDOMAIN,              7).
-define(INTL_BINDTEXTDOMAIN,          8).
-define(INTL_BIND_TEXTDOMAIN_CODESET, 9).
-define(INTL_SETLOCALE,               10).


-define(INTL_OK,       0).
-define(INTL_ERROR,    1).
-define(INTL_INTEGER,  2).
-define(INTL_UINTEGER, 3).
-define(INTL_STRING,   4).

-define(INTL_LC_CTYPE,           0).
-define(INTL_LC_NUMERIC,         1).
-define(INTL_LC_TIME,            2).
-define(INTL_LC_COLLATE,         3).
-define(INTL_LC_MONETARY,        4).
-define(INTL_LC_MESSAGES,        5).
-define(INTL_LC_ALL,             6).
-define(INTL_LC_PAPER,           7).
-define(INTL_LC_NAME,            8).
-define(INTL_LC_ADDRESS,         9).
-define(INTL_LC_TELEPHONE,	10).
-define(INTL_LC_MEASUREMENT,	11).
-define(INTL_LC_IDENTIFICATION,  12).


%% FIXME suervisor etc
start() ->
    Pid = spawn_link(?MODULE, init, [self()]),
    receive
	{Pid, Result} ->
	    Result
    end.

stop() ->
    intl_srv ! stop,
    ok.

    
init(Starter) ->
    case catch register(intl_srv, self()) of
	{'EXIT', _} ->
	    unlink(Starter),
	    Starter ! {self(),ok};
	true ->
	    Priv = code:priv_dir(intl),
	    ok = erl_ddll:load_driver(Priv, "intl_drv"),
	    Port = open_port({spawn, intl_drv}, []),
	    register(intl, Port),
	    unlink(Starter),
	    Starter ! {self(),ok},
	    receive
		stop -> ok
	    after infinity -> 
		    ok
	    end
    end.

gettext(MsgId) ->
    reply(port_control(intl, ?INTL_GETTEXT, 
		       [string_arg(MsgId)])).

ngettext(MsgId,MsgIdPlural,N) ->
    reply(port_control(intl, ?INTL_NGETTEXT, 
		       [string_arg(MsgId),
			string_arg(MsgIdPlural),
			uinteger_arg(N)])).

dgettext(DomainName, MsgId) ->
    reply(port_control(intl, ?INTL_DGETTEXT, 
		       [string_arg(DomainName),
			string_arg(MsgId)])).

dngettext(DomainName, MsgId, MsgIdPlural, N) ->
    reply(port_control(intl, ?INTL_DNGETTEXT, 
		       [string_arg(DomainName),
			string_arg(MsgId),
			string_arg(MsgIdPlural),
			uinteger_arg(N)])).

dcgettext(DomainName, MsgId, Category) ->
    reply(port_control(intl, ?INTL_DCGETTEXT,
		       [string_arg(DomainName),
			string_arg(MsgId),
			integer_arg(category(Category))])).

dcngettext(DomainName, MsgId, MsgIdPlural, N, Category) ->
    reply(port_control(intl, ?INTL_DCNGETTEXT,
		       [string_arg(DomainName),
			string_arg(MsgId),
			string_arg(MsgIdPlural),
			uinteger_arg(N),
			integer_arg(category(Category))])).

textdomain(DomainName) ->
    reply(port_control(intl, ?INTL_TEXTDOMAIN,
		       [string_arg(DomainName)])).

bindtextdomain(DomainName,DirName) ->
    reply(port_control(intl, ?INTL_BINDTEXTDOMAIN,
		       [string_arg(DomainName),
			string_arg(DirName)])).

bind_textdomain_codeset(DomainName,Codeset) ->
    reply(port_control(intl, ?INTL_BIND_TEXTDOMAIN_CODESET,
		       [string_arg(DomainName),
			string_arg(Codeset)])).

setlocale(Category, Locale) ->
    reply(port_control(intl, ?INTL_SETLOCALE,
		       [integer_arg(category(Category)),
			string_arg(Locale)])).


category(C) ->
    case C of
	ctype -> ?INTL_LC_CTYPE;
	numeric -> ?INTL_LC_NUMERIC;
	time -> ?INTL_LC_TIME;
	collate -> ?INTL_LC_COLLATE;
	monetary -> ?INTL_LC_MONETARY;
	messages -> ?INTL_LC_MESSAGES;
	all -> ?INTL_LC_ALL;
	paper -> ?INTL_LC_PAPER;
	name -> ?INTL_LC_NAME;
	address -> ?INTL_LC_ADDRESS;
	telephone -> ?INTL_LC_TELEPHONE;
	measurement -> ?INTL_LC_MEASUREMENT;
	identification -> ?INTL_LC_IDENTIFICATION
    end.


string_arg(String) ->
    [?INTL_STRING, String, 0].

integer_arg(Int) ->
    [?INTL_INTEGER, <<Int:32>>].

uinteger_arg(Int) ->
    [?INTL_UINTEGER, <<Int:32>>].

reply([?INTL_OK]) -> ok;
reply([?INTL_OK|Str]) -> {ok, list_to_atom(Str)};
reply([?INTL_ERROR|Str]) -> {error, list_to_atom(Str)};
reply([?INTL_INTEGER,A3,A2,A1,A0]) ->
    <<X:32/integer>> = <<A3,A2,A1,A0>>,
    X;
reply([?INTL_UINTEGER,A3,A2,A1,A0]) ->
    <<X:32/unsigned-integer>> = <<A3,A2,A1,A0>>,
    X;
reply([?INTL_STRING|Str]) -> 
    Str.



    

