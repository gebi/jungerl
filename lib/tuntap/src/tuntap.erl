%%%-------------------------------------------------------------------
%%% File    : tuntap.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Linux "Universal TUN/TAP device" driver
%%%           See /usr/src/linux/Documentation/networking/tuntap.txt
%%%
%%% Created : 10 Nov 2001 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(tuntap).

-author('luke@bluetail.com').

-export([init/0,
	 open_tun/0, open_tap/0, open_tuntap/2,
	 device_name/1, write/2]).

-define(REPLY_ERROR, 0).
-define(REPLY_OK, 1).

-define(REQUEST_GET_DEVICE, 0).
-define(REQUEST_WRITE, 1).

%% Returns: ok
init() ->
    erl_ddll:start(),
    ok = erl_ddll:load_driver(code:priv_dir(tuntap), "tun_drv").

%% Returns port()
open_tun() -> open_tuntap(tun, undefined).
open_tap() -> open_tuntap(tap, undefined).

%% open_tuntap(tun|tap, undefined|string()) -> port()
open_tuntap(Type, Dev) ->
    TypeName = case Type of
		   tun -> "tun";
		   tap -> "tap"
	       end,
    DevArg = if Dev == undefined -> "";
		list(Dev)        -> " " ++ Dev
	     end,
    open_port({spawn, "tun_drv "++TypeName++DevArg}, [binary]).

%% Returns: {ok, Device}
device_name(Port) ->
    [?REPLY_OK|Name] = erlang:port_control(Port, ?REQUEST_GET_DEVICE, []),
    Name.

%% Returns: ok
write(Port, Packet) ->
    [?REPLY_OK] = erlang:port_control(Port, ?REQUEST_WRITE, Packet),
    ok.

