%%%-------------------------------------------------------------------
%%% File    : tuntap.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Linux "Universal TUN/TAP device" driver
%%%           See /usr/src/linux/Documentation/networking/tuntap.txt
%%%
%%% Created : 10 Nov 2001 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------

%% Note: if you want to be able to create tunnels as some user other
%% than root, you should chmod "/dev/net/tun" to be read/write by the
%% appropriate user.

%% To actually communicate between your host machine and Erlang via a
%% tun/tap interface, you will probably want to assign it an IP
%% address and setup routing. For example (on Linux):
%%
%%   ifconfig tun0 200.0.0.1 pointopoint 200.0.0.2
%%
%% Will configure the tun0 interface so that the host machine (Linux
%% itself) has the address 200.0.0.1 and that it believes Erlang has
%% the address 200.0.0.2 (i.e. it sets up routing of that address into
%% the tunnel).
%%
%% The exact setup varies from unix to unix. The point is that you
%% configure it exactly like a normal network interface: assign an
%% address for the local machine to use and a route for any addresses
%% you want to talk to "out there".

-module(tuntap).

-author('luke@bluetail.com').

-export([init/0,
	 open_tun/0, open_tap/0, open_tuntap/2,
	 device_name/1, write/2, set_active/2]).

-define(REPLY_ERROR, 0).
-define(REPLY_OK, 1).

-define(REQUEST_GET_DEVICE, 0).
-define(REQUEST_WRITE, 1).
-define(REQUEST_ACTIVE, 2).

-define(ACTIVE_FALSE, 0).
-define(ACTIVE_TRUE,  1).
-define(ACTIVE_ONCE,  2).

%% Returns: ok
init() ->
    erl_ddll:start(),
    ok = erl_ddll:load_driver(code:priv_dir(tuntap), "tun_drv").

%% Returns port()
open_tun() -> open_tuntap(tun, undefined).
open_tap() -> open_tuntap(tap, undefined).

%% open_tuntap(tun|tap, undefined|string()) -> port()
open_tuntap(Type, Dev) ->
    %% It seems to be okay to init() multiple times..
    ok = init(),
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
    erlang:port_command(Port, Packet),
    ok.

set_active(Port, false) -> set_active1(Port, ?ACTIVE_FALSE);
set_active(Port, true)  -> set_active1(Port, ?ACTIVE_TRUE);
set_active(Port, once)  -> set_active1(Port, ?ACTIVE_ONCE).

set_active1(Port, Arg) ->
    [?REPLY_OK] = erlang:port_control(Port, ?REQUEST_ACTIVE, [Arg]),
    ok.

