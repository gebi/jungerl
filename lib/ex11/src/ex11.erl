-module(ex11).
-author('tnt@home.se').
%%%---------------------------------------------------------------------
%%% Created : 14 Feb 1999 by tnt@home.se
%%% Function: X11 implementation.
%%% ====================================================================
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.0, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is x11-0-1
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1999, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%---------------------------------------------------------------------
-export([start/1,req/2,synchronize/2,flush/1,get_display/1,set_display/2,
	 lock_display/1,unlock_display/1]).

%% ---------------------
%% X11 client functions

start(Host)          -> ex11_client:start(Host).
req(X,Req)           -> ex11_client:req(X,Req).
synchronize(X,What)  -> ex11_client:synchronize(X,What).
flush(X)             -> ex11_client:flush(X).
lock_display(X)      -> ex11_client:lock_display(X).
unlock_display(X)    -> ex11_client:unlock_display(X).
get_display(X)       -> ex11_client:get_display(X).
set_display(X,Dpy)   -> ex11_client:set_display(X,Dpy).


