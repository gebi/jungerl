-ifndef(_ERADIUS_DICT).
-define(_ERADIUS_DICT, true).
%%%-------------------------------------------------------------------
%%% File        : eradius_dict.hrl
%%% Author      : tobbe@bluetail.com
%%% Description : Dictionary definitions.
%%% Created     : 25 Sep 2003 by tobbe@bluetail.com
%%%
%%% $Id$
%%%-------------------------------------------------------------------


-record(attribute, {
	  id,         % integer
	  type,       % atom
	  name}).     % string

-record(vendor, {
	  type,       % integer
	  name}).     % string


-endif.

