%%%----------------------------------------------------------------------
%%% File    : pan.hrl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created :  8 Mar 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-define(LOG(Tag, Info), panLib:log(Tag, ?MODULE, ?LINE, ?FUNCTION, Info)).
-define(FUNCTION, process_info(self(),current_function)).
