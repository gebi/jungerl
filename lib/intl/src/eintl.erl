%%%-------------------------------------------------------------------
%%% File    : eintl.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Erlang flavour of intl 
%%%
%%% Created : 30 Dec 2003 by Tony Rogvall <tony@bix.hemma.se>
%%%-------------------------------------------------------------------
-module(eintl).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-export([start_link/0,
	 start/0,
	 stop/0,
	 gettext/1,
	 gettext_noop/1,
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

-ifdef(debug).
-compile(export_all).
-endif.


-define(SERVER, eintl).
-define(EINTL,  eintl).

-define(DEFAULT_TEXTDOMAIN_DIR, "/usr/share/locale").
-define(DEFAULT_TEXTDOMAIN_CODESET, ""). 

-define(DEFAULT_LOCALE, "en_US").
-define(DEFAULT_RELEASE_DELAY, 5000).

%%
%% Storage of locale bindings
%%
%%   {monitor,Pid}                 => Ref
%%   
%%   {locale,Pid,Category}         => Locale
%%   {domain,Pid}                  => Domain
%%
%%   {domain_dir,Pid,Domain}       => Dir
%%   {domain_codeset,Pid,Domain}   => CodeSet
%%   {domain_mo,Pid,Locale,Domain} => Mo
%%
%%
%%   {mo,Dir,Locale,Domain}        => Mo
%%   {mo_ref, Mo}                  => Ref-Count
%%

-record(state,
	{
	  tab    %% key value table
	 }).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).


gettext(MsgId) -> 
    dcgettext("", MsgId, messages).

gettext_noop(MsgId) ->
    MsgId.

ngettext(MsgId,MsgIdPlural,N) ->
    dcngettext("", MsgId, MsgIdPlural, N, messages).

dgettext(DomainName, MsgId) ->
    dcgettext(DomainName, MsgId, messages).

dngettext(DomainName, MsgId, MsgIdPlural, N) ->
    dcngettext(DomainName, MsgId, MsgIdPlural, N, messages).

dcgettext("", MsgId, Category) ->
    do_gettext(locale(Category), domain(), MsgId, Category);
dcgettext(DomainName, MsgId, Category) ->
    do_gettext(locale(Category), DomainName, MsgId, Category).


dcngettext("", MsgId, MsgIdPlural, N, Category) ->
    do_ngettext(locale(Category),domain(),MsgId, MsgIdPlural, N, Category);
dcngettext(DomainName, MsgId, MsgIdPlural, N, Category) ->
    do_ngettext(locale(Category),DomainName,MsgId, MsgIdPlural, N, Category).

%%
%% Set current text domain i.e package
%%
textdomain(DomainName) ->
    gen_server:call(?SERVER, {textdomain,self(),DomainName}).

%%
%% Bind domain/package, for the calling process, to a package directory
%%
bindtextdomain(DomainName,DirName) ->
    gen_server:call(?SERVER, {bindtextdomain,self(),
			      DomainName,DirName}).

%%
%% Bind domain/package, for the calling process,to an output code set (charset)
%%
bind_textdomain_codeset(DomainName,Codeset) ->
    gen_server:call(?SERVER,{bind_textdomain_codeset,self(),
			     DomainName,Codeset}).
%%
%% Set locale Category = ctype, numeric, time ...
%%     locale = LL[_CC][.codeset][@modifier]
%%
%%  LL is a ISO 639 language code
%%  CC is a ISO 3166 country code
%%  codeset is character set or encoding identifier like ISO-8859-1 or UTF8
%%  
setlocale(Category, Locale) ->
    gen_server:call(?SERVER, {setlocale, self(), Category, Locale}).

%%
%% Load a MO file
%%
load_mo(Locale,Domain,Category) ->
    gen_server:call(?SERVER, {load_mo,self(),Locale,Domain,Category}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    T = ets:new(?EINTL, [public, named_table, set]),
    {ok, #state{ tab = T }}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({setlocale, Pid, Category, Locale}, From, State) ->
    monitor_proc(Pid),
    ets:insert(?EINTL, {{locale,Pid,Category}, Locale}),
    {reply, ok, State};

handle_call({textdomain,Pid,Domain}, From, State) ->
    monitor_proc(Pid),
    ets:insert(?EINTL, {{domain,Pid}, Domain}),
    {reply, ok, State};

handle_call({bindtextdomain,Pid,Domain,Dir}, From, State) ->
    monitor_proc(Pid),
    ets:insert(?EINTL, {{domain_dir,Pid,Domain}, Dir}),
    {reply, ok, State};

handle_call({bind_textdomain_codeset,Pid,Domain,Codeset}, From, State) ->
    monitor_proc(Pid),
    ets:insert(?EINTL, {{domain_codeset,Pid,Domain}, Codeset}),
    {reply, ok, State};

handle_call({load_mo,Pid,Locale,Domain,Category},From,State) ->
    Ls      = split_locale(Locale),
    Dir     = domain_dir(Pid,Domain),
    CodeSet = domain_codeset(Pid,Domain),
    Reply = 
	case try_lookup_mo(Ls, Dir, Category, Domain, CodeSet) of
	    false ->
		Mo = ets:new(eintl_mo, [set,public]),
		case try_load_mo(Ls,Mo,Dir,Category,Domain,CodeSet) of
		    false ->
			ets:delete(Mo),
			false;
		    Mo ->
			ets:insert(?EINTL,{{domain_mo,Pid,Locale,Domain},Mo}),
			Mo
		end;
	    Mo ->
		ets:insert(?EINTL,{{domain_mo,Pid,Locale,Domain},Mo}),
		Mo
	end,
    {reply, Reply, State};
handle_call(stop, From, State) ->    
    {stop, normal, ok, State};
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    case ets:lookup(?EINTL, {monitor,Pid}) of
	[] ->
	    {noreply, State};
	[{_,Ref}] ->
	    release(Pid),
	    {noreply, State}
    end;
handle_info({timeout,Ref,{release_mo, Mo}}, State) ->
    case ets:lookup(?EINTL, {mo_ref, Mo}) of
	[{_, 0}] ->
	    mo_release(Mo);
	_ ->
	    ignore
    end,
    {noreply, State};
handle_info(Info, State) ->
    io:format("eintl got info: ~p\n", [Info]),
    {noreply, State}.

	    
%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

release(Pid) ->
    ets:match_delete(?EINTL,{{locale,Pid,'_'},'_'}),
    ets:delete(?EINTL, {domain,Pid}), %% current domain
    ets:match_delete(?EINTL, {{domain_dir,Pid,'_'},'_'}),
    ets:match_delete(?EINTL, {{domain_codeset,Pid,'_'},'_'}),
    DMo = ets:match(?EINTL, {{domain_mo,Pid,'_','_'}, '$1'}),
    lists:foreach(
      fun([Mo]) ->
	      case ets:update_counter(?EINTL, {mo_ref,Mo}, -1) of
		  0 -> 
		      mo_release_after(Mo, ?DEFAULT_RELEASE_DELAY);
		  N ->
		      ok
	      end
      end, DMo),
    ets:match_delete(?EINTL, {{domain_mo,Pid,'_','_'},'_'}),
    %% FIXME deref each mo binding!
    ets:delete(?EINTL, {monitor,Pid}),
    ok.


mo_release_after(Mo, 0) ->
    mo_release(Mo);
mo_release_after(Mo, Time) ->
    erlang:start_timer(Time, self(), {release_mo, Mo}).

mo_release(Mo) ->
    ets:match_delete(?EINTL, {{mo,'_', '_', '_','_'}, Mo}),
    ets:delete(?EINTL, {mo_ref, Mo}),
    ets:delete(Mo).
    


locale(Category) -> 
    locale(self(),Category).
locale(Pid,Category) ->
    case ets:lookup(?EINTL, {locale,Pid,Category}) of
	[] -> ?DEFAULT_LOCALE;
	[{_,Locale}] -> Locale
    end.

domain() ->
    domain(self()).
domain(Pid) ->
    case ets:lookup(?EINTL, {domain,Pid}) of
	[] -> "";
	[{_,Domain}] -> Domain
    end.


domain_dir(Domain) ->
    domain_dir(self(),Domain).
domain_dir(Pid, Domain) ->
    case ets:lookup(?EINTL, {domain_dir,Pid,Domain}) of
	[] -> ?DEFAULT_TEXTDOMAIN_DIR;
	[{_,Dir}] -> Dir
    end.


domain_codeset(Domain) ->
    domain_codeset(self(),Domain).
domain_codeset(Pid, Domain) ->
    case ets:lookup(?EINTL, {domain_codeset,Pid,Domain}) of
	[] -> ?DEFAULT_TEXTDOMAIN_CODESET;
	[{_,CodeSet}] -> CodeSet
    end.


%% add a process monitor if needed
monitor_proc(Pid) ->
    Key = {monitor,Pid},
    case ets:lookup(?EINTL, Key) of
	[] ->
	    Ref = erlang:monitor(process, Pid),
	    ets:insert(?EINTL, {Key, Ref});
	[Mon] ->
	    true
    end.


do_gettext(Locale, Domain, MsgId, Category) ->
    case find_mo(Locale, Domain, Category) of
	false -> 
	    MsgId;
	Mo ->
	    mo:gettext(Mo,MsgId)
    end.

do_ngettext(Locale, Domain, MsgId, MsgIdPlural, N, Category) ->
    case find_mo(Locale, Domain, Category) of
	false -> 
	    if N == 1 -> MsgId; true -> MsgIdPlural end;
	Mo ->
	    mo:ngettext(Mo,MsgId,MsgIdPlural,N)
    end.

%% locate the mo file
find_mo(Locale, Domain, Category) ->
    case ets:lookup(?EINTL, {domain_mo,self(),Locale,Domain}) of
	[] ->
	    load_mo(Locale, Domain, Category);
	[{_, Mo}] ->
	    Mo
    end.


split_locale(L=[L1,L2]) ->
    [L];
split_locale(L=[L1,L2,$_,C1,C2]) ->
    Loc1 = [L1,L2],
    [ L, Loc1]; 
split_locale(L=[L1,L2,$_,C1,C2,$.|CodeSet]) ->
    Loc1 = [L1,L2],
    Loc2 = [L1,L2,$_,C1,C2],
    case string:tokens(CodeSet, "@") of
	[Set] ->
	    [ L, Loc2, Loc1];
	[Set,Mod] ->
	    [ L, Loc2 ++ "."++Set, Loc2, Loc1]
    end;
split_locale(_) -> 
    [].


try_lookup_mo([Locale|Ls], Dir, Category, Domain, CodeSet) ->
    case ets:lookup(?EINTL, {mo,Dir,Locale,Category,Domain}) of
	[{_,Mo}] ->
	    ets:update_counter(?EINTL, {mo_ref, Mo}, 1),
	    Mo;
	[] ->
	    try_lookup_mo(Ls, Dir, Category, Domain, CodeSet)
    end;
try_lookup_mo([], Dir, Category, Domain, CodeSet) ->
    false.


try_load_mo([Locale|Ls], Mo, Dir, Category, Domain, CodeSet) ->
    case mo:load(Mo,Dir, Locale, Category, Domain) of
	{ok,Mo} ->
	    Key = {Dir,Locale,Category,Domain},
	    ets:insert(?EINTL, {{mo,Dir,Locale,Category,Domain}, Mo}),
	    ets:insert(?EINTL, {{mo_ref, Mo}, 1}),
	    Mo;
	{error,_} ->
	    try_load_mo(Ls, Mo, Dir, Category, Domain, CodeSet)
    end;
try_load_mo([], _, _, _, _, _) ->
    false.
