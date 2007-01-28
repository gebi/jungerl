%%%-------------------------------------------------------------------
%%% Created : 18 Oct 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : A Yaws web-blog plugin.
%%%
%%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%%% 
%%% @doc <b>wblog</b> is a Yaws plugin that implements a simple (we)blog service.
%%%      With <b>wblog</b>, it is possible to add blog entries as well as list- 
%%%      and traverse the entries. Entries are stored in a Mnesia table that is 
%%%      created once with the {@link setup/0} function. New entries are added 
%%%      with the {@link new/3} and  {@link new/4} functions. A comment for
%%%      a specified entry is added by the {@link add_comment/3}.
%%%
%%%      <p>To make it as adaptable as possble it is possible to either
%%%      take care of the final formatting yourself, or to retrieve the result
%%%      in EHTML format. In some cases it is also possble to provide
%%%      CSS tag info, which is attached to the resulting EHTML.</p>
%%%
%%%      <p>In the wblog/priv directory, a Yaws configuration file can
%%%      be found that can be used for testing wblog. Start it as:</p>
%%%
%%%      <pre>yaws -i -c $path/priv/yaws.conf -pa $path/ebin</pre>
%%%
%%%      <p>The very first time you start the system you need to
%%%         setup Mnesia as shown below:</p>
%%%
%%%      <pre>
%%%       application:load(mnesia).
%%%       application:set_env(mnesia, dir, "/tmp/yfront").
%%%       mnesia:create_schema([node()]).
%%%       application:start(mnesia).
%%%       wblog:setup().
%%%      </pre>
%%% 
%%%      <p>Then point your browser at: http://localhost:4080/</p>
%%%
%%%      <p>Note that, once you have setup Mnesia, it suffice to start 
%%%         the system as shown below:</p>
%%%
%%%      <pre>
%%%       application:load(mnesia).
%%%       application:set_env(mnesia, dir, "/tmp/yfront").
%%%       application:start(mnesia).
%%%      </pre>
%%%
%%% @end
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(wblog).

-export([new/3, new/4, new/5, entry/1, 
	 add_comment/3, add_comment/4,
	 ehtml_entry/1, ehtml_entry/2, ehtml_entry/3,
	 entries/1, entries/2, entries/3, 
	 ehtml_list/2, ehtml_list/3,
	 replace/4, id/1, head/1, text/1, clr_comments/1,
	 clr_all/1, get_comments/1, 
	 fmt_comments/2, fmt_comment/3
	]).
%% Run only once, at setup.
-export([setup/0]).

-import(lists, [map/2, foldl/3, foldr/3, keysort/2, reverse/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("../include/wblog.hrl").

%%% Mnesia Meta-Info table definition
-record(wblog_meta, {
	  user,           % userId
	  first,          % first wblog entry (Id)
	  last            % last wblog entry (Id)
	 }).


%%% @doc Creates the Mnesia tables: <b>wblog</b> and <b>wblog_meta</b>.
%%%      This function should only be called once, when the wblog Mnesia-tables
%%%      are to be created.
setup() ->
    create_tables().

create_tables() ->
    {atomic, ok} = create_wblog(),
    {atomic, ok} = create_wblog_meta().

create_wblog() ->
    mnesia:create_table(wblog, [{attributes, record_info(fields, wblog)},
				{type, set},
				{index, [user]},
				{disc_copies, [node()]}]).
create_wblog_meta() ->
    mnesia:create_table(wblog_meta,[{attributes,record_info(fields,wblog_meta)},
				    {type, set},
				    {disc_copies, [node()]}]).

%%% @doc Return the identifier for this weblog entry.
id(Entry) when record(Entry, wblog) -> Entry#wblog.id.

%%% @doc Return the header for this weblog entry.
head(Entry) when record(Entry, wblog) -> Entry#wblog.head.

%%% @doc Return the text body for this weblog entry.
text(Entry) when record(Entry, wblog) -> Entry#wblog.text.

%%% @doc Add a new <i>wblog entry</i>.
%%%      Specify the header and the text body, and if the
%%%      entry should be feed into the Yaws RSS feed.
new(Head, Text, RSS) -> 
    new(?NO_USER, Head, Text, RSS).

%%% @doc Add a new <i>wblog entry</i> for a specified user.
%%%      Works as {@link new/3} but with a specific user added.
%%%      This makes it possible to maintain several distinct users.
new(User, Head, Text, RSS) -> 
    new(User, Head, Text, RSS, "").

%%% @doc Add a new <i>wblog entry</i> .
%%%      Works as {@link new/4} but with a specific page link added.
%%%      This is used for storing a link to the wblog entry into
%%%      the RSS feed (if RSS is enabled). For example, a link can
%%%      be <i>"http://localhost:4080/wblog.yaws"</i> , which will be
%%%      concatenated with <i>"?id=34545433"</i> before it is stored
%%%      in the Yaws RSS feed.
%%%
%%%      Note: If the link contains a '?' we will conctenate '&amp;id=345...'
new(User, Head, Text, RSS, Link) -> 
    Id = mk_id(),
    W = #wblog{id   = Id,
	       user = User,
	       head = Head,
	       text = Text,
	       date = now_to_gregsec()},
    store_rss(RSS, User, Head, Text, query_append(Link, "id="++i2l(Id))),
    F = fun() ->
		case mnesia:read({wblog_meta, User}) of
		    [M] ->
			[D] = mnesia:read({wblog, M#wblog_meta.last}),
			mnesia:write(D#wblog{next = Id}),
			mnesia:write(W#wblog{prev = M#wblog_meta.last}),
			mnesia:write(M#wblog_meta{last = Id});
		    [] ->
			mnesia:write(W),
			mnesia:write(#wblog_meta{user  = User,
						 first = Id,
						 last  = Id})
		end
	end,
    tVALUE(mnesia:transaction(F)).

%%% @doc Replace a <i>wblog entry</i> .
%%%      This makes it possible to edit and update a blog entry.
%%%      A check is made to assure that the user is the same as
%%%      stored in the entry. If not, the replace operation will fail.
replace(Id, User, Head, Text) -> 
    F = fun() ->
		case mnesia:read(wblog, Id, write) of
		    [E] when E#wblog.user == User ->
			mnesia:write(E#wblog{head = Head, 
					     text = Text});
		    _ ->
			{error, "wrong user"}
		end
	end,
    tVALUE(mnesia:transaction(F)).
	
%%% @doc Clear all wblog entries for specified user
clr_all(User) ->       
    L = qd(qlc:q([B || B <- mnesia:table(wblog),
		       B#wblog.user == User])),
    F = fun() -> 
		lists:foreach(fun(X) -> mnesia:delete_object(X) end, L),
		mnesia:delete({wblog_meta, User})
	end,
    mnesia:transaction(F).


qd(Q) ->
    mnesia:async_dirty(fun() -> qlc:eval(Q) end).		    

%%% @doc Add a comment to the specified wblog entry.
add_comment(Id, Text, Who) ->
    add_comment(Id, Text, Who, false).

%%% @doc Add a comment to the specified wblog entry.
%%%      Indicate if it is the author who makes the comment.
add_comment(Id, Text, Who, Author) ->
    C = #wblog_comment{text   = Text,
		       who    = Who,
		       author = Author,
		       date   = now_to_gregsec()},
    F = fun() ->
		case entry_t(Id) of
		    [E] -> 
			Cs = keysort(#wblog_comment.date, E#wblog.comments ++ [C]),
			mnesia:write(E#wblog{comments = Cs});
		    _   -> 
			mnesia:abort("wrong id")
		end
	end,
    tVALUE(mnesia:transaction(F)).

%%% @doc Get the list of #wblog_comments{} records
get_comments(Id) ->
    F = fun() ->
		case entry_t(Id) of
		    [E] -> E#wblog.comments;
		    _   -> mnesia:abort("wrong id")
		end
	end,
    tVALUE(mnesia:transaction(F)).
		    

%%% @doc Clear all comments of the specified wblog entry.
clr_comments(Id) ->
    F = fun() ->
		case entry_t(Id) of
		    [E] -> 
			mnesia:write(E#wblog{comments = []});
		    _   -> 
			mnesia:abort("wrong id")
		end
	end,
    tVALUE(mnesia:transaction(F)).


%%% @doc Returns a list with a matching wblog record.
entry(Id) ->
    mnesia:dirty_read({wblog, Id}).

%%% @doc Returns the N last wblog entries.
entries(N) when N > 0 ->
    entries(N, ?NO_USER).

%%% @doc Returns the N last wblog entries for specified User.
entries(N, User) when N > 0 ->
    F = fun() -> 
		case last_entry_t(User) of
		    [Last] -> last_entries_t(N, Last#wblog.id);
		    []     -> []
		end
	end,
    tVALUE(mnesia:transaction(F)).

%%% @doc Returns the N last wblog entries for specified User starting at Id.
entries(N, User, Id) when N > 0 ->
    F = fun() -> 
		case entry_t(Id) of
		    [E] when E#wblog.user == User ->
			last_entries_t(N, Id);
		    _ -> 
			mnesia:abort("wrong user")
		end
	end,
    tVALUE(mnesia:transaction(F)).

last_entry_t(User) ->
    case mnesia:read({wblog_meta, User}) of
	[M] -> 
	    mnesia:read({wblog_meta, User}),
	    mnesia:read({wblog, M#wblog_meta.last});
	_ ->
	    []
    end.

last_entries_t(_, false) -> [];
last_entries_t(0, _)     -> [];
last_entries_t(N, Id) ->
    case entry_t(Id) of
	[E] -> [E | last_entries_t(N-1, E#wblog.prev)];
	_   -> []
    end.

entry_t(Id) ->
    mnesia:read({wblog, Id}).

%%% @doc Produce a summary list
%%%      based on a list of wblog entries. 
%%%      The result is an EHTML table.
%%%      It is possible to attach CSS tags to the table by
%%%      providing a list of tuples like:
%%%      <c>{table, [{class, "my-table"}]}</c> 
%%%      Special targets are:
%%%      <c>{href, "/wblog_entry.yaws"}</c>
%%%      <c>{date, List-of-CSS-typles}</c>
%%%      where href will be used for creating the link for
%%%      entry. The link will be appended with a query
%%%      argument "?id=ID" (note: if the link contains a '?'
%%%      character we will append '&amp;id=Id'). The 'date' tag is used for
%%%      passing in CSS info for the Date information.
ehtml_list(Ws, Opts) when list(Opts) ->
    {table, opts(table, Opts),
     map(fun(W) ->
		 {tr, opts(tr, Opts),
		  [{td, opts(td, Opts), 
		    [{span, opts(date, Opts), date2str(W#wblog.date)},
		     {br, []},
		     lnk(W#wblog.id, W#wblog.head, Opts)]}]}
	 end, Ws)}.

%%% @doc Produce a summary list.
%%%      Works as {@link ehtml_list/2} but also takes a function
%%%      Func(Id, Text) that can be used to create the anchor
%%%      tag. Example:
%%%      <pre>
%%%  wblog:ehtml_list(Es, [{date, [{class, "wblog-date"}]}], sum_func())}.
%%%  sum_fun() ->
%%%    fun(Id, Text) ->
%%%	    {a, [{href, "#"}, {title, "Link to entry"},
%%%		 {class, "blog_link"}, {id, Id}], Text}
%%%    end.
%%%      </pre>
%%% @end
ehtml_list(Ws, Opts, Func) when list(Opts),function(Func) ->
    {table, opts(table, Opts),
     map(fun(W) ->
		 {tr, opts(tr, Opts),
		  [{td, opts(td, Opts), 
		    [{span, opts(date, Opts), date2str(W#wblog.date)},
		     {br, []},
		     Func(W#wblog.id, W#wblog.head)]}]}
	 end, Ws)}.

%%% @doc Produces an EHTML structure, representing the specified entry.
ehtml_entry(E) when record(E, wblog) -> 
    ehtml_entry(E, []).

%%% @doc Works as {@link ehtml_entry/1} but also takes a list of options.
%%%      By providing the option: {href, WblogPage} , links to Prev(ious),
%%%      Next, and Permalink will be created by appending WblogPage to the
%%%      query argument: id=ID. The option: {comment, CommentPage} will
%%%      create a link for creating comments, pointing to CommentsPage 
%%%      appended with the query argument id=ID. 
ehtml_entry(E, Opts) when record(E, wblog) -> 
    ehtml_entry(E, Opts, []).

%%% @doc Works as {@link ehtml_entry/2} but also takes a list of language translations.
%%%      By providing language specific Key-Value tuples, it is possible
%%%      adapt the wblog to different languages. A tuple consist of a predefined
%%%      key and a translated string. The predefined keys are:
%%%      <ul>
%%%      <li>{prev, String} - string denoting previous entry, 
%%%                           default is <i>"prev"</i></li>
%%%      <li>{next, String} - string denoting next entry, 
%%%                           default is <i>"next"</i></li>
%%%      <li>{permalink, String} - string denoting current entry, 
%%%                                default is <i>"permalink"</i></li>
%%%      <li>{add_comment, String} - string denoting link to add a comment, 
%%%                                  default is <i>"add comment"</i></li>
%%%      <li>{singular_comment, String} - string denoting the translation, 
%%%                                       of the word <i>"comment"</i> which
%%%                                       also is the default value.</li>
%%%      <li>{plural_comment, String} - string denoting the translation, 
%%%                                     of the word <i>"comments"</i> which
%%%                                     also is the default value.</li>
%%%      <li>{by, String} - string denoting the translation, 
%%%                         of the word <i>"by"</i> which
%%%                         also is the default value.</li>
%%%      </ul>
ehtml_entry(E, Opts, Lang) when record(E,wblog),list(Opts),list(Lang) ->
     {'div', [{class, "wblog-entry"}],
     [{p, [{class, "wblog-head"}], {pre_html, E#wblog.head}},
      {p, [{class, "wblog-body"}], {pre_html, E#wblog.text}},
      {p, [],
       [{span, [{class, "wblog-prev"}], 
	 lnk(E#wblog.prev, lang(prev, Lang, "prev"), Opts)},
	{span, [{class, "wblog-next"}], 
	 lnk(E#wblog.next, lang(next, Lang, "next"), Opts)},
	{span, [{class, "wblog-permalink"}], 
	 lnk(E#wblog.id, lang(permalink, Lang, "permalink"), Opts)},
	{span, [{class, "wblog-add-comment"}], 
	 comment(E#wblog.id, lang(add_comment, Lang, "add comment"), Opts)}]},
      maybe_comment(E, Lang)]}; 
%%
ehtml_entry(E, Func, Lang) when record(E,wblog),function(Func),list(Lang) ->
    %% A calback is provided which is supposed to create the {a, Opts, Body} 
    %% anhcor. It is expected to take two arguments F(tag, Id) , where Tag
    %% is any of: prev | next | permalink | add_comment
     {'div', [{class, "wblog-entry"}],
     [{p, [{class, "wblog-head"}], {pre_html, E#wblog.head}},
      {p, [{class, "wblog-body"}], {pre_html, E#wblog.text}},
      {p, [],
       [{span, [{class, "wblog-prev"}], 
	 Func(prev, E#wblog.prev)},
	{span, [{class, "wblog-next"}], 
	 Func(next, E#wblog.next)},
	{span, [{class, "wblog-permalink"}], 
	 Func(permalink, E#wblog.id)},
	{span, [{class, "wblog-add-comment"}], 
	 Func(add_comment,E#wblog.id)}]},
      maybe_comment(E, Lang)]}; 
%%
ehtml_entry(Id, Opts, Lang) -> 
    [E] = entry(Id),
    ehtml_entry(E, Opts, Lang).

maybe_comment(E, Lang) ->
    {'div', [{id, "wblog-comment-box"}],
     do_maybe_comment(E, Lang)}.

do_maybe_comment(E, Lang) when E#wblog.comments == [] ->
    [{p, [{class, "wblog-comments"}], 
      {pre_html, "0 "++lang(plural_comments, Lang, "comments")}}];
do_maybe_comment(E, Lang) ->
    Len = length(E#wblog.comments),
    [{p, [{class, "wblog-comments"}], 
      {pre_html, i2l(Len)++" "++maybe_plural(Len, Lang)}},
     fmt_comments(E#wblog.comments, Lang)].

maybe_plural(1, Lang) -> lang(singular_comment, Lang, "comment");
maybe_plural(_, Lang) -> lang(plural_comment, Lang, "comments").


%%% @doc Format a list of #weblog_comment{} entries into HTML
fmt_comments(Cs, Lang) ->
    fmt_comments(Cs, Lang, 1).

fmt_comments([H|T], Lang, N) ->
    [fmt_comment(H, Lang, N) | fmt_comments(T, Lang, N+1)];
fmt_comments([], _, _) ->
    [].

%%% @doc Format a #weblog_comment{} entry into HTML
fmt_comment(H, Lang, N) ->
    {'div', [{id, "wblog-comment"++integer_to_list(N)}],
     [{'div', [{class, "wblog-comment-head"++author(H)}],
       {pre_html, date2str(H#wblog_comment.date)++
	" "++lang(by, Lang, "by")++": "++H#wblog_comment.who}},
      {'div', [{class, "wblog-comment-body"++author(H)}],
       {pre_html, H#wblog_comment.text}}]}.

author(#wblog_comment{author = true}) -> " wblog-comment-author";
author(_)                             -> "".

%%% Store blog entry into the RSS feeder
store_rss(false, _User, _Head, _Text, _Link) ->   % RSS not enabled
    ok;
store_rss(true, User, Head, Text, Link) ->
    yaws_rss:insert(wblog, l2a(User), Head,
                    Link,  
                    Text,
                    User).

l2a(L) when list(L) -> list_to_atom(L);
l2a(A) when atom(A) -> A.


lnk(Id, Head, Opts) -> lnk(Id, Head, Opts, href).

comment(Id, Head, Opts) -> lnk(Id, Head, Opts, comment).

lnk(false, Head, _, _)   -> Head;
lnk(Id, Head, Opts, Tag) ->
    {a, [{href, query_append(opts(Tag, Opts),"id="++i2l(Id))} |
	 opts(a, Opts)],
     Head}.

%%% Be smart: if our query argument is the first then use '?' else '&'
query_append(P, S) ->
    case string:chr(P, $?) of
	I when I>0 -> P++"&"++S;
	_          -> P++"?"++S
    end.
	 
lang(Key, List, Default) ->
    opts(Key, List, Default).

opts(Key, Opts) ->
    case lists:keysearch(Key, 1, Opts) of
	{value, {_,V}} when list(V) -> V;
	_                           -> []
    end.

opts(Key, Opts, Default) ->
    case lists:keysearch(Key, 1, Opts) of
	{value, {_,V}} when list(V) -> V;
	_                           -> Default
    end.


date2str(GregSec) ->
    {{Y,Mo,D},{H,Mi,S}} = gregsec_to_datetime(GregSec),
    i2l(D)++" "++month(Mo)++" "++i2l(Y)++" ("++t(H)++":"++t(Mi)++":"++t(S)++")".

t(X) when integer(X) ->
    t1(integer_to_list(X));
t(_) ->
    "".

t1([X]) -> [$0,X];
t1(X)   -> X.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".


now_to_gregsec() ->
    calendar:datetime_to_gregorian_seconds({date(),time()}).

gregsec_to_datetime(GregSec) ->
    calendar:gregorian_seconds_to_datetime(GregSec).

mk_id() ->
    {A,B,C} = erlang:now(),
    i2l(A)++i2l(B)++i2l(C).

i2l(I) when integer(I) -> integer_to_list(I);
i2l(L) when list(L)    -> L.

%%% transaction value
tVALUE({atomic,Val})     -> {ok, Val};
tVALUE({aborted,Reason}) -> {error, {aborted, Reason}}.
