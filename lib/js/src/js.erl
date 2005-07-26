%%%-------------------------------------------------------------------
%%% Created : 18 Jun 2005 by Tobbe <tobbe@tornkvist.org>
%%% Desc.   : A rip-off of Ruby's javascript_helper.rb.
%%%-------------------------------------------------------------------
-module(js).
-export([link_to_function/2, link_to_function/3,
	 link_to_remote/2, link_to_remote/3,
	 observe_field/2, observe_form/2,
	 periodically_call_remote/1, periodically_call_remote_S/1, 
	 form_remote_tag/1,
	 prototype_js/0, fd2qs_js/0]).

-import(lists,[flatten/1, member/2]).

%%% 
%%% Use the following from another application to get hold of
%%% the javascript files to be delivered in the head.
%%% Example:
%%%
%%% <script type="text/javascript" src="/prototype.yaws"></script>
%%%
%%% In the file prototype.yaws, we call: js:prototype_js/0
%%%
%%%
prototype_js() ->
    Dir = code:priv_dir(?MODULE),
    file:read_file(filename:join([Dir,"docroot","js","prototype-1.2.0.js"])).

fd2qs_js() ->
    Dir = code:priv_dir(?MODULE),
    file:read_file(filename:join([Dir,"docroot","js","fd2qs.js"])).


%%% ================================================================
%%% Returns a link to a remote action defined by <tt>options[:url]</tt> 
%%% (using the url_for format) that's called in the background using 
%%% XMLHttpRequest. The result of that request can then be inserted into a
%%% DOM object whose id can be specified with <tt>options[:update]</tt>. 
%%% Usually, the result would be a partial prepared by the controller with
%%% either render_partial or render_partial_collection. 
%%%
%%% Examples:
%%%  link_to_remote "Delete this post", :update => "posts", :url => { :action => "destroy", :id => post.id }
%%%  link_to_remote(image_tag("refresh"), :update => "emails", :url => { :action => "list_emails" })
%%%
%%% By default, these remote requests are processed asynchronous during 
%%% which various callbacks can be triggered (for progress indicators and
%%% the likes).
%%%
%%% Example:
%%%   link_to_remote word,
%%%       :url => { :action => "undo", :n => word_counter },
%%%       :complete => "undoRequestCompleted(request)"
%%%
%%% The callbacks that may be specified are:
%%%
%%% <tt>:loading</tt>::       Called when the remote document is being 
%%%                           loaded with data by the browser.
%%% <tt>:loaded</tt>::        Called when the browser has finished loading
%%%                           the remote document.
%%% <tt>:interactive</tt>::   Called when the user can interact with the 
%%%                           remote document, even though it has not 
%%%                           finished loading.
%%% <tt>:complete</tt>::      Called when the XMLHttpRequest is complete.
%%%
%%% If you for some reason or another need synchronous processing (that'll
%%% block the browser while the request is happening), you can specify 
%%% <tt>options[:type] = :synchronous</tt>.
%%%
%%% You can customize further browser side call logic by passing
%%% in Javascript code snippets via some optional parameters. In
%%% their order of use these are:
%%%
%%% <tt>:confirm</tt>::      Adds confirmation dialog.
%%% <tt>:condition</tt>::    Perform remote request conditionally
%%%                          by this expression. Use this to
%%%                          describe browser-side conditions when
%%%                          request should not be initiated.
%%% <tt>:before</tt>::       Called before request is initiated.
%%% <tt>:after</tt>::        Called immediately after request was
%%%                          initiated and before <tt>:loading</tt>.
%%% ================================================================
%%%
%%% Erlang example:
%%%
%%% <div id="time_div">
%%%       I don't have the time, but
%%%       <%= js:link_to_remote("click here",
%%%                             [{update, "time_div"},
%%%			         {type, asynchronous},
%%%                              {position, "after"}, 
%%%                              {url, "/say_when.yaws" }]) %>
%%%       and I will look it up.
%%% </div>
%%%
%%% ================================================================
link_to_remote(Name, Function) ->
    link_to_remote(Name, Function, []).

link_to_remote(Name, Opts, HtmlOpts) ->
    link_to_function(Name, remote_function(Opts), HtmlOpts).

%%% ================================================================
%%% Periodically calls the specified url (<tt>options[:url]</tt>) every 
%%% <tt>options[:frequency]</tt> seconds (default is 10).
%%% Usually used to update a specified div (<tt>options[:update]</tt>) with 
%%% the results of the remote call. The options for specifying the target 
%%% with :url and defining callbacks is the same as link_to_remote.
%%% ================================================================
periodically_call_remote(Opts) ->
    Code = periodically_call_remote_S(Opts),
    content_tag(script, {pre_html, Code}, x_get(html_options, Opts, [])).

%%% Return only the string.
periodically_call_remote_S(Opts) ->
    Frequency = x_get(frequency, Opts, "10"),
    "new PeriodicalExecuter(function() {"++
	remote_function(Opts)++"}, "++Frequency++")".

    
%%% ================================================================
%%% Returns a form tag that will submit using XMLHttpRequest in the 
%%% background instead of the regular reloading POST arrangement. 
%%% Even though it's using Javascript to serialize the form elements, 
%%% the form submission will work just like a regular submission as 
%%% viewed by the receiving side (all elements available in @params).
%%% The options for specifying the target with :url and defining callbacks 
%%% is the same as link_to_remote.
%%% ================================================================
form_remote_tag(Opts0) ->
    Opts = x_set(form, Opts0, "true"),
    HtmlOpts = x_set(onsubmit, x_get(html, Opts, []), 
		     remote_function(Opts)++"; return false;"),
    tag(form, x_set(html, Opts, HtmlOpts), true).

%%% ================================================================
%%% Returns a link that'll trigger a javascript +function+ using the 
%%% onclick handler and return false after the fact.
%%%
%%% Examples:
%%%   link_to_function "Greeting", "alert('Hello world!')"
%%%   link_to_function(image_tag("delete"), "if confirm('Really?'){ do_delete(); }")
%%%
%%% ================================================================
link_to_function(Name, Function) ->
    link_to_function(Name, Function, []).

link_to_function(Name, Function, HtmlOpts) ->
    content_tag(a, Name,
		[{href, "#"},
		 {onclick, Function++"; return false;"} |
		 HtmlOpts]).


%%% ================================================================
%%% Observes the field with the DOM ID specified by +field_id+ and makes
%%% an Ajax when its contents have changed.
%%% 
%%% Required +options+ are:
%%% <tt>:frequency</tt>:: The frequency (in seconds) at which changes to
%%%                       this field will be detected.
%%% <tt>:url</tt>::       +url_for+-style options for the action to call
%%%                       when the field has changed.
%%% 
%%% Additional options are:
%%% <tt>:update</tt>::    Specifies the DOM ID of the element whose 
%%%                       innerHTML should be updated with the
%%%                       XMLHttpRequest response text.
%%% <tt>:with</tt>::      A Javascript expression specifying the
%%%                       parameters for the XMLHttpRequest. This defaults
%%%                       to 'value', which in the evaluated context 
%%%                       refers to the new field value.
%%%
%%% Additionally, you may specify any of the options documented in
%%% +link_to_remote.
%%% ================================================================
observe_field(FieldId, Opts) ->
    content_tag(script, 
		{pre_html, observe_field_S(FieldId, Opts)},
		[{type, "text/javascript"}]).

observe_field_S(FieldId, Opts) ->
    build_observer("Form.Element.Observer", FieldId, Opts).


%%% ================================================================
%%% Like +observe_field+, but operates on an entire form identified by the
%%% DOM ID +form_id+. +options+ are the same as +observe_field+, except 
%%% the default value of the <tt>:with</tt> option evaluates to the
%%% serialized (request string) value of the form.
%%% ================================================================
observe_form(FormId, Opts) ->
    content_tag(script, 
		{pre_html, observe_form_S(FormId, Opts)},
		[{type, "text/javascript"}]).

observe_form_S(FormId, Opts) ->
    build_observer("Form.Observer", FormId, Opts).


%%% ================================================================
%%% Creates a form tag and hidden <iframe> necessary for the upload progress
%%% status messages to be displayed in a designated +div+ on your page.
%%%
%%% == Upload IDs
%%%
%%% For the view and the controller to know about the same upload they must share
%%% a common +upload_id+.  This helper prepares the next available +upload_id+ when
%%% called.  There are other methods which use the +upload_id+ so the order in which
%%% you include your content is important.  Any content that depends on the 
%%% +upload_id+ will use the one defined +form_tag_with_upload_progress+
%%% otherwise you will need to explicitly declare the +upload_id+ shared among
%%% your progress elements.
%%%
%%% Status container after form:
%%%
%%%   <%= form_tag_with_upload_progress %>
%%%   <%= end_form_tag %>
%%%
%%%   <%= upload_status_tag %>
%%%
%%% Status container before form:
%%%
%%%   <% my_upload_id = next_upload_id %>
%%%
%%%   <%= upload_status_tag %>
%%%
%%%   <%= form_tag_with_upload_progress :upload_id => my_upload_id %>
%%%   <%= end_form_tag %>
%%%
%%% It is recommended that the helpers manage the +upload_id+ parameter.
%%%
%%% == Options
%%%
%%% +form_tag_with_upload_progress+ uses similar options as +form_tag+
%%% yet accepts another hash for the options used for the +upload_status+ action.
%%%
%%% <tt>url_for_options</tt>:: The same options used by +form_tag+ including:
%%% <tt>:upload_id</tt>:: the upload id used to uniquely identify this upload
%%%
%%% <tt>options</tt>:: similar options to +form_tag+ including:
%%% <tt>begin</tt>::   Javascript code that executes before the first status update occurs.
%%% <tt>finish</tt>::  Javascript code that executes after the action that receives the post returns.
%%% <tt>:frequency</tt>:: number of seconds between polls to the upload status action.
%%%
%%% <tt>status_url_for_options</tt>:: options passed to +url_for+ to build the url
%%% for the upload status action.
%%% <tt>:controller</tt>::  defines the controller to be used for the update status action
%%% <tt>:action</tt>::      defines the action to be used for the update status action
%%%
%%% Parameters passed to the action defined by status_url_for_options
%%%
%%% <tt>:upload_id</tt>::   the upload_id automatically generated by +form_tag_with_upload_progress+ or the user defined id passed to this method.
%%%   
%%% ================================================================

%%form_tag_with_upload_progress(UrlOpts, Opts, StatOpts, ParamsForUrl) ->
%%
%%    %% Setup the action URL and the server-side upload_status action for
%%    %% polling of status during the upload
%%
%%    UploadId = x_get(upload_id, Opts, random_upload_id()),
%%    put(current_upload_id, UploadId),
%%    UploadActionUrl = x_get(url, UrlOpts, "/upload_action.yaws"),
%%    StatOpts1 = x_defaults(url, 
%%			   x_defaults(upload_id, StatOpts, UploadId),
%%			   "/upload_status.yaws"),
%%    StatusUrl = x_get(url, StatOpts1),
%%    
%%    %% Prepare the form options.  Dynamically change the target and URL to 
%%    %% enable the status updating only if javascript is enabled, otherwise 
%%    %% perform the form submission in the same frame.
%%
%%    UploadTarget = x_get(target, Opts, upload_target_id()),
%%    UploadIdParam = url_append_qs("upload_id="++UploadId),
%%
%%    %% Externally :begin and :finish are the entry and exit points
%%    %% Internally, :finish is called :complete
%%
%%    JsOpts = [{decay, x_get(decay, Opts, frequency_decay())},
%%	      {frequency, x_get(frequency, Opts, frequency())}],
%%
%%    UpdaterOpts = js_opts_map(JsOpts),
%%
%%    %% Finish off the updating by forcing the progress bar to 100% 
%%    %% because the results of the post may load and finish in the 
%%    %% IFRAME before the last status update is loaded. 
%%
%%    Opts1 = x_set(complete, Opts, 
%%		  upload_progress_update_bar_js(100)++
%%		  x_get(finish, Opts, "")),
%%
%%    Opts2 = x_get(script, Opts1, "true"),
%%
%%    Updater = upload_update_object() ++
%%	" = new Ajax.PeriodicalUpdater('"++status_tag_id()++"',"++
%%	StatusUrl++
%%	options_for_ajax(Opts2++UpdaterOpts),
%%
%%    Updater2 = case x_get('begin', Opts2) of
%%		   {true, Begin} -> Begin++"; "++Updater;
%%		   false         -> Updater
%%	       end,
%%    Updater3 = upload_progress_update_bar_js(0)++"; "++Updater2,
%%    
%%    %% Touch up the form action and target to use the given target 
%%    %% instead of the default one. Then start the updater.
%%
%%    OnSubmit = "if (this.action.indexOf('upload_id') < 0){ this.action += '"++
%%	escape_javascript(UploadIdParam)++"'; }"++
%%	"this.target = '"++escape_javascript(UploadTarget)++"';"++
%%	Updater3++"; return true",
%%
%%    Opts3 = x_set(multipart, x_set(onsubmit, Opts2, OnSubmit), "true"),
%%
%%    Opts4 = x_dels(['begin',finish,complete,frequency,decay,script], Opts3),
%%		
%%    %% Create the tags
%%    %% If a target for the form is given then avoid creating the hidden IFRAME
%%
%%    Tag = form_tag(Opts4),
%%    case x_member(target, Opts4) of
%%	{true, _} -> Tag;
%%	false ->
%%	    Tag ++
%%		content_tag(iframe, "",
%%			    [{id, UploadTarget},
%%			     {name, UploadTarget},
%%			     {src, ""},
%%			     {style, "width:0px;height:0px;border:0"}])
%%    end.

%%% Default number of seconds between client updates
frequency() -> "2.0".

%%% Factor to decrease the frequency when the +upload_status+ action 
%%% returns the same results. To disable update decay, set this 
%%% constant to +false+
frequency_decay() -> "1.8".

random_upload_id() ->
    {_,_,X} = erlang:now(),
    i2l(X rem 97). 

%%% Append part of query string to Url
url_append_qs(Url, Qs) -> 
    url_append_qs(Url, Qs, true).

url_append_qs([$?|T], Qs, _)     -> [$?|url_append_qs(T, Qs, false)];
url_append_qs([H|T], Qs, First)  -> [H|url_append_qs(T, Qs, First)];
url_append_qs([], Qs, true)      -> "?"++Qs;
url_append_qs([], Qs, false)     -> "&"++Qs.
			    
%%% Default values
upload_target_id()     -> "UploadTarget"++get(current_upload_id).
status_tag_id()        -> "UploadStatus"++get(current_upload_id).
progress_bar_id()      -> "UploadProgressBar"++get(current_upload_id).
upload_update_object() -> "document.uploadStatus"++get(current_upload_id).




%%%
%%% Internal functions
%%%

escape_javascript(S) -> S.  % FIXME


%%form_tag(Opts) ->
%%    Hs1 = x_default(method, Opts, "post"),
%%    Hs2 = x_swap(multipart, Hs1, {enctype, "multipart/form-data"}),
%%    tag(form, x_set(html, Opts, Hs2), true).


content_tag(Tag, Content, Opts) ->
    {Tag, Opts, [Content]}.

tag(Tag, Opts) ->
    tag(Tag, Opts, false).

tag(Tag, Opts, true)  -> {Tag, Opts};
tag(Tag, Opts, false) -> {Tag, Opts, []}.

%%tag(Tag, Opts, Bool) when Bool==true; Bool==false ->
%%    "<"++a2l(Tag)++" "++tag_options(Opts)++
%%	if (Bool) -> ">"; true -> "/>" end.

tag_options(Opts) ->
    lists:foldr(fun({K,V}, Acc) ->
			a2l(K)++"=\""++a2l(V)++"\" "++Acc
		end,
		[], Opts).

remote_function(Opts) ->
    JsOpts = options_for_ajax(Opts),
    Func = [case x_member(update, Opts) of
		{true, Val} ->
		    "new Ajax.Updater('"++a2l(Val)++"', ";
		false ->
		    "new Ajax.Request("
	    end,
	    "'"++a2l(x_get(url, Opts))++"'", 
	    add_not_nil(", ", JsOpts), ")"],
    flatten(x_confirm(Opts, 
		      x_condition(Opts, 
				  x_after(Opts, 
					  x_before(Opts, Func))))).

add_not_nil(_, []) -> [];
add_not_nil(S, L)  -> S++L.

x_before(Opts, Func) ->
    case x_member(before, Opts) of
	{true, Val} -> a2l(Val)++"; "++ Func;
	false       -> Func
    end.

x_after(Opts, Func) ->
    case x_member('after', Opts) of
	{true, Val} -> Func++"; "++a2l(Val);
	false       -> Func
    end.

x_condition(Opts, Func) ->
    case x_member(condition, Opts) of
	{true, Val} -> "if ("++a2l(Val)++") { "++Func++"; }";
	false       -> Func
    end.

x_confirm(Opts, Func) ->
    case x_member(confirm, Opts) of
	{true, Val} -> "if (confirm('"++escape(a2l(Val))++"') { "++Func++"; }";
	false       -> Func
    end.

x_member(Key, [{Key, Val}|_]) -> {true, Val};
x_member(Key, [_|T])          -> x_member(Key, T);
x_member(_, [])               -> false.

x_get(Key, Opts) ->
    x_get(Key, Opts, "").

x_get(Key, Opts, Def) ->
    case x_member(Key, Opts) of
	{true, Val} -> Val;
	_           -> Def
    end.

%%% Replace option iff it exist.
x_replace(Key, [{Key,_}|T], Val) -> [{Key,Val}|T];
x_replace(Key, [H|T], Val)       -> [H|x_replace(Key, T, Val)];
x_replace(_, [], _)              -> [].

%%% Replace option or create it.
x_set(Key, [{Key,_}|T], Val) -> [{Key,Val}|T];
x_set(Key, [H|T], Val)       -> [H|x_set(Key, T, Val)];
x_set(Key, [], Val)          -> [{Key,Val}].

%%% Set option iff it don't exist.
x_default(Key, [{Key,_} = H|T], _) -> [H|T];
x_default(Key, [H|T], Val)         -> [H|x_default(Key, T, Val)];
x_default(Key, [], Val)            -> [{Key,Val}].

%%% Swap singleton option
x_swap1(H, [H|T], E) -> [E|T];
x_swap1(X, [H|T], E) -> [H|x_swap1(X, T, E)];
x_swap1(_, [], _)    -> [].

%%% Delete option.
x_del(Key, [{Key,_}|T]) -> T;
x_del(Key, [H|T])       -> [H|x_del(Key, T)];
x_del(_, [])            -> [].

%%% Delete options
x_dels(Keys, Opts) ->
    lists:foldl(fun(K, Acc) -> x_del(K, Acc) end, Opts, Keys).

%%% Option value => list
x_a2l(Key, [{Key,Val}|T]) -> [{Key,a2l(Val)}|T];
x_a2l(Key, [H|T])         -> [H|x_a2l(Key, T)];
x_a2l(_, [])              -> [].


%%% Escape carrier returns and single and double quotes for Javascript segments.
escape(Str) ->
    Str.  % FIXME


options_for_ajax(Opts) ->
    js_opts_map(js_options(build_callbacks(Opts))).

js_opts_map(JsOpts) ->
    "{"++lists:foldl(fun({X,Y}, Acc) -> 
			     X++":"++Y++add_not_nil(", ", Acc) 
		     end,
		     "",
		     JsOpts)++"}".

js_options([{type, synchronous}|T]) ->
    [{"asynchronous", "false"}|js_options(T)];
js_options([{type, asynchronous}|T]) ->
    [{"asynchronous", "true"}|js_options(T)];
js_options([{method, Method}|T]) ->
    [{"method", "'"++a2l(Method)++"'"}|js_options(T)];
js_options([{position, Pos}|T]) ->
    [{"insertion", "Insertion."++camelize(a2l(Pos))}|js_options(T)];
js_options([form|T]) ->
    [{"parameters", "'Form.serialize(this)'"}|js_options(T)];
js_options([{with, With}|T]) ->
    [{"parameters", a2l(With)}|js_options(T)];
js_options([_|T]) ->
    js_options(T);
js_options([]) ->
    [].

build_observer(Klass, Name, Opts) ->
    Opts2 = case x_member(update, Opts) of
		{true, _} -> 
		    case x_member(with, Opts) of
			{true, _} -> 
			    x_a2l(with, Opts);
			false -> 
			    x_set(with, Opts, "value")
		    end;
		false ->
		    Opts
	    end,
    Callback = remote_function(Opts2),
    Frequency = x_get(frequency, Opts2, "10"),
    "new "++a2l(Klass)++"('"++a2l(Name)++"', "++
	Frequency++", function(element, value) {"++
	Callback++"})".


%%% Take a list [{complete, "undoRequestCompleted(request)"},...]
%%% and return it a bit massaged...
build_callbacks(Opts) ->
    Callbacks = callbacks(),
    lists:foldl(fun({Callback,Code} = E, Acc) ->
			case member(Callback, Callbacks) of
			    {true, Code} -> 
				[{"on"++capitalize(Callback), 
				  "function(request){"++a2l(Code)++"}"}|Acc];
			    false ->
				[E|Acc]
			end
		end, [], Opts).


%%%
%%% The callbacks that may be specified are:
%%%
%%% loading              Called when the remote document is being 
%%%                        loaded with data by the browser.
%%% loaded               Called when the browser has finished loading
%%%                        the remote document.
%%% interactive          Called when the user can interact with the 
%%%                        remote document, even though it has not 
%%%                        finished loading.
%%% complete             Called when the XMLHttpRequest is complete.
%%%
callbacks() -> 
    [uninitialized, loading, loaded, interactive, complete].


%%% "helLo_world" => "Hello_world"
capitalize([H|T]) when H>=$a,H=<$z -> [H-32|T];
capitalize(Str)                    -> Str.


%%% "helLo_world" => "helloWorld" ?
camelize(Str) ->
    capitalize(Str).  % FIXME


a2l(A) when atom(A) -> atom_to_list(A);
a2l(L) when list(L) -> L.

i2l(I) when integer(I) -> integer_to_list(I);
i2l(L) when list(L)    -> L.


