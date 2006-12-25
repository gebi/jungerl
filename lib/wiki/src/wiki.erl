-module(wiki).

%% Copyright (C) 2000, Bluetail AB
%% File    : wiki.erl
%% Author  : Joe Armstrong (joe@bluetail.com)
%% Purpose : Wiki web in Erlang

%% History: First started by Luke Gorrie who wrote the first
%%          Erlang wiki. Subsequently re-written many times by Joe Armstrong
%%          and Luke Gorrie.
%%          This version by Joe Armstrong.
%%          Thanks to Luke and Robert Virding for many helpful
%%          discussions clarifying the design.
%%          This also makes use of the new pico_http_server which has
%%          a much simplified interface.

%% Note that *all* commands start with /wiki/ this is so I can proxy 
%% through Apache 
%%  
%% Add the following to
%%  /etc/httpd/conf/httpd.conf
%%      RewriteEngine On
%%      RewriteRule ^/wiki/(.*)$ http://127.0.0.1:5999/wiki/$1 [L,P]
%%  Then things directed to locallost:80/wiki 
%%  will be redirected to   localhost:5999/wiki
%%  Thanks to Petru Paler <ppetru@ppetru.net> for this tip

      
%% -compile(export_all).

-export([start_handler/1, event_handler/2, stop_handler/2]).

-import(pico_utils, [h1/1, body/1, classify/1, header/1, show/1, 
		     str2urlencoded/1, urlencoded2str/1]).

-import(lists, [map/2,member/2, reverse/1, sort/1, flatten/1]).

-import(wiki_templates, [template/4]).

-export([start/1, stop/1, start_handler/1, event_handler/2]).

-export([ls/0, root/0, read_page/1, p/1, background/1,set_status/3]).


%% root() hardcodes the location of the depositry
%% you might need to change this

root() -> os:getenv("WIKI_STORE").


%% You should not need to edit anything below the line
%%______________________________________________________________________

%% batch is for starting from a script
start([APort,Root]) ->
    io:format("Start:~p~n",[{APort,Root}]),
    PortStr = atom_to_list(APort),
    os:putenv("WIKI_PORT", PortStr),
    os:putenv("WIKI_STORE", atom_to_list(Root)),
    Port = list_to_integer(PortStr),
    pico_http_server:start(Port, 20, ?MODULE, []).

stop([Node]) ->
    io:format("Stop:~p~n",[Node]),
    case net:ping(Node) of
	pong -> ok;
	pang ->
	    io:format("There is no node with this name~n")
    end,
    rpc:cast(Node, init, stop, []),
    init:stop().
    
%% start_handler/1 and event_handler/2 are callbacks required by the 
%% pico server.

start_handler(_Args) -> State = 1.

%% event_handler(Request, State) -> {HTML, State'}

event_handler({_,From,"/",[]}, State) ->
    event_handler({a,From,"/wiki/showPage",[{"node","home"}]}, State); 
event_handler({_,From,"/wiki/",[]}, State) ->
    event_handler({post,From,"/wiki/showPage",[{"node","home"}]}, State); 
event_handler({_,From,"/wiki/showPage",Args}, State) ->
    {showPage(Args), State+1};
event_handler({_,From,"/wiki/showOldPage",Args}, State) ->
    {showOldPage(Args), State+1};
event_handler({_,From,"/wiki/editPage",Args}, State) ->
    {editPage(Args, From), State+1};
event_handler({_,From,"/wiki/editTag",Args}, State) ->
    {editTag(Args, From), State+1};
event_handler({_,From,"/wiki/storePage",Args}, State) ->
    {storePage(Args, From), State+1};
event_handler({_,From,"/wiki/storeTagged",Args}, State) ->
    {storeTagged(Args, From), State+1};
event_handler({_,From,"/wiki/storeNewPage",Args}, State) ->
    {storeNewPage(Args, From), State+1};
event_handler({_,From,"/wiki/showHistory",Args}, State) ->
    {showHistory(Args, From), State+1};
event_handler({_,From,"/wiki/previewPage",Args}, State) ->
    {previewPage(Args, From), State+1};
event_handler({_,From,"/wiki/previewTagged",Args}, State) ->
    {previewTagged(Args, From), State+1};
event_handler({_,From,"/wiki/previewNewPage",Args}, State) ->
    {previewNewPage(Args, From), State+1};
event_handler({_,From,"/wiki/createNewPage",Args}, State) ->
    {createNewPage(Args, From), State+1};
event_handler({_,From,"/wiki/sendMeThePassword",Args}, State) ->
    {sendMeThePassword(Args, From), State+1};
event_handler({_,From,"/wiki/allPages",Args}, State) ->
    {allPages(), State+1};
event_handler({_,From,"/wiki/lastEdited",Args}, State) ->
    {lastEdited(), State+1};
event_handler({_,From,"/wiki/wikiZombies",Args}, State) ->
    {wikiZombies(), State+1};
event_handler({_,From,"/wiki/allRefsToMe",Args}, State) ->
    {allRefsToMe(Args), State+1};
event_handler({_,_,"/wiki/image/" ++ File,Args}, State) ->
    {get_file(File, Args), State+1};
event_handler(Event, State) -> 
    io:format("Invalid request:~p~n",[Event]),
    {header({error,"400 Bad Request",show({invalid_request,Event})}), State+1}.

%% stop_handler(Reason, State) -> State'

stop_handler(stop, State) ->
    io:format("stop signal received~n"),
    init:stop();
stop_handler(Reason, State) ->
    io:format("Stopping:~p ~p~n", [Reason, State]),
    {stopoyyes, State}.

%% These are all called in response to POST requests

%% This is the fall through case when the user just requests a file
%% There is a slight security hole here. The user can request any file
%% through this routine.

get_file(F,Args) ->
    case legal(F) of
	true ->
	    Full = root() ++ "/" ++ F,
	    %% io:format("Get_file: ~p ~p~n",[Full,Args]),
	    case file:read_file(Full) of
		{ok, Bin} ->
		    [header({ok,classify(F)}), Bin];
		_ ->
		    header({error,"404 Not Found",show({no_such_file,F})})
	    end;
	false ->
	    header({error,"400 Bad Request",show({illegal_request,F})})
    end.

%% check that we are not trying to access a file
%% where we should not !

legal(".." ++ _) -> false;
legal([_|T])     -> legal(T);
legal([])        -> true.

notfound(Page) ->
	header({error,"404 Not Found",show({no_such_page,Page})}).

storePage([_,{"node",Page},{"password", Password},{"txt", Txt0}], From) ->
    Txt = zap_cr(urlencoded2str(Txt0)),
    %% Check the password
    File = page2filename(Page),
    case file:read_file(File) of
	{ok, Bin} ->
	    Wik = {wik001,Pwd,_Email,_Time,_Who,_OldTxt,_Patches} = 
		binary_to_term(Bin),
	    case Pwd of 
		"" ->
		    store_ok(Page, From, Txt, Wik);
		Password ->
		    store_ok(Page, From, Txt, Wik);
		_ ->
		    header({error,"403 Forbidden",show({invalid_password, shouldbe, Pwd, was, Password})})
	    end;
	_ ->
	    notfound(Page)
   end.

storeNewPage([_,{"node",Page},{"password", Password},
	      {"email", Email0}, {"txt", Txt0}], From) ->
    Txt = zap_cr(urlencoded2str(Txt0)),
    Email = urlencoded2str(Email0),
    %% Check the password
    File = page2filename(Page),
    Time = {date(),time()},
    Who = ident(From),
    B = term_to_binary({wik001,Password,Email,Time,Who,Txt,[]}),
    file:write_file(File, B),
    showPage([{"node", Page}]).

storeTagged([_,{"node",Page},{"tag", Tag},{"txt", Txt0}], From) ->
    Txt = zap_cr(urlencoded2str(Txt0)),
    File = page2filename(Page),
    case file:read_file(File) of
	{ok, Bin} ->
	    Wik = {wik001,_Pwd,_Email,_Time,_Who,OldTxt,_Patches} = 
		binary_to_term(Bin),
	    W    = wiki_split:str2wiki(OldTxt),
	    ITag = list_to_integer(Tag),
	    {Type, Old} = wiki_split:getRegion(ITag, W),
	    W2 = case Type of
		     open ->
			 wiki_split:putRegion(ITag, W, Txt);
		     write_append ->
			 Time = format_time({date(), time()}),
			 wiki_split:putRegion(ITag, W, 
					      "''" ++ Time ++ "''\n\n" ++
					      Txt ++ "\n\n____\n" ++ Old)
		   end,
	    Str2 = wiki_split:wiki2str(W2),
	    store_ok(Page, From, Str2, Wik);
	_ ->
	    notfound(Page)
   end.

store_ok(Page, From, OldTxt,{wik001,Pwd,Email,Time,Who,OldTxt,Patches}) ->
    showPage([{"node", Page}]);
store_ok(Page, From, NewTxt,{wik001,Pwd,Email,_Time,_Who,OldTxt,Patches}) ->
    Patch = diff:diff(NewTxt, OldTxt),
    Time = {date(), time()},
    Who = ident(from),
    Patches1 = [{Patch,Time,Who}|Patches],
    Ds = {wik001,Pwd, Email,Time,Who,NewTxt,Patches1},
    B = term_to_binary(Ds),
    File = page2filename(Page),
    file:write_file(File, B),
    showPage([{"node", Page}]).

ident(From) ->
    %% io:format("Ident=~p~n",[From]),
    "joe".

showHistory([{"node", Page}], _) ->
    File = page2filename(Page),
    case file:read_file(File) of
	{ok, Bin} ->
	    {wik001,Pwd,Email,_Time,_Who,OldTxt,Patches} = 
		binary_to_term(Bin),
	    Links = reverse(mk_history_links(reverse(Patches), Page, 1)),
	    template("History",background("info"), "", Links);
	_ ->
	    notfound(Page)
    end.

mk_history_links([{C,Time,Who}|T], Page, N) ->
    [["<li>",i2s(N)," modified on <a href='/wiki/showOldPage?node=",Page,
      "&index=",i2s(N),
     "'>",format_time(Time),"</a> size ", i2s(size(C)), " bytes",
     "\n"]|mk_history_links(T, Page, N+1)];
mk_history_links([], _, _) ->
    [].

format_time({{Year,Month,Day},{Hour,Min,Sec}}) ->
    [i2s(Year),"-",i2s(Month),"-",i2s(Day)," ",
     i2s(Hour),":",i2s(Min),":",i2s(Sec)].

createNewPage([{"node",Page}], From) ->
    template("New Page",bgcolor("white"),"",
	 [h1(Page),
	  p("Creating a new page. "
	    "If you want a password protected page "
	    "then fill in both the password fields - otherwise "
	    "leave them blank."),
	  p("If you fill in the email field and forget the page password "
	    "then the system can mail you back the password of the page if "
	    "you forget it."),
	  p("Click on 'Preview' when you are ready to store the page."),
	  form("POST", "/wiki/previewNewPage",
	       [input("submit", "review", "Preview"),
		input("hidden", "node", Page),
		"Password1:",
		password_entry("password1", 8),
		"Password2:",
		password_entry("password2", 8),p(),
		"Email:",
		input("text","email",""), 
		p(),
		textarea("text", 25, 72,initial_page_content())])]).

initial_page_content() -> "\nEnter your text here\n".

showPage([{"node",Page}]) ->
    File = page2filename(Page),
    %% io:format("Reading:~p~n",[Page]),
    case file:read_file(File) of
	{ok, Bin} ->
	    {wik001, Pwd,_Email,_Time,_Who,TxtStr,_Patches} = 
		binary_to_term(Bin),
	    Wik = wiki_split:str2wiki(TxtStr),
	    DeepStr = wiki_to_html:format_wiki(Page, Wik, root()),
	    template(Page, body_pic(Pwd),banner(Page, Pwd),
		     [top_header(Page),DeepStr,footer(Page,Pwd)]);
	_ ->
	    header({error,"404 Not Found",show({no_such_page, Page, fullname, File})})
    end.

top_header(Page) ->
    F1 = add_blanks_nicely(Page),
    ["<h1><a href='/wiki/allRefsToMe?node=",Page,"'>",F1,"</a></h1>\n"].

add_blanks_nicely([H1,H2|T]) ->
    case {little_letter(H1),
	 big_letter(H2)} of
	{true,true} ->
	    [H1,$ ,H2|add_blanks_nicely(T)];
	_ ->
	    [H1|add_blanks_nicely([H2|T])]
    end;
add_blanks_nicely([H|T]) ->
    [H|add_blanks_nicely(T)];
add_blanks_nicely([]) ->
    [].

midpanel(Data) ->
    Space = "<td width=50><pre>&nbsp;</pre></td>\n",
    ["<table>\n<tr>\n",Space,"<td>\n",Data,"</td>\n",
     Space,
     "</tr>\n</table>\n"].

allPages() ->
    Files = sort(find:files(Root=root(), "*.wob", false)),
    template("All Pages", background("info"), "",
	     [h1("All Pages"),
	      p("This is a list of all pages known to the system."),
	      map(fun(I) ->
			  F = filename:basename(I, ".wob"),
			  [wiki_to_html:format_link(F, Root),"<br>"] end, 
		  Files)]).

lastEdited() ->
    Files = sort(find:files(root(), "*.wob", false)),
    S = flatten(map(fun(I) ->
				 "~" ++ filename:basename(I, ".wob") ++"\n\n"
			 end, Files)),
    V = reverse(sort(map(fun(I) -> {last_edited_time(I), I} end, Files))),
    Groups = group_by_day(V),
    Root = root(),
    S1 = map(fun({{Year,Month,Day},Fx}) ->
		     [p(),i2s(Year),"-",i2s(Month),"-",i2s(Day),"<p>",
		      "<ul>",
		      map(fun(F) -> 
				  F1 = filename:basename(F, ".wob"),
				  J = wiki_to_html:format_link(F1, Root),
				  [J,"<br>"] end, Fx),
		      "</ul>"]
	     end, Groups),
    template("Last Edited", background("info"), "", 
	     [h1("Last Edited"),
	      p("These are the last edited files."),S1]).

group_by_day([]) ->
    [];
group_by_day([{{Day,Time}, File}|T]) ->
    {Stuff, T1} = collect_this_day(Day, T, [File]),
    T2 = group_by_day(T1),
    [Stuff|T2].

collect_this_day(Day, [{{Day,Time},File}|T], L) ->
    collect_this_day(Day, T, [File|L]);
collect_this_day(Day, T, L) ->
    {{Day,reverse(L)}, T}.

last_edited_time(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    {wik001,Pwd,_Email,Time,_Who,_Txt,_Patches} = 
		binary_to_term(Bin),
	    Time;
	_ ->
	    error
    end.
	
showOldPage([{"node",Page},{"index", Nt}]) ->
    Index = list_to_integer(Nt),
    File = page2filename(Page),
    case file:read_file(File) of
	{ok, Bin} ->
	    Wik = {wik001,Pwd,_Email,_Time,_Who,Txt,Patches} = 
		binary_to_term(Bin),
	    %% N = #patches to do
	    N = length(Patches) - Index + 1,
	    ThePatches = take(N, Patches),
	    TxtStr = diff:patchL(Txt, ThePatches),
	    W = wiki_split:str2wiki(TxtStr),
	    DeepStr = wiki_to_html:format_wiki(Page, W, root()),
	    Form = form("POST", "/wiki/noop",
			[textarea("text", 25, 75, TxtStr)]),
	    template(Page, old_pic(), "",
		     [h1(Page),DeepStr,"<hr>",Form]);
	_ ->
	    notfound(Page)
    end.

take(0, _) -> [];
take(N, [{P,_,_}|T]) -> [P|take(N-1, T)].

old_pic() -> background("old").

body_pic("") -> background("normal");
body_pic(_)  -> background("locked").

banner(File, Password) ->			    
    [table("#FFFFFF",
	   [
	    mk_image_link("/wiki/showPage?node=home", "home.gif"),
	    mk_image_link("/wiki/showHistory?node=" ++ File, "history.gif"),
	    mk_image_link("/wiki/allPages", "allpages.gif"),
	    mk_image_link("/wiki/lastEdited", "lastedited.gif"),
	    mk_image_link("/wiki/wikiZombies", "zombies.gif"),
	    banner_edit_link(File, Password)])].

footer(File, "") ->
    "";
footer(File, _) ->
    [p(),hr(),
     p("This page is password protected. To edit the page enter "
       "the password and click on edit."),
     form("POST", "/wiki/editPage",
	  [input("hidden", "node",File),
	   "Password:",
	   password_entry("password", 8),
	   input("submit", "submit", "Edit")])].

banner_edit_link(File, "") ->
    mk_image_link("/wiki/editPage?node=" ++ File, "editme.gif");
banner_edit_link(File, _) ->
    "".

mk_link(X, Y) ->
    ["<a href=\"", X, "\">", Y, "</a>&nbsp;&nbsp;\n"].

mk_image_link(X, Img) ->
    ["<a href=\"", X, "\"><img border=0 src='/wiki/image/",Img,
     "'></a>&nbsp;&nbsp;\n"].

editPage([{"node",N},{"password",P},_], From) ->
    editPage(N, P, From);
editPage([{"node",N}], From) ->
    editPage(N, "", From).

editPage(Page, Password, From) ->
    File = page2filename(Page),
    case file:read_file(File) of
	{ok, Bin} ->
	    {wik001, Pwd,_Email,_Time,_Who,TxtStr,_Patches} =
		binary_to_term(Bin),
	    case Pwd of 
		"" ->
		    edit1(Page, Password, TxtStr);
		Password ->
		    edit1(Page, Password, TxtStr);
		_ ->
		    template("Error", bgcolor("white"), "",
			 [
			  h1("Incorrect password"),
			  p("You have supplied an incorrect password"),
			  p("To find out the the password fill in your "
			    "email address and click on <i>Show password</i>. "
			    " If you are "
			    "the registered "
			    "owner of this page then I will tell you "
			    "the password."),
			  form("POST", "/wiki/sendMeThePassword",
			       [input("hidden", "node", Page),
				"email address:",
				input("text", "email", ""),
				input("submit", "send", "Show password")])
			 ])
		end;
	_ ->
	    notfound(Page)
    end.

edit1(Page, Password, Content) ->
    Txt = quote_lt(Content),
    template("Edit", background("info"), "",
	 [h1(Page),
	  p("Edit this page - when you have finished hit the 'Preview' "
	    "button to check your results."),
	  form("POST", "/wiki/previewPage",
	       [input("submit", "review", "preview"),
		input("hidden", "node", Page),
		input("hidden", "password", Password),
		p(),
		textarea("text", 25, 75, Txt),
		p(),
		hr()])]).

sendMeThePassword([{"node",Page},{"email",Email}|_], From) ->
    File = page2filename(Page),
    case file:read_file(File) of
	{ok, Bin} ->
	    Wik = {wik001,Pwd,EmailOwner,_Time,_Who,_OldTxt,_Patches} = 
		binary_to_term(Bin),
	    %% io:format("Here Email=~p EMailOwner=~p~n",[Email,EmailOwner]),
	    case Email of
		"" ->
		    template("Error", bgcolor("pink"), "",
			 [h1("Failure"),
			  p("This page has no associated email address")]);
		EmailOwner ->
		    mail(Page, Email, Pwd),
		    template("Ok",bgcolor("white"),"",
			 [h1("Success"),
			  p("The password has been mailed to "),
			  Email,
			  p("Have a nice day")]);
		Other ->
		    template("Error",bgcolor("pink"),"",
			 [h1("Failure"),
			  p("Incorrect email address")])
	    end;
	_ ->
	    notfound(Page)
    end.

editTag([{"node",Page},{"tag",Tag}], From) ->
    File = page2filename(Page),
    case file:read_file(File) of
	{ok, Bin} ->
	    {wik001,Pwd,_Email,_Time,_Who,OldTxt,_Patches} = 
		binary_to_term(Bin),
	    Wik = wiki_split:str2wiki(OldTxt),
	    {Type, Str} = wiki_split:getRegion(list_to_integer(Tag), Wik),
	    Str1 = case Type of 
		       open -> quote_lt(Str);
		       write_append -> ""
		   end,
	    template("Edit", bgcolor("white"),"",
		     [h1(Page),
		      p("Edit this page - when you have finished hit the "
			"'Preview' button to check your results."),
		      form("POST", "/wiki/previewTagged",
			   [input("submit", "review", "preview"),
			    input("hidden", "node", Page),
			    input("hidden", "tag", Tag),
			    p(),
			    textarea("text", 25, 75, Str1),
			    p(),
			    hr()])]);
	Error ->
	    notfound(Page)
    end.

quote_lt("&lt;" ++ T) ->
    "&amp;lt;" ++ quote_lt(T);
quote_lt([H|T]) -> [H|quote_lt(T)];
quote_lt([]) -> [].

p() -> "<p>".

p(X) -> ["<p>", X, "</p>\n"].

hr() -> ["<hr>\n"].

i2s(I) -> integer_to_list(I).

table(Color, X) ->
    ["<table width=\"100%\"><tr><td bgcolor=\"", Color, "\">\n",
     X,"</td></tr></table>\n"].

password_entry(Name, Size) ->
    ["<INPUT TYPE=password name=", Name," SIZE=", i2s(Size),">\n"].

form(Method, Action, Args) ->
    ["<FORM METHOD=", Method," ACTION=\"", Action, "\">",
     Args, "</form>\n"].

input(Type, Name, Value) ->
    ["<INPUT TYPE=",Type," Name=",Name," Value=\"", Value, "\">\n"].

textarea(Name, Row, Cols, Txt) ->
     ["<textarea name=", Name, " rows=", i2s(Row),
      " cols=", i2s(Cols), " wrap=virtual>",
      Txt, "</textarea>\n"].

previewPage([_,{"node",Page},{"password",Password},{"text",Txt0}], From) ->
    Txt = zap_cr(Txt0),
    Wik = wiki_split:str2wiki(Txt),
    template("Preview",background("info"),"",
	     [h1(Page),
	      p("If this page is ok hit the \"Store\" button "
		"otherwise return to the editing phase by clicking the back "
		"button in your browser."),
	      form("POST", "/wiki/storePage",
		   [input("submit", "store", "Store"),
		    input("hidden", "node", Page),
		    input("hidden", "password", Password),
		    input("hidden", "txt", str2urlencoded(Txt))]),
	      p(),hr(),h1(Page), 
	      wiki_to_html:format_wiki(Page, Wik, root())]).


%% Preview Tagged
%% Tagged stuff is inside comment and append regions
%% We *dont* want any structure here

previewTagged([_,{"node",Page},{"tag",Tag},{"text", Txt0}], From) ->
    Txt = zap_cr(Txt0),
    %% we want this stuff to *only* be txt
    %% io:format("Here previewTagged:~p~n",[Txt]),
    case legal_flat_text(Txt) of
	true ->
	    template("Preview",bgcolor("white"),"",
		     [p("If this region is ok hit the <i>Store</i> button "
			"otherwise return to the editing phase by clicking "
			"the back button in your browser."),
		      form("POST", "/wiki/storeTagged",
			   [input("submit", "store", "Store"),
			    input("hidden", "node", Page),
			    input("hidden", "tag", Tag),
			    input("hidden", "txt", str2urlencoded(Txt))]),
		      p(),hr(), 
		      wiki_to_html:format_wiki(Page,{txt,10000,Txt},root())]);
	false ->
	    header({error,"400 Bad Request",show({text_contains,'< or >', in_col_1_which_is_illegal})})
    end.

%% Flat text is *not* allowed to contain <

legal_flat_text("<" ++ _) -> false;
legal_flat_text(X)        -> legal_flat_text1(X).
    
legal_flat_text1("\n<" ++ _) -> false;
legal_flat_text1("\n>" ++ _) -> false;
legal_flat_text1([_|T])      -> legal_flat_text1(T);
legal_flat_text1([])         -> true.
    
previewNewPage([_,{"node", Page},
		{"password1",Password},
		{"password2",Password},
		{"email", Email},
		{"text", Txt0}], From) ->
    Txt = zap_cr(Txt0),
    Wik = wiki_split:str2wiki(Txt),
    template("Preview", bgcolor("white"),"",
	     [p("If this page is ok hit the \"Store\" button otherwise return "
		"to the editing phase by clicking the back button in your "
		"browser."),
	      form("POST", "/wiki/storeNewPage",
		   [input("submit", "store", "Store"),
		    input("hidden", "node", Page),
		    input("hidden", "password", Password),
		    input("hidden", "email", str2urlencoded(Email)),
		    input("hidden", "txt", str2urlencoded(Txt))]),
	      wiki_to_html:format_wiki(Page, Wik, root())]);
previewNewPage([_,{"node",Page},{"password1",P1},{"password2",P2}|_],
	       From) ->
    header({error,"403 Forbidden",show({passwords_differ,P1,P2})}).

zap_cr([$\r,$\n|T]) -> [$\n|zap_cr(T)];
zap_cr([H|T])       -> [H|zap_cr(T)];
zap_cr([])          -> [].

wikiZombies() ->
    wiki_utils:zombies().

allRefsToMe([{"node",Page}]) ->
    wiki_utils:findallrefsto(Page).

page2filename(Page) ->
    root() ++ "/" ++ Page ++ ".wob".

mail(Page, Email, Pwd) ->
    send(Email,"Wiki password",
	 "The password of " ++ Page ++ " is " ++ Pwd ++ "\n").

send(To, Subject, Data) ->
    {TmpFile, S} = open_tmp_file("/tmp", ".mail"),
    io:format(S, "To: <~s>~n", [To]),
    io:format(S, "Subject: ~s~n~n", [Subject]),
    io:format(S, "~s~n",[Data]),
    io:format(S, ".~nquit~n", []),
    file:close(S),
    io:format("sending ...~n", []),
    unix:cmd("sendmail -t > /dev/null < " ++ TmpFile),
    file:delete(TmpFile).
                               
open_tmp_file(RootName, Suffix) ->
    open_tmp_file(10, RootName, Suffix).
 
open_tmp_file(0, _, Suffix) ->
    exit({cannot_open_a_temporary_file, Suffix});
open_tmp_file(N, RootName, Suffix) ->
    {_,_,M} = erlang:now(),
    FileName = RootName ++ "/" ++ integer_to_list(M) ++ Suffix,
    %% io:format("trying to open:~p~n", [FileName]),
    case file:open(FileName, write) of
        {ok, Stream} ->
            {FileName, Stream};
        {error, _} ->
            open_tmp_file(N-1, RootName, Suffix)
    end.      

%% set_status sets the password and email address for a wiki page
set_status(Page, Password, Email) ->
    File = page2filename(Page),
    case file:read_file(File) of
	{ok, B1} ->
	    {wik001,_Pwd,_Email,Time,Who,OldTxt,Patches} = 
		binary_to_term(B1),
	    B2 = term_to_binary(
		   {wik001,Password,Email,Time,Who,OldTxt,Patches}),
	    file:write_file(File, B2);
	E -> E
    end.

ls() ->
    Files = find:files(root(), "*.wob", false),
    map(fun(I) -> filename:basename(I, ".wob") end, Files).

read_page(Page) ->
    File = page2filename(Page),
    %% io:format("Reading:~p~n",[Page]),
    case file:read_file(File) of
	{ok, Bin} ->
	    {wik001,_Pwd,_Email,_Time,_Who,OldTxt,_Patches} = 
		binary_to_term(Bin),
	    {ok, OldTxt};
	_ ->
	    error
    end.
    
big_letter(H) when $A =< H, H =< $Z -> true;
big_letter($Å) -> true;
big_letter($Ä) -> true;
big_letter($Ö) -> true;
big_letter(_)  -> false.
    
little_letter(H) when $a =< H, H =< $z -> true;
little_letter($å) -> true;
little_letter($ä) -> true;
little_letter($ö) -> true;
little_letter(_)  -> false.
    
bgcolor(C) ->
    ["<body bgcolor='", C, "'>\n"].

background(F) ->
    ["<body background='/wiki/image/", F, ".gif'>\n"].
