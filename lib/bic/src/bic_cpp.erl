%%% File    : bic_cpp.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : CPP 
%%% Created :  3 Jan 2006 by Tony Rogvall <tony@iMac.local>

-module(bic_cpp).

-compile(export_all).

-export([init/3,open/1,close/1,read/1]).
-export([file/1]).
-export([cline/1, cfile/1, cvalue/2]).

-import(lists, [foreach/2, reverse/1, map/2, foldl/3]).

-include("bic.hrl").

-define(debug, true).

-ifdef(debug).
-define(dbg(F,A), io:format((F),(A))).
-else.
-define(dbg(F,A), ok).
-endif.

-record(save,
	{
	  fd,        %% current fd
	  file,      %% real file name
	  line,      %% real line number
	  if_stack,  %% current if stack
	  pwd,       %% current working directory
	  buf,       %% stored data from file
	  defs       %% "some" saved defs
	 }).

-record(s,
	{
	  fd,           %% current fd
	  file,         %% real file name
	  line,         %% real line number
	  if_stack=[],  %% {Tag,0|1|2,Ln}
	  inc_stack=[], %% #save {}
	  pwd,          %% current working directory
	  include=[],   %% search path for #include <foo.h> 
	  qinclude=[],  %% search path for #include "foo.h"
	  preds,        %% dict of assertions {Pred,Answer}
	  defs          %% dict of defines {Name,Value}
	 }).

-ifdef(debug).
-define(CHUNK_SIZE, 1). 
-else.
-define(CHUNK_SIZE, 1024).
-endif.

%% test to cpp a file and print it
file(File) ->
    case open(File) of
	{ok,PFd} ->
	    fd(PFd),
	    close(PFd),
	    ok;
	{error,Err} ->
	    {error,Err}
    end.

fd(PFd) ->
    case read(PFd) of
	eof ->
	    ok;
	Line ->
	    io:format("~s", [Line]),
	    fd(PFd)
    end.

open(File) ->
    open(File, []).

open(File, Opts) ->
    Pid = spawn_link(?MODULE, init, [self(),File,Opts]),
    Mon = mon(Pid),
    receive
	{'DOWN',Mon,_,_,Reason} ->
	    {error, Reason};
	{Pid,Result} ->
	    demon(Mon),
	    Result
    end.

close(PFd) when is_pid(PFd) ->
    call(PFd, close).

%% return Line of characters | eof
read(PFd) when is_pid(PFd) ->
    call(PFd, read).

cline(PFd) when is_pid(PFd) ->
    cvalue(PFd, "__LINE__").

cfile(PFd) when is_pid(PFd) ->
    cvalue(PFd,"__FILE__").

cvalue(PFd, Var) ->
    call(PFd, {value, Var}).

call(PFd, Request) ->
    Ref = mon(PFd),
    PFd ! {call,self(),Ref,Request},
    receive
	{'DOWN',Ref,_,_,Reason} ->
	    {error, Reason};
	{Ref, Reply} ->
	    demon(Ref),
	    Reply
    end.

mon(Pid) ->
    erlang:monitor(process, Pid).

demon(Ref) ->
    Res = erlang:demonitor(Ref),
    receive
	{'DOWN',Ref,_,_,_} ->
	    Res
    after 0 ->
	    Res
    end.
%%
%% Initial processing
%%
init(Pid,File,Opts) ->
    ?dbg("OPEN: ~s\n", [File]),
    case file:open(File, [read]) of
	{ok,Fd} ->
	    Pid ! {self(),{ok,self()}},
	    init_loop(Pid, Fd, File, Opts);
	Error ->
	    Pid ! {self(), Error}
    end.

init_loop(Pid, Fd, File, UserOpts) ->
    Defs = init_defs(default_opts(File)++UserOpts),
    Preds = init_preds(UserOpts),
    Include = init_include(UserOpts,["/usr/local/include", "/usr/include"]),
    QInclude = init_qinclude(UserOpts,[]),
    S0 = #s { file = File,
	      fd = Fd,
	      line = 1,
	      pwd = ".",
	      include = Include,
	      qinclude = QInclude,
	      defs = Defs,
	      preds = Preds
	     },
    loop(Pid, S0, [], [auto_line(1,File)]).


init_include([{include,Path} | Opts], Paths) when list(Path) ->
    init_include(Opts, Paths ++ [Path]);
init_include([{include,Path} | Opts], Paths) when atom(Path) ->
    init_include(Opts, Paths ++ [atom_to_list(Path)]);
init_include([_|Opts], Paths) ->
    init_include(Opts, Paths);
init_include([], Paths) ->
    Paths.

init_qinclude([{qinclude,Path} | Opts], Paths) when list(Path) ->
    init_qinclude(Opts, Paths ++ [Path]);
init_qinclude([{qinclude,Path} | Opts], Paths) when atom(Path) ->
    init_qinclude(Opts, Paths ++ [atom_to_list(Path)]);
init_qinclude([_|Opts], Paths) ->
    init_qinclude(Opts, Paths);
init_qinclude([], Paths) ->
    Paths.

init_defs(Opts) ->
    foldl(fun({define,N,V},D) ->
		  dict:store(N, V, D);
	     (_, D) ->
		  D
	  end, dict:new(), Opts).

init_preds(Opts) ->
    foldl(fun({assert,P,V},D) ->
		  dict:store({P,V},true,D);
	     (_,D) ->
		  D
	  end, dict:new(), Opts).


loop(Pid, S, Cs, LBuf) ->
    receive
	{call,From,Ref,read} ->
	    case LBuf of
		[] ->
		    case iread(S,Cs) of
			{S1,{[],0,eof}} ->
			    From ! {Ref, eof},
			    loop(Pid,S1,[],LBuf);
			{S1,{[],0,Err={error,_}}} ->
			    From ! {Ref, Err},
			    loop(Pid,S1,[],LBuf);
			{S1,{Line,NL,Cs1}} ->
			    From ! {Ref, Line},
			    loop(Pid,S1,Cs1,LBuf)
		    end;
		[Line|LBuf1] ->
		    From ! {Ref, Line},
		    loop(Pid, S, Cs, LBuf1)
	    end;
	{call,From,Ref,close} ->
	    %% close all files in save state and the main one
	    foreach(fun(Save) ->
			    ?dbg("CLOSE: ~s\n", [Save#save.file]),
			    file:close(Save#save.fd)
		    end, S#s.inc_stack),
	    ?dbg("CLOSE: ~s\n", [S#s.file]),
	    file:close(S#s.fd),
	    From ! {Ref, ok},
	    ok;
	{call,From,Ref,{value,Var}} ->
	    From ! {Ref, value(S,Var)},
	    loop(Pid,S,Cs,LBuf)
    end.

default_opts(File) ->
    {Dy,Dm,Dd} = date(),
    {Th,Tm,Ts} = time(),
    Mon = case Dm of
	      1->"Jan"; 2->"Feb"; 3->"Mar"; 4->"Apr"; 5->"May"; 6->"Jun";
	      7->"Jul"; 8->"Aug"; 9->"Sep"; 10->"Oct"; 11->"Nov"; 12->"Dec"
	  end,
    Time=lists:flatten(io_lib:format("~2..0w:~2..0w:~2..0w",[Th,Tm,Ts])),
    Date=lists:flatten(io_lib:format("~s ~2w ~4w",[Mon,Dd,Dy])),
    [{define,"__FILE__", File},        %% may be #undef
     {define,"__LINE__", 1},           %% may be #undef
     {define,"__INCLUDE_LEVEL__",0},   %% read only can not be #undef
     {define,"__DATE__", Date},
     {define,"__TIME__",Time}
    ].

%%
%% Read a continued line
%% Take care of \ and /*comments */ and // comments
%%

-define(FALSE, 0).
-define(TRUE,  1).
-define(SKIP,  2).

iread(S,Cs) ->
    iread(S,0,Cs).

iread(S0,NL,Cs) ->
    {S,Read} = iread(S0,Cs,false,NL,[]),
%%    ?dbg("Read: ~p\n", [Read]),
    case Read of
	{{D,Bool},NL1,Cs1} when D=='#if'; D=='#ifdef'; D=='#ifndef'-> 
	    case peek_if(S) of
		{Tag,V,Ln} ->
		    if V == ?FALSE ->
			    S1 = push_if(S, D,?SKIP,S#s.line),
			    iread(S1,NL1,Cs1);
		       V == ?TRUE ->
			    S1 = push_if(S, D,Bool,S#s.line),
			    iread(S1,NL1,Cs1);
		       V == ?SKIP ->
			    S1 = push_if(S,D,?SKIP,S#s.line),
			    iread(S1,NL1,Cs1)
		    end;
		empty ->
		    S1 = push_if(S,D,Bool,S#s.line),
		    iread(S1,NL1,Cs1)
	    end;

	{{'#elif',?TRUE},NL1,Cs1} ->
	    case peek_if(S) of
		{Tag,V,Ln} ->
		    if Tag == '#ifdef'; Tag == '#ifndef'; Tag=='#else' ->
			    error(S, "#elif missing #if", []),
			    iread(S,NL1,Cs);
		       V == ?SKIP ->
			    iread(S,NL1,Cs1);
		       V == ?TRUE -> %% skip,alread processed
			    S1 = mod_if(S,Tag,?SKIP,Ln),
			    iread(S1,NL1,Cs1);
		       V == ?FALSE -> %% take this clause
			    S1 = mod_if(S, '#elif',?TRUE,S#s.line),
			    iread(S1,NL1,Cs1)
		    end;
		empty ->
		    error(S, "#elif missing #if", []),
		    iread(S,NL1,Cs1)
	    end;

	{{'#elif',?FALSE},NL1,Cs1} ->
	    case peek_if(S) of
		{Tag,V,Ln} ->
		    if Tag == '#ifdef'; Tag == '#ifndef'; Tag=='#else' ->
			    error(S, "#elif missing #if", []),
			    iread(S,NL1,Cs);
		       V == ?SKIP ->
			    iread(S,NL1,Cs1);
		       V == ?TRUE -> %% skip,alread processed
			    S1 = mod_if(S,Tag,?SKIP,Ln),
			    iread(S1,NL1,Cs1);
		       V == ?FALSE -> %% ignore this clause
			    S1 = mod_if(S,'#elif',?FALSE,S#s.line),
			    iread(S1,NL1,Cs1)
		    end;
		empty ->
		    error(S, "#elif missing #if", []),
		    iread(S,NL1,Cs1)
	    end;

	{'#else',NL1,Cs1} ->
	    case peek_if(S) of
		{'#else',V,Ln} ->
		    error(S, "#else missing #if/#ifdef", []),
		    iread(S,NL1,Cs1);
		    
		{Tag,V,Ln} ->
		    if V == ?SKIP ->
			    iread(S,NL1,Cs1);
		       V == ?TRUE ->
			    S1 = mod_if(S,Tag,?SKIP,Ln),
			    iread(S1,NL1,Cs1);
		       V == ?FALSE ->
			    S1 = mod_if(S,'#else',?TRUE,S#s.line),
			    iread(S1,NL1,Cs1)
		    end;
		[] ->
		    error(S, "#else missing #if", []),
		    iread(S,NL1,Cs1)
	    end;

	{'#endif',NL1,Cs1} -> 
	    case pop_if(S) of
		{S1,true} ->
		    iread(S1,NL1,Cs1);
		{S1,false} ->
		    error(S, "#endif missing #if/#ifdef", []), 
		    iread(S1,NL1,Cs1)
	    end;

	{[],_NL,eof} ->
	    case restore(S) of
		{S1,empty} ->
		    {S1,{[],0,eof}};
		{S1,Cs1} ->
		    ?dbg("CLOSE: ~s\n", [S#s.file]),
		    file:close(S#s.fd),
		    {S1,{auto_line(S1#s.line,S1#s.file),0,Cs1}}
	    end;

	{Data,NL1,Cs1} when list(Data) ->
	    case peek_if(S) of
		{_,?FALSE,_} ->
		    iread(S,NL1,Cs1);
		{_,?SKIP,_} ->
		    iread(S,NL1,Cs1);
		{_,?TRUE,_} ->
		    {S,{Data,NL1,Cs1}};
		empty ->
		    {S,{Data,NL1,Cs1}}
	    end
    end.

%%
%% Scan a line and remove comments
%%
iread(S,[$/,$\\|Cs],Skip,NL,Buf) -> 
    iread_bsl(S,Cs,[$/],[],Skip,NL,Buf);
iread(S,[$\\|Cs],Skip,NL,Buf)    -> 
    iread_bsl(S,Cs,[],[],Skip,NL,Buf);
iread(S,[$/,$/|Cs],Skip,NL,Buf) ->
    {S1,{Ln,NL1,Cs1}} = iread(S,Cs,true,NL,[]),
    process(S1,Skip,reverse([$\n,$\s|Buf]),NL1-1,Cs1);
iread(S,[$/,$*|Cs],Skip,NL,Buf) ->
    Cs1 = comment(S,NL,Cs),
    iread(S,Cs1,Skip,[],Buf);
iread(S,Cs=[$/],Skip,NL,Buf)  ->
    iread_more(S,Cs,Skip,NL,Buf);
iread(S,[$\n|Cs],Skip,NL,Buf) -> 
    S1 = update_line(S,1),
    process(S1,Skip,reverse([$\n|Buf]),NL,Cs);
iread(S,[C|Cs],Skip,NL,Buf)  -> 
    iread(S,Cs,Skip,NL,[C|Buf]);
iread(S,[],Skip,NL,Buf)      -> 
    iread_more(S,[],Skip,NL,Buf).

%% scan after \  to look for WS*\n
iread_bsl(S,[C|Cs],Hs,Acc,Skip,NL,Buf) ->
    if C == $\s; C == $\t; C == $\r ->
	    iread_bsl(S,Cs,Hs,[$\s|Acc],Skip,NL,Buf);
       C == $\n ->
	    iread(S,Hs++Cs,Skip,NL+1,Buf);
       true ->
	    iread(S,[C|Cs],Skip,NL,Acc++Hs++[$\\|Buf])
    end;
iread_bsl(S,[],Hs,Acc,Skip,NL,Buf) ->
    case file:read(S#s.fd, ?CHUNK_SIZE) of
	eof -> process(S,Skip,reverse(Buf),NL,eof);
	Err={error,_} -> process(S,Skip,reverse(Buf),NL,[]);
	{ok,Chars} -> iread_bsl(S,Chars,Hs,Acc,Skip,NL,Buf)
    end.

iread_more(S,[],_,NL,eof) ->
    {S,{[],0,eof}};
iread_more(S,Cs,Skip,NL,Buf) ->
    case file:read(S#s.fd, ?CHUNK_SIZE) of
	eof ->
	    process(S,Skip,reverse(Buf),NL,eof);
	Err={error,_} -> 
	    process(S,Skip,reverse(Buf),NL,Err);
	{ok,Chars} -> 
	    iread(S,Cs++Chars,Skip,NL,Buf)
    end.


%% Skip a block comment ( after reading /* )
comment(S, Cs) ->
    comment(S, [], Cs).

comment(_S,NL, [$*,$/ | Cs]) -> [$\s|nl(NL)]++Cs;
comment(S,NL,[$*,$\\|Cs]) -> comment_bsl(S,NL,Cs);
comment(S,NL,Cs=[$*])  -> comment_more(S,NL,Cs);
comment(S,NL,[$\n|Cs]) -> comment(S,NL+1, Cs);
comment(S,NL,[_C|Cs]) -> comment(S, NL, Cs);
comment(S,NL,[]) -> comment_more(S,NL,[]).

comment_more(S,NL,Cs) ->
    case file:read(S#s.fd, ?CHUNK_SIZE) of
	eof        -> [$\s|nl(NL)]++Cs;
	{error,_}  -> [$\s|nl(NL)]++Cs;
	{ok,Chars} -> comment(S,NL,Cs++Chars)
    end.

nl(NL) ->
    lists:duplicate(NL, $\n).

%% handle the case:
%%  *\
%%  /
comment_bsl(S,NL,[$\s|Cs]) -> comment_bsl(S,NL,Cs);
comment_bsl(S,NL,[$\t|Cs]) -> comment_bsl(S,NL,Cs);
comment_bsl(S,NL,[$\r|Cs]) -> comment_bsl(S,NL,Cs);
comment_bsl(S,NL,[$\n|Cs]) -> comment(S,NL+1,[$*|Cs]);
comment_bsl(S,NL,[C|Cs]) -> comment(S,NL,[C|Cs]);
comment_bsl(S,NL,[]) ->
    case file:read(S#s.fd, ?CHUNK_SIZE) of
	eof -> [$\s|nl(NL)];
	{error,_} -> [$\s|nl(NL)];
	{ok,Chars} -> comment_bsl(S,NL,Chars)
    end.

process(S,false,Line,NL,Cs) ->
    %% io:format("process: line=~p, cs=~p\n", [Line,Cs]),
    case scan_one(Line,true,true) of
	{[{char,"#"}], LCs} ->
	    process_cpp(S,LCs,NL,Cs);
	{_, _} ->
	    ELine = expand(S, Line),
	    %% io:format("expand: line=~p\n", [ELine]),
	    {S,{ELine,NL,Cs}}
    end;
process(S,true,Line,NL,Cs) ->
    %% io:format("process:skip: line=~p, cs=~p\n", [Line,Cs]),
    {S,{[],NL+1,Cs}}.


%% process characters after seen #
process_cpp(S,LCs, NL, Cs) ->
    case scan_one(LCs,true,true) of
	{[],[]} -> %% null directive produce a signle line with #
	    {S,{"#\n",NL,Cs}};
	{[{id,"ident"}],_LCs1} -> %% ignored
	    {S,{[],NL+1,Cs}};
	{[{id,"sccs"}],_LCs1} ->  %% ignored
	    {S,{[],NL+1,Cs}};
	{[{id,"pragma"}],LCs1} -> %% ignored for now
	    {S,{[],NL+1,Cs}};
	{[{id,"else"}],_} ->  %% ignore trailing data?
	    {S,{'#else',NL+1,Cs}};
	{[{id,"endif"}],_} ->  %% ignore trailing data?
	    {S,{'#endif',NL+1,Cs}};
	{[{id,"define"}],LCs1} ->
	    %%    #define Name Value
	    %%  | #define Name(A1,..An)  Value
	    case scan_one(LCs1,true,true) of
		{[{id,Var}],LCs2="("++_} ->
		    S1 = define_macro(S,Var,LCs2),
		    {S1,{[],NL+1,Cs}};
		{[{id,Var}], LCs2} ->
		    S1 = define(S,Var,LCs2),
		    {S1,{[],NL+1,Cs}};
		{[],_} ->
		    error(S, "no macro name given in #define directive", []),
		    {S,{[],NL+1,Cs}};
		{_, _} ->
		    error(S, "bad macro name given in #define directive", []),
		    {S,{[],NL+1,Cs}}
	    end;
	{[{id,"undef"}],LCs1} ->   
	    %% #undef <id>
	    case scan_all(LCs1,true,true) of
		{[{id,Var}|_],_} ->
		    S1 = undef(S,Var),
		    {S1,{[],NL+1,Cs}};
		{_, _} ->
		    error(S, "no macro name given in #undef directive", []),
		    {S,{[],NL+1,Cs}}
	    end;
	{[{id,"assert"}],LCs1} ->
	    %%    #assert predicate value
	    case scan_all(LCs1,true,true) of
		{[{id,Pred},{id,Answer}],_} ->
		    S1 = assert(S, Pred, Answer),
		    {S1,{[],NL+1,Cs}};
		{_, _} ->
		    error(S, "bad arguments to #assert directive", []),
		    {S,{[],NL+1,Cs}}
	    end;
	{[{id,"unassert"}],LCs1} ->
	    %%    #assert predicate value
	    case scan_all(LCs1,true,true) of
		{[{id,Pred},{id,Answer}],_} ->
		    S1 = unassert(S, Pred, Answer),
		    {S1,{[],NL+1,Cs}};
		{[{id,Pred}],_} ->
		    S1 = unassert(S,Pred),
		    {S1,{[],NL+1,Cs}};
		{_, _} ->
		    error(S, "bad arguments to #unassert directive", []),
		    {S,{[],NL+1,Cs}}
	    end;
	{[{id,"if"}],LCs1} -> 
	    cpp_if(S,LCs1,'#if',NL,Cs);
	{[{id,"elif"}],LCs1} ->
	    cpp_if(S,LCs1,'#elif',NL,Cs);
	{[{id,"ifdef"}],LCs1} ->
	    %% #ifndef <id>
	    %%   [text]
	    %% [#else
	    %%   text]
	    %% #endif
	    %%
	    case scan_all(LCs1,true,true) of
		{[{id,Var} | _],_} ->
		    {S,{{'#ifdef', defined(S,Var)},NL+1,Cs}};
		{_,_} ->
		    error(S,"no macro name given in #ifdef directive",[]),
		    {S,{{'#ifdef', 0},NL+1,Cs}}
	    end;
	{[{id,"ifndef"}],LCs1} ->  
	    %% #ifndef <id>
	    %%   text
	    %% [#else
	    %%   text]
	    %% #endif
	    case scan_all(LCs1,true,true) of
		{[{id,Var} | _],_} ->
		    {S,{{'#ifndef', 1-defined(S,Var)},NL+1,Cs}};
		{_,_} ->
		    error(S, "no macro name given in #ifdef directive",[]),
		    {S,{{'#ifndef', 0},NL+1,Cs}}
	    end;
	{[{id,"include"}],LCs1} ->
	    case scan_all(LCs1,true,false) of
		{[{string,File}],_} ->
		    cpp_include(S,File,quoted,NL,Cs);
		{[{search,File}],_} ->
		    cpp_include(S,File,search,NL,Cs);
		{_,_} ->
		    error(S, "bad arguments given in #include directive",[]),
		    {S,{[],NL+1,Cs}}
	    end;

	{[{id,"include_next"}],LCs1} ->
	    error(S, "include_next NOT implemented", []),
	    {S,{[],NL+1,Cs}};
		
	{[{id,"line"}],LCs1} ->
	    case scan_all(LCs1,true,true) of
		%%   #line <num>
		%% | #line <num> <string>
		{[{num,Ln}],_} ->
		    case catch list_to_integer(Ln) of
			{'EXIT',_} ->
			    error(S, "bad linenumber ~p in #line directive",
				  [Ln]),
			    {S,{[],NL+1,Cs}};
			N ->
			    S1 = set_line(S, N),
			    {S1,{[],NL+1,Cs}}
		    end,
		    {[],NL+1,Cs};
		{[{num,Ln},{string,File}],_} ->
		    case catch list_to_integer(Ln) of
			{'EXIT',_} ->
			    error(S, "bad linenumber ~p in #line directive", 
				  [Ln]),
			    {S,{[],NL+1,Cs}};
			N ->
			    S1 = set_line(S, N, File),
			    {S1,{[],NL+1,Cs}}
		    end;
		{_, _} ->
		    error(S, "bad #line directive", []),
		    {S,{[],NL+1,Cs}}
	    end;
	{[{id,"error"}],LCs1} ->
	    error_ts(S, scan_all(LCs1,true,true)),
	    {S,{[],NL+1,Cs}};

	{[{id,"warning"}],LCs1} ->
	    warning_ts(S, scan_all(LCs1,true,true)),
	    {S,{[],NL+1,Cs}};

	{_,_} ->
	    error(S, "preprocessor directive not understood",[]),
	    {S,{[],NL+1,Cs}}
    end.

%%
%% #if <expr>
%%   [text]
%% [#elif <expr>
%%    text]
%% [#elif <expr>
%%    text]
%% [#else
%%   text]
%% #endif
%%

cpp_if(S,LCs,Tag, NL, Cs) ->
    case bic_scan:string(LCs) of
	{ok,Ts,_} ->
	    %% Put scanned tokens in a parsable for:
	    %% int _ = Ts;
	    Ln = S#s.line,
	    case bic_parse:parse([{int,Ln},
				  {identifier,Ln,"_"},
				  {'=',Ln}|Ts]++
				 [{';',Ln}]) of
		{ok,[#'DECL' { value=Expr }]} ->
		    {S,{{Tag,eval(S,Expr)},NL+1,Cs}};
		{error,{_Ln,Mod,Message}} ->
		    Err = Mod:format_error(Message),
		    error(S, "~s", [Err]),
		    {S,{{Tag, 0},NL+1,Cs}}
	    end;
	{error,{_Ln,Mod,Message}} ->
	    Err = Mod:format_error(Message),
	    error(S, "~s", [Err]),
	    {S,{{Tag, 0},NL+1,Cs}}
    end.

%%   #include "file"
%% | #include <file>
%%
cpp_include(S,File,quoted,NL,Cs) ->
    Path = [S#s.pwd]++S#s.qinclude ++ S#s.include,
    cpp_include_path(S, File, Path, NL, Cs);
cpp_include(S,File,search,NL,Cs) ->
    Path = S#s.include,
    cpp_include_path(S, File, Path, NL, Cs).

cpp_include_path(S, Name, [Path | Ps], NL, Cs) ->
    File = filename:join(Path, Name),
    ?dbg("OPEN: ~s\n", [File]),
    case file:open(File,[read]) of
	{ok, Fd} ->
	    ?dbg("Open file: ~s\n", [File]),
	    S1 = save(S, Cs),
	    S2 = S1#s { fd = Fd,
			file = File,
			line = 1,
			if_stack = [],
			pwd = filename:dirname(File)
		       },
	    {S2,{auto_line(1,File),0,[]}};
	{error,_} ->
	    cpp_include_path(S,File,Ps,NL,Cs)
    end;
cpp_include_path(S,File,[],NL,Cs) ->
    error(S, "file ~s not found", [File]),
    {S, {[],NL+1, Cs}}.

    
%%
%% FIXME: store buffers positions and reopend to save 
%%        open file descriptiors (when regular files!)
%%
save(S, Buf) ->
    Elem = 
	#save { fd = S#s.fd, file = S#s.file, line = S#s.line,
		if_stack = S#s.if_stack,
		pwd = S#s.pwd, buf = Buf,
		defs     = [{"__FILE__", value(S, "__FILE__")},
			    {"__LINE__", value(S, "__LINE__")},
			    {"__INCLUDE_LEVEL__", value(S,"__INCLUDE_LEVEL__")}
			   ]
	       },
    ?dbg("Save file ~s:~w\n", [S#s.file,S#s.line]),
    Defs = dict_set(S#s.defs,"__INCLUDE_LEVEL__",
		    value(S,"__INCLUDE_LEVEL__")+1),
    S#s { defs = Defs, inc_stack =  [Elem | S#s.inc_stack]}.

restore(S) ->
    case S#s.inc_stack of
	[] ->
	    {S,empty};
	[#save { fd=Fd, file=File, line=Line, 
		 if_stack=Stack, pwd=Pwd, 
		 buf=Buf, defs=DefsList }|Is] ->
	    ?dbg("Restore file ~s:~w\n", [File,Line]),
	    Defs = foldl(fun({Name,Value},D) ->
				 dict_set(D, Name, Value)
			 end, S#s.defs, DefsList),
	    S1 = S#s { file=File,
		       fd = Fd,
		       line = Line,
		       if_stack = Stack,
		       inc_stack = Is,
		       pwd = Pwd,
		       defs = Defs
		      },
	    {S1, Buf}
    end.

auto_line(N, File) ->
    lists:flatten(io_lib:format("# ~w \"~s\"\n",[N, File])).

%% push directive status line on IF-STACK
push_if(S,D,V,Ln) ->
    Stack = S#s.if_stack,
    S#s { if_stack = [{D,V,Ln} | Stack]}.

%% pop IF-STACK
pop_if(S) ->
    case S#s.if_stack of
	[] -> 
	    {S,false};
	[_|Stack] ->
	    {S#s { if_stack = Stack }, true}
    end.

%% peek top element on IF-STACK
peek_if(S) ->
    case S#s.if_stack of
	[] -> 
	    empty;
	[E|_] -> 
	    E
    end.

%% modifiy top element 
mod_if(S,D,V,Ln) ->
    [_ | Stack] = S#s.if_stack,
    S#s { if_stack = [{D,V,Ln} | Stack]}.

assert(S,Pred,Answer) ->    
    P = dict:store({Pred,Answer},true,S#s.preds),
    S#s { preds = P }.

unassert(S,Pred,Answer) ->
    P = dict:erase({Pred,Answer},S#s.preds),
    S#s { preds = P }.

unassert(S,Pred) ->
    P = 
	foldl(
	  fun({P,A},P0) when P == Pred ->
		  dict:erase({P,A},P0);
	     (_, P0) ->
		  P0
	  end,S#s.preds, dict:fetch_keys(S#s.preds)),
    S#s { preds = P }.
    

define_macro(S, Name, Cs) ->
    {Ts, Def} = scan_until_char(Cs,false,true,")"),
    case parse_macro_args(Ts) of
	{ok,Ps} ->
	    define(S,Name,{Ps,Cs});
	{error,Msg} ->
	    error(S, "~s in macro argument list", [Msg]),
	    S
    end.

parse_macro_args([{char,"("} | Ts]) ->
    parse_macro_args(Ts,[]);
parse_macro_args(_) ->
    {error, "missing ("}.

parse_macro_args([{id,Name},{char,","}|Ts], Ps) ->
    parse_macro_args(Ts, [Name | Ps]);
parse_macro_args([{id,Name},{char,")"}], Ps) ->
    check_macro_args(reverse([Name|Ps]));
parse_macro_args([{char,"."},{char,"."},{char,"."},{char,")"}],Ps) ->
    check_macro_args(reverse([".__VA_ARGS__"|Ps]));
parse_macro_args([{id,Name},{char,"."},{char,"."},{char,"."},{char,")"}],Ps) ->
    check_macro_args(reverse(["."++Name|Ps]));
parse_macro_args([{char,")"}], Ps) ->
    check_macro_args(reverse(Ps));
parse_macro_args(_, _Ps) ->
    {error, "syntax error"}.
    
check_macro_args(Ns) ->
    N1 = length(Ns),
    N2 = length(lists:usort(Ns)),
    if N1 == N2 ->
	    {ok, Ns};
       true ->
	    {error, "argument multiply defined"}
    end.


%% expand a line
%%  x = 1
%%  xx = 2
%%
%%  x/*   */x  => 1 1
%%  
expand(S,[]) ->
    [];
expand(S,Cs) ->
    case scan_one(Cs,false,true) of
	{[{id,Name}], Cs1} ->
	    case dict:find(Name,S#s.defs) of
		error ->
		    Name ++ expand(S,Cs1);
		{ok,{Ps,Cs2}} ->
		    io:format("FIXME macro expand\n"),
		    Cs3 = Cs2, %% parse arguments
		    expand(S,Cs3++Cs1);
		{ok,N} when integer(N) -> %% for internals like __LINE__
		    integer_to_list(N) ++ expand(S,Cs1);
		{ok,Cs2} ->
		    Cs2++expand(S,Cs1)
	    end;
	{[{char,[C]}],Cs1} ->
	    [C | expand(S,Cs1)];
	{[{num,Num}],Cs1} ->
	    Num ++ expand(S,Cs1);
	{[{string,Str}],Cs1} ->
	    [$\"|Str] ++ [$\" | expand(S,Cs1)];
	{[{search,Str}],Cs1} ->
	    [$\<|Str] ++ [$\> | expand(S,Cs1)]
    end.

%% Evaluate a #if expression 
%% return 0 or 1
eval(S,#'EXPR' { op=Op, arg1=Arg1, arg2=Arg2}) ->
    case Op of
	'+' -> eval(S,Arg1)+eval(S,Arg2);
	'-' -> eval(S,Arg1)-eval(S,Arg2);
	'*' -> eval(S,Arg1)*eval(S,Arg2);
	'/' -> 
	    A1=eval(S,Arg1),A2=eval(S,Arg2),
	    if is_integer(A1), is_integer(A2) ->
		    A1 div A2;
	       true ->
		    A1 / A2
	    end;
	'%' -> eval(S,Arg1) rem eval(S,Arg2);
	'<<' -> eval(S,Arg1) bsl eval(S,Arg2);
	'>>' -> eval(S,Arg1) bsr eval(S,Arg2);
	'&' -> eval(S,Arg1) band eval(S,Arg2);
	'|' -> eval(S,Arg1) bor eval(S,Arg2);
	'^' -> eval(S,Arg1) bxor eval(S,Arg2);
	'&&' -> case eval(S,Arg1) of
		    0 -> 0;
		    _ -> eval(S,Arg2)
		end;
	'||' -> case eval(S,Arg1) of
		    0 -> eval(S,Arg2);
		    V -> V
		end;
	'>' -> case eval(S,Arg1) > eval(S,Arg2) of
		   true -> 1;
		   false -> 0
	       end;
	'>=' -> case eval(S,Arg1) >= eval(S,Arg2) of
		    true -> 1;
		    false -> 0
		end;
	'<' -> case eval(S,Arg1) < eval(S,Arg2) of
		   true -> 1;
		   false -> 0
	       end;
	'<=' -> case eval(S,Arg1) =< eval(S,Arg2) of
		    true -> 1;
		    false -> 0
		end;
	'==' -> case eval(S,Arg1) == eval(S,Arg2) of
		    true -> 1;
		    false -> 0
		end;
	'!=' -> case eval(S,Arg1) =/= eval(S,Arg2) of
		    true -> 1;
		    false -> 0
		end;
	_ ->
	    0
    end;
eval(S,#'UNARY' { op=Op, arg=Arg }) ->
    case Op of
	'~' -> bnot eval(S,Arg);
	'+' -> eval(S,Arg);
	'-' -> - eval(S,Arg);
	'!' -> case eval(S,Arg) of
		   0 -> 1;
		   _ -> 0
	       end;
	_ -> 0
    end;
eval(S,#'CALL' { func=Func, args=Args }) ->
    case Func of
	{identifier,_,"defined"} ->
	    case Args of
		[{identifier,_,ID}] ->
		    defined(S,ID);
		_ ->
		    0
	    end;
	_ -> 0
    end;
eval(_S,#'CONSTANT' { base=Base, value=Value }) ->
    if is_integer(Base) ->
	    erlang:list_to_integer(Value,Base);
       Base==char -> hd(Value);
       Base==float -> list_to_float(Value)
    end;
eval(S,#'ID' { name=ID }) -> value(S,ID).

%%
%% scan {id,ID}          ID = {L}({L}|{D})*
%%      {num,Decimal}
%%      {string,String}   '"' C* '"'
%%      {search,String}   '<' C* '>'
%%      {char,[C]}

scan_all(Chars,SkipWs,SkipSearch) ->
    scan_until(Chars,SkipWs,SkipSearch,fun(_) -> false end).

scan_one(Chars,SkipWs,SkipSearch) ->
    scan_until(Chars,SkipWs,SkipSearch,fun(_) -> true end).

scan_until_char(Chars,SkipWs,SkipSearch,Char) ->
    scan_until_token(Chars,SkipWs,SkipSearch,{char,Char}).

scan_until_token(Chars,SkipWs,SkipSearch,Token) ->
    scan_until(Chars,SkipWs,SkipSearch,fun(Tok) -> Tok==Token end).
    
scan_until(Chars,SkipWs,SkipSearch,Until) ->
    scan_until(Chars,SkipWs,SkipSearch,[],Until).

scan_until([$\"|Cs],SkipWs,SkipSearch,Ts,Until) ->
   scan_quoted(Cs, $\", string, SkipWs, SkipSearch, Ts, [], Until);
scan_until([$\<|Cs],SkipWs,false,Ts, Until) ->
    %% we only scan for <abc.h> once
    scan_quoted(Cs, $\>, search, SkipWs, true, Ts, [], Until);

scan_until([C|Cs], SkipWs, SkipSearch, Ts, Until) ->
    if  C >= $0, C =< $9 ->
	    scan_num(Cs, SkipWs,SkipSearch,Ts,[C],Until);
	C >= $a, C =< $z; C >= $A, C =< $Z; C == $_ ->
	    scan_id(Cs, SkipWs,SkipSearch,Ts, [C], Until);
       true ->
	    scan_t(Cs, SkipWs,SkipSearch,{char,[C]}, Ts, Until)
    end;
scan_until([], _SkipWs, _SkipSearch, Ts, _Until) ->
    {reverse(Ts), ""}.


scan_t(Cs,true,SkipSearch,{char,[C]},Ts,Until) 
  when C >= 0, C =< $\s ->
    scan_until(Cs,true,SkipSearch,Ts,Until);
scan_t(Cs,SkipWs,SkipSearch,T,Ts,Until) ->
    case Until(T) of
	true ->
	    {reverse([T|Ts]), Cs};
	false -> 
	    scan_until(Cs,SkipWs,SkipSearch,[T|Ts],Until)
    end.

%% scan quouted stuff like 'abc' "abc" <abc>
scan_quoted([Q|Cs], Q, Tag, SkipWs,SkipSearch, Ts, Acc, Until) ->
    scan_t(Cs, SkipWs, SkipSearch, {Tag,reverse(Acc)}, Ts, Until);
scan_quoted([$\\,C|Cs], Q, Tag, SkipWs,SkipSearch,Ts,Acc,Until) ->
    scan_quoted(Cs, Q, Tag, SkipWs,SkipSearch,Ts,[$\\,C|Acc],Until);
scan_quoted([C|Cs], Q, Tag, SkipWs,SkipSearch,Ts,Acc,Until) ->
    scan_quoted(Cs, Q, Tag, SkipWs,SkipSearch,Ts,[C|Acc],Until);
scan_quoted([], Q, Tag, SkipWs,SkipSearch,Ts,Acc,Until) ->
    %% FIXME: abort scan...
    scan_t([], SkipWs, SkipSearch, {Tag,reverse(Acc)},Ts,Until).

scan_num([C|Cs], SkipWs,SkipSearch,Ts,Acc,Until) 
  when C >= $0, C =< $9 ->
    scan_num(Cs,SkipWs,SkipSearch,Ts,[C|Acc],Until);
scan_num(Cs, SkipWs,SkipSearch,Ts,Acc,Until) ->
    scan_t(Cs,SkipWs,SkipSearch,{num,reverse(Acc)},Ts,Until).

scan_id([C|Cs],SkipWs,SkipSearch,Ts,Acc,Until) 
  when C >= $a, C =< $z; C >= $A, C =< $Z; C =< $0, C >= $9; C == $_ -> 
    scan_id(Cs,SkipWs,SkipSearch,Ts,[C|Acc],Until);
scan_id(Cs, SkipWs,SkipSearch,Ts,Acc,Until) ->
    scan_t(Cs,SkipWs,SkipSearch,{id,reverse(Acc)},Ts,Until).


%% try rewrite Var as an integer value if possible    
value(S,Var) ->
    case dict:find(Var,S#s.defs) of
	error -> 0;
	{ok,Val} when integer(Val) -> Val;
	{ok,Val} when list(Val) ->
	    case catch list_to_integer(Val) of
		{'EXIT',_} -> Val;
		N -> N
	    end
    end.

defined(S,Var) ->
    case dict:find(Var,S#s.defs) of
	error -> 0;
	_ -> 1
    end.

%% macro operations
undef(S, Var) ->
    if  Var == "__INCLUDE_LEVEL__" ->
	    warning(S, "can not undefine ~s", [Var]),
	    S;
	Var == "__LINE__";
	Var == "__FILE__" ->
	    warning(S, "undefining ~s", [Var]),
	    D = dict:erase(Var, S#s.defs),
	    S#s { defs = D };
	true ->
	    D = dict:erase(Var, S#s.defs),
	    S#s { defs = D }
    end.

define(S,Var,Value) ->
    if  Var == "__INCLUDE_LEVEL__" ->
	    warning(S, "can not redefine ~s", [Var]),
	    S;
	true ->
	    case defined(S,Var) of
		1 -> warning(S, "~s redefined\n", [Var]),
		     set(S,Var,Value);
		0 ->
		    set(S,Var,Value)
	    end
    end.

%% set only if defined
dict_update(D,Var,Value) ->
    case dict:find(Var, D) of
	error -> D;
	_ -> dict:store(Var,Value,D)
    end.

dict_set(D,Var,Value) ->
    dict:store(Var,Value,D).
	    
set(S,Var,Value) ->
    D = dict_set(S#s.defs,Var,Value),
    S#s { defs = D }.

update_line(S, N) ->
    N1 = S#s.line + N,
    DN1 = value(S, "__LINE__") + N,
    Defs = dict_update(S#s.defs, "__LINE__", DN1),
    S#s { line = N1, defs = Defs }.

set_line(S, Line) ->
    D0 = dict_update(S#s.defs, "__LINE__", Line),
    S#s { defs = D0 }.

set_line(S, Line, File) ->
    D0 = dict_update(S#s.defs, "__LINE__", Line),
    D1 = dict_update(D0, "__FILE__", File),
    S#s { defs = D1 }.

%% warning and errors use __FILE__ / __LINE__ instead of
%% S#s.line and S#s.file. This is because the #line dirctive should
%% make sure the errors are refering to the correct file and line
warning(S, Fmt, As) ->
    io:format("~s:~w: warning:"++Fmt++"\n", 
	      [value(S,"__FILE__"), value(S,"__LINE__") | As]).    

error(S, Fmt, As) ->
    io:format("~s:~w: error:"++Fmt++"\n", 
	      [value(S,"__FILE__"), value(S,"__LINE__") | As]).

warning_ts(S, Ts) ->
    Chars = map(fun({_,Cs}) -> Cs end, Ts),
    io:format("~s:~w: warning ~s\n", 
	      [value(S,"__FILE__"), value(S,"__LINE__"),Chars]).

error_ts(S, Ts) ->
    Chars = map(fun({_,Cs}) -> Cs end, Ts),
    io:format("~s:~w: error ~s\n", 
	      [value(S,"__FILE__"), value(S,"__LINE__"),Chars]).
