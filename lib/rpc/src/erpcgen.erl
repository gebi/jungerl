%% Copyright (c) 2000 Sendmail, Inc.  All rights reserved.
%%
%% Compile xdr files into erlang modules
%%
%% Input:  proto.x
%%
%% Output: [ proto_clnt.erl ]
%%         [ proto_svc.erl ]
%%         [ proto_xdr.erl ]
%%         [ proto.hrl ]
%%         [ proto_stub.erl ]
%%
%%        stubb_server.erl.gen  
%%        stubb_client.erl.gen
%%
%% options:   hrl          -- generate proto.hrl
%%            clnt         -- generate proto_clnt.erl
%%            svc          -- generate proto_svc.erl, user impl. gen_server
%%            svc_callback -- generate proto_svc.erl, user impl. rpc_server
%%            xdr          -- generate proto_xdr.erl
%%            trace        -- make stubs profilable via 'trace' option
%%            xdr_inc      -- generate proto_xdr.hrl
%%            svc_stub     -- generate server server.erl.stub
%%            client       -- [hrl,clnt,xdr]
%%            server       -- [hrl,svc,xdr,svc_stub]
%%            xdrlib       -- [hrl,xdr]
%%            all          -- [hrl,clnt,svc,xdr,svc_stub]
%%
%%
-module(erpcgen).
-behavior(application).

-export([start/0, start/2]).
-export([file/1, file/2, file/3]).

-import(lists, [map/2, filter/2, foreach/2, reverse/1]).
-import(lists, [member/2, keysearch/3, concat/1, flatten/1]).
-import(xdrgen, [genname/2]).
-import(xdrgen, [mkfun/1, mkcall/2, mkcall/3, mkcase/2, mkif/1, mkvar/1]).
-import(xdrgen, [mkatom/1, mkatom/2, mkint/1, mkfloat/1, mkop/3, mkop/2]).
-import(xdrgen, [mkclause/3, mktuple/1, mkcons/2, mknil/0, mklist/1]).
-import(xdrgen, [mkmatch/2, mkblock/1, mkmodule/2, mkexport/2]).
-import(xdrgen, [mkfunction/3]).

%% Called by application startup.
start() -> start(normal, []).
start(normal, []) ->
    {ok, [[InFile]]} = init:get_argument(infile),
    {ok, [[OutFile]]} = init:get_argument(outfile),
    {ok, [[OptionsArg]]} = init:get_argument(options),
    {ok, Scan, _} = erl_scan:string(OptionsArg),
    {ok, OptList} = erl_parse:parse_term(Scan),
    case file(list_to_atom(InFile), list_to_atom(OutFile), OptList) of
	[ok,ok,ok,ok,ok] ->
	    %% io:format("Compilation of ~s successful\n", [InFile]),
	    {error, 'Compilation successful'};	% For application behavior.
	Error ->
	    %% Error is too complex to figure out.  Let user figure it out.
	    {error, 'Compilation failed'}	% For application behavior.
    end.

file(File) ->
    file(File, [all]).

file(File, Opts) ->
    file(File, File, Opts).

file(In, Out, Opts) ->
    file1(In, Out, trans_opts(Opts)).

file1(In, Out, {error,Reason}) -> {error, Reason};
file1(In, Out, {ok, Opts}) when atom(In), atom(Out) ->
    File = atom_to_list(In) ++ ".x",
    Base = atom_to_list(Out),
    case xdr_scan:file(File) of
	{error, {Line,Where,Reason}} ->
	    Message = xdr_scan:format_error(Reason),
	    io:format("~s:~w : ~s~n", [File, Line, Message]),
	    {error, {Line,Where,Reason}};
	{error, Reason} -> 
	    {error, Reason};
	Tokens ->
	    case xdr_parse:parse(Tokens) of
		{error, {Line,Where,Reason}} ->
		    Message = xdr_parse:format_error(Reason),
		    io:format("~s:~w : ~s~n", [File, Line,Message]),
		    {error, {Line,Where,Reason}};
		{ok, Spec} ->
		    case catch transform(Spec,File) of
			{ok,Spec1, Env} ->
			    generate(Base, Spec1, Env, Opts);
			error ->
			    {error, File}
		    end
	    end
    end.

generate(Base, Spec, Env, Opts) ->
    map(
      fun(hrl)  -> gen_hrl(Base, Spec,  Env, Opts);
	 (clnt) -> gen_clnt(Base, Spec, Env, Opts);
	 (svc) ->  gen_svc(Base, Spec,  Env, Opts, gen_server);
	 (svc_callback) ->  gen_svc(Base, Spec,  Env, Opts, rpc_server);
	 (xdr) ->  gen_xdr(Base, Spec,  Env, Opts);
	 (xdr_inc) -> gen_xdr_inc(Base, Spec,  Env, Opts);
	 (svc_stub) -> gen_stub(Base, Spec, Env, Opts);
	 (_) -> ok
      end, Opts).
    

trans_opts(Opts) ->
    tr_opts(Opts, []).

%% check and expand options

tr_opts([Opt | Opts], L) ->
    case Opt of
	all          -> tr_opts(Opts, add_opts([hrl,clnt,svc,xdr,svc_stub],L));
	client       -> tr_opts(Opts, add_opts([hrl,clnt,xdr],L));
	server       -> tr_opts(Opts, add_opts([hrl,svc,xdr,svc_stub],L));
	xdrlib       -> tr_opts(Opts, add_opts([hrl,xdr],L));
	hrl          -> tr_opts(Opts, add_opt(hrl,L));
	clnt         -> tr_opts(Opts, add_opt(clnt,L));
	svc          -> tr_opts(Opts, add_opt(svc,L));
	svc_callback -> tr_opts(Opts, add_opt(svc_callback,L));
	xdr          -> tr_opts(Opts, add_opt(xdr,L));
	xdr_inc      -> tr_opts(Opts, add_opt(xdr_inc,L));
	trace        -> tr_opts(Opts, add_opt(trace,L));
	svc_stub     -> tr_opts(Opts, add_opt(svc_stub,L));
	_            -> {error, {option, Opts}}
    end;
tr_opts([], L) -> 
    case lists:member(svc, L) and lists:member(svc_callback, L) of
	true -> {error, {option, [svc, svc_callback]}};
	false -> {ok,L}
    end.

add_opt(Opt, L) when atom(Opt) ->
    case member(Opt, L) of
	true -> L;
	false -> [Opt | L]
    end.

add_opts([Opt|Opts], L) ->
    add_opts(Opts, add_opt(Opt, L));
add_opts([], L) -> L.


emit_list(L) ->
    io:format("------------------------------------\n"),
    foreach(fun(E) ->
		    io:format("~p~n", [E])
	    end, L).
%%
%% Month conversion
%%
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

%%
%% emit header
%%
gen_header(Fd, Module) ->
    io:format(Fd, "%%\n%% ~s was generated by erpcgen (do not edit)\n", 
	      [Module]),
    {YY,MM,DD} = date(),
    {H,M,S} = time(),
    io:format(Fd, "%% date: ~s ~w ~2.2.0w:~2.2.0w:~2.2.0w ~w~n",
	      [month(MM), DD, H, M, S, YY]),
    io:format(Fd, "%%\n", []).

%%
%% Generate module for encode/decoder
%%
emit_fun(Fd, Func) ->
    io:put_chars(Fd, "\n"),
    io:put_chars(Fd, erl_pp:form(Func)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% Generate XDR File
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_xdr(Base, Spec, Env, Opts) ->
    gen_xdr_base(Base, Spec, Env, Opts, mod).

gen_xdr_inc(Base, Spec, Env, Opts) ->
    gen_xdr_base(Base, Spec, Env, Opts, inc).

gen_xdr_base(Base, Spec, Env, Opts, Type) ->
    Module = Base ++ "_xdr",
    File = if Type == mod -> Module ++ ".erl";
	      Type == inc -> Module ++ ".hrl"
	   end,
    case file:open(File, write) of
	{ok, Fd} ->
	    if Type == mod ->
		    gen_header(Fd, Module),
		    io:format(Fd, "-module(~s).~n", [Module]),
		    case lists:member(trace, Opts) of
			true -> io:format(Fd, "-compile([verbose, "
					  "report_errors, report_warnings, "
					  "trace]).~n", []);
			_ -> skip_it
		    end;
	       Type == inc ->
		    ok
	    end,
	    gen_xdr_base(Fd, Base, Spec, Type),
	    file:close(Fd);
	{error, Reason} ->
	    io:format("WARNING: could not open ~s for write~n", [File]),
	    {error, Reason}
    end.

gen_xdr_base(Fd, Base, Spec, Type) ->
    if Type == mod ->
	    foreach(
	      fun({type,Id,_}) ->
		      io:format(Fd, "-export([enc_~s/1, dec_~s/2]).~n",[Id,Id]);
		 (_) ->
		      true
	      end, Spec);
       Type == inc ->
	    ok
    end,
    put(type_module, []),
    foreach(
      fun({type,Id,Type}) ->
	      [Enc] = xdrgen:encode({type,Id,Type}, []),
	      emit_fun(Fd, Enc),
	      foreach(fun(Dec) -> emit_fun(Fd, Dec) end,
		      xdrgen:decode({type,Id,Type}, []));
	 (_) ->
	      true
      end, Spec),
    gen_map_elem(get(map_elem), Fd),
    gen_io_list_len(get(io_list_len), Fd),
    gen_enc_align(get(enc_align), Fd),
    gen_align(get(align), Fd).


gen_map_elem(true, Fd) ->
    io:format(Fd,
	      "\nmap_elem(Fun, Bin, Off, infinity, N) ->\n"
	      "  map_elem0(Fun, Bin, Off, N, []);\n"
	      "map_elem(Fun, Bin, Off, Max, N) when N =< Max ->\n"
	      "  map_elem0(Fun, Bin, Off, N, []).\n"
	      "\n"
	      "map_elem0(Fun, Bin, Off, 0, L) ->\n"
	      "  {lists:reverse(L,[]), Off};\n"
	      "map_elem0(Fun, Bin, Off, N, L) ->\n"
	      "  {E,Off1} = Fun(Bin, Off),\n"
	      "map_elem0(Fun, Bin, Off1, N-1, [E|L]).\n", []);
gen_map_elem(_, _Fd) ->
    ok.

gen_io_list_len(true, Fd) ->
    io:format(Fd,
	      "\nio_list_len(L) -> io_list_len(L, 0).\n"
	      "io_list_len([H|T], N) ->\n"
	      "  if\n"
	      "    H >= 0, H =< 255 -> io_list_len(T, N+1);\n"
	      "    list(H) -> io_list_len(T, io_list_len(H,N));\n"
	      "    binary(H) -> io_list_len(T, size(H) + N);\n"
	      "    true -> exit({xdr, opaque})\n"
	      "  end;\n"
	      "io_list_len(H, N) when binary(H) ->\n"
	      "  size(H) + N;\n"
	      "io_list_len([], N) ->\n"
	      "N.\n", []);
gen_io_list_len(_, _Fd) ->
    ok.


gen_enc_align(true, Fd) ->
    io:format(Fd,
	      "\nenc_align(Len) ->\n"
	      "  case Len rem 4 of\n"
	      "    0 -> <<>>;\n"
	      "    1 -> <<0,0,0>>;\n"
	      "    2 -> <<0,0>>;\n"
	      "    3 -> <<0>>\n"
	      "  end.\n", []);
gen_enc_align(_, _Fd) ->
    ok.

gen_align(true, Fd) ->
    io:format(Fd,
	      "\nalign(Len) ->\n"
	      "  case Len rem 4 of\n"
	      "    0 -> Len;\n"
	      "    1 -> Len+3;\n"
	      "    2 -> Len+2;\n"
	      "    3 -> Len+1\n"
	      "  end.\n", []);
gen_align(_, _Fd) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% Generate CLNT File
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_clnt(Base, Spec, Env, Opts) ->
    Module = Base ++ "_clnt",
    File = Module ++ ".erl",
    case file:open(File, write) of
	{ok, Fd} ->
	    gen_header(Fd, Module),
	    io:format(Fd, "-module(~s).~n", [Module]),
	    case lists:member(trace, Opts) of
		true -> io:format(Fd, "-compile([verbose, report_errors, report_warnings, trace]).~n", []);
		_ -> skip_it
	    end,
	    io:format(Fd, "-include(\"~s\").~n", [Base ++ ".hrl"]),
	    gen_clnt(Fd, Base, Spec),
	    file:close(Fd);
	{error, Reason} ->
	    io:format("WARNING: could not open ~s for write~n", [File]),
	    {error, Reason}
    end.

gen_clnt(Fd, Base, Spec) ->
    foreach(
      fun ({program,_,Prog,Vs}) ->
	      foreach(
		fun({version,_,Ver,Ps}) ->
			foreach(
			  fun({procedure,Id,Proc,_,As}) ->
				  Call = genname(Id,Ver),
				  io:format(Fd, "-export([~s/~w,~s/~w]).\n",
					    [Call,length(As)+1,
					     Call,length(As)+2])
			  end, Ps)
		end, Vs);
	  (_) -> 
	      true
      end, Spec),
    put(type_module, list_to_atom(concat([Base,"_xdr"]))),
    foreach(
      fun ({program,Id,Prog,Vers}) ->
	      Es = xdrgen:clnt({program,Id,Prog,Vers}, []),
	      foreach(fun(F) -> emit_fun(Fd, F) end, Es);
	  (_) ->
	      true
      end, Spec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%  Generate SVC
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_svc(Base, Spec, Env, Opts, Type) ->
    Module = Base ++ "_svc",
    File = Module ++ ".erl",
    case file:open(File, write) of
	{ok, Fd} ->
	    gen_header(Fd, Module),
	    io:format(Fd, "-module(~s).~n", [Module]),
	    case lists:member(trace, Opts) of
		true -> io:format(Fd, "-compile([verbose, report_errors, report_warnings, trace]).~n", []);
		_ -> skip_it
	    end,
	    io:format(Fd, "-include(\"~s\").~n", [Base ++ ".hrl"]),
	    gen_svc(Fd, Base, Spec, Type),
	    file:close(Fd);
	{error, Reason} ->
	    io:format("WARNING: could not open ~s for write~n", [File]),
	    {error, Reason}
    end.
%%
%% Generate:
%%   prog_ver1(Args) ->
%%        call prog_ver1:init(Args) -- {reply, Reply, State0}
%%   prog_ver1(Reason,StateN) ->
%%        call prog_ver1:terminate(Reason, StateN) -- ignore reply
%%
%%   prog_ver1(Proc,Params,Client) -> ...
%%
%%   prog_ver2(Proc,Params,Client) -> ...
%%   ...   
%%   prog_verN(Proc,Params,Client) -> ...
%%   ...
%%
%% If Type == gen_server:
%%   implementation gen_server must be locally registred as:
%%        prog_server
%% 
%%   calls are made like:
%%      gen_server:call(prog_server, {proc_1}, infinity)
%%      gen_server:call(prog_server, {proc_2, Args}, infinity)
%%      gen_server:call(prog_server, {proc_2, Args}, infinity)
%%
%% If Type == rpc_server:
%%
%%   implementation module must be called:
%%        prog_server
%%   calls are made like:
%%      prog_server(proc_1, Clnt, S)
%%      prog_server(proc_2, Args..., Clnt, S)
%%      prog_server(proc_3, Args..., Clnt, S)
%%

gen_svc(Fd, Base, Spec, Type) ->
    foreach(
      fun ({program,Id,Prog,Vs}) ->
	      foreach(
		fun({version,_,Ver,Ps}) ->
			ProgN = genname(Id,Ver),
			io:format(Fd, "-export([~s/5]).\n", [ProgN])
		end, Vs);
	  (_) -> 
	      true
      end, Spec),
    io:format(Fd, "-export([init/1, handle_call/3, handle_cast/2, \n"
	          "         handle_info/2, terminate/2]).\n", []),
    put(type_module, list_to_atom(concat([Base,"_xdr"]))),
    Fs = xdrgen:svc_gen_funcs(Type, Base, []),
    foreach(fun(F) -> emit_fun(Fd, F) end, Fs),
    foreach(
      fun ({program,Id,Pn,Prog}) ->
	      Es = xdrgen:svc_prog({program,Id,Pn,Prog}, Type, Base, []),
	      foreach(fun(F) -> emit_fun(Fd, F) end, Es);
	  (_) ->
	      true
      end, Spec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% x_xdr.hrl
%% INCLUDES:
%%       defines for RPC procedure numbers.
%%       data type records?
%% 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_hrl(Base, Spec, Env, Opts) ->
    File = Base ++ ".hrl",
    case file:open(File, write) of
	{ok, Fd} ->
	    gen_header(Fd, File),
	    gen_hrl(Fd, Base, Spec, Env, Opts),
	    file:close(Fd);
	{error, Reason} ->
	    io:format("WARNING: could not open ~s for write~n", [File]),
	    {error, Reason}
    end.
%%
%% emit all constant as -define(CONSTANT, Value).
%%
gen_hrl(Fd, Base, Spec, Env, Opts) ->
    foreach(
      fun ({program,Pid,Prog,Vs}) ->
	      io:format(Fd, "-define(~s, ~w).~n",[Pid,Prog]),
	      foreach(
		fun({version,Vid,Ver,Ps}) ->
			io:format(Fd, "-define(~s, ~w).~n",[Vid,Ver])
		end, Vs);
	  (_) -> 
	      true
      end, Spec),
    %% emit define and record. (unions?)
    foreach(
      fun({Id,const,Value}) ->
	      io:format(Fd, "-define(~s, ~w).\n",[Id,Value]);
%% FIXME: think this over - these are not used, the xdr routines can't handle
%% these records.
%	 ({Id,type,{struct,[{Eid1,T1} | Es]}}) ->
%	      io:format(Fd, "\n-record(~s,\n\t{\n\t ~s", [Id, Eid1]),
%	      foreach(
%		fun({Eid,T}) ->
%			io:format(Fd, ",\n\t ~s", [Eid])
%		end, Es),
%	      io:format(Fd, "\n\t}).\n\n", []);
	 (_) -> ok
      end, Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%  Generate SVC Stubb file(s)
%%  one for each program version.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_stub(Base, Spec, Env, Opts) ->
    nyi.

%%
%% Transform + Verify
%%
error(Line,Fmt,As) ->
    Mess = flatten(io_lib:format(Fmt, As)),
    io:format("~s:~w: error : ~s~n", [get(infile), Line, Mess]),
    put(errors, get(errors)+1).

%% format a type
fmt_type(int) -> "int";
fmt_type(unsigned_int) -> "unsigned int";
fmt_type(hyper) -> "hyper";
fmt_type(unsigned_hyper) -> "unsigned hyper";
fmt_type(bool) -> "bool";
fmt_type(void) -> "void";
fmt_type(float) -> "float";
fmt_type(double) -> "double";
fmt_type(string) -> "string";
fmt_type(opaque) -> "opaque";
fmt_type({struct,Elems}) -> "struct ...";
fmt_type({union,{DT,Elems}}) -> "union ...";
fmt_type({optional,T})   -> fmt_flatt("*~s", [fmt_type(T)]);
fmt_type({varray,infinity,T}) -> fmt_flatt("~s<>", [fmt_type(T)]);
fmt_type({varray,N,T})   -> fmt_flatt("~s<~w>", [fmt_type(T),N]);
fmt_type({array,N,T})    -> fmt_flatt("~s[~w]", [fmt_type(T),N]);
fmt_type({type,Id}) -> Id.

fmt_flatt(Fmt, Args) ->
    flatten(io_lib:format(Fmt, Args)).
%%
%% 1 remove line numbers.
%% 2 check errors.
%%
    
transform(Spec, File) ->
    put(infile, File),
    put(errors, 0),
    {Spec1,Env} = trans(Spec),
    case get(errors) of
	0 -> {ok, Spec1, Env};
	N -> error
    end.

%%
%% Verify some aspects of specs
%%
trans(Spec) ->
    trans(Spec, [], []).

trans([{const,Line,Id,Value} | Fs], S0, Env) ->
    case lookup(Id, Env) of
	false -> 
	    trans(Fs, S0, insert(Id,const,Value,Env));
	_ ->
	    error(Line, "identifier ~s multiply defined", [Id]),
	    trans(Fs, S0, Env)
    end;
trans([{typedef,Line,Id,Type} | Fs], S0, Env) ->
    case lookup(Id, Env) of
	false ->
	    Env1 = insert(Id,type,current,Env),
	    Type1 = trans_type(Type, Env1),
	    trans(Fs, [{type,Id,Type1}|S0], insert(Id,type,Type1,Env));
	{Id,type,T} ->
	    error(Line, "type ~s multiply defined", [Id]),
	    trans(Fs, S0, Env);
	_ ->
	    error(Line, "identifier ~s multiply defined", [Id]),
	    trans(Fs, S0, Env)
    end;
trans([{program,Line,Id,ProgNo,Vers} | Fs], S0, Env) ->
    case lookup(Id, Env) of
	false ->
	    Prog1 = trans_vers(Vers, Env),
	    trans(Fs, [{program,Id,ProgNo,Prog1}|S0],
		  insert(Id,program,{ProgNo,Prog1},Env));
	{_,_,_} ->
	    error(Line, "identifier ~s multiply defined", [Id]),
	    trans(Fs, S0, Env)
    end;
trans([], S0, Env) ->
    { reverse(S0), Env }.


trans_vers([{version,Line,Id,VersNo,Procs} | Vs], Env) ->
    Procs1 = trans_procs(Procs, Env),
    [{version,Id,VersNo,Procs1} | trans_vers(Vs,Env)];
trans_vers([], Env) -> [].

trans_procs([{procedure,Line,Id,ProcNo,Ret,Args} | Ps], Env) ->
    Ret1 = trans_type(Ret, Env),
    Args1 = trans_type_list(Args, Env),
    [{procedure,Id,ProcNo,Ret1,Args1} | trans_procs(Ps, Env)];
trans_procs([], Env) -> [].

trans_type_list([T | Ts], Env) ->
    [ trans_type(T, Env) | trans_type_list(Ts,Env)];
trans_type_list([], Env) -> [].


trans_type({struct, _, Elems}, Env) ->
    {struct, trans_struct_elems(Elems, [], Env)};
trans_type({union, Line, {{Did,DLine,Disc}, Elems}}, Env) ->
    Disc1 = trans_type(Disc,Env),
    DT = trans_disc_type(Disc1, Line, Env),
    Elems1 = trans_union_elems(Elems, [], [], [], Env, DT),
    {union, {{Did,DT},Elems1}};
trans_type({enum, _, Enums}, Env) ->
    {enum, trans_enum(Enums, Env)};
trans_type({array, Line, N, Type}, Env) ->
    N1 = trans_value(N, Env),
    if N1 < 0 ->  error(Line, "bad array size ~w", [N]);
	true -> true
    end,
    Type1 = 
	if Type == opaque -> Type;
	    true -> trans_type(Type, Env)
	end,
    {array, N1, Type1};

trans_type({varray,Line,Max,Type}, Env) ->
    Max1 = if
	       Max == infinity ->
		   infinity;
	       true -> 
		   Max2 = trans_value(Max, Env),
		   if Max2 < 0 ->
			   error(Line, "bad dynamic array max ~w", [Max]);
		       true -> true
		   end,
		   Max2
	   end,
    Type1 = if
		Type == opaque -> opaque;
		Type == string -> string;
		true -> trans_type(Type, Env)
	    end,
    {varray, Max1, Type1};
trans_type({int,_}, Env) -> int;
trans_type({unsigned_int,_},Env) -> unsigned_int;
trans_type({hyper,_},Env) -> hyper;
trans_type({unsigned_hyper,_},Env) -> unsigned_hyper;
trans_type({float,_},Env) -> float;
trans_type({double,_},Env) -> double;
trans_type({bool,_},Env) -> bool;
trans_type({void,_},Env) -> void;
trans_type({type,Line,Id}, Env) when list(Id) ->
    case lookup(Id, Env) of
	{_,type,Type1} -> true;
	_ -> error(Line, "type ~s undefined", [Id])
    end,
    {type, Id};
trans_type({optional,Line,Type}, Env) ->
    {optional, trans_type(Type, Env)}.

%% trans_type({optional,Line,Type}, Env) ->
%%    Type1 = trans_type(Type, Env),
%%    {union, {{"$opted", bool}, 
%%	     [{ {true, 1}, {"$element", Type1}},
%%	      { {false,0}, {"", void} }]}}.
    
%%
%% Check struct elements
%%
trans_struct_elems([{Id, Line, Type} | Elems], Ids, Env) ->
    Type1 = trans_type(Type, Env),
    case member(Id, Ids) of
	true -> 
	    error(Line, "struct member ~s multiply defined", [Id]),
	    [{Id,Type1} | trans_struct_elems(Elems, Ids, Env)];
	false ->
	    [{Id,Type1} | trans_struct_elems(Elems, [Id|Ids], Env)]
    end;
trans_struct_elems([], _, _) -> [].

%%
%% Check union elements
%% The Tag is translated as Tag -> {Tag,Value} 
%%
trans_union_elems([{Tag,Line,{Id,_,Type}} | Elems],Tags,Ids,Vals,Env,Disc) ->
    Tag1 = trans_tag(Tag),
    case member(Tag1, Tags) of
	true -> error(Line, "union tag ~w multiply defined", [Tag1]);
	false -> true
    end,
    Ids1 = 
	if Id == [] -> Ids;
	    true ->
		case member(Id, Ids) of
		    true -> error(Line, "union id ~s multiply defined", [Id]),
			    Ids;
		    false -> [Id | Ids]
		end
	end,
    Type1 = trans_type(Type, Env),
    Tag2 = {_,Val} = trans_tag_type(Tag1, Line, Env, Disc),
    Vals1 = case member(Val, Vals) of
		true -> error(Line, "case ~w multiply defined", [Val]), Vals;
		false -> [Val | Vals]
	    end,
    [{ Tag2, {Id, Type1}} | 
     trans_union_elems(Elems, [Tag|Tags], Ids1, Vals1, Env, Disc)];
trans_union_elems([], _, _, _, _,_) -> [].

trans_tag({identifier,_,Id}) -> Id;
trans_tag({integer,_,Value}) -> Value;
trans_tag(default) -> default.


%%
%% Translate Tag into -> {Tag,Value}
%%
trans_tag_type("TRUE", _, _, bool) ->   {true,1};
trans_tag_type("FALSE", _, _, bool) -> {false,0};
trans_tag_type(N, _, _, int) when integer(N) -> {N,N};
trans_tag_type(N, Line, _, unsigned_int) when integer(N) ->
    if N < 0 -> error(Line, "bad tag ~w for unsigned type", [N]);
	true -> true
    end,
    {N,N};
trans_tag_type(default, _, _, _) -> {default,default};
trans_tag_type(Tag, Line,_,{enum,Nums}) ->
    case keysearch(Tag, 1, Nums) of
	{value,{_,Value}} -> 
	    {Tag,Value};
	false ->
	    error(Line, "tag ~p is not an enumerated value", [Tag]),
	    {Tag,0}
    end;
trans_tag_type(Tag,Line,Env,{type,Id}) ->
    {_, type, T} = lookup(Id, Env),
    trans_tag_type(Tag,Line,Env,T);
trans_tag_type(_, _, _, _) -> {0,0}.


trans_disc_type(int, _, Env) -> int;
trans_disc_type(unsigned_int, _, Env) -> unsigned_int;
trans_disc_type(bool, _, Env) -> bool;
trans_disc_type({enum,Nums}, _, Env) ->  {enum,Nums};
trans_disc_type({type,Id}, Line, Env) ->
    case lookup(Id, Env) of
	{_,type,T} ->
	    trans_disc_type(T, Line, Env),
	    {type,Id};
	false ->
	    error(Line, "type ~s undefined", [Id]),
	    int
    end;
trans_disc_type(T, Line, Env) ->
    error(Line, "type ~s is not a valid discriminator type", [fmt_type(T)]).

%%
%% Check uniqness if enums (1-1)
%% i.e exactly one of each identifier
%%     exactly one of each value
%%
trans_enum(Enums, Env) ->
    trans_enums(Enums, Env, []).

trans_enums([{Tag,Line,Value} | Es], Env, Ids) ->
    V = trans_value(Value, Env),
    case member(Tag, Ids) of
	true -> error(Line, "enumeration ~s multiply defined", [Tag]);
	false -> true
    end,
    [{Tag,V} | trans_enums(Es, Env, [Tag|Ids])];
trans_enums([], _, _) -> [].


trans_value({integer,_,Value}, Env) -> Value;
trans_value({identifier,Line,Id}, Env) ->
    case lookup(Id, Env) of
	{_,const,Value} -> Value;
	false -> error(Line, "constant ~s undefined", [Id]), 0
    end.

insert(Id,Type,Value,Env) ->
    [{Id,Type,Value} | Env].

lookup(Id, [{Id,Type,Value}|_]) -> {Id,Type,Value};
lookup(Id, [_|Env]) -> lookup(Id, Env);
lookup(Id, []) -> false.
