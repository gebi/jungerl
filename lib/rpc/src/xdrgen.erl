%% Copyright (c) 2000 Sendmail, Inc.  All rights reserved.
%%
%% Xdr code generator
%%
-module(xdrgen).

-export([encode/2, decode/2]).
-export([clnt/2, svc_gen_funcs/3, svc_prog/4]).

-import(lists, [map/2, foldl/3, foldr/3, reverse/1, concat/1]).


-compile(export_all).

-record(grec, 
	{
	 var = 1    %% variable number for temporary variables
	}).

%%
%% Build abstract forms
%%
mkfun(CL) ->                    {'fun',0,{clauses,CL}}.
mkcall(F, As) ->
    case get(type_module) of
	undefined -> {call,0,mkatom(F),As};
	[] ->        {call,0,mkatom(F),As};
	Mod ->       {call,0,{remote,0,mkatom(Mod),mkatom(F)},As}
    end.
mklocalcall(F, As) ->
    {call,0,mkatom(F),As}.
mkcatch(Expr) ->                {'catch',0,Expr}.
mkcall(M, F, As) ->             {call,0,{remote,0,mkatom(M),mkatom(F)}, As}.
mkcase(Expr,CL) ->              {'case',0,Expr,CL}.
mkif(CL) ->                     {'if',0,CL}.
mkvar(V) ->                     {var,0,V}.
mkatom(A) when atom(A) ->       {atom,0,A};
mkatom(A) when list(A) ->       {atom,0,list_to_atom(A)}.
mkint(X) when integer(X) ->     {integer,0,X}.
mkfloat(X) when float(X) ->     {float,0,X}.
mkop(Op,L,R) ->                 {op,0,Op,L,R}.
mkop(Op,X) ->                   {op,0,Op,X}.
mkclause(H,G,B) ->              {clause,0,H,G,B}.
mktuple(T)  ->                  {tuple,0,T}.
mkcons(H,T) ->                  {cons,0,H,T}.
mknil() ->                      {nil,0}.
mklist(L) ->                    foldr(fun mkcons/2, mknil(), L).
mkmatch(L,R) ->                 {match,0,L,R}.
mkblock(Es) ->                  {block,0,Es}.
mkmodule(M) ->                  {attribute,0,module,M}.
mkexport(F,A) ->                {attribute,0,export,[{F,A}]}.
mkfunction(Name,Arity,CL) ->    {function,0,Name,Arity,CL}.

mkbin(Elems) ->                 {bin, 0, Elems}.
mkbinelem(Expr, Sz, Types) ->   {bin_element, 0, Expr, Sz, Types}.

%% #Name { f1 = v1, ... fn = vn }
mkrecord(Name, Fs)  -> {record,0,Name,Fs}.
%%  Rec#Name { f1 = v1, ... fn = vn }
mkrecord(Rec, Name, Fs)      -> {record,0,Rec,Name,Fs}.
%% Rec#Name.F
mkrecord_field(Rec, Name, F) -> {record_field,0,Rec,Name,F}.
%% #Rec.F
mkrecord_field(Rec, F) -> {record_field,0,Rec,F}.

%% #Name.field (generate tuple index)
mkrecord_index(Name, F) -> {record_index, 0, Name, F}.

%% f or f = value
mkfield(F, Value)  -> {record_field,0,mkatom(F),Value}.
mkfield(F)         ->  {record_field,0,mkatom(F)}.

mkint2(infinity) -> mkatom(infinity);
mkint2(Int) -> mkint(Int).
    
%% Common constructs...     
mkbinoff(Off) -> mkbinelem(mkvar('_'), Off, [binary]).
mkbintail() -> mkbinelem(mkvar('_'), default, [binary]).
     
mkexitlimit() ->
    mkcall(exit, [mktuple([mkatom(xdr), mkatom(limit)])]).

genvar(R) ->
    X = R#grec.var,
    {mkvar(X), R#grec{ var = X + 1}}.


tolower([C|Cs]) when C >= $A, C =< $Z ->
    [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs]) -> [C | tolower(Cs)];
tolower([]) -> [].

genname(Name,Vi) ->
    list_to_atom(concat([tolower(Name), "_", Vi])). 

%% ------------------------------------------------------------------------
%%
%%                         ENCODE
%% 
%% ------------------------------------------------------------------------

%%
%% Encode function:
%% enc_Id(V) -> enc_T(V).
%%
encode({type,Id,Type}, Fs0) ->
    FName = list_to_atom("enc_" ++ Id),
    {V0, R0} = genvar(#grec{}),
    {Enc, R1} = enc_type(Type, V0, R0),
    [ mkfunction(FName, 1, [mkclause([V0], [], [Enc])]) | Fs0].

%% <<V:32>>
enc_prim_type(int, V, R) ->
    {mkbin([mkbinelem(V, mkint(32), default)]), R};
enc_prim_type(unsigned_int, V, R) ->
    enc_prim_type(int, V, R);
%% <<V:64>>
enc_prim_type(hyper, V, R) ->
    {mkbin([mkbinelem(V, mkint(64), default)]), R};
enc_prim_type(unsigned_hyper, V, R) ->
    enc_prim_type(hyper, V, R);
%% <<V:32/float>>
enc_prim_type(float, V, R) ->
    {mkbin([mkbinelem(V, mkint(32), [float])]), R};
%% <<V:64/float>>
enc_prim_type(double, V, R) ->
    {mkbin([mkbinelem(V, mkint(64), [float])]), R};

%% if V == true -> <<1:32>>;
%%    V == false -> <<0:32>>;
%% end
enc_prim_type(bool, V, R) ->
    {mkif([mkclause([],
		   [mkop('==', V, mkatom(true))],
		   [mkbin([mkbinelem(mkint(1), mkint(32), default)])]),
	   mkclause([],
		    [mkop('==', V, mkatom(false))],
		    [mkbin([mkbinelem(mkint(0), mkint(32), default)])])]),
     R};

%% case io_list_len(V) of
%%   N -> V;  % when N rem 4 == 0
%%   N -> [V, <<0,...>>]; % when alignement is needed
%%   _ -> exit({xdr, limit})
%% end
enc_prim_type({array, N, opaque}, V, R) ->
    put(io_list_len, true),
    Ret = if (N rem 4) == 0 -> V;
	     true ->
		  mklist([V, enc_align(N)])
	  end,
    {mkcase(mkcall(io_list_len, [V]),
	    [mkclause([mkint(N)],
		      [],
		      [Ret]),
	     mkclause([mkvar('_')],
		      [],
		      [mkexitlimit()])]),
     R};

%% If Max /= infinity:
%% begin
%%   Sz = io_list_len(V),
%%   if Sz =< Max -> [<<Sz:32>>, V, enc_align(Sz)];
%%	true -> exit({xdr, limit})
%%   end
%% end
%% 
%% If Max == infinity:
%% begin
%%   Sz = io_list_len(V),
%%   [<<Sz:32/unsigned>>, V, enc_align(Sz)]
%% end
enc_prim_type({varray, Max, opaque}, V, R) ->
    put(enc_align, true),
    put(io_list_len, true),
    {Sz, R1} = genvar(R),
    Ret = mklist([mkbin([mkbinelem(Sz, mkint(32), [unsigned])]),
		  V,
		  mkcall(enc_align, [Sz])]),
    if Max == infinity -> 
	    {mkblock([mkmatch(Sz, mkcall(io_list_len, [V])),
		      Ret]), R1};
       true ->
	    {mkblock([mkmatch(Sz, mkcall(io_list_len, [V])),
		      mkif([mkclause([], [mkop('=<', Sz, mkint(Max))], [Ret]),
			    mkclause([], [mkatom(true)], [mkexitlimit()])])]),
	     R1}
    end;
enc_prim_type({varray,Max,string}, V, R) ->
    enc_prim_type({varray, Max, opaque}, V, R).


enc_type(int, V, R) ->
    enc_prim_type(int, V, R);
enc_type(unsigned_int, V, R) ->
    enc_prim_type(unsigned_int, V, R);
enc_type(hyper, V, R) ->
    enc_prim_type(hyper, V, R);
enc_type(unsigned_hyper, V, R) ->
    enc_prim_type(unsigned_hyper, V, R);
enc_type(float, V, R) ->
    enc_prim_type(float, V, R);
enc_type(double, V, R) ->
    enc_prim_type(double, V, R);
enc_type(bool,V,R) ->
    enc_prim_type(bool, V, R);

enc_type(void,V,R) ->
    { mknil(),  R};
enc_type({type,Id}, V, R) ->
    { mkcall("enc_" ++ Id, [V]), R};
%%
%% enums:
%% case V of
%%   Tag1 -> Val1;
%%   Tag2 -> Val2;
%%   ..
%%   TagN -> Valn
%% end
%%
enc_type({enum,Nums},V,R) ->
    CL = map(
	   fun({Tag,Val}) ->
		   mkclause([mkatom(Tag)],[],
			    [mkbin([mkbinelem(mkint(Val),mkint(32), default)])])
	   end, Nums),
    {mkcase(V, CL), R};
    
%%
%% array:
%% if length(V) == N ->
%%     map(fun(X) -> enc_T(X) end, V)
%% end
%%
enc_type({array,N,opaque}, V, R) ->
    enc_prim_type({array,N,opaque}, V, R);

enc_type({array,N,Type}, V, R) ->
    {V1,R1} = genvar(R),
    {E1,R2} = enc_type(Type,V1,R1),
    CL = [mkclause([V1], [], [E1])],
    E3 = mkcall(lists, map, [mkfun(CL), V]),
    E4 = mkif([mkclause([],
			[mkop('==',
			      mkcall(length,[V]),
			      mkint(N))],
			[ E3 ])]),
    { E4, R2};
%%
%% varaible array
%%
enc_type({varray,Max,opaque}, V, R) ->
    enc_prim_type({varray,Max,opaque}, V, R);
enc_type({varray,Max,string}, V, R) ->
    enc_prim_type({varray,Max,string}, V, R);


%% varray:
%% If Max == infinity:
%% begin
%%   Len = length(V),
%%   [<<Len:32>>, map(fun(X) -> enc_T(X) end, V)]
%% end
%%
%% otherwise:
%%
%% begin
%%   Len = length(V),
%%   if Len when Len =< N ->
%%        [<<Len:32/unsigned>>,
%%         map(fun(X) -> enc_T(X) end, V)];
%%      true -> exit({xdr, limit}
%%   end
%% end
%%
enc_type({varray,Max,Type}, V, R0) ->
    {V1, R1} = genvar(R0),
    {Len, R2} = genvar(R1),
    {E1, R3} = enc_type(Type,V1,R2),
    Match = mkmatch(Len, mkcall(length, [V])),
    E2 = mkbin([mkbinelem(Len, mkint(32), [unsigned])]),
    CL = [mkclause([V1], [], [E1])],
    E3 = mkcall(lists, map, [mkfun(CL), V]),
    E4 = case Max of
	     infinity ->
		 mkblock([Match, mklist([E2, E3])]);
	     M when integer(M) ->
		 mkblock([Match,
			  mkif([mkclause([],
					 [mkop('=<', Len, mkint(M))],
					 [mklist([E2, E3])]),
				mkclause([],
					 [mkatom(true)],
					 [mkexitlimit()])])])
	 end,
    {E4, R3};

%% structures are encoded recursively for each member as:
%%
%% case V of
%%  {E1,E2,...,En} ->
%%      [enc_T1(E1), enc_T2(E2), ..., enc_T3(E3)]
%% end
%%
enc_type({struct, Elems}, V, R) ->
    {EL,VL,R1} = foldr(
		   fun({Id,T}, {Enc0, VL, RR0}) ->
			   {VV,RR1} = genvar(RR0),
			   {Enc1,RR2} = enc_type(T, VV, RR1),
			   {mkcons(Enc1,Enc0), [VV|VL], RR2}
		   end, {mknil(),[],R}, Elems),
    { mkcase(V, [mkclause([mktuple(VL)], [], [EL])]), R1};

%%
%% union are encoded recursively for each arm as:
%% case V of
%%  {Tag, V1} ->
%%    [enc_T(Tag),
%%     case Tag of
%%        Tag1 -> enc_T1(V);
%%        Tag2 -> enc_T2(V);
%%        Tag3 -> enc_T3(V);
%%        _    -> enc_Td(V)
%%     end]
%% end.
%%
enc_type({union, {{DId,DT}, Elems}}, V, R) ->
    {V0,R1} = genvar(R),
    {V1,R2} = genvar(R1),
    {DEnc,R3} = enc_type(DT, V0, R2),
    {CL1,R4} = foldr(
		 fun ({{default,_},{_,T}}, {CL0, R0}) ->
			 {Enc,RR} = enc_type(T, V1, R0),
			 {[mkclause([mkvar('_')],[],[Enc]) | CL0], RR};
		     ({{Tag,Val},{Uid,T}}, {CL0, R0}) ->
			 {Enc,RR} = enc_type(T, V1, R0),
			 ETag =
			     if
				 integer(Tag) -> mkint(Tag);
				 true -> mkatom(Tag)
			     end,
			 {[mkclause([ETag],[],[Enc]) | CL0], RR}
		 end, {[],R3}, Elems),
    Case1 = mkcase(V0, CL1),
    CL2 = [mkclause([mktuple([V0, V1])],
		    [], [mklist([DEnc, Case1])])],
    { mkcase(V, CL2), R4};
%%
%% void is used to mark that data is not present !!!
%%
%% Special case of union:
%% case V of
%%    void -> <<0:32>>;
%%    _    -> [<<1:32>>, enc_type(V)]
%% end.
%%
enc_type({optional,Type}, V0, R0) ->
    {BF, R1} = enc_prim_type(int, mkint(0), R0),
    {BT, R2} = enc_prim_type(int, mkint(1), R1),
    C1 = mkclause([mkatom(void)], [], [BF]),
    {Enc,R3} = enc_type(Type, V0, R2),
    C2 = mkclause([mkvar('_')], [], [mklist([BT, Enc])]),
    { mkcase(V0, [C1,C2]), R3}.


%% ------------------------------------------------------------------------
%%
%%                         DECODE
%% 
%% ------------------------------------------------------------------------

%% generate decoder of type or program.
%%
%% Note on variable names.  The I_xxx variable names are used to denote
%% in-parameters to the generated function.  O_xxx denotes out-parameters.
%% Generates a function dec_xxx(I_Bin, I_Off) -> {Val, O_NOff}
decode({type,Id,Type}, Fs0) ->
    FName = list_to_atom("dec_" ++ Id),
    {I_Bin, R0} = genvar(#grec{}),
    {I_Off, R1} = genvar(R0),
    {Dec, R2} = dec_type(Type,I_Bin,I_Off,R1),
    F1 = mkfunction(FName, 2, [mkclause([I_Bin, I_Off], [], [Dec])]),
    case Type of
	{enum, Nums} ->
	    FName2 = list_to_atom("dec_" ++ Id ++ "_i2a"),
	    {I_Int, R3} = genvar(R2),
	    {Dec2, R4} = dec_enum_i2a(Nums, I_Int, R3),
	    F2 = mkfunction(FName2, 1, [mkclause([I_Int], [], [Dec2])]),
	    [F1, F2 | Fs0];
	_ ->
	    [F1 | Fs0]
    end.

is_prim_dec_p(int)                -> true;
is_prim_dec_p(unsigned_int)       -> true;
is_prim_dec_p(hyper)              -> true;
is_prim_dec_p(unsigned_hyper)     -> true;
is_prim_dec_p(float)              -> true;
is_prim_dec_p(double)             -> true;
is_prim_dec_p(bool)               -> true;
is_prim_dec_p({array, N, opaque}) -> true;
is_prim_dec_p(_) -> false.
     

%% <<_:I_Off/binary, Val:Sz/Sign, _/binary>> = B,
dec_prim_int(I_Bin, I_Off, O_Val, Sz, Sign, R) ->
    {mkmatch(mkbin([mkbinoff(I_Off),
		    mkbinelem(O_Val, mkint(Sz), [Sign]),
		    mkbintail()]),
	     I_Bin),
     Sz div 8,
     R}.

%% <<_:I_Off/binary, Val:Sz/float, _/binary>> = B,
dec_prim_float(I_Bin, I_Off, O_Val, Sz, R) ->
    {mkmatch(mkbin([mkbinoff(I_Off),
		    mkbinelem(O_Val, mkint(Sz), [float]),
		    mkbintail()]),
	     I_Bin),
     Sz div 8,
     R}.

dec_prim_type(int, I_Bin, I_Off, O_Val, R) ->
    dec_prim_int(I_Bin, I_Off, O_Val, 32, signed, R);
dec_prim_type(unsigned_int, I_Bin, I_Off, O_Val, R) ->
    dec_prim_int(I_Bin, I_Off, O_Val, 32, unsigned, R);
dec_prim_type(hyper, I_Bin, I_Off, O_Val, R) ->
    dec_prim_int(I_Bin, I_Off, O_Val, 64, signed, R);
dec_prim_type(unsigned_hyper, I_Bin, I_Off, O_Val, R) ->
    dec_prim_int(I_Bin, I_Off, O_Val, 64, unsigned, R);
dec_prim_type(float, I_Bin, I_Off, O_Val, R) ->
    dec_prim_float(I_Bin, I_Off, O_Val, 32, R);
dec_prim_type(double, I_Bin, I_Off, O_Val, R) ->
    dec_prim_float(I_Bin, I_Off, O_Val, 64, R);

%% begin
%%   <<_:Off/binary, Bool:32/unsigned, _/binary>> = B,
%%   Val = if Bool == 0 -> false;
%%            Bool == 1 -> true
%%         end,
%% end
dec_prim_type(bool, I_Bin, I_Off, O_Val, R0) ->
    {Bool, R1} = genvar(R0),
    {mkblock([mkmatch(mkbin([mkbinoff(I_Off),
			     mkbinelem(Bool, mkint(32), [unsigned]),
			     mkbintail()]),
		      I_Bin),
	      mkmatch(O_Val,
		      mkif([mkclause([],
				     [mkop('==', Bool, mkint(0))],
				     [mkatom(false)]),
			    mkclause([],
				     [mkop('==', Bool, mkint(1))],
				     [mkatom(true)])]))]),
     4,
     R1};

%% opaque[N] : N is the fixed size length
%% 
%% <<_:Off/binary, Val:N/binary, _/binary>> = B,
%% NOff = Off + ?align(N)).
dec_prim_type({array, N, opaque}, I_Bin, I_Off, O_Val, R) ->
    {mkmatch(mkbin([mkbinoff(I_Off),
		    mkbinelem(O_Val, mkint(N), [binary]),
		    mkbintail()]),
	     I_Bin),
     align(N),
     R}.
				   				     

%%-----------------------------------------------------------------
%% Func: dec_type(Type, I_Bin, I_Off, R) -> {DecExpr, R'}
%% Types: 
%% Purpose: DecExpr is an expression which decodes the type,
%%          and returns {O_Val, O_NOff}
%%-----------------------------------------------------------------
dec_type(Type, I_Bin, I_Off, R0) ->
    case is_prim_dec_p(Type) of
	true ->
	    {O_Val, R1} = genvar(R0),
	    {Dec, Sz, R2} = dec_prim_type(Type, I_Bin, I_Off, O_Val, R1),
	    {mkblock([Dec, mktuple([O_Val, mkop('+', I_Off, mkint(Sz))])]), R2};
	false ->
	    dec_compound(Type, I_Bin, I_Off, R0)
    end.

%%
%% decode enums Int -> Atom
%% case Enum of
%%    Val1 -> Tag1;
%%    Val2 -> Tag2;
%%    ...
%%    ValN -> TagN
%% end
%%
dec_enum_i2a(Nums, I_Int, R0) ->
    CL = map(fun({Tag,Val}) -> mkclause([mkint(Val)],[],[mkatom(Tag)]) end,
	     Nums),
    {mkcase(I_Int, CL), R0}.
    
dec_compound(void, I_Bin, I_Off,R) ->
    {mktuple([mkatom(void), I_Off]), R};
dec_compound({type,Id}, I_Bin, I_Off, R)  ->
    {mkcall("dec_" ++ Id, [I_Bin, I_Off]), R};

%%
%% decode enums:
%% begin
%%   <_:I_Off/binary, Enum:32, _/binary>> = I_Bin,
%%   case Enum of
%%      Val1 -> {Tag1, O_NOff};
%%      Val2 -> {Tag2, O_NOff};
%%      ...
%%      ValN -> {TagN, O_NOff}
%%   end
%% end
%%
dec_compound({enum,Nums}, I_Bin, I_Off, R0) ->
    {Enum,R1} = genvar(R0),
    Match = mkmatch(mkbin([mkbinoff(I_Off),
			   mkbinelem(Enum, mkint(32), default),
			   mkbintail()]),
		    I_Bin),
    CL = map(
	   fun({Tag,Val}) ->
		   E1 = mkint(Val),
		   E2 = mktuple([mkatom(Tag), mkop('+', I_Off, mkint(4))]),
		   mkclause([E1],[],[E2])
	   end, Nums),
    {mkblock([Match, mkcase(Enum, CL)]), R1};
    
%%
%% fix array:
%%
%% map_elem(fun(I_Bin_F, I_Off_F) -> 
%%               dec_T(I_Bin_F, I_Off_F)
%%          end, I_Bin, I_Off, infinity, Size)
%%
%%
dec_compound({array,Size,Type}, I_Bin, I_Off, R0) ->
    put(map_elem, true),
    {I_Bin_F,R1} = genvar(R0),
    {I_Off_F,R2} = genvar(R1),
    {Dec,R3} = dec_type(Type, I_Bin_F, I_Off_F, R2),
    E1 = mkcall(map_elem,
		[mkfun([mkclause([I_Bin_F, I_Off_F],[],[Dec])]),
		 I_Bin, I_Off, mkatom(infinity), mkint(Size)]),
    {E1, R3};


%% If Max == infinity:
%% begin
%%   <<_:I_Off/binary, N:32/unsigned, _/binary>> = I_Bin,
%%   T_Off = I_Off + 4,
%%   <<_:T_Off/binary, O_Val:N/binary, _/binary>> = B,
%%   {O_Val, T_Off + align(N)}
%% end
%%
%% otherwise:
%%
%% begin
%%   <<_:I_Off/binary, N:32/unsigned, _/binary>> = I_Bin,
%%   if N > Max -> exit({xdr, limit});
%%      true ->
%%        T_Off = I_Off + 4,
%%        <<_:T_Off/binary, O_Val:N/binary, _/binary>> = B,
%%        {O_Val, T_Off + align(N)}
%%   end
%% end
dec_compound({varray,Max,opaque}, I_Bin, I_Off, R0) ->
    put(align, true),
    {N, R1} = genvar(R0),
    {T_Off, R2} = genvar(R1),
    {O_Val, R3} = genvar(R2),
    Match1 = mkmatch(mkbin([mkbinoff(I_Off),
			    mkbinelem(N, mkint(32), [unsigned]),
			    mkbintail()]),
		     I_Bin),
    TOffAssign = mkmatch(T_Off, mkop('+', I_Off, mkint(4))),
    Match2 = mkmatch(mkbin([mkbinoff(T_Off),
			    mkbinelem(O_Val, N, [binary]),
			    mkbintail()]),
		     I_Bin),
    Ret = mktuple([O_Val, mkop('+', T_Off, mkcall(align, [N]))]),
    if Max == infinity ->
	    {mkblock([Match1, TOffAssign, Match2, Ret]), R3};
       true ->
	    If = mkif([mkclause([],
				[mkop('>', N, mkint(Max))],
				[mkexitlimit()]),
		       mkclause([],
				[mkatom(true)],
				[TOffAssign, Match2, Ret])]),
	    {mkblock([Match1, If]), R3}
    end;
dec_compound({varray,Max,string}, I_Bin, I_Off, R0) ->
    dec_compound({varray,Max,opaque}, I_Bin, I_Off, R0);

%%
%% begin
%%   <_:I_Off/binary, N:32/unsigned, _/binary>> = I_Bin,
%%   map_elem(fun(I_Bin_F, I_Off_F) ->
%%                 dec_type(Type, I_Bin_F, I_Off_F)
%%             end, I_Bin, I_Off+4, Max, N)
%% end
%%
dec_compound({varray,Max,Type}, I_Bin, I_Off, R0) ->
    put(map_elem, true),
    {N, R1} = genvar(R0),
    {I_Bin_F, R2} = genvar(R1),
    {I_Off_F, R3} = genvar(R2),
    {Dec, R4} = dec_type(Type, I_Bin_F, I_Off_F, R3),
    Match = mkmatch(mkbin([mkbinoff(I_Off),
			   mkbinelem(N, mkint(32), [unsigned]),
			   mkbintail()]),
		    I_Bin),
    E = mkcall(map_elem,
	       [mkfun([mkclause([I_Bin_F, I_Off_F],[],[Dec])]),
		I_Bin, mkop('+', I_Off, mkint(4)), mkint2(Max), N]),
    {mkblock([Match, E]), R4};
    
%%
%% decode struct  - stmts depend on type:
%% begin
%%   {Val1,Off1}  = dec_T1(I_Bin, I_Off),
%%   begin % primitive example
%%     <_:I_Off/binary, Val1:32/unsigned, _/binary>> = I_Bin,
%%     O_NOff = I_Off + 4
%%   end
%%   ...
%%   {{Val1, Val2, ..., Valn}, Offn}
%% end
%%
dec_compound({struct, Elems}, I_Bin, I_Off, R0) ->
    {Decs, Vals, O_NOff, R1} = 
	foldl(
	  fun({Id,Type}, {Decs, Vals, Off, RR1}) ->
		  {Val_i,RR2} = genvar(RR1),
		  {Off_i,RR3} = genvar(RR2),
		  {Dec, RR4} = 
		      mk_type_match(Type, I_Bin, Off, Val_i, Off_i, RR3),
		  {[Dec | Decs], [Val_i|Vals], Off_i, RR4}
	  end,
	  {[], [], I_Off, R0},
	  Elems),
    Ret = mktuple([mktuple(reverse(Vals)), O_NOff]),
    { mkblock(reverse(Decs) ++ [Ret]), R1};

%%
%% decode union:
%% begin
%%   <_:I_Off/binary, TagAsInt:32, _/binary>> = I_Bin,
%%   T_Off = I_Off + 4,
%%   case TagAsInt of
%%     TagAsInt1 -> 
%%         begin % primitive example
%%           <_:T_Off/binary, Val1:32/unsigned, _/binary>> = I_Bin,
%%           O_NOff = T_Off + 4
%%         end
%%         {{Tag1, Val}, O_NOff};
%%     TagAsInt2 ->
%%         {Val, O_NOff} = dec_T2(I_Bin, T_Off),
%%         {{Tag2, Val}, O_NOff};
%%     ...
%%     _ ->
%%         DefTag = dec_enum_i2a(TagAsInt),
%%         {Val, O_NOff} = dec_T2(I_Bin, T_Off),
%%         {{DefTag, Val}, O_NOff}
%%   end
%% end
%%
%%
dec_compound({union, X={{DId,DT}, Elems}}, I_Bin, I_Off, R0) ->
    {Tag,R1} = genvar(R0),
    {Val,R2} = genvar(R1),
    {O_NOff,R3} = genvar(R2),
    {T_Off,R4} = genvar(R3),
    %% Discriminant is int, unsigned_int, bool or enum
    %% For enum, shortcut the decoding
    PrimType = case is_prim_dec_p(DT) of
		   true when DT /= bool ->
		       DT;
		   true -> % bool
		       int;
		   false -> % enum
		       int
	       end,
    {TagMatch, 4, R5} = dec_prim_type(PrimType, I_Bin, I_Off, Tag, R4),
    TOffAssign = mkmatch(T_Off, mkop('+', I_Off, mkint(4))),
    {CL,R6} = 
	foldr(
	  fun ({{default,_},{_,Type}},{CL0,RR0}) ->
		  {Dec, RR1} =
		      mk_type_match(Type,I_Bin,T_Off,Val,O_NOff,RR0),
		  case DT of
		      {type, Id} ->
			  {TagRet, RR2} = genvar(RR1),
			  I2A = list_to_atom("dec_" ++ Id ++ "_i2a"),
			  TM = mkmatch(TagRet, mkcall(I2A, [Tag])),
			  Ret = mktuple([mktuple([TagRet, Val]), O_NOff]),
			  {[mkclause([mkvar('_')],[],[Dec, TM, Ret])|CL0],RR2};
		      _ ->
			  Ret = mktuple([mktuple([Tag, Val]),
					 O_NOff]),
			  {[mkclause([mkvar('_')],[],[Dec, Ret])|CL0], RR1}
		  end;
	      ({{UTag,UTagV},{Uid,Type}}, {CL0,RR0}) ->
		  {Dec, RR1} =
		      mk_type_match(Type,I_Bin,T_Off,Val,O_NOff,RR0),
		  ETag = mkint(UTagV),
		  TagRet = if integer(UTag) -> mkint(UTag);
			      true -> mkatom(UTag)
			   end,
		  Ret = mktuple([mktuple([TagRet, Val]), O_NOff]),
		  {[mkclause([ETag],[],[Dec, Ret]) | CL0], RR1}
	  end, {[], R5}, Elems),
    Case = mkcase(Tag, CL),
    {mkblock([TagMatch, TOffAssign, Case]), R6};

%%
%% decode optional:
%%
%% begin
%%   <<_:I_Off/binary, Bool:32/unsigned, _/binary>> = B,
%%   if Bool == 0 -> {void, I_Off + 4}
%%      Bool == 1 -> dec_type(I_Bin, I_Off + 4)
%%   end
%% end
dec_compound({optional,Type}, I_Bin, I_Off, R0) ->
    {Bool, R1} = genvar(R0),
    {T_Off, R2} = genvar(R1),
    TOffAssign = mkmatch(T_Off, mkop('+', I_Off, mkint(4))),
    {Dec, R3} = dec_type(Type, I_Bin, T_Off, R2),
    {mkblock([mkmatch(mkbin([mkbinoff(I_Off),
			     mkbinelem(Bool, mkint(32), [unsigned]),
			     mkbintail()]),
		      I_Bin),
	      TOffAssign,
	      mkif([mkclause([],
			     [mkop('==', Bool, mkint(0))],
			     [mktuple([mkatom(void), T_Off])]),
		    mkclause([],
			     [mkop('==', Bool, mkint(1))],
			     [Dec])])]),
     R3}.

    
%% Generates a stmt as (compound)
%%   {O_Val, O_NOff} = dec_T1(I_Bin, I_Off)
%% or (primitive, example)
%%   begin
%%     <_:I_Off/binary, Val1:32/unsigned, _/binary>> = I_Bin,
%%     O_NOff = I_Off + 4
%%   end
%%
%% Ret: {Dec, R1}
mk_type_match(Type, I_Bin, I_Off, O_Val, O_NOff, R0) ->
    case is_prim_dec_p(Type) of
	true ->
	    {Dec, Sz, R1} = dec_prim_type(Type, I_Bin, I_Off, O_Val, R0),
	    {mkblock([Dec, mkmatch(O_NOff, mkop('+', I_Off, mkint(Sz)))]), R1};
	false ->
	    {D1, R1} = dec_compound(Type, I_Bin, I_Off, R0),
	    D2 = mktuple([O_Val, O_NOff]),
	    {mkmatch(D2,D1), R1}
    end.

% Increment the argument a minimal amount to bring it to a multiple of 4.
align(Len) ->
    case Len rem 4 of
	0 -> Len;
	1 -> Len+3;
	2 -> Len+2;
	3 -> Len+1
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Rpc client
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clnt({program,Id,Prog,Vs}, Fs0) ->
    clnt_versions(Vs, Prog, Fs0).

clnt_versions([{version,Id,Ver,Ps} | Vs], Prog, Fs0) ->
    Fs1 = clnt_procs(Ps, Prog, Ver, Fs0),
    clnt_versions(Vs, Prog, Fs1);
clnt_versions([], _, Fs0) -> Fs0.

%%
%% The rpc client should contain the program number
%%
%% Name_<Vi>(Rpc, V1,V2,...,Vn) ->
%%   Args = [enc_T1(V1), enc_T2(V2), ..., enc_Tn(Vn) ]
%%   case rpc_client:call(Clnt, Proc, Args) of
%%     {ok, ResBin} -> 
%%         {Reply,_} = dec_T(ResBin, 0),
%%         {ok, Reply}
%%     Error -> Error
%%   end
%%
clnt_procs([{procedure,Name,Proc,Ret,Args} | Ps], Prog, Ver, Fs0) ->
    FName = genname(Name,Ver),
    {F0, F1} = clnt_call(FName,Args,Ret,Proc,Ver,Prog),
    clnt_procs(Ps, Prog, Ver, [F0, F1 | Fs0]);
clnt_procs([], _, _, Fs0) -> Fs0.

clnt_call(FName,Args,Ret,Proc,Ver,Prog) ->
    R0 = #grec{},
    {EL,VL,R1} = foldr(
		   fun(T, {Enc0, VL, RR0}) ->
			   {VV,RR1} = genvar(RR0),
			   {Enc1,RR2} = enc_type(T, VV, RR1),
			   {mkcons(Enc1,Enc0), [VV|VL], RR2}
		   end, {mknil(),[],R0}, Args),
    {I_Args,R2} = genvar(R1),
    {O_Res,R3} = genvar(R2),
    Assign = mkmatch(I_Args, EL),
    {Dec,R4} = dec_type(Ret, O_Res, mkint(0), R3),
    Case = mkcase(mkcall(rpc_client, call,
			 [mkvar('Clnt'),
			  mkint(Proc),
			  I_Args,
			  mkvar('Timeout')]),
		  [mkclause([mktuple([mkatom(ok),O_Res])],[],
			    [mkmatch(mktuple([mkvar('Reply'),mkvar('_')]),Dec),
			     mktuple([mkatom(ok),mkvar('Reply')])]),
		   mkclause([mkvar('Error')],[],[mkvar('Error')]) ]),
    F0 = mkfunction(FName, length(Args)+1, 
		    [mkclause([mkvar('Clnt')|VL], [], 
			      [mklocalcall(FName, 
					   [mkvar('Clnt') | VL] ++ 
					   [mkatom(infinity)])])]),
    F1 = mkfunction(FName, length(Args)+2, 
		    [mkclause([mkvar('Clnt')|VL]++[mkvar('Timeout')], [], 
			      [Assign, Case])]),
    {F0, F1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the server program part of rpc
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
svc_gen_funcs(Type, Base, Fs0) ->
    Serv = Base ++ "_server",
    if Type == gen_server ->
	    svc_genprocs_gs(Serv, Fs0);
       Type == rpc_server ->
	    svc_genprocs_rs(Serv, Fs0)
    end.

svc_prog({program,Id,Prog,Vs},Type,Base,Fs0) ->
    Serv = Base ++ "_server",
    if Type == gen_server ->
	    svc_versions_gs(Vs, Id, Serv, Fs0);
       Type == rpc_server ->
	    svc_versions_rs(Vs, Id, Serv, Fs0)
    end.

%% generate the init, handle_* and terminate functions
svc_genprocs_gs(Serv, Fs0) ->
    Init = mkfunction(init, 1,
		      [mkclause([mkvar('_')], [],
				[mktuple([mkatom(ok), mklist([])])])]),
    HCall = mkfunction(handle_call, 3,
		       [mkclause([mkvar('_'), mkvar('_'), mkvar('_')], [],
				 [mktuple([mkatom(reply),
					   mkatom(ok),
					   mklist([])])])]),
    NoReply = mktuple([mkatom(noreply), mklist([])]),
    HCast = mkfunction(handle_cast, 2,
		       [mkclause([mkvar('_'), mkvar('_')], [],
				 [NoReply])]),
    HInfo = mkfunction(handle_info, 2,
		       [mkclause([mktuple([mkatom(tcp_new),
					   mkvar('Sock')]),
				  mkvar('_')],
				 [],
				 [mkcall(erlang, send, 
					 [mkatom(Serv),
					  mktuple([mkatom(tcp_new),
						   mkvar('Sock')])]),
				  NoReply]),
			mkclause([mktuple([mkatom(tcp_closed),
					   mkvar('Sock')]),
				  mkvar('_')],
				 [],
				 [mkcall(erlang, send, 
					 [mkatom(Serv),
					  mktuple([mkatom(tcp_closed),
						   mkvar('Sock')])]),
				  NoReply]),
		        mkclause([mkvar('_'), mkvar('_')], [],
				 [NoReply])]),
    Term = mkfunction(terminate, 2,
		      [mkclause([mkvar('_'), mkvar('_')], [],
				[mklist([])])]),
    [Init, HCall, HCast, HInfo, Term | Fs0].


svc_versions_gs(Vsns, ProgId, Serv, Fs0) ->
    svc_versions_gen(Vsns, ProgId, Serv, Fs0, fun svc_procs_gs/8).

svc_versions_gen([{version,Id,Ver,Ps} | Vs], ProgId, Serv, Fs0,ProcFun) ->
    Bin = mkvar('Bin'),
    Off = mkvar('Offset'),
    R0 = #grec{},
    CL = ProcFun(Ps, ProgId, Serv, Ver, Bin, Off, R0, []),
    FName = genname(ProgId,Ver),
    F = mkfunction(FName, 5,
		   [mkclause([mkvar('Proc'), Bin, Off, mkvar('Clnt'),
			      mkvar('State')], [],
			     [mkcase(mkvar('Proc'), reverse(CL))])]),
    svc_versions_gen(Vs, ProgId, Serv, [F | Fs0], ProcFun);
svc_versions_gen([], _, _, Fs0, _) -> Fs0.

%%
%% generate procedure clauses
%%
%% prog_n(Proc, Bin, Off, Clnt, _State) ->
%%   case Proc of
%%      I ->
%%        {A1, Off1} = dec_type(Bin, 0)
%%        Off2 = Off1 + X,
%%        {A2, Off2} = dec_type(Bin, Off1)
%%        ...
%%        Reply = gen_server:call(prog_server, {proc_v,A1,A2..,An,Clnt}),
%%        {success, reg_proto_xdr:enc_regres(Res), []};
%%        
%%
svc_procs_gs([{procedure,Name,Proc,Ret,Args} | Ps], 
	  ProgId, Serv,Ver,Bin,Off,R0,CLs) ->
    CL = svc_call_gs(Name,Proc,Args,Ret,ProgId,Serv,Ver,Bin,Off,R0),
    svc_procs_gs(Ps, ProgId, Serv, Ver, Bin, Off, R0, [CL | CLs]);
svc_procs_gs([], _, _, _, _, _, _, CLs) -> CLs.

svc_call_gs(Name,Proc,Args,Ret,ProgId,Serv,Ver,Bin,Off,R0) ->
    {DL, As, _Off, R1} = gen_call_dec(Bin, Off, R0, Args),
    E1 = mkmatch(
	   mkvar('Res'),
	   mkcall(gen_server, call, 
		  [mkatom(Serv),
		   mktuple([mkatom(genname(Name,Ver)) | reverse(As)] ++
			   [mkvar('Clnt')]),
		   mkatom(infinity)])),
    {E2,R2} = enc_type(Ret, mkvar('Res'), R1),
    E3 = mktuple([mkatom(success), E2, mklist([])]),
    mkclause([mkint(Proc)], [], reverse(DL) ++ [E1,E3]).

%% generate the init, handle_* and terminate functions
svc_genprocs_rs(Serv, Fs0) ->
    Init = mkfunction(init, 1,
		      [mkclause([mkvar('Args')], [],
				[mkcall(Serv, init,
					[mkvar('Args')])])]),
    VReq = mkvar('Req'),
    VFrom = mkvar('From'),
    VS = mkvar('S'),
    VReason = mkvar('Reason'),
    HCall = mkfunction(handle_call, 3,
		       [mkclause([VReq, VFrom, VS], [],
				 [mkcall(Serv, handle_call,
					 [VReq, VFrom, VS])])]),
    NoReply = mktuple([mkatom(noreply), mklist([])]),
    HCast = mkfunction(handle_cast, 2,
		       [mkclause([VReq, VS], [],
				 [mkcall(Serv, handle_cast,
					 [VReq, VS])])]),
    HInfo = mkfunction(handle_info, 2,
		       [mkclause([VReq, VS], [],
				 [mkcall(Serv, handle_info,
					 [VReq, VS])])]),
    Term = mkfunction(terminate, 2,
		      [mkclause([VReason, VS], [],
				[mkcall(Serv, terminate,
					[VReason, VS])])]),

    [Init, HCall, HCast, HInfo, Term | Fs0].

svc_versions_rs(Vsns, ProgId, Serv, Fs0) ->
    svc_versions_gen(Vsns, ProgId, Serv, Fs0, fun svc_procs_rs/8).

%%
%% generate procedure clauses
%%
%% prog_n(Proc, Bin, Off, Clnt, _State) ->
%%   case Proc of
%%      I ->
%%        {A1, Off1} = dec_type(Bin, 0)
%%        {A2, Off2} = dec_type(Bin, Off1)
%%        ...
%%        case catch prog_server:proc_v(A1,A2..,An,Clnt,S) of
%%          {reply, Res, S'} ->
%%            {success, reg_proto_xdr:enc_regres(Res), S'};
%%          {noreply, S'} ->
%%            {noreply, S'}
%%          {error, S'} ->
%%            {error, S'}
%%        end;
%%
svc_procs_rs([{procedure,Name,Proc,Ret,Args} | Ps], 
	     ProgId, Serv,Ver,Bin,Off,R0,CLs) ->
    CL = svc_call_rs(Name,Proc,Args,Ret,ProgId,Serv,Ver,Bin,Off,R0),
    svc_procs_rs(Ps, ProgId, Serv, Ver, Bin, Off, R0, [CL | CLs]);
svc_procs_rs([], _, _, _, _, _, _, CLs) -> CLs.

svc_call_rs(Name,Proc,Args,Ret,ProgId,Serv,Ver,Bin,Off,R0) ->
    {DL, As, _Off, R1} = gen_call_dec(Bin, Off, R0, Args),
    {E2,R2} = enc_type(Ret, mkvar('Res'), R1),
    E1 = mkcase(mkcatch(mkcall(Serv, genname(Name,Ver),
			       reverse(As) ++ [mkvar('Clnt'), mkvar('State')])),
		[mkclause([mktuple([mkatom(reply), mkvar('Res'),
				    mkvar('NState')])], [],
			  [mktuple([mkatom(success), E2, mkvar('NState')])]),
		 mkclause([mkvar('Else')], [], [mkvar('Else')])]),
    mkclause([mkint(Proc)], [], reverse(DL) ++ [E1]).


enc_align(Len) ->
  case Len rem 4 of
    0 -> <<>>;
    1 -> <<0,0,0>>;
    2 -> <<0,0>>;
    3 -> <<0>>
  end.


gen_call_dec(Bin, Off, R0, Args) ->
    foldl(
      fun(T, {DL, As, NOff, RR1}) ->
	      {Ai,RR2} = genvar(RR1),
	      {Offi,RR3} = genvar(RR2),
	      case is_prim_dec_p(T) of
		  true ->
		      {D1, Sz, RR4} = dec_prim_type(T, Bin, NOff, Ai, RR3),
		      D2 = mkmatch(Offi, mkop('+', NOff, mkint(Sz))),
		      {[D1, D2 | DL], [Ai|As], Offi, RR4};
		  false ->
		      {D1,RR4} = dec_type(T, Bin, NOff, RR3),
		      D2 = mktuple([Ai,Offi]),
		      D3 = mkmatch(D2,D1),
		      {[D3 | DL], [Ai|As], Offi, RR4}
	      end
      end,
      {[], [], Off, R0}, Args).
