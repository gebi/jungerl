%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
%%
%% XDR + RPC grammar file
%%
%% Spec:
%%  {typedef, Line, Id, Type}
%%  {const,   Line, Id, ConstantValue}
%%  {program, Line, Id, ProgNo, Versions}
%%
%% Version:
%%  {version, Line, Id, VersNo, Procedures}
%%
%% Procedure:
%%  {procedure,Line,Id, ProcNo, RetType,ArgTypes}
%%   
%% Possible Extensions: (forward)
%%   typedef struct struct-name ['*'] identifier;
%%   typedef union  union-name ['*'] identifier;
%%   typedef enum   enum-name ['*'] identifier;
%%
%%
%% Type:
%%  {struct,        Line, [{Id,Line,Type}]}
%%  {union,         Line, {EnumDecl, [{Tag,Line,Type}, {default,Line,Type}]}}
%%  {enum,          Line, [{Id, Line, Value}]}
%%  {array,         Line, N, Type}
%%  {varray,        Line, Max, Type}
%%  {int,           Line}
%%  {unsigned_int,  Line}
%%  {hyper,         Line}
%%  {unsigned_hyper,Line}
%%  {float,         Line}
%%  {double,        Line}
%%  {bool,          Line}
%%  {void,          Line}
%%  {type,          Line, Id}
%%  {optional,      Line, Type}
%%
Nonterminals
declaration value opt_value type_specifier enum_type_spec struct_type_spec 
union_type_spec enum_body enum_decls struct_body struct_decls
union_body case_decls default_decl constant_def type_def 
definition specification program_def version_def version_defs
procedure_def procedure_defs procedure_args.

Terminals 
'[' ']' '{' '}' '(' ')' '<' '>' '*' '=' ';' ':' ','
'opaque' 'string' 'void' 'float' 'double' 'bool' 'int' 'hyper' 'unsigned'
'enum' 'struct' 'union' 'case' 'default' 'typedef' 'switch'
'const' 'version' 'program' identifier constant.

Rootsymbol specification.

Endsymbol '$end'.

declaration -> type_specifier identifier :
	{val('$2'), line('$2'), '$1'}.
declaration -> type_specifier identifier '[' value ']' :
	{val('$2'), line('$2'), {array, line('$2'), '$4','$1'}}.
declaration -> type_specifier identifier '<' opt_value '>' :
	{val('$2'), line('$2'), {varray, line('$2'), '$4','$1'}}.
declaration -> 'opaque' identifier '[' value ']' :
	{val('$2'), line('$2'),  {array, line('$2'), '$4', opaque}}.
declaration -> 'opaque' identifier '<' opt_value '>' :
	{val('$2'), line('$2'), {varray, line('$2'), '$4', opaque}}.
declaration -> 'opaque' identifier :
	{val('$2'), line('$2'), {varray, line('$2'), infinity, opaque}}.
declaration -> 'string' identifier :
	{val('$2'), line('$2'), 
		{varray, line('$2'), infinity, string}}.
declaration -> 'string' identifier '<' opt_value '>' :
	{val('$2'), line('$2'),
		 {varray,line('$2'),'$4',string}}.

declaration -> type_specifier '*' identifier :
	{val('$3'), line('$3'), {optional,line('$3'),'$1'}}.

declaration -> 'void' : {"", line('$1'), {'void',line('$1')} }.

value -> constant :   {integer, line('$1'), val('$1')}.
value -> identifier : {identifier, line('$1'), val('$1')}.

opt_value -> value : '$1'.
opt_value -> '$empty' : infinity.


type_specifier -> 'unsigned' 'int' : {unsigned_int,line('$1')}.
type_specifier -> 'unsigned' 'hyper' : {unsigned_hyper,line('$1')}.
type_specifier -> 'unsigned' : {unsigned_int,line('$1')}.
type_specifier -> 'int' : {int,line('$1')}.
type_specifier -> 'hyper' : {hyper,line('$1')}.
type_specifier -> 'float' : {'float',line('$1')}.
type_specifier -> 'double' : {double,line('$1')}.
type_specifier -> 'bool' : {bool,line('$1')}.
type_specifier -> 'void' : {void,line('$1')}.
type_specifier ->  enum_type_spec : '$1'.
type_specifier ->  struct_type_spec : '$1'.
type_specifier ->  union_type_spec : '$1'.
type_specifier ->  identifier : {type,line('$1'),val('$1')}.

enum_type_spec -> 'enum' enum_body :  {enum, line('$1'),'$2'}.

enum_body -> '{' enum_decls '}' : '$2'.

enum_decls -> identifier '=' value ',' enum_decls : 
	[{val('$1'), line('$1'), '$3'} | '$5'].
enum_decls -> identifier '=' value :
	[{val('$1'), line('$1'), '$3'}].

struct_type_spec -> 'struct' struct_body : {struct, line('$1'), '$2'}.

struct_body -> '{' struct_decls '}' : '$2'.

struct_decls -> declaration ';' struct_decls :
	['$1' | '$3'].
struct_decls -> declaration ';' :
	['$1'].

union_type_spec -> 'union' union_body : {union, line('$1'), '$2'}.

union_body -> 'switch' '(' declaration ')' '{' case_decls default_decl '}' :
	{'$3', '$6' ++ '$7'}.

case_decls -> 'case' value ':' declaration ';' case_decls :
	[{'$2', line('$1'), '$4'} | '$6'].
case_decls -> 'case' value ':' declaration ';' :
	[{'$2', line('$1'), '$4'}].

default_decl -> 'default' ':' declaration ';' : [{default,line('$1'),'$3'}].
default_decl -> '$empty' : [].

constant_def -> 'const' identifier '=' constant ';' : 
	{const, line('$1'), val('$2'), val('$4')}.

type_def -> 'typedef' declaration ';'       : 
	{Id, _, Type} = '$2', {typedef, line('$1'), Id, Type}.

type_def -> 'enum' identifier enum_body ';' : 
	{typedef, line('$1'), val('$2'), {enum,line('$1'),'$3'}}.
type_def -> 'struct' identifier struct_body ';' :
	{typedef, line('$1'), val('$2'), {struct,line('$1'),'$3'}}.
type_def ->  'struct' '*' identifier struct_body ';' :
	{typedef, line('$1'), val('$3'),
		{optional, line('$1'), {struct,line('$1'),'$4'}}}.
type_def -> 'union' identifier union_body ';' :
	{typedef, line('$1'), val('$2'), {union,line('$1'),'$3'}}.

program_def -> 'program' identifier '{' version_defs '}' '=' constant ';' :
	{program, line('$2'), val('$2'), val('$7'), '$4'}.

version_defs -> version_def version_defs : ['$1' | '$2'].
version_defs -> version_def : ['$1'].

version_def -> 'version' identifier '{' procedure_defs '}' '=' constant ';' :
	{version, line('$2'), val('$2'), val('$7'), '$4'}.
		
procedure_def -> type_specifier identifier '(' procedure_args ')'
	'=' constant ';' :
	proc_def(line('$2'), val('$2'), val('$7'), '$1', '$4').

procedure_defs -> procedure_def procedure_defs : ['$1' | '$2'].
procedure_defs -> procedure_def : ['$1'].

procedure_args -> type_specifier ',' procedure_args : ['$1' | '$3'].
procedure_args -> type_specifier : ['$1'].


definition -> type_def     : '$1'.
definition -> constant_def : '$1'.
definition -> program_def  : '$1'.

specification -> definition specification : ['$1' | '$2'].
specification -> '$empty' : [].

Erlang code.

val({_,_,Value}) -> Value.

line(Token) -> element(2, Token).

proc_def(Line,Id,Proc,Ret,[{void,_}]) ->
	{procedure,Line,Id,Proc,Ret,[]};
proc_def(Line,Id,Proc,Ret,Args) ->
	{procedure,Line,Id,Proc,Ret,Args}.
