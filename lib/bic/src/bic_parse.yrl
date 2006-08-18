
Nonterminals
	primary_expr postfix_expr argument_expr_list
	unary_expr unary_operator cast_expr
	multiplicative_expr additive_expr shift_expr
	relational_expr equality_expr and_expr exclusive_or_expr
	inclusive_or_expr logical_and_expr logical_or_expr
	conditional_expr assignment_expr assignment_operator
	expr constant constant_expr declaration declaration_specifiers
	init_declarator_list init_declarator
	storage_class_specifier type_specifier type_name
	struct_or_union_specifier struct_or_union
	struct_declaration_list struct_declaration
	struct_declarator_list struct_declarator
	enum_specifier enumerator_list enumerator
	declarator declarator2 pointer type_specifier_list
	parameter_identifier_list identifier_list
	parameter_type_list parameter_list
	parameter_declaration abstract_declarator
	abstract_declarator2 initializer
	initializer_list statement labeled_statement
	compound_statement declaration_list
	statement_list expression_statement 
	selection_statement iteration_statement jump_statement 
	file external_definition function_definition
	function_body 
	.

Terminals 
	  hexnum octnum decnum flonum chrnum
	  identifier string sizeof type
	  '->' '++' '--' '<<' '>>' '<' '>' '>=' '<=' '==' '!='
	  '&&' '||' '*=' '/=' '%=' '+='
	  '-=' '<<=' '>>=' '&=' '^=' '|=' 
	  '(' ')' '[' ']' '{' '}' ',' '.' '&' '*' '+' '-' '~' '!'
	  '/' '%' '^' '|' ':' '?' '=' ';'
	  'typedef' 'extern' 'static' 'auto' 'register'
	  'char' 'short' 'int' 'long' 'signed' 'unsigned' 'float' 'double' 
	  'const' 'volatile' 'void'
	  'struct' 'union' 'enum' '...'
	  'case' 'default' 'if' 'else' 'switch' 'while' 'do' 
	  'for' 'goto' 'continue' 'break' 'return'
	  .

Rootsymbol file.

%%

primary_expr -> identifier : id('$1').
primary_expr -> constant : '$1'.
primary_expr -> string : str('$1').
primary_expr -> '(' expr ')' : '$2'.

postfix_expr -> primary_expr : '$1'.
postfix_expr -> postfix_expr '[' expr ']' : 
		    #'EXPR' {line=line('$1'),op='[]',arg1='$1',arg2='$3'}.
postfix_expr -> postfix_expr '(' ')' : 
		    #'CALL' {line=line('$1'),func='$1',args=[]}.
postfix_expr -> postfix_expr '(' argument_expr_list ')' :
		    #'CALL' {line=line('$1'),func='$1',args='$3'}.
postfix_expr -> postfix_expr '.' identifier : 
		    #'EXPR' {line=line('$1'),op='.',arg1='$1',arg2=id('$3') }.
postfix_expr -> postfix_expr '->' identifier : 
		    #'EXPR' {line=line('$1'),op='->',arg1='$1',arg2=id('$3')}.
postfix_expr -> postfix_expr '++' : 
		    #'UNARY' {line=line('$1'),op='++',arg='$1'}.
postfix_expr -> postfix_expr '--' : 
		    #'UNARY' {line=line('$1'),op='--',arg='$1'}.

argument_expr_list -> assignment_expr : ['$1'].
argument_expr_list -> argument_expr_list ',' assignment_expr : '$1'++['$3'].

unary_expr -> postfix_expr : '$1'.
unary_expr -> '++' unary_expr :
		  #'UNARY' {line=line('$1'),op='+++',arg='$2'}.
unary_expr -> '--' unary_expr : 
		  #'UNARY' {line=line('$1'),op='---',arg='$2'}.
unary_expr -> unary_operator cast_expr : 
		  #'UNARY' {line=line('$1'),op=op('$1'),arg='$2'}.
unary_expr -> 'sizeof' unary_expr : 
		  #'UNARY' {line=line('$1'),op='sizeof',arg='$2'}.
unary_expr -> 'sizeof' '(' type_name ')' : 
		  #'UNARY' {line=line('$1'),op='sizeof',arg='$3'}.

unary_operator -> '&' : '$1'.
unary_operator -> '*' : '$1'.
unary_operator -> '+' : '$1'.
unary_operator -> '-' : '$1'.
unary_operator -> '~' : '$1'.
unary_operator -> '!' : '$1'.

cast_expr -> unary_expr : '$1'.
cast_expr -> '(' type_name ')' cast_expr :
		 #'EXPR' {line=line('$1'),op=cast,arg1='$2',arg2='$4'}.

multiplicative_expr -> cast_expr : '$1'.
multiplicative_expr -> multiplicative_expr '*' cast_expr : 
		   #'EXPR' { line=line('$2'), op='*',arg1='$1',arg2='$3'}.
multiplicative_expr -> multiplicative_expr '/' cast_expr :
		   #'EXPR' { line=line('$2'), op='/',arg1='$1',arg2='$3'}.
multiplicative_expr -> multiplicative_expr '%' cast_expr :
		   #'EXPR' { line=line('$2'), op='%',arg1='$1',arg2='$3'}.

additive_expr -> multiplicative_expr : '$1'.
additive_expr -> additive_expr '+' multiplicative_expr :
		   #'EXPR' { line=line('$2'), op='+',arg1='$1',arg2='$3'}.
additive_expr -> additive_expr '-' multiplicative_expr :
		   #'EXPR' { line=line('$2'), op='-',arg1='$1',arg2='$3'}.

shift_expr -> additive_expr : '$1'.
shift_expr -> shift_expr '<<' additive_expr :
		   #'EXPR' { line=line('$2'),op='<<',arg1='$1',arg2='$3'}.
shift_expr -> shift_expr '>>' additive_expr :
		  #'EXPR' { line=line('$2'),op='>>',arg1='$1',arg2='$3'}.

relational_expr -> shift_expr : '$1'.
relational_expr -> relational_expr '<' shift_expr :
		  #'EXPR' { line=line('$2'),op='<',arg1='$1',arg2='$3'}.
relational_expr -> relational_expr '>' shift_expr :
		  #'EXPR' { line=line('$2'),op='>',arg1='$1',arg2='$3'}.
relational_expr -> relational_expr '<=' shift_expr :
		  #'EXPR' { line=line('$2'),op='<=',arg1='$1',arg2='$3'}.
relational_expr -> relational_expr '>=' shift_expr :
		  #'EXPR' { line=line('$2'),op='>=',arg1='$1',arg2='$3'}.

equality_expr -> relational_expr : '$1'.
equality_expr -> equality_expr '==' relational_expr :
		  #'EXPR' { line=line('$2'),op='==',arg1='$1',arg2='$3'}.
equality_expr -> equality_expr '!=' relational_expr :
		  #'EXPR' { line=line('$2'),op='!=',arg1='$1',arg2='$3'}.

and_expr -> equality_expr : '$1'.
and_expr -> and_expr '&' equality_expr :
		  #'EXPR' { line=line('$2'),op='&',arg1='$1',arg2='$3'}.

exclusive_or_expr -> and_expr : '$1'.
exclusive_or_expr -> exclusive_or_expr '^' and_expr :
		  #'EXPR' { line=line('$2'),op='^',arg1='$1',arg2='$3'}.

inclusive_or_expr -> exclusive_or_expr : '$1'.
inclusive_or_expr -> inclusive_or_expr '|' exclusive_or_expr :
		  #'EXPR' { line=line('$2'),op='|',arg1='$1',arg2='$3'}.

logical_and_expr -> inclusive_or_expr : '$1'.
logical_and_expr -> logical_and_expr '&&' inclusive_or_expr :
		  #'EXPR' { line=line('$2'),op='&&',arg1='$1',arg2='$3'}.

logical_or_expr -> logical_and_expr : '$1'.
logical_or_expr -> logical_or_expr '||' logical_and_expr :
		  #'EXPR' { line=line('$2'),op='||',arg1='$1',arg2='$3'}.

conditional_expr -> logical_or_expr : '$1'.
conditional_expr -> logical_or_expr '?' logical_or_expr ':' conditional_expr :
		  #'IFEXPR' { line=line('$2'),test='$1',then='$3',else='$5'}.

assignment_expr -> conditional_expr : '$1'.
assignment_expr -> unary_expr assignment_operator assignment_expr :
	          #'ASSIGN' {line=line('$2'),op=op('$2'),lhs='$1',rhs='$3'}.

assignment_operator -> '=' : '$1'.
assignment_operator -> '*=' : '$1'.
assignment_operator -> '/=' : '$1'.
assignment_operator -> '%=' : '$1'.
assignment_operator -> '+=' : '$1'.
assignment_operator -> '-=' : '$1'.
assignment_operator -> '<<=' : '$1'.
assignment_operator -> '>>=' : '$1'.
assignment_operator -> '&=' : '$1'.
assignment_operator -> '^=' : '$1'.
assignment_operator -> '|=' : '$1'.

expr -> assignment_expr : '$1'.
expr -> expr ',' assignment_expr : 
     	#'EXPR' { line=line('$1'), op=',',arg1='$1',arg2='$3'}.

constant_expr -> conditional_expr : '$1'.

constant -> hexnum : hex('$1').
constant -> octnum : oct('$1').
constant -> decnum : dec('$1').
constant -> flonum : flo('$1').
constant -> chrnum : chr('$1').

declaration -> declaration_specifiers ';' : 
		 [#'DECL' { type='$1'}].
declaration -> declaration_specifiers init_declarator_list ';' :
		 map(fun(D) -> D#'DECL' { type='$1'++D#'DECL'.type} end,
		     '$2').

declaration_specifiers -> storage_class_specifier : 
		['$1'].
declaration_specifiers -> storage_class_specifier declaration_specifiers :
		['$1' | '$2'].
declaration_specifiers -> type_specifier : 
		['$1'].
declaration_specifiers -> type_specifier declaration_specifiers :
		['$1'|'$2'].

init_declarator_list -> init_declarator : 
		['$1'].
init_declarator_list -> init_declarator_list ',' init_declarator : 
		'$1'++['$3'].

init_declarator -> declarator : 
		'$1'.
init_declarator -> declarator '=' initializer : 
		'$1'#'DECL' { value = '$3'}.


storage_class_specifier -> 'typedef' : op('$1').
storage_class_specifier -> 'extern' : op('$1').
storage_class_specifier -> 'static' : op('$1').
storage_class_specifier -> 'auto' : op('$1').
storage_class_specifier -> 'register' : op('$1').

type_specifier -> 'char' : op('$1').
type_specifier -> 'short' : op('$1').
type_specifier -> 'int' : op('$1').
type_specifier -> 'long' : op('$1').
type_specifier -> 'signed' : op('$1').
type_specifier -> 'unsigned' : op('$1').
type_specifier -> 'float' : op('$1').
type_specifier -> 'double' : op('$1').
type_specifier -> 'const' : op('$1').
type_specifier -> 'volatile' : op('$1').
type_specifier -> 'void' : op('$1').
type_specifier -> struct_or_union_specifier : '$1'.
type_specifier -> enum_specifier : '$1'.
type_specifier -> type : typeid('$1').

struct_or_union_specifier -> struct_or_union identifier '{' struct_declaration_list '}' : 
	case op('$1') of
	     struct -> #'STRUCT' {line=line('$1'),name=arg('$2'),elems='$4'};
	     union ->  #'UNION'  {line=line('$1'),name=arg('$2'),elems='$4'}
	end.
		     
struct_or_union_specifier -> struct_or_union '{' struct_declaration_list '}' :
	case op('$1') of
	     struct -> #'STRUCT' {line=line('$1'),elems='$3'};
	     union ->  #'UNION'  {line=line('$1'),elems='$3'}
	end.

struct_or_union_specifier -> struct_or_union identifier :
	case op('$1') of
	     struct -> #'STRUCT' {line=line('$1'),name=arg('$2')};
	     union ->  #'UNION'  {line=line('$1'),name=arg('$2')}
	end.

struct_or_union -> 'struct' : '$1'.
struct_or_union -> 'union' : '$1'.

struct_declaration_list -> struct_declaration : ['$1'].
struct_declaration_list -> struct_declaration_list struct_declaration :
			'$1'++['$2'].

struct_declaration -> type_specifier_list struct_declarator_list ';' :
		 map(fun(D) -> D#'DECL' { type='$1'++D#'DECL'.type} end,
		     '$2').

struct_declarator_list -> struct_declarator : ['$1'].
struct_declarator_list -> struct_declarator_list ',' struct_declarator :
		       '$1'++['$3'].

struct_declarator -> declarator : 
		   '$1'.
struct_declarator -> ':' constant_expr : 
		   #'DECL' { line=line('$1'),value ='$2'} .
struct_declarator -> declarator ':' constant_expr : 
		   '$1'#'DECL' { value ='$3'}.

enum_specifier -> 'enum' '{' enumerator_list '}' : 
	       #'ENUM' {line=line('$1'),elems='$3'}.
enum_specifier -> 'enum' identifier '{' enumerator_list '}' :
	       #'ENUM' {line=line('$1'),name=arg('$2'),elems='$4'}.
enum_specifier -> 'enum' identifier :
	       #'ENUM' {line=line('$1'),name=arg('$2')}.

enumerator_list -> enumerator : ['$1'].
enumerator_list -> enumerator_list ',' enumerator : '$1'++['$3'].

enumerator -> identifier : { arg('$1'),undefined}.
enumerator -> identifier '=' constant_expr : {arg('$1'),'$3'}.

declarator -> declarator2 : '$1'.
declarator -> pointer declarator2 : 
           '$2'#'DECL' { type='$1'++'$2'#'DECL'.type}.

declarator2 -> identifier : 
		   #'DECL' { line=line('$1'), name=arg('$1') }.
declarator2 -> '(' declarator ')' : 
		   '$2'.
declarator2 -> declarator2 '[' ']' : 
           '$1'#'DECL' { type=[{array,[]}|'$1'#'DECL'.type]}.
declarator2 -> declarator2 '[' constant_expr ']' : 
           '$1'#'DECL' { type=[{array,'$3'}|'$1'#'DECL'.type]}.
declarator2 -> declarator2 '(' ')' : 
           '$1'#'DECL' { type=[{fn,[]}|'$1'#'DECL'.type]}.
declarator2 -> declarator2 '(' parameter_type_list ')' :
           '$1'#'DECL' { type=[{fn,'$3'}|'$1'#'DECL'.type]}.
declarator2 -> declarator2 '(' parameter_identifier_list ')' :
           '$1'#'DECL' { type=[{fn,'$3'}|'$1'#'DECL'.type]}.

pointer -> '*' : [pointer].
pointer -> '*' type_specifier_list : [pointer|'$2'].
pointer -> '*' pointer : [pointer|'$2'].
pointer -> '*' type_specifier_list pointer : [pointer|['$2'++'$3']].

type_specifier_list -> type_specifier : ['$1'].
type_specifier_list -> type_specifier_list type_specifier : '$1'++['$2'].

parameter_identifier_list -> identifier_list : '$1'.
parameter_identifier_list -> identifier_list ',' '...' : '$1'++['$3'].

identifier_list -> identifier : [id('$1')].
identifier_list -> identifier_list ',' identifier : '$1'++[id('$3')].

parameter_type_list -> parameter_list : '$1'.
parameter_type_list -> parameter_list ',' '...' : '$1' ++ ['$3'].

parameter_list -> parameter_declaration : ['$1'].
parameter_list -> parameter_list ',' parameter_declaration : '$1'++['$3'].

parameter_declaration -> type_specifier_list declarator :
		'$2'#'DECL' { type='$1'++'$2'#'DECL'.type}.
parameter_declaration -> type_name : 
		#'DECL' { type='$1'}.

type_name -> type_specifier_list : '$1'.
type_name -> type_specifier_list abstract_declarator : '$1'++'$2'.

abstract_declarator -> pointer : '$1'.
abstract_declarator -> abstract_declarator2 : '$1'.
abstract_declarator -> pointer abstract_declarator2 : 
			   '$1'++'$2'.

abstract_declarator2 -> '(' abstract_declarator ')' : 
		     '$2'.
abstract_declarator2 -> '[' ']' :
		     [{array,[]}].
abstract_declarator2 -> '[' constant_expr ']' : 
		     [{array,'$2'}].
abstract_declarator2 -> abstract_declarator2 '[' ']' : 
		     '$1'++[{array,[]}].
abstract_declarator2 -> abstract_declarator2 '[' constant_expr ']' : 
		     '$1'++[{array,'$3'}].
abstract_declarator2 -> '(' ')' : 
		     [{params,[]}].
abstract_declarator2 -> '(' parameter_type_list ')' : 
		     [{params,'$2'}].
abstract_declarator2 -> abstract_declarator2 '(' ')' : 
		     '$1'++[{params,[]}].
abstract_declarator2 -> abstract_declarator2 '(' parameter_type_list ')' :
		     '$1'++[{params,'$3'}].

initializer -> assignment_expr : '$1'.
initializer -> '{' initializer_list '}' : '$2'.
initializer -> '{' initializer_list ',' '}' : '$2'.

initializer_list -> initializer : ['$1'].
initializer_list -> initializer_list ',' initializer : '$1'++['$3'].

statement -> labeled_statement : '$1'.
statement -> compound_statement : '$1'.
statement -> expression_statement : '$1'.
statement -> selection_statement : '$1'.
statement -> iteration_statement : '$1'.
statement -> jump_statement : '$1'.

labeled_statement -> identifier ':' statement : 
			 #'LABEL' { line=line('$1'),name=arg('$1'),code='$3'}.
labeled_statement -> 'case' constant_expr ':' statement : 
			 #'CASE' { line=line('$1'),expr='$2',code='$3'}.
labeled_statement -> 'default' ':' statement : 
			 #'DEFAULT' {line=line('$1'),code='$3'}.

compound_statement -> '{' '}' : [].
compound_statement -> '{' statement_list '}' : '$2'.
compound_statement -> '{' declaration_list '}' : '$2'.
compound_statement -> '{' declaration_list statement_list '}' : '$2'++'$3'.

declaration_list -> declaration : '$1'.
declaration_list -> declaration_list declaration : '$1'++'$2'.

statement_list -> statement : ['$1'].
statement_list -> statement_list statement : '$1' ++ ['$2'].

expression_statement -> ';' : {noop,line('$1')}.
expression_statement -> expr ';' : '$1'.

selection_statement -> 'if' '(' expr ')' statement :
	    #'IF' {line=line('$1'),test='$3',then='$5'}.
selection_statement -> 'if' '(' expr ')' statement 'else' statement :
	    #'IF' {line=line('$1'),test='$3',then='$5',else='$7'}.
selection_statement -> 'switch' '(' expr ')' statement :
	    #'SWITCH' { line=line('$1'),expr='$3',body='$5'}.

iteration_statement -> 'while' '(' expr ')' statement :
	    #'WHILE' { line=line('$1'),test='$3',body='$5'}.
iteration_statement -> 'do' statement 'while' '(' expr ')' ';' :
	    #'DO' { line=line('$1'),body='$2',test='$5'}.
iteration_statement -> 'for' '(' ';' ';' ')' statement :
	    #'FOR' {line=line('$1'),body='$6'}.
iteration_statement -> 'for' '(' ';' ';' expr ')' statement :
	    #'FOR' {line=line('$1'),update='$5',body='$7'}.
iteration_statement -> 'for' '(' ';' expr ';' ')' statement :
	    #'FOR' {line=line('$1'),test='$4',body='$7'}.
iteration_statement -> 'for' '(' ';' expr ';' expr ')' statement :
	    #'FOR' {line=line('$1'),test='$4',update='$6',body='$8'}.
iteration_statement -> 'for' '(' expr ';' ';' ')' statement :
	    #'FOR' {line=line('$1'),init='$3',body='$7'}.
iteration_statement -> 'for' '(' expr ';' ';' expr ')' statement :
	    #'FOR' {line=line('$1'),init='$3',update='$6',body='$8'}.
iteration_statement -> 'for' '(' expr ';' expr ';' ')' statement :
	    #'FOR' {line=line('$1'),init='$3',test='$5',body='$8'}.
iteration_statement -> 'for' '(' expr ';' expr ';' expr ')' statement :
	    #'FOR' {line=line('$1'),init='$3',test='$5',update='$7',body='$9'}.

jump_statement -> 'goto' identifier ';' : 
	    #'GOTO' {line=line('$1'),label=arg('$2')}.
jump_statement -> 'continue' ';' : 
	    #'CONTINUE' {line=line('$1')}.
jump_statement -> 'break' ';' : 
	    #'BREAK' {line=line('$1')}.
jump_statement -> 'return' ';' : 
	    #'RETURN' {line=line('$1')}.
jump_statement -> 'return' expr ';' : 
	   #'RETURN' {line=line('$1'),expr='$2'}.

file -> external_definition : '$1'.
file -> file external_definition : '$1'++'$2'.

external_definition -> function_definition : ['$1'].
external_definition -> declaration : 
		    case '$1' of
			  [#'DECL' { name=Name,
				     type=[typedef|Type]
				    }] ->
			    put({bic_type,Name},Type), ok;
			_ -> ok
		    end,
		    '$1'.

function_definition -> declarator function_body : 
		   {OldDecl,Body} = '$2',
		    #'FUNCTION' { line='$1'#'DECL'.line,
				  name='$1'#'DECL'.name,
				  type='$1'#'DECL'.type,
				  params=OldDecl,
				  body=Body}.
function_definition -> declaration_specifiers declarator function_body :
		   {OldDecl,Body} = '$3',
		    #'FUNCTION' { line='$2'#'DECL'.line,
				  name='$2'#'DECL'.name,
				  storage='$1',
				  type='$2'#'DECL'.type,
				  params=OldDecl,
				  body=Body}.


function_body -> compound_statement : {[],'$1'}.
function_body -> declaration_list compound_statement : {'$1','$2'}.

Erlang code.

-include("../include/bic.hrl").
-import(lists, [map/2]).
-export([init/0]).

init() ->
    %% erase dictionay use
    lists:foreach(
      fun
	  ({{bic_type,T},_}) -> erase({bic_type,T});
	  (_) -> ok
      end, get()).

id({identifier,Line,Name}) ->
    #'ID' { line=Line, name=Name}.

typeid({type,Line,Name}) ->
    #'TYPEID' { line=Line, name=Name}.

    
oct({octnum,Line,Val}) ->
    #'CONSTANT' { line=Line, base=8, value=Val}.

hex({hexnum,Line,Val}) ->
    #'CONSTANT' { line=Line, base=16, value=Val}.

dec({decnum,Line,Val}) ->
    #'CONSTANT' { line=Line, base=10, value=Val}.

chr({chrnum,Line,Val}) ->
    #'CONSTANT' { line=Line, base=char, value=Val}.

flo({flonum,Line,Val}) ->
    #'CONSTANT' { line=Line, base=float, value=Val}.

str({string,Line,Val}) ->
    #'CONSTANT' { line=Line, base=string, value=Val}.
     

op({Type,_Line})     -> Type.

arg({Type,Line,Val}) -> Val.

line([H|_]) -> line(H);
line({_,Line}) -> Line;
line({_,Line,_}) -> Line;
line({_,Line,_,_}) -> Line.



