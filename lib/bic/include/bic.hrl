-record('ID',
	{ 
	  line,
	  name
	 }).

-record('TYPEID',
	{
	  line,
	  name
	 }).

-record('CONSTANT',
	{
	  line,
	  base,  %% 8,10,16 | char | float
	  value  %% string rep of value
	 }).
	  
-record('UNARY',
	{
	  line,
	  op,
	  arg
	 }).

-record('EXPR',
	{
	  line,
	  op,
	  arg1,
	  arg2
	 }).

-record('CALL',
	{
	  line,
	  func,
	  args
	 }).
	
%%  cond ? then : else FIXME? GNU:  cond ? then
-record('IFEXPR',
	{
	  line,
	  test,
	  then,
	  else
	 }).

-record('ASSIGN',
	{
	  line,
	  op,
	  lhs,
	  rhs
	 }).

%% Function declaration
-record('FUNCTION',
	{
	  line,       %% line number
	  name,
	  storage,    %% list of specifiers
	  type,       %% return type 
	  params,     %% list of parameters [#decl]
	  body        %% function body
	 }).

%% variable & element declarations
-record('DECL',
	{
	  line,    %% line number
	  name,    %% optional identifier
	  type=[], %% type (specifier list)
	  value    %%  init value / bit filed size - optional
	 }).

-record('STRUCT',
	{
	  line,
	  name,
	  elems
	 }).

-record('UNION',
	{
	  line,
	  name,
	  elems
	 }).

-record('ENUM',
	{
	  line,
	  name,   %% string() | undefined
	  elems   %% [{id,value|undefined}]
	 }).



-record('FOR',
	{
	  line,    %% line number
	  init,
	  test,
	  update,
	  body 
	 }).

-record('WHILE',
	{
	  line,    %% line number
	  test,
	  body 
	 }).

-record('DO',
	{
	  line,    %% line number
	  test,
	  body 
	 }).

-record('IF',
	  {
	  line,
	  test,
	  then,
	  else
	  }).

-record('SWITCH',
	{
	  line,
	  expr,
	  body 
	 }).

-record('CASE',
	{
	  line,
	  expr,
	  code
	 }).

-record('DEFAULT',
	{
	  line,
	  code
	 }).

-record('LABEL',
	{
	  line,
	  name,
	  code
	 }).

	
-record('GOTO',
	{
	  line,
	  label
	 }).

-record('CONTINUE',
	{
	  line
	 }).

-record('BREAK',
	{
	  line
	 }).
	
-record('RETURN',
	{
	  line,
	  expr
	 }).


	  
