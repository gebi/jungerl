%%
%% Ansi C scanner
%%

Definitions.

D	= [0-9]
L	= [a-zA-Z_]
H	= [a-fA-F0-9]
E	= [Ee][+-]?{D}+
FS	= (f|F|l|L)
IS	= (u|U|l|L)*
WS      = [\000-\s]

Rules.

auto		: {token,{auto,YYline}}.
break		: {token,{break,YYline}}.
case		: {token,{'case',YYline}}.
char		: {token,{char,YYline}}.
const		: {token,{const,YYline}}.
continue	: {token,{continue,YYline}}.
default	        : {token,{default,YYline}}.
do		: {token,{do,YYline}}.
double		: {token,{double,YYline}}.
else		: {token,{else,YYline}}.
enum		: {token,{enum,YYline}}.
extern		: {token,{extern,YYline}}.
float		: {token,{float,YYline}}.
for		: {token,{for,YYline}}.
goto		: {token,{goto,YYline}}.
if		: {token,{'if',YYline}}.
int		: {token,{int,YYline}}.
long		: {token,{long,YYline}}.
register	: {token,{register,YYline}}.
return	        : {token,{return,YYline}}.
short		: {token,{short,YYline}}.
signed		: {token,{signed,YYline}}.
sizeof		: {token,{sizeof,YYline}}.
static		: {token,{static,YYline}}.
struct		: {token,{struct,YYline}}.
switch		: {token,{switch,YYline}}.
typedef		: {token,{typedef,YYline}}.
union		: {token,{union,YYline}}.
unsigned	: {token,{unsigned,YYline}}.
void		: {token,{void,YYline}}.
volatile	: {token,{volatile,YYline}}.
while		: {token,{while,YYline}}.

"(\^.|\.|[^\"])*" :	S = lists:sublist(YYtext,2,length(YYtext)-2),
		  	{token,{string, YYline, S}}.

{L}({L}|{D})*	    :	case get({bic_type,YYtext}) of
			     undefined -> {token,{identifier,YYline,YYtext}};
			     Type -> {token,{type,YYline,YYtext}}
			end.

0[xX]{H}+{IS}?      : {token,{hexnum,YYline,YYtext}}.
0{D}+{IS}?	    : {token,{octnum,YYline,YYtext}}.
{D}+{IS}?	    : {token,{decnum,YYline,YYtext}}.

'(.|[^\'])+'	    : {token,{chrnum,YYline,YYtext}}.

{D}+{E}{FS}?	      : {token,{flonum,YYline,YYtext}}.
{D}*\.{D}+({E})?{FS}? : {token,{flonum,YYline,YYtext}}.
{D}+\.{D}*({E})?{FS}? : {token,{flonum,YYline,YYtext}}.

>>=		: {token,{'>>=',YYline}}.
<<=		: {token,{'<<=',YYline}}.
\+=		: {token,{'+=',YYline}}.
\-=		: {token,{'-=',YYline}}.
\*=		: {token,{'*=',YYline}}.
/=		: {token,{'/=',YYline}}.
\%=		: {token,{'%=',YYline}}.
&=		: {token,{'&=',YYline}}.
\^=		: {token,{'^=',YYline}}.
\|=		: {token,{'|=',YYline}}.
>>		: {token,{'>>',YYline}}.
<<		: {token,{'<<',YYline}}.
\+\+		: {token,{'++',YYline}}.
--		: {token,{'--',YYline}}.
->		: {token,{'->',YYline}}.
&&		: {token,{'&&',YYline}}.
\|\|		: {token,{'||',YYline}}.
<=		: {token,{'<=',YYline}}.
>=		: {token,{'>=',YYline}}.
==		: {token,{'==',YYline}}.
!=		: {token,{'!=',YYline}}.
;		: {token,{';',YYline}}.
{		: {token,{'{',YYline}}.
}		: {token,{'}',YYline}}.
,		: {token,{',',YYline}}.
:		: {token,{':',YYline}}.
=		: {token,{'=',YYline}}.
\(		: {token,{'(',YYline}}.
\)		: {token,{')',YYline}}.
\[		: {token,{'[',YYline}}.
\]		: {token,{']',YYline}}.
\.\.\.		: {token,{'...',YYline}}.
\.		: {token,{'.',YYline}}.
&		: {token,{'&',YYline}}.
!		: {token,{'!',YYline}}.
~		: {token,{'~',YYline}}.
-		: {token,{'-',YYline}}.
\+		: {token,{'+',YYline}}.
\*		: {token,{'*',YYline}}.
/		: {token,{'/',YYline}}.
\%		: {token,{'%',YYline}}.
<		: {token,{'<',YYline}}.
>		: {token,{'>',YYline}}.
\^		: {token,{'^',YYline}}.
\|		: {token,{'|',YYline}}.
\?		: {token,{'?',YYline}}.
#		: {token,{'#',YYline}}.
{WS}+		: .


Erlang code.

-export([init/0, scan/1]).

init() -> 
    erase(bic_scan_chars).

%%
%% The scanner MUST use bic_scan:token to get ONE token at the time
%% otherwise the parser won't work. This is beacuse of the grammer
%% is not capable of handling type names and identifiers in a good way.
%%
scan(Fd) ->
    case get(bic_scan_chars) of
	undefined ->
	    put(bic_scan_chars,[]),
	    scan(Fd, [], []);
	Chars ->
	    scan(Fd,[],Chars)
    end.

scan(Fd, Cont, eof) ->
    put(bic_scan_chars, eof),
    {eof, bic_cpp:cline(Fd)};
scan(Fd, Cont, {error,Reason}) ->
    put(bic_scan_chars, {error,Reason}),
    {error, Reason, bic_cpp:cline(Fd)};
scan(Fd, Cont, Data) ->
    CLn = bic_cpp:cline(Fd),
    Tr = token(Cont,Data,CLn),
    %% io:format("~w: data=~p, R=~p\n", [CLn,Data,Tr]),
    case Tr of
    	{more, Cont1} ->
	    scan(Fd, Cont1, bic_cpp:read(Fd));
	{done,{ok,{'#',Ln},_Ln1}, Data1} ->
	    %% skip injected lines
	    scan(Fd, [], bic_cpp:read(Fd));
	{done,{ok,Tok,Line1}, Data1} ->
	    put(bic_scan_chars, Data1),
	    {ok, [Tok], Line1};
	{done, {error,Reason}, Data1} ->
	    put(bic_scan_chars, {error,Reason}),
	    {error, Reason, bic_cpp:cline(Fd)}
    end.
