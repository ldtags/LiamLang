%{
//#include "yaccExample.h"
#include "IOMngr.h"
#include "y.tab.h"

#define YY_INPUT(buf,result,max_size) \
	{ int c = getNextSourceChar(); \
    result = (c == EOF) ? YY_NULL : (buf[0] = c, 1); \
  }

%}


letter [A-Za-z]
digit [0-9]

%%
if					                                                          {return IF;}
else                                                                  {return ELSE;}
while                                                                 {return WHILE;}
int					                                                          {return Int;}
bool                                                                  {return Bool;}
print                                                                 {return Print;}
printlines                                                            {return WriteLines;}
printspaces                                                           {return WriteSpaces;}
printstring                                                           {return WriteString;}       
read                                                                  {return Read;}
true                                                                  {return TRUE;}
false                                                                 {return FALSE;}
{letter}({letter}|{digit})*	                                          {return Ident;}
{digit}{digit}*		                                                    {return IntLit;}
\"(.|\n)*\"                                                           {return StringLit;}
\=\=					                                                        {return EQ;}
\!\=                                                                  {return NEQ;}
\|\|                                                                  {return OR;}
\&\&                                                                  {return AND;}
(\<\=)|(\=\<)                                                         {return LTE;}
(\>\=)|(\=\>)                                                         {return GTE;}
\+\+                                                                  {return INCR;}
\=					                                                          {return '=';}
\<                                                                    {return '<';}
\>                                                                    {return '>';}
\!                                                                    {return '!';}
\+					                                                          {return '+';}
\-                                                                    {return '-';}
\*					                                                          {return '*';}
\/                                                                    {return '/';}
\%                                                                    {return '%';}
\^                                                                    {return '^';}
\;					                                                          {return ';';}
\{					                                                          {return '{';}
\}					                                                          {return '}';}
\(					                                                          {return '(';}
\)					                                                          {return ')';}
\[                                                                    {return '[';}
\]                                                                    {return ']';}
\,                                                                    {return ',';}

\/\/.*\n                                                              {}
\/\*(.|\n)*\*\/                                                       {}
[ ]				                                                            {}
\t				                                                            {}
\r				                                                            {}
\n				                                                            {}

.				{writeIndicator(getCurrentColumnNum());
				 writeMessage("Illegal Character in lex");}

%%

	
int yywrap () {
    return 1;
}