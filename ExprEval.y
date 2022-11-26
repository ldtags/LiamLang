%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "SymTab.h"
#include "IOMngr.h"
#include "Semantics.h"
#include "CodeGen.h"

extern int yylex();	/* The next token function. */
extern char *yytext;   /* The matched token text.  */
extern int yyleng;      /* The token text length.   */
extern int yyparse();
extern int yyerror(char* s);
void dumpTable();

extern SymTab *table;


%}


%union {
  long val;
  char * string;
  struct ExprRes * ExprRes;
  struct InstrSeq * InstrSeq;
}

%type <string> Id
%type <ExprRes> Factor
%type <ExprRes> Unary
%type <ExprRes> Expon
%type <ExprRes> Term
%type <ExprRes> Expr
%type <ExprRes> CmpExpr
%type <ExprRes> EqExpr
%type <ExprRes> BExpr
%type <ExprRes> iExpr
%type <InstrSeq> StmtSeq
%type <InstrSeq> Stmt

%token Ident 		
%token IntLit 	
%token Int
%token Write
%token Bool
%token TRUE
%token FALSE
%token WHILE
%token IF
%token ELSE
%token EQ
%token NEQ
%token OR
%token AND
%token LTE
%token GTE

%%

Prog			    :	Declarations StmtSeq						                        { Finish($2); };
Declarations	:	Dec Declarations						                            { };
Declarations	:											                                    { };
Dec			      :	Int Id ';'	                                            { enterName(table, $2); };
Dec           : Bool Id ';'                                             { enterName(table, $2); };
StmtSeq 	    :	Stmt StmtSeq								                            { $$ = AppendSeq($1, $2); };
StmtSeq		    :											                                    { $$ = NULL; };
Stmt			    :	Write iExpr ';'								                          { $$ = doPrint($2); };
Stmt			    :	Id '=' iExpr ';'								                        { $$ = doAssign($1, $3); };
Stmt			    :	IF '(' iExpr ')' '{' StmtSeq '}'	                      { $$ = doIf($3, $6); };
Stmt          : IF '(' iExpr ')' '{' StmtSeq '}' ELSE '{' StmtSeq '}'   { $$ = doIfElse($3, $6, $10); }; 
Stmt          : WHILE '(' iExpr ')' '{' StmtSeq '}'                     { $$ = doWhile($3, $6); };
iExpr         : iExpr OR BExpr                                          { $$ = doOr($1, $3); };
iExpr         : BExpr                                                   { $$ = $1; };
BExpr         : BExpr AND EqExpr                                        { $$ = doAnd($1, $3); };
BExpr         : EqExpr                                                  { $$ = $1; };
EqExpr		    :	EqExpr EQ CmpExpr								                        { $$ = doEq($1, $3); };
EqExpr        : EqExpr NEQ CmpExpr                                      { $$ = doNeq($1, $3); };
EqExpr        : CmpExpr                                                 { $$ = $1; };
CmpExpr       : CmpExpr '<' Expr                                        { $$ = doLT($1, $3); };
CmpExpr       : CmpExpr LTE Expr                                        { $$ = doLTE($1, $3); };
CmpExpr       : CmpExpr '>' Expr                                        { $$ = doGT($1, $3); };
CmpExpr       : CmpExpr GTE Expr                                        { $$ = doGTE($1, $3); };
CmpExpr       : Expr                                                    { $$ = $1; };
Expr			    :	Expr '+' Term								                            { $$ = doAdd($1, $3); };
Expr          : Expr '-' Term                                           { $$ = doSub($1, $3); };
Expr			    :	Term									                                  { $$ = $1; };
Term		      :	Term '*' Expon								                          { $$ = doMult($1, $3); };
Term          : Term '/' Expon                                          { $$ = doDiv($1, $3); };
Term          : Term '%' Expon                                          { $$ = doMod($1, $3); };
Term		      :	Expon 									                                { $$ = $1; };
Expon         : Expon '^' Unary                                         { $$ = doExp($1, $3); };
Expon         : Unary                                                   { $$ = $1; };
Unary         : '-' Unary                                               { $$ = doUMin($2); };
Unary         : '!' Unary                                               { $$ = doNot($2); };
Unary         : Factor                                                  { $$ = $1; };
Factor        : '(' iExpr ')'                                           { $$ = $2; };
Factor		    :	IntLit									                                { $$ = doIntLit(yytext); };
Factor		    :	Id									                                    { $$ = doRval($1); };
Factor        : TRUE                                                    { $$ = doBoolLit(1); };
Factor        : FALSE                                                   { $$ = doBoolLit(0); };
Id			      : Ident	  								                                { $$ = strdup(yytext); };
 
%%

int yyerror(char *s)  {
  writeIndicator(getCurrentColumnNum());
  writeMessage("Illegal Character in YACC");
}
