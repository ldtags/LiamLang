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
  int type;
  char * string;
  struct SymEntry * SymEntry;
  struct IdList *  IdList;
  struct ExprRes * ExprRes;
  struct ExprList * ExprList;
  struct InstrSeq * InstrSeq;
}

%type <type> Var
%type <string> Id
%type <string> String
%type <IdList> IdListItem
%type <IdList> IdList
%type <ExprList> ExprListItem
%type <ExprList> ExprList
%type <ExprRes> Factor
%type <ExprRes> Unary
%type <ExprRes> Expon
%type <ExprRes> Term
%type <ExprRes> Expr
%type <ExprRes> CmpExpr
%type <ExprRes> EqExpr
%type <ExprRes> BExpr
%type <ExprRes> iExpr
%type <ExprRes> ArrFactor
%type <ExprRes> OptArr
%type <InstrSeq> StmtSeq
%type <InstrSeq> Stmt

%token Ident 		
%token IntLit
%token ListLit
%token StringLit
%token Int
%token Print
%token Println
%token WriteLines
%token WriteSpaces
%token WriteString
%token Read
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
%token INCR

%%

Prog			    :	Declarations StmtSeq						                        { Finish($2); };
Declarations	:	Dec Declarations						                            { };
Declarations	:											                                    { };
Dec			      :	Var Id ';'	                                            { declare($2, $1, NULL, NULL); };
Dec           : Var Id '[' ArrFactor ']' ';'                            { declare($2, $1, $4, NULL); };
Dec           : Var Id '[' ArrFactor ']' '[' ArrFactor ']' ';'          { declare($2, $1, $4, $7); };
Var           : Int                                                     { $$ = 0; };
Var           : Bool                                                    { $$ = 1; };
ArrFactor     : IntLit									                                { $$ = doIntLit(yytext); };
StmtSeq 	    :	Stmt StmtSeq								                            { $$ = AppendSeq($1, $2); };
StmtSeq		    :											                                    { $$ = NULL; };
Stmt          : Print '(' ExprList ')' ';'                              { $$ = doIOPrint($3); };
Stmt          : Println '(' ExprList ')' ';'                            { $$ = doPrintln($3); };
Stmt          : Read '(' IdList ')' ';'                                 { $$ = doIORead($3); };
Stmt          : WriteLines '(' iExpr ')' ';'                            { $$ = doPrintLines($3); };
Stmt          : WriteSpaces '(' iExpr ')' ';'                           { $$ = doPrintSpaces($3); };
Stmt          : WriteString '(' String ')' ';'                          { $$ = doPrintString($3); };
Stmt          : Id INCR ';'                                             { $$ = doIncr($1); };
Stmt			    :	Id '=' iExpr ';'								                        { $$ = doAssign($1, $3); };
Stmt          : Id '[' iExpr ']' '=' iExpr ';'                          { $$ = doArrAssign($1, $3, $6); };
Stmt          : Id '[' iExpr ']' '[' iExpr ']' '=' iExpr ';'            { $$ = do2DAssign($1, $3, $6, $9); };
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
Expon         : Unary '^' Expon                                         { $$ = doExp($1, $3); };
Expon         : Unary                                                   { $$ = $1; };
Unary         : '-' Unary                                               { $$ = doUMin($2); };
Unary         : '!' Unary                                               { $$ = doNot($2); };
Unary         : Factor                                                  { $$ = $1; };
Factor        : '(' iExpr ')'                                           { $$ = $2; };
Factor		    :	IntLit									                                { $$ = doIntLit(yytext); };
Factor		    :	Id         								                              { $$ = doLoadVal($1); };
Factor        : Id '[' iExpr ']'																				{ $$ = doLoadArrVal($1, $3); };
Factor        : Id '[' iExpr ']' '[' iExpr ']'                          { $$ = doLoad2DArrVal($1, $3, $6); };
Factor        : TRUE                                                    { $$ = doBoolLit(1); };
Factor        : FALSE                                                   { $$ = doBoolLit(0); };
ExprList      : ExprListItem                                            { $$ = $1; };
ExprList      : ExprListItem ',' ExprList                               { $$ = addToExprList($3, $1); };
ExprListItem  : iExpr                                                   { $$ = createExprListItem($1); };
IdList        : IdListItem                                              { $$ = $1; };
IdList        : IdListItem ',' IdList                                   { $$ = addToIdList($3, $1); };
IdListItem    : Id OptArr                                               { $$ = createIdListItem($1, $2); };
OptArr        : '[' iExpr ']'                                           { $$ = $2; };
OptArr        : '[' iExpr ']' '[' iExpr ']'                             { $$ = doMult($2, $5); };
OptArr        :                                                         { $$ = NULL; };
Id			      : Ident	  								                                { $$ = strdup(yytext); };
String        : StringLit                                               { $$ = strdup(yytext); };
 
%%

int yyerror(char *s)  {
  writeIndicator(getCurrentColumnNum());
  writeMessage("Illegal Character in YACC");
}
