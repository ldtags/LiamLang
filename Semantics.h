/* Semantics.h
   The action and supporting routines for performing semantics processing.
*/
#define MAX_STRING 32

/* Semantic Records */
enum Type {INT, BOOL, STRING, VOID};

typedef struct Attribute {
  enum Type type;
  int array;
  int size;
  int factor;
} Attribute;

struct ExprRes {
  int Reg;
  struct Attribute * Attr;
  struct InstrSeq * Instrs;
};

struct ExprList {
	struct ExprRes * Expr;
	struct ExprList * Next;
};

struct IdList {
  struct SymEntry * Entry;
  struct ExprRes * OffExpr;
  struct IdList * Next;
};


/* Semantics Actions */
extern struct ExprRes   *  doIntLit(char * digits);
extern struct ExprRes   *  doBoolLit(int val);
extern struct ExprRes   *  doLoadVal(char * name);
extern struct ExprRes   *  doLoadArrVal(char * name, struct ExprRes * offExpr);
extern struct ExprRes   *  doLoad2DArrVal(char * name, struct ExprRes * offExpr1, struct ExprRes * offExpr2);
extern struct InstrSeq  *  doAssign(char * name, struct ExprRes * Res1);
extern struct InstrSeq  *  doArrAssign(char * name, struct ExprRes * offExpr, struct ExprRes * valExpr);
extern struct InstrSeq  *  do2DAssign(char * name, struct ExprRes * offExpr1, struct ExprRes * offExpr2, struct ExprRes * valExpr);
extern struct ExprRes   *  doAdd(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doSub(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doMult(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doDiv(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  GAR(struct ExprRes * Res1, struct ExprRes * Res2, char * OpCode);
extern struct ExprRes   *  doMod(struct ExprRes * Res1, struct ExprRes * Res2);
extern struct ExprRes   *  doExp(struct ExprRes * Res1, struct ExprRes * Res2);
extern struct ExprRes   *  doUMin(struct ExprRes * Res);
extern struct InstrSeq  *  doIncr(char * name);
extern struct ExprRes   *  doEq(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doNeq(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doLT(struct ExprRes * Res1, struct ExprRes * Res2);
extern struct ExprRes   *  doLTE(struct ExprRes * Res1, struct ExprRes * Res2);
extern struct ExprRes   *  doGT(struct ExprRes * Res1, struct ExprRes * Res2);
extern struct ExprRes   *  doGTE(struct ExprRes * Res1, struct ExprRes * Res2);
extern struct ExprRes   *  GEQ(struct ExprRes * Res1, struct ExprRes * Res2, char * OpCode);
extern struct ExprRes   *  doAnd(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doOr(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doNot(struct ExprRes * Res);
extern struct InstrSeq  *  doIf(struct ExprRes *bRes, struct InstrSeq * seq);
extern struct InstrSeq  *  doIfElse(struct ExprRes * Res, struct InstrSeq * ifseq, struct InstrSeq * elseq);
extern struct InstrSeq  *  doWhile(struct ExprRes * Res, struct InstrSeq * seq);
extern struct ExprList  *  createExprListItem(struct ExprRes * Res);
extern struct ExprList  *  addToExprList(struct ExprList * list, struct ExprList * listItem);
extern struct InstrSeq  *  doIOPrint(struct ExprList * list);
extern struct InstrSeq  *  doPrintln(struct ExprList * list);
extern struct IdList    *  createIdListItem(char * id, struct ExprRes * offExpr);
extern struct IdList    *  addToIdList(struct IdList * list, struct IdList * listItem);
extern struct InstrSeq  *  doIORead(struct IdList * list);
extern struct InstrSeq  *  doPrintLines(struct ExprRes * Expr);
extern struct InstrSeq  *  doPrintSpaces(struct ExprRes * Expr);
extern struct InstrSeq  *  doPrintString(char * string);
extern struct InstrSeq  *  doPrintInt(int reg);
extern struct InstrSeq  *  doPrintBool(int reg);

extern void	Finish(struct InstrSeq *Code);
extern void declare(char * name, enum Type type, struct ExprRes * Res1, struct ExprRes * Res2);

/*
  Returns the size of specified enum Type type
  Sizes based off of the C native types
*/
extern int size(enum Type type);