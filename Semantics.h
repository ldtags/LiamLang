/* Semantics.h
   The action and supporting routines for performing semantics processing.
*/

/* Semantic Records */
struct IdList {
  struct SymEntry * TheEntry;
  struct IdList * Next;
};

struct ExprRes {
  int Reg;
  struct InstrSeq * Instrs;
};

struct ExprResList {
	struct ExprRes *Expr;
	struct ExprResList * Next;
};


/* Semantics Actions */
extern struct ExprRes   *  doIntLit(char * digits);
extern struct ExprRes   *  doBoolLit(int val);
extern struct ExprRes   *  doRval(char * name);
extern struct InstrSeq  *  doAssign(char * name,  struct ExprRes * Res1);
extern struct ExprRes   *  doAdd(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doSub(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doMult(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doDiv(struct ExprRes * Res1,  struct ExprRes * Res2);
extern struct ExprRes   *  doMod(struct ExprRes * Res1, struct ExprRes * Res2);
extern struct ExprRes   *  doExp(struct ExprRes * Res1, struct ExprRes * Res2);
extern struct ExprRes   *  doUMin(struct ExprRes * Res);
extern struct ExprRes   *  doIncr(struct ExprRes * Res);
extern struct InstrSeq  *  doPrint(struct ExprRes * Expr);
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

extern void	Finish(struct InstrSeq *Code);
