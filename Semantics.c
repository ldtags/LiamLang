/* Semantics.c
   Support and semantic action routines.
   
*/

#include <strings.h>
#include <stdlib.h>

#include "CodeGen.h"
#include "Semantics.h"
#include "SymTab.h"
#include "IOMngr.h"

extern SymTab *table;

/* Semantics support routines */

struct ExprRes * doIntLit(char * digits)  { 

   	struct ExprRes *res;
  
  	res = (struct ExprRes *) malloc(sizeof(struct ExprRes));
  	res->Reg = AvailTmpReg();
  	res->Instrs = GenInstr(NULL,"li",TmpRegName(res->Reg),digits,NULL);

  	return res;
}

struct ExprRes * doBoolLit(int val) {
	struct ExprRes *Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));
	Res->Reg = AvailTmpReg();
	Res->Instrs = GenInstr(NULL, "li", TmpRegName(Res->Reg), Imm(val), NULL);
	return Res;
}

struct ExprRes * doRval(char * name)  { 

   	struct ExprRes *res;
  
   	if (!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared variable");
   	}
  	res = (struct ExprRes *) malloc(sizeof(struct ExprRes));
  	res->Reg = AvailTmpReg();
  	res->Instrs = GenInstr(NULL,"lw",TmpRegName(res->Reg),name,NULL);

  	return res;
}

struct ExprRes * doAdd(struct ExprRes * Res1, struct ExprRes * Res2)  { 
   	int reg = AvailTmpReg();
  	AppendSeq(Res1->Instrs,Res2->Instrs);
  	AppendSeq(Res1->Instrs,GenInstr(NULL,"add",
  	                                     TmpRegName(reg),
  	                                     TmpRegName(Res1->Reg),
  	                                     TmpRegName(Res2->Reg)));
  	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
  	Res1->Reg = reg;
  	free(Res2);
  	return Res1;
}

struct ExprRes * doSub(struct ExprRes * Res1, struct ExprRes * Res2) {
	int reg = AvailTmpReg();
	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs,GenInstr(NULL, "sub",
											TmpRegName(reg),
											TmpRegName(Res1->Reg),
											TmpRegName(Res2->Reg)));
	ReleaseTmpReg(Res1->Reg);
	ReleaseTmpReg(Res2->Reg);
	Res1->Reg = reg;
	free(Res2);
	return Res1;
}

struct ExprRes * doMult(struct ExprRes * Res1, struct ExprRes * Res2)  { 
   	int reg = AvailTmpReg();
  	AppendSeq(Res1->Instrs,Res2->Instrs);
  	AppendSeq(Res1->Instrs,GenInstr(NULL,"mul",
  	                                     TmpRegName(reg),
  	                                     TmpRegName(Res1->Reg),
  	                                     TmpRegName(Res2->Reg)));
  	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
  	Res1->Reg = reg;
  	free(Res2);
  	return Res1;
}

struct ExprRes * doDiv(struct ExprRes * Res1, struct ExprRes * Res2) {
	int reg = AvailTmpReg();
	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "div",
											TmpRegName(reg),
											TmpRegName(Res1->Reg),
											TmpRegName(Res2->Reg)));
	ReleaseTmpReg(Res1->Reg);
	ReleaseTmpReg(Res2->Reg);
	Res1->Reg = reg;
	free(Res2);
	return Res1;
}

struct ExprRes * doMod(struct ExprRes * Res1, struct ExprRes * Res2) {
	int reg = AvailTmpReg();

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "div", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));
	AppendSeq(Res1->Instrs, GenInstr(NULL, "mfhi", TmpRegName(reg), NULL, NULL));

	ReleaseTmpReg(Res1->Reg);
	ReleaseTmpReg(Res2->Reg);
	Res1->Reg = reg;
	free(Res2);
	return Res1;
}

struct ExprRes * doExp(struct ExprRes * Res1, struct ExprRes * Res2) {
	int reg = AvailTmpReg();
	int index = AvailTmpReg();
	char * loop = GenLabel();
	char * done = GenLabel();

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "li", TmpRegName(reg), "1", NULL));
	AppendSeq(Res1->Instrs, GenInstr(NULL, "li", TmpRegName(index), "0", NULL));
	AppendSeq(Res1->Instrs, GenInstr(loop, NULL, NULL, NULL, NULL));
	AppendSeq(Res1->Instrs, GenInstr(NULL, "beq", TmpRegName(index), TmpRegName(Res2->Reg), done));
	AppendSeq(Res1->Instrs, GenInstr(NULL, "mul", TmpRegName(reg), TmpRegName(reg), TmpRegName(Res1->Reg)));
	AppendSeq(Res1->Instrs, GenInstr(NULL, "addi", TmpRegName(index), TmpRegName(index), "1"));
	AppendSeq(Res1->Instrs, GenInstr(NULL, "j", loop, NULL, NULL));
	AppendSeq(Res1->Instrs, GenInstr(done, NULL, NULL, NULL, NULL));

	ReleaseTmpReg(index);
	ReleaseTmpReg(Res1->Reg);
	ReleaseTmpReg(Res2->Reg);
	Res1->Reg = reg;
	free(Res2);
	free(loop);
	free(done);
	return Res1;
}

struct ExprRes * doUMin(struct ExprRes * Res) {
	int reg = AvailTmpReg();
	int temp = AvailTmpReg();
	AppendSeq(Res->Instrs, GenInstr(NULL, "addi", TmpRegName(temp), "$zero", "-1"));
	AppendSeq(Res->Instrs, GenInstr(NULL, "mul",
										   TmpRegName(reg),
										   TmpRegName(Res->Reg),
										   TmpRegName(temp)));
	ReleaseTmpReg(Res->Reg);
	ReleaseTmpReg(temp);
	Res->Reg = reg;
	return Res;
}

struct InstrSeq * doPrint(struct ExprRes * Expr) { 

  	struct InstrSeq *code;
    
  	code = Expr->Instrs;
  
	AppendSeq(code,GenInstr(NULL,"li","$v0","1",NULL));
	AppendSeq(code,GenInstr(NULL,"move","$a0",TmpRegName(Expr->Reg),NULL));
	AppendSeq(code,GenInstr(NULL,"syscall",NULL,NULL,NULL));

    AppendSeq(code,GenInstr(NULL,"li","$v0","4",NULL));
    AppendSeq(code,GenInstr(NULL,"la","$a0","_nl",NULL));
    AppendSeq(code,GenInstr(NULL,"syscall",NULL,NULL,NULL));

    ReleaseTmpReg(Expr->Reg);
    free(Expr);

  	return code;
}

struct InstrSeq * doAssign(char *name, struct ExprRes * Expr) { 

  	struct InstrSeq *code;
  

   	if (!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared variable");
   	}

  	code = Expr->Instrs;
  
  	AppendSeq(code,GenInstr(NULL,"sw",TmpRegName(Expr->Reg), name,NULL));

  	ReleaseTmpReg(Expr->Reg);
  	free(Expr);
  
  	return code;
}

struct ExprRes * doEq(struct ExprRes * Res1,  struct ExprRes * Res2) {
    int reg = AvailTmpReg();
 	struct ExprRes * Res = (struct ExprRes *) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "seq", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

    Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
	free(Res1);
	free(Res2);
	return Res;
}

struct ExprRes * doNeq(struct ExprRes * Res1,  struct ExprRes * Res2) {
	int reg = AvailTmpReg();
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "sne", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

    Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
	free(Res1);
	free(Res2);
	return Res;
}

struct ExprRes * doLT(struct ExprRes * Res1, struct ExprRes * Res2) {
	int reg = AvailTmpReg();
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "slt", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

    Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
	free(Res1);
	free(Res2);
	return Res;
}

struct ExprRes * doLTE(struct ExprRes * Res1, struct ExprRes * Res2) {
	int reg = AvailTmpReg();
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "sle", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

    Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
	free(Res1);
	free(Res2);
	return Res;
}

struct ExprRes * doGT(struct ExprRes * Res1, struct ExprRes * Res2) {
	int reg = AvailTmpReg();
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "sgt", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

    Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
	free(Res1);
	free(Res2);
	return Res;
}

struct ExprRes * doGTE(struct ExprRes * Res1, struct ExprRes * Res2) {
	int reg = AvailTmpReg();
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "sge", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

    Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
	free(Res1);
	free(Res2);
	return Res;
}

// struct ExprRes * GEQ(struct ExprRes * Res1, struct ExprRes * Res2, char * OpCode) {
// 	int reg = AvailTmpReg();
// 	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));

// 	AppendSeq(Res1->Instrs, Res2->Instrs);
// 	AppendSeq(Res1->Instrs, GenInstr(NULL, OpCode, TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

//     Res->Reg = reg;
// 	Res->Instrs = Res1->Instrs;
// 	ReleaseTmpReg(Res1->Reg);
//   	ReleaseTmpReg(Res2->Reg);
// 	free(Res1);
// 	free(Res2);
// 	free(OpCode);
// 	return Res;
// }

struct ExprRes * doAnd(struct ExprRes * Res1,  struct ExprRes * Res2) {
	int reg = AvailTmpReg();

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "and", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

	ReleaseTmpReg(Res1->Reg);
	ReleaseTmpReg(Res2->Reg);
	Res1->Reg = reg;
	free(Res2);
	return Res1;
}

struct ExprRes * doOr(struct ExprRes * Res1,  struct ExprRes * Res2) {
	int reg = AvailTmpReg();

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, "or", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

	ReleaseTmpReg(Res1->Reg);
	ReleaseTmpReg(Res2->Reg);
	Res1->Reg = reg;
	free(Res2);
	return Res1;
}

struct ExprRes * doNot(struct ExprRes * Res) {
    int reg = AvailTmpReg();
	char * els = GenLabel();
	char * end = GenLabel();
 	
	AppendSeq(Res->Instrs, GenInstr(NULL, "beq", "$zero", TmpRegName(Res->Reg), els));
	AppendSeq(Res->Instrs, GenInstr(NULL, "li", TmpRegName(reg), "0", NULL));
	AppendSeq(Res->Instrs, GenInstr(NULL, "j", end, NULL, NULL));
	AppendSeq(Res->Instrs, GenInstr(els, NULL, NULL, NULL, NULL));
	AppendSeq(Res->Instrs, GenInstr(NULL, "li", TmpRegName(reg), "1", NULL));
	AppendSeq(Res->Instrs, GenInstr(end, NULL, NULL, NULL, NULL));

	ReleaseTmpReg(Res->Reg);
	Res->Reg = reg;
	free(els);
	free(end);
	return Res;
}

extern struct InstrSeq * doIf(struct ExprRes * Res, struct InstrSeq * seq) {
	struct InstrSeq * seq2;
    char * label = GenLabel();

    AppendSeq(Res->Instrs, GenInstr(NULL, "beq", "$zero", TmpRegName(Res->Reg), label));
	seq2 = AppendSeq(Res->Instrs, seq);
	AppendSeq(seq2, GenInstr(label, NULL, NULL, NULL, NULL));

	ReleaseTmpReg(Res->Reg);
	free(Res);
	free(seq);
	free(label);
	return seq2;
}

struct InstrSeq  *  doIfElse(struct ExprRes * Res, struct InstrSeq * ifseq, struct InstrSeq * elseq) {
	struct InstrSeq * seq;
	char * els = GenLabel();
	char * end = GenLabel();

	AppendSeq(Res->Instrs, GenInstr(NULL, "beq", "$zero", TmpRegName(Res->Reg), els));
	seq = AppendSeq(Res->Instrs, ifseq);
	AppendSeq(seq, GenInstr(NULL, "j", end, NULL, NULL));
	AppendSeq(seq, GenInstr(els, NULL, NULL, NULL, NULL));
	AppendSeq(seq, elseq);
	AppendSeq(seq, GenInstr(end, NULL, NULL, NULL, NULL));

	ReleaseTmpReg(Res->Reg);
	free(Res);
	free(ifseq);
	free(elseq);
	free(els);
	free(end);
	return seq;
}

struct InstrSeq * doWhile(struct ExprRes * Res, struct InstrSeq * lpseq) {
	struct InstrSeq * seq;
	char * loop = GenLabel();
	char * done = GenLabel();

	AppendSeq(Res->Instrs, GenInstr(loop, NULL, NULL, NULL, NULL));
	AppendSeq(Res->Instrs, GenInstr(NULL, "beq", "$zero", TmpRegName(Res->Reg), done));
	seq = AppendSeq(Res->Instrs, lpseq);
	AppendSeq(seq, GenInstr(NULL, "j", loop, NULL, NULL));
	AppendSeq(seq, GenInstr(done, NULL, NULL, NULL, NULL));

	ReleaseTmpReg(Res->Reg);
	free(Res);
	free(lpseq);
	free(loop);
	free(done);
	return seq;
}

/*

extern struct InstrSeq * doIf(struct ExprRes *res1, struct ExprRes *res2, struct InstrSeq * seq) {
	struct InstrSeq *seq2;
	char * label;
	label = GenLabel();
	AppendSeq(res1->Instrs, res2->Instrs);
	AppendSeq(res1->Instrs, GenInstr(NULL, "bne", TmpRegName(res1->Reg), TmpRegName(res2->Reg), label));
	seq2 = AppendSeq(res1->Instrs, seq);
	AppendSeq(seq2, GenInstr(label, NULL, NULL, NULL, NULL));
	ReleaseTmpReg(res1->Reg);
  	ReleaseTmpReg(res2->Reg);
	free(res1);
	free(res2);
	return seq2;
}

*/
void							 
Finish(struct InstrSeq *Code)
{ 	
	struct InstrSeq *code;
  	//struct SymEntry *entry;
    int hasMore;
  	struct Attr * attr;


  	code = GenInstr(NULL,".text",NULL,NULL,NULL);
  	//AppendSeq(code,GenInstr(NULL,".align","2",NULL,NULL));
  	AppendSeq(code,GenInstr(NULL,".globl","main",NULL,NULL));
  	AppendSeq(code, GenInstr("main",NULL,NULL,NULL,NULL));
  	AppendSeq(code,Code);
  	AppendSeq(code, GenInstr(NULL, "li", "$v0", "10", NULL)); 
  	AppendSeq(code, GenInstr(NULL,"syscall",NULL,NULL,NULL));
  	AppendSeq(code,GenInstr(NULL,".data",NULL,NULL,NULL));
  	AppendSeq(code,GenInstr(NULL,".align","4",NULL,NULL));
  	AppendSeq(code,GenInstr("_nl",".asciiz","\"\\n\"",NULL,NULL));

 	hasMore = startIterator(table);
 	while (hasMore) {
		AppendSeq(code,GenInstr((char *) getCurrentName(table),".word","0",NULL,NULL));
    	hasMore = nextEntry(table);
 	}
  
  	WriteSeq(code);
  
  	return;
}
