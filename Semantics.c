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

struct ExprRes *  doIntLit(char * digits)  { 

   	struct ExprRes *res;
  
  	res = (struct ExprRes *) malloc(sizeof(struct ExprRes));
  	res->Reg = AvailTmpReg();
  	res->Instrs = GenInstr(NULL,"li",TmpRegName(res->Reg),digits,NULL);

  	return res;
}

struct ExprRes *  doRval(char * name)  { 

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

struct ExprRes *  doAdd(struct ExprRes * Res1, struct ExprRes * Res2)  { 

   	int reg;
   
  	reg = AvailTmpReg();
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

struct ExprRes *  doMult(struct ExprRes * Res1, struct ExprRes * Res2)  { 

   	int reg;
   
  	reg = AvailTmpReg();
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

struct ExprRes * doBoolLit(char * val) {

}

struct ExprRes * doEq(struct ExprRes * Res1,  struct ExprRes * Res2) {
	struct ExprRes * Res;
    int reg = AvailTmpReg();
	AppendSeq(Res1->Instrs, Res2->Instrs);
 	Res = (struct ExprRes *) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, GenInstr(NULL, "seq", TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));
    Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
	free(Res1);
	free(Res2);
	return Res;
}

struct ExprRes * doAnd(struct ExprRes * Res1,  struct ExprRes * Res2) {

}

struct ExprRes * doOr(struct ExprRes * Res1,  struct ExprRes * Res2) {

}

extern struct InstrSeq * doIf(struct ExprRes * Res, struct InstrSeq * seq) {
	struct InstrSeq * seq2;
    char * label = GenLabel();
    AppendSeq(Res->Instrs, GenInstr(NULL, "beq", "$zero", TmpRegName(Res->Reg), label));
	seq2 = AppendSeq(Res->Instrs, seq);
	AppendSeq(seq2, GenInstr(label, NULL, NULL, NULL, NULL));
	free(Res);
	return seq2;
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
