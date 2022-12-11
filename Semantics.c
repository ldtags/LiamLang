/* Semantics.c
   Support and semantic action routines.
   
*/

#include <strings.h>
#include <string.h>
#include <stdlib.h>

#include "CodeGen.h"
#include "Semantics.h"
#include "SymTab.h"
#include "IOMngr.h"

extern SymTab *table;
extern SymTab *stringTable;

/* Semantics support routines */

struct ExprRes * doIntLit(char * digits)  { 
   	struct ExprRes * res = (struct ExprRes *) malloc(sizeof(struct ExprRes));
	struct Attribute * attr = (struct Attribute *) malloc(sizeof(struct Attribute));
  	res->Reg = AvailTmpReg();
  	res->Instrs = GenInstr(NULL, "li", TmpRegName(res->Reg), digits, NULL);
	attr->type = INT;
	attr->array = 0;
	attr->factor = 0;
	attr->size = sizeof(int);
	res->Attr = attr;
  	return res;
}

struct ExprRes * doBoolLit(int val) {
	struct ExprRes *res = (struct ExprRes*) malloc(sizeof(struct ExprRes));
	struct Attribute * attr = (struct Attribute *) malloc(sizeof(struct Attribute));
	res->Reg = AvailTmpReg();
	res->Instrs = GenInstr(NULL, "li", TmpRegName(res->Reg), Imm(val), NULL);
	attr->type = BOOL;
	attr->array = 0;
	attr->factor = 0;
	attr->size = sizeof(_Bool);
	res->Attr = attr;
	return res;
}

void declare(char * name, enum Type type, struct ExprRes * Res1, struct ExprRes * Res2) {
	if(!enterName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Variable already exists");
	}

	int size = 1;
	char ** buf;
	struct Attribute * attr = (struct Attribute*) malloc(sizeof(struct Attribute));
	attr->type = type;
	attr->array = 0;
	attr->factor = 0;

	if(Res1 != NULL) {
		attr->array = 1;
		size = (int) strtol(Res1->Instrs->Oprnd2, buf, 10);
		ReleaseTmpReg(Res1->Reg);
		free(Res1);
	}

	if(Res2 != NULL) {
		attr->factor = (int) strtol(Res2->Instrs->Oprnd2, buf, 10);
		size *= attr->factor;
		ReleaseTmpReg(Res2->Reg);
		free(Res2);
	}

	switch(type) {
		case INT:
			attr->size = size * (int) sizeof(int);
			break;
		case BOOL:
			attr->size = size * (int) sizeof(_Bool);
			break;
		case VOID:
			writeIndicator(getCurrentColumnNum());
			writeMessage("Void only supported for function use");
			break;
		default:
			writeIndicator(getCurrentColumnNum());
			writeMessage("How did you even do that?");
	}

	setCurrentAttr(table, attr);
}

void declareFunction() {
	
}

struct ExprRes * doLoadVal(char * name)  { 
   	if (!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared variable");
   	}

  	struct ExprRes * res = (struct ExprRes *) malloc(sizeof(struct ExprRes));
  	res->Reg = AvailTmpReg();
	res->Attr = getCurrentAttr(table);
	res->Instrs = GenInstr(NULL, "lw" , TmpRegName(res->Reg), name, NULL);

  	return res;
}

struct ExprRes * doLoadArrVal(char * name, struct ExprRes * offExpr) {
	if(!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared array");
	}

	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));
	Res->Reg = AvailTmpReg();
	Res->Attr = getCurrentAttr(table);
	Res->Instrs = offExpr->Instrs;
	AppendSeq(Res->Instrs, GenInstr(NULL, "la", TmpRegName(Res->Reg), name, NULL));
	AppendSeq(Res->Instrs, GenInstr(NULL, "sll", TmpRegName(offExpr->Reg), TmpRegName(offExpr->Reg), Imm(2)));
	AppendSeq(Res->Instrs, GenInstr(NULL, "add", TmpRegName(Res->Reg), TmpRegName(Res->Reg), TmpRegName(offExpr->Reg)));
	AppendSeq(Res->Instrs, GenInstr(NULL, "lw", TmpRegName(Res->Reg), RegOff(0, TmpRegName(Res->Reg)), NULL));

	ReleaseTmpReg(offExpr->Reg);
	free(offExpr);
	return Res;
}

struct ExprRes * doLoad2DArrVal(char * name, struct ExprRes * offExpr1, struct ExprRes * offExpr2) {
	if(!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared array");
	}

	int off = AvailTmpReg();
	struct Attribute * attr = getCurrentAttr(table);
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));
	Res->Reg = AvailTmpReg();
	Res->Attr = attr;
	Res->Instrs = offExpr1->Instrs;
	AppendSeq(Res->Instrs, offExpr2->Instrs);
	AppendSeq(Res->Instrs, GenInstr(NULL, "la", TmpRegName(Res->Reg), name, NULL));
	AppendSeq(Res->Instrs, GenInstr(NULL, "li", TmpRegName(off), Imm(attr->factor), NULL));
	AppendSeq(Res->Instrs, GenInstr(NULL, "mul", TmpRegName(off), TmpRegName(off), TmpRegName(offExpr1->Reg)));
	AppendSeq(Res->Instrs, GenInstr(NULL, "add", TmpRegName(off), TmpRegName(off), TmpRegName(offExpr2->Reg)));
	AppendSeq(Res->Instrs, GenInstr(NULL, "sll", TmpRegName(off), TmpRegName(off), Imm(2)));
	AppendSeq(Res->Instrs, GenInstr(NULL, "add", TmpRegName(Res->Reg), TmpRegName(Res->Reg), TmpRegName(off)));
	AppendSeq(Res->Instrs, GenInstr(NULL, "lw", TmpRegName(Res->Reg), RegOff(0, TmpRegName(Res->Reg)), NULL));

	ReleaseTmpReg(offExpr1->Reg);
	ReleaseTmpReg(offExpr2->Reg);
	ReleaseTmpReg(off);
	free(offExpr1);
	free(offExpr2);
	return Res;
}

struct InstrSeq * doAssign(char * name, struct ExprRes * Expr) { 
   	if (!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared variable");
   	}

  	struct InstrSeq * code = Expr->Instrs;
  	AppendSeq(code,GenInstr(NULL, "sw", TmpRegName(Expr->Reg), name, NULL));

  	ReleaseTmpReg(Expr->Reg);
  	free(Expr);
  	return code;
}

struct InstrSeq * doArrAssign(char * name, struct ExprRes * offExpr, struct ExprRes * valExpr) {
	if(!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared array");
	}

	int reg = AvailTmpReg();
	struct InstrSeq * seq = offExpr->Instrs;
	AppendSeq(seq, valExpr->Instrs);
	AppendSeq(seq, GenInstr(NULL, "la", TmpRegName(reg), name, NULL));
	AppendSeq(seq, GenInstr(NULL, "sll", TmpRegName(offExpr->Reg), TmpRegName(offExpr->Reg), Imm(2)));
	AppendSeq(seq, GenInstr(NULL, "add", TmpRegName(reg), TmpRegName(reg), TmpRegName(offExpr->Reg)));
	AppendSeq(seq, GenInstr(NULL, "sw", TmpRegName(valExpr->Reg), RegOff(0, TmpRegName(reg)), NULL));

	ReleaseTmpReg(offExpr->Reg);
	ReleaseTmpReg(valExpr->Reg);
	ReleaseTmpReg(reg);
	free(offExpr);
	free(valExpr);
	return seq;
}

struct InstrSeq * do2DAssign(char * name, struct ExprRes * offExpr1, struct ExprRes * offExpr2, struct ExprRes * valExpr) {
	if(!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared 2D array");
	}

	int reg = AvailTmpReg();
	int off = AvailTmpReg();
	struct Attribute * attr = getCurrentAttr(table);
	struct InstrSeq * seq = offExpr1->Instrs;
	AppendSeq(seq, offExpr2->Instrs);
	AppendSeq(seq, valExpr->Instrs);
	AppendSeq(seq, GenInstr(NULL, "li", TmpRegName(off), Imm(attr->factor), NULL));
	AppendSeq(seq, GenInstr(NULL, "mul", TmpRegName(off), TmpRegName(off), TmpRegName(offExpr1->Reg)));
	AppendSeq(seq, GenInstr(NULL, "add", TmpRegName(off), TmpRegName(off), TmpRegName(offExpr2->Reg)));
	AppendSeq(seq, GenInstr(NULL, "li", TmpRegName(reg), Imm(size(attr->type)), NULL));
	AppendSeq(seq, GenInstr(NULL, "mul", TmpRegName(off), TmpRegName(off), TmpRegName(reg)));
	AppendSeq(seq, GenInstr(NULL, "la", TmpRegName(reg), name, NULL));
	AppendSeq(seq, GenInstr(NULL, "add", TmpRegName(reg), TmpRegName(reg), TmpRegName(off)));
	AppendSeq(seq, GenInstr(NULL, "sw", TmpRegName(valExpr->Reg), RegOff(0, TmpRegName(reg)), NULL));

	ReleaseTmpReg(offExpr1->Reg);
	ReleaseTmpReg(offExpr2->Reg);
	ReleaseTmpReg(valExpr->Reg);
	ReleaseTmpReg(reg);
	ReleaseTmpReg(off);
	free(offExpr1);
	free(offExpr2);
	free(valExpr);
	return seq;
}

struct ExprRes * doAdd(struct ExprRes * Res1, struct ExprRes * Res2)  { 
   	return GAR(Res1, Res2, "add");
}

struct ExprRes * doSub(struct ExprRes * Res1, struct ExprRes * Res2) {
	return GAR(Res1, Res2, "sub");
}

struct ExprRes * doMult(struct ExprRes * Res1, struct ExprRes * Res2)  { 
   	return GAR(Res1, Res2, "mul");
}

struct ExprRes * doDiv(struct ExprRes * Res1, struct ExprRes * Res2) {
	return GAR(Res1, Res2, "div");
}

struct ExprRes * GAR(struct ExprRes * Res1, struct ExprRes * Res2, char * OpCode) {
	if(Res1->Attr->type != Res2->Attr->type) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("WARNING -- illegal typecasting");
	}
	int reg = AvailTmpReg();
	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, OpCode,
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
	AppendSeq(Res->Instrs, GenInstr(NULL, "mul", TmpRegName(reg), TmpRegName(Res->Reg), TmpRegName(temp)));
	ReleaseTmpReg(Res->Reg);
	ReleaseTmpReg(temp);
	Res->Reg = reg;
	return Res;
}

struct InstrSeq * doIncr(char * name) {
	if(!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Variable does not exist");
	}

	int reg = AvailTmpReg();
	struct InstrSeq * seq = GenInstr(NULL, "lw", TmpRegName(reg), name, NULL);
	AppendSeq(seq, GenInstr(NULL, "addi", TmpRegName(reg), TmpRegName(reg), Imm(1)));
	AppendSeq(seq, GenInstr(NULL, "sw", TmpRegName(reg), name, NULL));
	ReleaseTmpReg(reg);
	return seq;
}

struct ExprRes * doEq(struct ExprRes * Res1,  struct ExprRes * Res2) {
    return GEQ(Res1, Res2, "seq");
}

struct ExprRes * doNeq(struct ExprRes * Res1,  struct ExprRes * Res2) {
	return GEQ(Res1, Res2, "sne");
}

struct ExprRes * doLT(struct ExprRes * Res1, struct ExprRes * Res2) {
	return GEQ(Res1, Res2, "slt");
}

struct ExprRes * doLTE(struct ExprRes * Res1, struct ExprRes * Res2) {
	return GEQ(Res1, Res2, "sle");
}

struct ExprRes * doGT(struct ExprRes * Res1, struct ExprRes * Res2) {
	return GEQ(Res1, Res2, "sgt");
}

struct ExprRes * doGTE(struct ExprRes * Res1, struct ExprRes * Res2) {
	return GEQ(Res1, Res2, "sge");
}

struct ExprRes * GEQ(struct ExprRes * Res1, struct ExprRes * Res2, char * OpCode) {
	if(Res1->Attr->type != Res2->Attr->type) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("WARNING -- illegal typecasting");
	}
	int reg = AvailTmpReg();
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, OpCode, 
										   TmpRegName(reg), 
										   TmpRegName(Res1->Reg), 
										   TmpRegName(Res2->Reg)));
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
	Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
	free(Res1);
	free(Res2);
	return Res;
}

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
	free(els);
	free(end);
	return seq;
}

struct InstrSeq * doWhile(struct ExprRes * Res, struct InstrSeq * lpseq) {
	struct InstrSeq * seq;
	char * loop = GenLabel();
	char * done = GenLabel();
	
	seq = AppendSeq(GenInstr(loop, NULL, NULL, NULL, NULL), Res->Instrs);
	AppendSeq(seq, GenInstr(NULL, "beq", "$zero", TmpRegName(Res->Reg), done));
	AppendSeq(seq, lpseq);
	AppendSeq(seq, GenInstr(NULL, "b", loop, NULL, NULL));
	AppendSeq(seq, GenInstr(done, NULL, NULL, NULL, NULL));

	ReleaseTmpReg(Res->Reg);
	free(Res);
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

struct ExprList * createExprListItem(struct ExprRes * Res) {
	struct ExprList * listItem = (struct ExprList*) malloc(sizeof(struct ExprList));
	listItem->Expr = Res;
	listItem->Next = NULL;
	return listItem;
}

struct ExprList * addToExprList(struct ExprList * list, struct ExprList * listItem) {
	listItem->Next = list;
	return listItem;
}

struct InstrSeq * doIOPrint(struct ExprList * list) {
	struct InstrSeq * instr = NULL;
	struct ExprRes * expr;
	int type;

	while(list != NULL) {
		expr = list->Expr;
		instr = AppendSeq(instr, list->Expr->Instrs);

		switch(expr->Attr->type) {
			case INT:
				AppendSeq(instr, doPrintInt(expr->Reg));
				break;
			case BOOL:
				AppendSeq(instr, doPrintBool(expr->Reg));
				break;
			default:
				writeIndicator(getCurrentColumnNum());
 				writeMessage("Unknown type, what are you trying to print?");
		}

		if(list->Next) {
			AppendSeq(instr, GenInstr(NULL, "la", "$a0", "_space", NULL));
			AppendSeq(instr, GenInstr(NULL, "li", "$v0", Imm(4), NULL));
			AppendSeq(instr, GenInstr(NULL, "syscall", NULL, NULL, NULL));
		}

		ReleaseTmpReg(list->Expr->Reg);
		free(list->Expr);
		list = list->Next;
	}

	free(list);
	return instr;
}

struct InstrSeq * doPrintln(struct ExprList * list) { 
  	struct InstrSeq * instr = doIOPrint(list);
	AppendSeq(instr, GenInstr(NULL, "la", "$a0", "_space", NULL));
	AppendSeq(instr, GenInstr(NULL, "li", "$v0", Imm(4), NULL));
	AppendSeq(instr, GenInstr(NULL, "syscall", NULL, NULL, NULL));
  	return instr;
}

struct IdList * createIdListItem(char * id, struct ExprRes * offExpr) {
	if(!findName(table, id)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("must declare variables");
	}
	struct IdList * listItem = (struct IdList*) malloc(sizeof(struct IdList));
	listItem->Entry = table->current;
	if(offExpr) {
		listItem->OffExpr = offExpr;
	} else {
		listItem->OffExpr = NULL;
	}
	listItem->Next = NULL;
	return listItem;
}

struct IdList * addToIdList(struct IdList * list, struct IdList * listItem) {
	listItem->Next = list;
	return listItem;
}

struct InstrSeq * doIORead(struct IdList * list) {
	struct InstrSeq * code = NULL;
	char *name;
	int reg = AvailTmpReg();

	while(list != NULL) {
		if(list->OffExpr) { code = AppendSeq(code, list->OffExpr->Instrs); }
		name = list->Entry->name;
		code = AppendSeq(code, GenInstr(NULL, "la", TmpRegName(reg), name, NULL));
		AppendSeq(code, GenInstr(NULL, "li", "$v0", Imm(5), NULL));
		AppendSeq(code, GenInstr(NULL, "syscall", NULL, NULL, NULL));

		if(list->OffExpr) {
			AppendSeq(code, GenInstr(NULL, "sll", TmpRegName(list->OffExpr->Reg), TmpRegName(list->OffExpr->Reg), Imm(2)));
			AppendSeq(code, GenInstr(NULL, "add", TmpRegName(reg), TmpRegName(reg), TmpRegName(list->OffExpr->Reg)));
			ReleaseTmpReg(list->OffExpr->Reg);
		}
		
		AppendSeq(code, GenInstr(NULL, "sw", "$v0", RegOff(0, TmpRegName(reg)), NULL));

		free(list->OffExpr);
		list = list->Next;
	}

	ReleaseTmpReg(reg);
	free(list);
	return code;
}

struct InstrSeq * doPrintLines(struct ExprRes * Expr) {
	struct InstrSeq * code = Expr->Instrs;
	char * loop = GenLabel();
	char * done = GenLabel();
	int reg = AvailTmpReg();

	AppendSeq(code, GenInstr(NULL, "li", TmpRegName(reg), Imm(0), NULL));
	AppendSeq(code, GenInstr(loop, NULL, NULL, NULL, NULL));
	AppendSeq(code, GenInstr(NULL, "beq", TmpRegName(reg), TmpRegName(Expr->Reg), done));
	AppendSeq(code, GenInstr(NULL, "li", "$v0", "4", NULL));
    AppendSeq(code, GenInstr(NULL, "la", "$a0", "_nl", NULL));
    AppendSeq(code, GenInstr(NULL, "syscall", NULL, NULL, NULL));
	AppendSeq(code, GenInstr(NULL, "addi", TmpRegName(reg), TmpRegName(reg), Imm(1)));
	AppendSeq(code, GenInstr(NULL, "b", loop, NULL, NULL));
	AppendSeq(code, GenInstr(done, NULL, NULL, NULL, NULL));

	ReleaseTmpReg(reg);
	ReleaseTmpReg(Expr->Reg);
	free(Expr);
	free(loop);
	free(done);
	return code;
}

struct InstrSeq * doPrintSpaces(struct ExprRes * Expr) {
	struct InstrSeq * code = Expr->Instrs;
	char * loop = GenLabel();
	char * done = GenLabel();
	int reg = AvailTmpReg();

	AppendSeq(code, GenInstr(NULL, "li", TmpRegName(reg), Imm(0), NULL));
	AppendSeq(code, GenInstr(loop, NULL, NULL, NULL, NULL));
	AppendSeq(code, GenInstr(NULL, "beq", TmpRegName(reg), TmpRegName(Expr->Reg), done));
	AppendSeq(code, GenInstr(NULL, "la", "$a0", "_space", NULL));
	AppendSeq(code, GenInstr(NULL, "li", "$v0", Imm(4), NULL));
	AppendSeq(code, GenInstr(NULL, "syscall", NULL, NULL, NULL));
	AppendSeq(code, GenInstr(NULL, "addi", TmpRegName(reg), TmpRegName(reg), Imm(1)));
	AppendSeq(code, GenInstr(NULL, "b", loop, NULL, NULL));
	AppendSeq(code, GenInstr(done, NULL, NULL, NULL, NULL));

	ReleaseTmpReg(reg);
	ReleaseTmpReg(Expr->Reg);
	free(Expr);
	free(loop);
	free(done);
	return code;
}

struct InstrSeq * doPrintString(char *string) {
	char *label = GenLabel();
	enterName(stringTable, label);
	setCurrentAttr(stringTable, string);

	struct InstrSeq * seq = GenInstr(NULL, "li", "$v0", Imm(4), NULL);
	AppendSeq(seq, GenInstr(NULL, "la", "$a0", label, NULL));
	AppendSeq(seq, GenInstr(NULL, "syscall", NULL, NULL, NULL));
	
	return seq;
}

struct InstrSeq * doPrintInt(int reg) {
	struct InstrSeq * instr = GenInstr(NULL, "li", "$v0", Imm(1), NULL);
	AppendSeq(instr, GenInstr(NULL, "move", "$a0", TmpRegName(reg), NULL));
	AppendSeq(instr, GenInstr(NULL, "syscall", NULL, NULL, NULL));
	return instr;
}

struct InstrSeq * doPrintBool(int reg) {
	char* els = GenLabel();
	char* end = GenLabel();
	struct InstrSeq * instr = GenInstr(NULL, "beq", TmpRegName(reg), "$zero", els);
	AppendSeq(instr, GenInstr(NULL, "la", "$a0", "_true", NULL));
	AppendSeq(instr, GenInstr(NULL, "j", end, NULL, NULL));
	AppendSeq(instr, GenInstr(els, NULL, NULL, NULL, NULL));
	AppendSeq(instr, GenInstr(NULL, "la", "$a0", "_false", NULL));
	AppendSeq(instr, GenInstr(end, NULL, NULL, NULL, NULL));
	AppendSeq(instr, GenInstr(NULL, "li", "$v0", Imm(4), NULL));
	AppendSeq(instr, GenInstr(NULL, "syscall", NULL, NULL, NULL));
	free(els);
	free(end);
	return instr;
}

int size(enum Type type) {
  	switch(type) {
  	  	case INT:
  	  	  	return sizeof(int);
  	  	case BOOL:
  	  	  	return sizeof(_Bool);
  	  	default:
  	    	writeIndicator(getCurrentColumnNum());
			writeMessage("Unrecognized type");
  	    	return 1;
  	}
}


void Finish(struct InstrSeq * Code)
{ 	
	struct InstrSeq * code;
  	//struct SymEntry *entry;
    int hasMore;
	char *name;
  	struct Attribute * attr;


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
	AppendSeq(code,GenInstr("_true",".asciiz","\"true\"",NULL,NULL));
	AppendSeq(code,GenInstr("_false",".asciiz","\"false\"",NULL,NULL));
	AppendSeq(code,GenInstr("_space",".asciiz","\" \"",NULL,NULL));

	hasMore = startIterator(stringTable);
	while(hasMore) {
		name = getCurrentName(stringTable);
		AppendSeq(code, GenInstr(name, ".asciiz", (char*) getCurrentAttr(stringTable), NULL, NULL));
		hasMore = nextEntry(stringTable);
	}

 	hasMore = startIterator(table);
 	while (hasMore) {
		attr = (struct Attribute*) getCurrentAttr(table);
		if(!attr->array) {
			AppendSeq(code, GenInstr(getCurrentName(table), ".word", "0", NULL, NULL));
		} 
    	hasMore = nextEntry(table);
 	}

	hasMore = startIterator(table);
 	while (hasMore) {
		attr = (struct Attribute*) getCurrentAttr(table);
		if(attr->array) {
			AppendSeq(code, GenInstr(getCurrentName(table), ".space", Imm(attr->size), NULL, NULL));
		} 
    	hasMore = nextEntry(table);
 	}

  	WriteSeq(code);
  
  	return;
}
