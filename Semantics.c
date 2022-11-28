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
extern SymTab *printstrings;

/* Semantics support routines */

struct ExprRes * doIntLit(char * digits)  { 
   	struct ExprRes * res = (struct ExprRes *) malloc(sizeof(struct ExprRes));
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

char * doStringLit(char * string) {
	return strdup(string);
}

char ** doListLit(char * string) {
	char ** list;
	char *strlist = strdup(string);
	int size = 0;
	for(int i = 0; i < strlen(strlist); i++) {
		if(idChar(strlist[i])) { size++; }
	}

	list = (char**) malloc(size);
	char *token;
	char *cleanToken;
	char delim[] = ",";
	int i = 0;
	int j, k;
	while((token = strtok(strlist, delim)) != NULL) {
		j = 0;
		k = 0;
		while(token[j] != '\0') {
			if(idChar(token[j])) {
				cleanToken[k] = token[j];
				k++;
			}
			j++;
		}
		cleanToken[k] = '\0';
		list[i] = cleanToken;
		i++;
	}

	free(strlist);
	free(token);
	free(cleanToken);
	return list;
}

int idChar(char c) {
	return c > -1 && c < 10 || 
		   c > 64 && c < 91 || 
		   c > 96 && c < 123;
}

struct ExprRes * doRval(char * name)  { 
   	if (!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared variable");
   	}

  	struct ExprRes * res = (struct ExprRes *) malloc(sizeof(struct ExprRes));
  	res->Reg = AvailTmpReg();
  	res->Instrs = GenInstr(NULL,"lw" ,TmpRegName(res->Reg), name, NULL);

  	return res;
}

struct ExprRes * doArrVal(char * name, struct ExprRes * offExpr) {
	if(!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared array");
	}

	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));
	Res->Reg = AvailTmpReg();
	Res->Instrs = offExpr->Instrs;
	AppendSeq(Res->Instrs, GenInstr(NULL, "la", TmpRegName(Res->Reg), name, NULL));
	AppendSeq(Res->Instrs, GenInstr(NULL, "sll", TmpRegName(offExpr->Reg), TmpRegName(offExpr->Reg), Imm(2)));
	AppendSeq(Res->Instrs, GenInstr(NULL, "add", TmpRegName(Res->Reg), TmpRegName(Res->Reg), TmpRegName(offExpr->Reg)));
	AppendSeq(Res->Instrs, GenInstr(NULL, "lw", TmpRegName(Res->Reg), RegOff(0, TmpRegName(Res->Reg)), NULL));

	ReleaseTmpReg(offExpr->Reg);
	free(offExpr);
	return Res;
}

struct ExprRes * do2DVal(char * name, struct ExprRes * offExpr1, struct ExprRes * offExpr2) {
	if(!findName(table, name)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("Undeclared array");
	}

	int off = AvailTmpReg();
	struct Attribute * attr = getCurrentAttr(table);
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));
	Res->Reg = AvailTmpReg();
	Res->Instrs = offExpr1->Instrs;
	AppendSeq(Res->Instrs, offExpr2->Instrs);
	AppendSeq(Res->Instrs, GenInstr(NULL, "la", TmpRegName(Res->Reg), name, NULL));
	AppendSeq(Res->Instrs, GenInstr(NULL, "li", TmpRegName(off), Imm(attr->size / size(attr->type)), NULL));
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
	AppendSeq(seq, GenInstr(NULL, "li", TmpRegName(off), Imm(attr->size / size(attr->type)), NULL));
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

	if(Res1 != NULL) {
		attr->array = 1;
		size = (int) strtol(Res1->Instrs->Oprnd2, buf, 10);
		ReleaseTmpReg(Res1->Reg);
		free(Res1);
	}

	if(Res2 != NULL) {
		attr->array = 2;
		size *= (int) strtol(Res2->Instrs->Oprnd2, buf, 10);
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
		default:
			writeIndicator(getCurrentColumnNum());
			writeMessage("How did you even do that?");
	}

	setCurrentAttr(table, attr);
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
	AppendSeq(Res1->Instrs, GenInstr(NULL, "div", 
											TmpRegName(reg), 
											TmpRegName(Res1->Reg), 
											TmpRegName(Res2->Reg)));
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

struct InstrSeq * doIOPrint(char ** list) {
	struct InstSeq * code;
	struct Attribute * attr;
	char *name;
	char *els;
	char *end;
	int reg;

	for(int i = 0; i < strlen(list); i++) {
		if(!findName(table, list[i])) {
			writeIndicator(getCurrentColumnNum());
			writeMessage("Undeclared variable cannot be printed");
		} else {
			name = getCurrentName(table);
			attr = getCurrentAttr(table);
			reg = AvailTmpReg();
			switch(attr->type) {
				case INT:
					code = AppendSeq(code, GenInstr(NULL, "li", "$v0", Imm(1), NULL));
					AppendSeq(code, GenInstr(NULL, "lw", TmpRegName(reg), name, NULL));
					AppendSeq(code, GenInstr(NULL, "move", "$a0",TmpRegName(reg), NULL));
					AppendSeq(code, GenInstr(NULL, "syscall", NULL, NULL, NULL));
				case BOOL:
					els = GenLabel();
					end = GenLabel();
					code = AppendSeq(code, GenInstr(NULL, "lw", TmpRegName(reg), name, NULL));
					AppendSeq(code, GenInstr(NULL, "beq", TmpRegName(reg), "$zero", els));
					AppendSeq(code, GenInstr(NULL,"la","$a0","_true",NULL));
					AppendSeq(code, GenInstr(NULL, "b", end, NULL, NULL));
					AppendSeq(code, GenInstr(els, NULL, NULL, NULL, NULL));
					AppendSeq(code, GenInstr(NULL,"la","$a0","_false",NULL));
					AppendSeq(code, GenInstr(end, NULL, NULL, NULL, NULL));
					AppendSeq(code, GenInstr(NULL, "li", "$v0", Imm(4), NULL));
					free(els);
					free(end);
				case STRING:
					writeIndicator(getCurrentColumnNum());
					writeMessage("To print strings, use printstring(\"StringLiteral\")");
				default:
					writeIndicator(getCurrentColumnNum());
					writeMessage("Unknown type, what are you trying to print?");
			}
			ReleaseTmpReg(reg);
		}
	}

	return code;
}

struct InstrSeq * doIORead(char ** list) {
	struct InstrSeq * code;
	char *name;

	for(int i = 0; i < strlen(list); i++) {
		if(!findName(table, list[i])) {
			writeIndicator(getCurrentColumnNum());
			writeMessage("Cannot read into undeclared variable");
		} else {
			name = getCurrentName(table);
			code = GenInstr(NULL, "li", "$v0", Imm(5), NULL);
			AppendSeq(code, GenInstr(NULL, "syscall", NULL, NULL, NULL));
			AppendSeq(code, GenInstr(NULL, "sw", "$v0", name, NULL));
			// syscall?
		}
	}

	return code;
}

struct InstrSeq * doPrintLines(struct ExprRes * Expr) {
	struct InstrSeq * code = Expr->Instrs;
	char * loop = GenLabel();
	char * done = GenLabel();
	int reg = AvailTmpReg();

	AppendSeq(code, GenInstr(NULL, "addi", TmpRegName(reg), "$zero", Imm(1)));
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

	AppendSeq(code, GenInstr(NULL, "addi", TmpRegName(reg), "$zero", Imm(1)));
	AppendSeq(code, GenInstr(loop, NULL, NULL, NULL, NULL));
	AppendSeq(code, GenInstr(NULL, "beq", TmpRegName(reg), TmpRegName(Expr->Reg), done));
	AppendSeq(code, GenInstr(NULL, "li", "$v0", "11", NULL));
    AppendSeq(code, GenInstr(NULL, "li", "$a0", Imm(32), NULL));
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

struct InstrSeq * doPrintString(char * string) {
	int i = 0;
	char *name = malloc(strlen(string) + 2);
	name[0] = "_";
	while(i < strlen(string)) {
		name[i+1] = string[i];
		i++;
	}
	name[i+1] = '\0';
	enterName(printstrings, name);

	struct InstrSeq * seq = GenInstr(NULL, "li", "$v0", Imm(4), NULL);
	AppendSeq(seq, GenInstr(NULL, "la", "$a0", name, NULL));
	AppendSeq(seq, GenInstr(NULL, "syscall", NULL, NULL, NULL));

	free(string);
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
	int reg = AvailTmpReg();
	struct ExprRes * Res = (struct ExprRes*) malloc(sizeof(struct ExprRes));

	AppendSeq(Res1->Instrs, Res2->Instrs);
	AppendSeq(Res1->Instrs, GenInstr(NULL, OpCode, TmpRegName(reg), TmpRegName(Res1->Reg), TmpRegName(Res2->Reg)));

    Res->Reg = reg;
	Res->Instrs = Res1->Instrs;
    ReleaseTmpReg(Res->Reg);
	ReleaseTmpReg(Res1->Reg);
  	ReleaseTmpReg(Res2->Reg);
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


void Finish(struct InstrSeq *Code)
{ 	
	struct InstrSeq *code;
  	//struct SymEntry *entry;
    int hasMore, i;
	char *string = "_";
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

	enterName(printstrings, "_true\0");
	enterName(printstrings, "_false\0");

	hasMore = startIterator(printstrings);
	while(hasMore) {
		name = getCurrentName(printstrings);
		string = strdup(name);
		string[0] = '\"';
		string[strlen(string)] = '\"';
		AppendSeq(code, GenInstr(name, ".asciiz", string, NULL, NULL));
		hasMore = nextEntry(printstrings);
	}

 	hasMore = startIterator(table);
 	while (hasMore) {
		attr = (struct Attribute*) getCurrentAttr(table);
		if(attr->array) {
			AppendSeq(code, GenInstr(getCurrentName(table), ".space", Imm(attr->size), NULL, NULL));
		} else {
			AppendSeq(code, GenInstr(getCurrentName(table), ".word", "0", NULL, NULL));
		}
    	hasMore = nextEntry(table);
 	}
  
  	WriteSeq(code);
  
  	return;
}
