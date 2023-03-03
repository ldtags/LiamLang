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

/*
	@digits -> char* representation of integer literal

	Creates an ExprRes struct from char* @digits
	Creates Attribute struct to represent its INT typing
	Uses li to load digits into variable in asm 
*/
struct ExprRes * doIntLit(char* digits)  {
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

/*
	@val -> integer representation of boolean literal

	Creates an ExprRes struct from int @val
	Creates Attribute struct to represent its BOOL typing
	Uses li to load int @val of bool into variable in asm
	Integer values of booleans follows C boolean values
*/
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

/*
	@name -> Id of variable being declared
	@type -> enumerated type of variable (0->INT, 1->BOOL)
	@Res1 -> if the declared variable is an array, Res1 will be the ExprRes for the size
			 else, Res1 will be NULL
	@Res2 -> if the declared variable is a 2D array, Res2 will be the ExprRes for the length of each array
			 else, Res2 will be NULL

	Declares the variable by loading it's Id into the symbol table, along with an attribute struct
	The size calculations of variables are based off of C native type sizes
*/
void declare(char* name, enum Type type, struct ExprRes * Res1, struct ExprRes * Res2) {
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
		default:
			writeIndicator(getCurrentColumnNum());
			writeMessage("How did you even do that?");
	}

	setCurrentAttr(table, attr);
}

/* 
	@name -> Id of variable being loaded

	Creates a new ExprRes struct containing a li instruction to load the 
	value of Id into the register of the ExprRes struct 
	The ExprRes struct is returned
*/
struct ExprRes * doLoadVal(char* name)  { 
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

/* 
	@name -> Id of variable being loaded
	@offExpr -> ExprRes struct of offset to desired address

    follows general instruction flow of an asm array access
	returns an ExprRes struct with the desired value stored in the Reg
*/
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

/* 
	@name -> Id of variable being loaded
	@offExpr1 -> ExprRes struct of row of value
	@offExpr2 -> ExprRes struct of column of value

    follows general instruction flow of an asm 2D array access
	returns an ExprRes struct with the desired value stored in the Reg
*/
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

/* 
	@name -> Id of variable being assigned the value
	@Expr -> ExprRes struct of value being assigned

	uses sw instruction to store value in @Expr->Reg in the variable
	InstrSeq of assignment is returned
*/
struct InstrSeq * doAssign(char* name, struct ExprRes * Expr) { 
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

/* 
	@name -> Id of variable being assigned the value
	@offExpr -> Offset to index of array being assigned
	@valExpr -> ExprRes struct of value being assigned

	follows general flow of asm array assignment instructions
	InstrSeq struct of assignment is returned
*/
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

/*
	@type -> an enum Type value

	returns the C spec size of the provided type
*/
int size(enum Type type) {
  	switch (type) {
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

/* 
	@name -> Id of variable being assigned the value
	@offExpr1 -> Row offset to index of 2d array being assigned
	@offExpr2 -> Column offset to index of 2d array being assigned
	@valExpr -> ExprRes struct of value being assigned

	follows general flow of asm 2d array assignment instructions
	InstrSeq struct of assignment is returned
*/
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

/*  
	@Res1 -> left value of general arithmatic function
	@Res2 -> right value of general arithmatic function
	@OpCode -> OpCode of desired arithmatic instruction

	Performs the desired arithmatic instruction using @Res1 and @Res2 as the values
	Follows general asm arithmatic instruction flow
	Returns ExprRes containing the instructions of the GAR
*/
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
	// Res1->Attr->type = INT;
	free(Res2);
	return Res1;
}


/* 
	@Res1 -> left value of modulo
	@Res2 -> right value of modulo

	returns ExprRes struct containing instructions for modulo @Res1 % @Res2
*/
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

/* 
	@Res1 -> base value
	@Res2 -> exponential value

	exponentiation is right associative
	returns ExprRes struct containing instructions for exponentiation @Res1 ^ @Res2
*/
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

/*  
	@Res -> ExprRes containing register of value to perform unary minus on

	returns ExprRes struct containing instructions for unary minus of @Res
*/
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

/*
	@name -> Id of variable to increment

	returns InstrSeq struct of instructions to increment the desired variable
*/
struct InstrSeq * doIncr(char* name) {
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

	AppendSeq(Res->Instrs, GenInstr( NULL, "beq", "$zero", TmpRegName(Res->Reg), els ));
	seq = AppendSeq(Res->Instrs, ifseq);
	AppendSeq(seq, GenInstr( NULL, "j", end, NULL, NULL ));
	AppendSeq(seq, GenInstr( els, NULL, NULL, NULL, NULL ));
	AppendSeq(seq, elseq);
	AppendSeq(seq, GenInstr( end, NULL, NULL, NULL, NULL ));

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
	
	seq = AppendSeq(GenInstr( loop, NULL, NULL, NULL, NULL ), Res->Instrs);
	AppendSeq(seq, GenInstr( NULL, "beq", "$zero", TmpRegName(Res->Reg), done ));
	AppendSeq(seq, lpseq);
	AppendSeq(seq, GenInstr( NULL, "b", loop, NULL, NULL ));
	AppendSeq(seq, GenInstr( done, NULL, NULL, NULL, NULL ));

	ReleaseTmpReg(Res->Reg);
	free(Res);
	free(loop);
	free(done);
	return seq;
}

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

	while (list != NULL) {
		expr = list->Expr;
		instr = AppendSeq(instr, list->Expr->Instrs);

		switch (expr->Attr->type) {
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

		if (list->Next) {
			AppendSeq(instr, GenInstr( NULL, "la", "$a0", "_space", NULL ));
			AppendSeq(instr, GenInstr( NULL, "li", "$v0", Imm(4), NULL ));
			AppendSeq(instr, GenInstr( NULL, "syscall", NULL, NULL, NULL ));
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
	AppendSeq(instr, GenInstr( NULL, "la", "$a0", "_space", NULL ));
	AppendSeq(instr, GenInstr( NULL, "li", "$v0", Imm(4), NULL ));
	AppendSeq(instr, GenInstr( NULL, "syscall", NULL, NULL, NULL ));
  	return instr;
}

struct IdList * createIdListItem(char * id, struct ExprRes * offExpr) {
	if (!findName(table, id)) {
		writeIndicator(getCurrentColumnNum());
		writeMessage("must declare variables");
	}

	struct IdList * listItem = (struct IdList*) malloc(sizeof(struct IdList));
	listItem->Entry = table->current;
	if (offExpr) {
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

	while (list != NULL) {
		if (list->OffExpr) { code = AppendSeq(code, list->OffExpr->Instrs); }
		name = list->Entry->name;
		code = AppendSeq(code, GenInstr( NULL, "la", TmpRegName(reg), name, NULL ));
		AppendSeq(code, GenInstr( NULL, "li", "$v0", Imm(5), NULL ));
		AppendSeq(code, GenInstr( NULL, "syscall", NULL, NULL, NULL ));

		if (list->OffExpr) {
			AppendSeq(code, GenInstr( NULL, "sll", TmpRegName(list->OffExpr->Reg), TmpRegName(list->OffExpr->Reg), Imm(2) ));
			AppendSeq(code, GenInstr( NULL, "add", TmpRegName(reg), TmpRegName(reg), TmpRegName(list->OffExpr->Reg) ));
			ReleaseTmpReg(list->OffExpr->Reg);
		}
		
		AppendSeq(code, GenInstr( NULL, "sw", "$v0", RegOff(0, TmpRegName(reg)), NULL ));

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

	AppendSeq(code, GenInstr( NULL, "li", TmpRegName(reg), Imm(0), NULL ));
	AppendSeq(code, GenInstr( loop, NULL, NULL, NULL, NULL ));
	AppendSeq(code, GenInstr( NULL, "beq", TmpRegName(reg), TmpRegName(Expr->Reg), done ));
	AppendSeq(code, GenInstr( NULL, "li", "$v0", "4", NULL ));
    AppendSeq(code, GenInstr( NULL, "la", "$a0", "_nl", NULL ));
    AppendSeq(code, GenInstr( NULL, "syscall", NULL, NULL, NULL ));
	AppendSeq(code, GenInstr( NULL, "addi", TmpRegName(reg), TmpRegName(reg), Imm(1) ));
	AppendSeq(code, GenInstr( NULL, "b", loop, NULL, NULL ));
	AppendSeq(code, GenInstr( done, NULL, NULL, NULL, NULL ));

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

	AppendSeq(code, GenInstr( NULL, "li", TmpRegName(reg), Imm(0), NULL ));
	AppendSeq(code, GenInstr( loop, NULL, NULL, NULL, NULL ));
	AppendSeq(code, GenInstr( NULL, "beq", TmpRegName(reg), TmpRegName(Expr->Reg), done ));
	AppendSeq(code, GenInstr( NULL, "la", "$a0", "_space", NULL ));
	AppendSeq(code, GenInstr( NULL, "li", "$v0", Imm(4), NULL ));
	AppendSeq(code, GenInstr( NULL, "syscall", NULL, NULL, NULL ));
	AppendSeq(code, GenInstr( NULL, "addi", TmpRegName(reg), TmpRegName(reg), Imm(1) ));
	AppendSeq(code, GenInstr( NULL, "b", loop, NULL, NULL ));
	AppendSeq(code, GenInstr( done, NULL, NULL, NULL, NULL ));

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

	struct InstrSeq * seq = GenInstr( NULL, "li", "$v0", Imm(4), NULL );
	AppendSeq(seq, GenInstr( NULL, "la", "$a0", label, NULL ));
	AppendSeq(seq, GenInstr( NULL, "syscall", NULL, NULL, NULL ));
	
	return seq;
}

struct InstrSeq * doPrintInt(int reg) {
	struct InstrSeq * instr = GenInstr( NULL, "li", "$v0", Imm(1), NULL );
	AppendSeq(instr, GenInstr( NULL, "move", "$a0", TmpRegName(reg), NULL ));
	AppendSeq(instr, GenInstr( NULL, "syscall", NULL, NULL, NULL ));
	return instr;
}

struct InstrSeq * doPrintBool(int reg) {
	char * els = GenLabel();
	char * end = GenLabel();
	struct InstrSeq * instr = GenInstr(NULL, "beq", TmpRegName(reg), "$zero", els);
	AppendSeq(instr, GenInstr( NULL, "la", "$a0", "_true", NULL ));
	AppendSeq(instr, GenInstr( NULL, "j", end, NULL, NULL ));
	AppendSeq(instr, GenInstr( els, NULL, NULL, NULL, NULL ));
	AppendSeq(instr, GenInstr( NULL, "la", "$a0", "_false", NULL ));
	AppendSeq(instr, GenInstr( end, NULL, NULL, NULL, NULL ));
	AppendSeq(instr, GenInstr( NULL, "li", "$v0", Imm(4), NULL ));
	AppendSeq(instr, GenInstr( NULL, "syscall", NULL, NULL, NULL ));
	free(els);
	free(end);
	return instr;
}

/*
	@progInstrs -> assembly instructions interpreted from the source file

	adds the .text and .data fields to a new InstrSeq struct
	returns the new InstrSeq struct
*/
struct InstrSeq * prepInstructions(struct InstrSeq * progInstrs) {
	struct InstrSeq * code = GenInstr( NULL, ".text", NULL, NULL, NULL );
	AppendSeq(code, GenInstr( NULL, ".globl", "main", NULL, NULL ));
  	AppendSeq(code, GenInstr( "main", NULL, NULL, NULL, NULL ));
  	AppendSeq(code, progInstrs);
  	AppendSeq(code, GenInstr( NULL, "li", "$v0", "10", NULL )); 
  	AppendSeq(code, GenInstr( NULL, "syscall", NULL, NULL, NULL ));
  	AppendSeq(code, GenInstr( NULL, ".data", NULL, NULL, NULL ));
  	AppendSeq(code, GenInstr( NULL, ".align", "4", NULL, NULL ));
  	AppendSeq(code, GenInstr( "_nl", ".asciiz", "\"\\n\"", NULL, NULL ));
	AppendSeq(code, GenInstr( "_true", ".asciiz", "\"true\"", NULL, NULL ));
	AppendSeq(code, GenInstr( "_false", ".asciiz", "\"false\"", NULL, NULL ));
	AppendSeq(code, GenInstr( "_space", ".asciiz", "\" \"", NULL, NULL ));
	return code;
}

/*
	@progInstrs -> assembly instructions interpreted from the source file

	prepares the interpreted assembly instructions to be run
	adds all variables and strings stored in the Symbol Tables
*/
void Finish(struct InstrSeq * progInstrs) { 	
	struct InstrSeq * code;
    int hasMore;
	char *name;
  	struct Attribute * attr;

  	code = prepInstructions(progInstrs); 

	hasMore = startIterator(stringTable);
	while (hasMore) {
		name = getCurrentName(stringTable);
		AppendSeq(code, GenInstr(name, ".asciiz", (char*) getCurrentAttr(stringTable), NULL, NULL));
		hasMore = nextEntry(stringTable);
	}

 	hasMore = startIterator(table);
 	while (hasMore) {
		attr = (struct Attribute*) getCurrentAttr(table);
		if (!attr->array) {
			AppendSeq(code, GenInstr(getCurrentName(table), ".word", "0", NULL, NULL));
		} 
    	hasMore = nextEntry(table);
 	}

	hasMore = startIterator(table);
 	while (hasMore) {
		attr = (struct Attribute*) getCurrentAttr(table);
		if (attr->array) {
			AppendSeq(code, GenInstr(getCurrentName(table), ".space", Imm(attr->size), NULL, NULL));
		} 
    	hasMore = nextEntry(table);
 	}

  	WriteSeq(code);
  	return;
}
