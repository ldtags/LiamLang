comp:
	yacc -d ExprEval.y
	lex lex1.l
	gcc -o comp y.tab.c lex.yy.c Semantics.c CodeGen.c main.c IOMngr.c SymTab.c

clean:
	rm -rf comp lex.yy.c y.tab.c y.tab.h asmCode.asm