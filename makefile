prefixe=scalpa

all: y.tab.o lex.yy.o
	gcc y.tab.o lex.yy.o -lfl -o $(prefixe) -lm

y.tab.o: $(prefixe).y
	yacc -d -g -v $(prefixe).y -Wno-yacc
	gcc -c y.tab.c

lex.yy.o: $(prefixe).l y.tab.h
	lex $(prefixe).l
	gcc -c lex.yy.c

clean:
	rm -f *.o y.tab.c y.tab.h lex.yy.c a.out $(prefixe)
	rm y.dot
	rm y.output
