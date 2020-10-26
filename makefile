prefixe=scalpa

all: y.tab.o lex.yy.o linked-list.o
	gcc y.tab.o lex.yy.o linked-list.o -lfl -o $(prefixe) -lm

linked-list.o: linked-list.c linked-list.h
	gcc -c linked-list.c -o linked-list.o
	
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
