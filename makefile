prefixe=scalpa

all: y.tab.o lex.yy.o linked_list.o var_declaration.o
	gcc y.tab.o lex.yy.o linked_list.o var_declaration.o -lfl -o $(prefixe) -lm

var_declaration.o: var_declaration.c var_declaration.h
	gcc -c var_declaration.c -o var_declaration.o

linked_list.o: linked_list.c linked_list.h
	gcc -c linked_list.c -o linked_list.o
	
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
