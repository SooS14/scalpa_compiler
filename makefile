CFLAGS=-lm -lfl
TARGET=scalpa
YACCFLAGS=-d -g -v -Wno-yacc

all: y.tab.o lex.yy.o linked_list.o table_of_symbol.o args-parser.o quad.o expr.o
	gcc y.tab.o lex.yy.o linked_list.o table_of_symbol.o args-parser.o quad.o expr.o $(CFLAGS) -o $(TARGET)

expr.o: expr.c expr.h
	gcc -c expr.c -o expr.o -lm

quad.o: quad.c quad.h
	gcc -c quad.c -o quad.o

table_of_symbol.o: table_of_symbol.c table_of_symbol.h
	gcc -c table_of_symbol.c -o table_of_symbol.o

linked_list.o: linked_list.c linked_list.h
	gcc -c linked_list.c -o linked_list.o

args-parser.o : args-parser.c args-parser.h
	gcc -c args-parser.c -o args-parser.o

y.tab.o: $(TARGET).y
	yacc $(YACCFLAGS) $(TARGET).y
	gcc -c y.tab.c

lex.yy.o: $(TARGET).l y.tab.h
	lex $(TARGET).l
	gcc -c lex.yy.c

clean:
	$(RM) -f *.o y.tab.c y.tab.h lex.yy.c a.out $(TARGET)
	$(RM) y.dot
	$(RM) y.output

cleanall:
	$(RM) -f *.o y.tab.c y.tab.h lex.yy.c a.out $(TARGET)
	$(RM) y.dot
	$(RM) y.output
	$(RM) *.s
