%{
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include "scalpa.h"
#include "linked_list.h"
#include "table_of_symbol.h"
#include "args-parser.h"
#include "quad.h"

struct symbol_table_t symbol_table;
struct quad_table_t quad_table;

int fd_out;

int yylex(void);

int yylex_destroy(void);

void yyerror(const char * msg);

/* -------------------------------------------------------------------------- */
/*                    operation on  constant expression                       */
/* -------------------------------------------------------------------------- */

/*
 * Display an element of type cste_value_t
 * print its type and value
 */
void display_cste(struct cste_value_t cste);

/*
 * write the string associated the operator code : op in str_op
 */
void get_operator_str (int op, char (*str_op)[5]);

/*
 * Compute and return the result of : expr1 opb expr2 
 * exit if expr1 or expr2 have a type that doesn't support opb
 * example : string + string, true < false, int xor int
 * exit if expr1 and expr2 have different type
 */
struct cste_value_t compute_opb(struct cste_value_t expr1, 
                                struct cste_value_t expr2,
                                int opb);

/*
 * Compute and return the result of : opu expr
 * exit if expr have a type that doesn't support opu
 * example : opu string, - bool, not int
 */
struct cste_value_t compute_opu(struct cste_value_t expr, int opu);

/* -------------------------------------------------------------------------- */
/*                             write spim file                                */
/* -------------------------------------------------------------------------- */

void write_main(char * program_name) {
    int size_buff = 40 + strlen(program_name);
    char * buff = malloc(40 + strlen(program_name));
    MCHECK(buff);
    check_snprintf(snprintf(buff, size_buff, "# program : %s\n\t.text\n\t.globl\
 main\nmain:\n", program_name), size_buff);
    CHECK(write(fd_out, buff, strlen(buff)));
    free(buff);
}

void write_data() {
    char * buff = malloc(9);
    MCHECK(buff);
    check_snprintf(snprintf(buff, 9, "\n\t.data\n"), 9);
    CHECK(write(fd_out, buff, strlen(buff)));
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        if (symbol_table.symbols[i].var_func_par == VAR_T) {
            struct variable_t cur_sym = symbol_table.symbols[i].type.var;
            int n = (symbol_table.symbols[i].scope == 0) ? 
                1 : floor(log10(symbol_table.symbols[i].scope)) + 1;
            if (cur_sym.typename->symbol_type == ATOMIC_TYPE) {
                int size_buff = 13 + n + symbol_table.symbols[i].ident_length;
                buff = realloc(buff, size_buff);
                MCHECK(buff);
                check_snprintf(snprintf(buff, size_buff, "\t%s_%i:\t.word 0\n",
                    symbol_table.symbols[i].ident,
                    symbol_table.symbols[i].scope), size_buff);
                CHECK(write(fd_out, buff, strlen(buff)));
            }
            else {
                int nb_element = 1;
                for (int j = 0; j < cur_sym.typename->len_range_list; j++) {
                    nb_element *= (cur_sym.typename->range_array[j][1] 
                        - cur_sym.typename->range_array[j][0] + 1); 
                }
                int size_buff = 12 + n + symbol_table.symbols[i].ident_length 
                    + 3 * nb_element;
                buff = realloc(buff, size_buff);
                MCHECK(buff);
                check_snprintf(snprintf(buff, size_buff, "\t%s_%i:\t.word ",
                    symbol_table.symbols[i].ident,
                    symbol_table.symbols[i].scope), size_buff);
                for (int j = 0; j < nb_element; j++) {
                    char *buff_cpy = malloc(strlen(buff)+1);
                    MCHECK(buff_cpy);
                    buff_cpy = strncpy(buff_cpy, buff, strlen(buff)+1);
                    check_snprintf(snprintf(buff, size_buff, "%s0, ",
                        buff_cpy), size_buff);
                    free(buff_cpy);
                }
                buff[strlen(buff) - 2] = '\n';
                buff[strlen(buff) - 1] = '\0';
                CHECK(write(fd_out, buff, strlen(buff)));
            }
        }
        // TODO param ?? 
    }
    free(buff);
}

%}
%code requires {
    #include "linked_list.h"
    #include "scalpa.h"
    #include "table_of_symbol.h"
    #include "quad.h"
}

%union {
    struct cste_value_t cst_u;
    int int_u;
    char *str_u;
    struct linked_list *list_u;
    struct typename_t *typename_u;
    struct param_t par_u;
    struct vardecl_t *vardecl_u;
    struct fundecl_t *fundecl_u;
    struct lvalue_t lvalue_u;
}

%token <cst_u> CTE
%token <str_u> IDENT
%type <cst_u> expr
%type <int_u> opb opu
%type <int_u> atomictype integer
%type <typename_u> typename arraytype
%type <list_u> identlist rangelist parlist vardecllist fundecllist exprlist
%type <vardecl_u> varsdecl
%type <fundecl_u> fundecl
%type <lvalue_u> lvalue
%type <par_u> par

%token '(' ')' '[' ']' ',' ';' ':' ASSIGNMENT FUNCTION REF READ WRITE BEGIN_
%token END IF THEN ELSE RETURN WHILE DO
%token VAR UNIT_TYPE BOOL_TYPE INT_TYPE
%token OF ARRAY RANGELIST_SEPARATOR PROGRAM

%left OPB
%left OPU
%left OP_MINUS OPU_NOT
%left OPB_POW
%left OPB_STAR OPB_DIVIDE OPB_AND
%left OPB_PLUS OPB_OR OPB_XOR
%left OPB_L_EQ OPB_L OPB_G_EQ OPB_G OPB_EQ OPB_DIFF

%start program

%%

program :
    PROGRAM IDENT vardecllist fundecllist {
        write_main($2);
        add_vardecllist_table($3);
        add_fundecllist_table($4);
        free($2);
        write_data();
    }
/* TODO instr add later */

/* -------------------------------------------------------------------------- */
/*                         variable declaration                               */
/* -------------------------------------------------------------------------- */

vardecllist : 
      /* empty */ {$$ = list_init();}
    | varsdecl {
        $$ = list_init(); 
        list_push($$, $1, sizeof(struct vardecl_t));
        free($1);
    }
    | varsdecl ';' vardecllist {
        $$ = $3;
        list_push($$, $1, sizeof(struct vardecl_t));
        free($1);
    }

varsdecl :
    VAR identlist ':' typename {$$ = create_vardecl($2, $4);}

identlist :
      IDENT {
        $$ = list_init();
        list_push($$, $1, strlen($1)+1);
        free($1);
    }
    | IDENT ',' identlist {
        list_push($3, $1, strlen($1)+1);
        $$ = $3;
        free($1);
    }

typename :
      atomictype {$$ = create_typename_atomic($1);}
    | arraytype  {$$ = $1;}

atomictype :
      UNIT_TYPE  {$$ = VOID_A;}
    | BOOL_TYPE  {$$ = BOOL_A;}
    | INT_TYPE   {$$ = INT_A;}

arraytype :
    ARRAY '[' rangelist ']' OF atomictype {$$ = create_typename_array($3, $6);}

rangelist :
      integer RANGELIST_SEPARATOR integer {
        if ($1 > $3) {
            handle_error("[%i..%i], invalid rangelist (%i > %i)",
                $1, $3, $1, $3);
        }
        $$ = list_init();
        int x1 = $1;
        int x2 = $3;
        list_push($$, &x2, sizeof(int));
        list_push($$, &x1, sizeof(int));
    }
    | integer RANGELIST_SEPARATOR integer ',' rangelist {
        if ($1 > $3) {
            handle_error("[%i..%i], invalid rangelist (%i > %i)",
                $1, $3, $1, $3);
        }
        $$ = $5;
        int x1 = $1;
        int x2 = $3;
        list_push($$, &x2, sizeof(int));
        list_push($$, &x1, sizeof(int));
    }

integer :
    expr {
        if ($1.type == INT) {
            $$ = $1.val.iconst;
        }
        else {
            handle_error("elements of a rangelist for array declaration\
 must be integers\n");
        }
    }

/* -------------------------------------------------------------------------- */
/*                         function declaration                               */
/* -------------------------------------------------------------------------- */

fundecllist : 
      /* empty */ {$$ = list_init();}
    | fundecl ';' fundecllist {
        $$ = $3;
        list_push($$, $1, sizeof(struct fundecl_t));
        free($1);
    }

fundecl : FUNCTION IDENT '(' parlist ')' ':' atomictype vardecllist {
    $$ = create_fundecl($2, $7, $4, $8);
}
// TODO instr add later

parlist : 
      /* empty */ {$$ = NULL;}
    | par {
        $$ = list_init();
        list_push($$, &$1, sizeof(struct param_t));
    }
    | par ',' parlist {
        list_push($3, &$1, sizeof(struct param_t));
        $$ = $3;
    }

par :
      IDENT ':' typename {
        if ($3->atomic_type == VOID_A) {
            handle_error("parameter can't be of type unit");
        }
        $$.ident = malloc(strlen($1)+1);
        MCHECK($$.ident);
        strncpy($$.ident, $1, strlen($1)+1);
        $$.ref = 0;
        $$.typename = $3;
        free($1);
    }
    | REF IDENT ':' typename {
        if ($4->atomic_type == VOID_A) {
            handle_error("parameter can't be of type ref unit");
        }
        $$.ident = malloc(strlen($2)+1);
        MCHECK($$.ident);
        strncpy($$.ident, $2, strlen($2)+1);
        $$.ref = 1;
        $$.typename = $4;
        free($2);
    }

/* -------------------------------------------------------------------------- */
/*                            instruction                                     */
/* -------------------------------------------------------------------------- */

instr :
      IF expr THEN instr 
    | IF expr THEN instr ELSE instr
    | WHILE expr DO instr 
    | lvalue ASSIGNMENT expr {lvalue}
    | RETURN expr 
    | RETURN
    | IDENT '(' exprlist ')'
    | IDENT '(' ')' 
    | BEGIN_ sequence END 
    | BEGIN_ END
    | READ lvalue 
    | WRITE expr

sequence :
      instr ';' sequence 
    | instr ';' 
    | instr

/* -------------------------------------------------------------------------- */
/*                            expression                                      */
/* -------------------------------------------------------------------------- */

lvalue :
      IDENT                   {}
    | IDENT '[' exprlist ']'  {}

exprlist :
      expr                    {printf("TODO expr\n");}
    | expr ',' exprlist       {printf("TODO expr , exprlist\n");}

expr :
      CTE                     {$$ = $1;}
    | '(' expr ')'            {$$ = $2;}
    | expr opb expr %prec OPB {$$ = compute_opb($1, $3, $2);}
    | opu expr %prec OPU      {$$ = compute_opu($2, $1);}
    | IDENT '(' exprlist ')'  {printf("TODO IDENT ( exprlist )\n");}
    | IDENT '(' ')'           {printf("TODO IDENT ( )\n");}
    | IDENT '[' exprlist ']'  {printf("TODO IDENT [ exprlist ]\n");}
    | IDENT                   {printf("TODO IDENT\n");}

opb : 
      OPB_PLUS   {$$ = OPB_PLUS;}
    | OP_MINUS   {$$ = OP_MINUS;}
    | OPB_STAR   {$$ = OPB_STAR;}
    | OPB_DIVIDE {$$ = OPB_DIVIDE;}
    | OPB_POW    {$$ = OPB_POW;}
    | OPB_L      {$$ = OPB_L;}
    | OPB_L_EQ   {$$ = OPB_L_EQ;}
    | OPB_G      {$$ = OPB_G;}
    | OPB_G_EQ   {$$ = OPB_G_EQ;}
    | OPB_EQ     {$$ = OPB_EQ;}
    | OPB_DIFF   {$$ = OPB_DIFF;}
    | OPB_AND    {$$ = OPB_AND;}
    | OPB_OR     {$$ = OPB_OR;}
    | OPB_XOR    {$$ = OPB_XOR;}

opu : 
      OP_MINUS   {$$ = OP_MINUS;}
    | OPU_NOT    {$$ = OPU_NOT;}

%%

/* -------------------------------------------------------------------------- */

void yyerror(const char * msg) {
    fprintf(stderr, "%s\n", msg);
}

noreturn void handle_error(const char * msg, ...) {
    va_list ap;

    va_start(ap, msg);
    vfprintf(stderr, "error : ", ap);
    vfprintf(stderr, msg, ap);
    fprintf(stderr, "\n");
    va_end(ap);

    exit(EXIT_FAILURE);
}

noreturn void handle_perror(const char * msg, ...) {
    va_list ap;

    va_start(ap, msg);
    vfprintf(stderr, "perror : ", ap);
    vfprintf(stderr, msg, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    perror("");

    exit(EXIT_FAILURE);
}

void check_snprintf(int result, int wsize) {
    if ((result) < 0 || (result) >= wsize) {
        handle_perror("snprintf failed\n");
    }
}

/* -------------------------------------------------------------------------- */

void display_cste(struct cste_value_t cste) {
    switch(cste.type) {
        case INT:    printf("int : %i\n", cste.val.iconst);      break;
        case STRING: printf("string : [%s]\n", cste.val.sconst); break;
        case BOOL:   printf("bool : %i\n", cste.val.bconst);     break;
        default: break;
    }
}

void get_operator_str (int op, char (*str_op)[5]) {
    switch(op) {
        case OPB_PLUS   : strncpy(*str_op, "+", 2);    break;
        case OP_MINUS   : strncpy(*str_op, "-", 2);    break;
        case OPB_STAR   : strncpy(*str_op, "*", 2);    break;
        case OPB_DIVIDE : strncpy(*str_op, "/", 2);    break;
        case OPB_POW    : strncpy(*str_op, "^", 2);    break;
        case OPB_L      : strncpy(*str_op, "<", 2);    break;
        case OPB_L_EQ   : strncpy(*str_op, "<=", 3);   break;
        case OPB_G      : strncpy(*str_op, ">", 2);    break;
        case OPB_G_EQ   : strncpy(*str_op, ">=", 3);   break;
        case OPB_EQ     : strncpy(*str_op, "=", 3);    break;
        case OPB_DIFF   : strncpy(*str_op, "<>", 3);   break;
        case OPB_AND    : strncpy(*str_op, "and", 4);  break;
        case OPB_OR     : strncpy(*str_op, "or", 3);   break;
        case OPB_XOR    : strncpy(*str_op, "xor", 4);  break;
        case OPU_NOT    : strncpy(*str_op, "not", 4);  break;
        default         : strncpy(*str_op, "null", 5); break; 
    }
}

struct cste_value_t compute_opb(struct cste_value_t expr1, 
                                struct cste_value_t expr2,
                                int opb) {
    if (expr1.type == STRING || expr2.type == STRING) {
        handle_error("No operation allowed for constant string");
    }
    if (expr1.type != expr2.type) {
        handle_error("No operation allowed between int constant and bool\
 constant : expr1 is type [%s] and expr2 is type [%s]", 
        expr1.type == INT ? "int" : "bool", expr2.type == INT ? "int" : "bool");
    }
    struct cste_value_t result;
    char str_op[5];
    if (expr1.type == INT) {
        result.type = INT;
        switch(opb) {
            case OPB_PLUS   :
                result.val.iconst = expr1.val.iconst + expr2.val.iconst;
                break;
            case OP_MINUS   :
                result.val.iconst = expr1.val.iconst - expr2.val.iconst;
                break;
            case OPB_STAR   :
                result.val.iconst = expr1.val.iconst * expr2.val.iconst;
                break;
            case OPB_DIVIDE :
                result.val.iconst = expr1.val.iconst / expr2.val.iconst;
                break;
            case OPB_POW    :
                result.val.iconst = pow(expr1.val.iconst, expr2.val.iconst);
                break;
            case OPB_L      :
                result.type = BOOL;
                result.val.bconst = (expr1.val.iconst < expr2.val.iconst);
                break;
            case OPB_L_EQ   :
                result.type = BOOL;
                result.val.bconst = (expr1.val.iconst <= expr2.val.iconst);
                break;
            case OPB_G      :
                result.type = BOOL;
                result.val.bconst = (expr1.val.iconst > expr2.val.iconst);
                break;
            case OPB_G_EQ   :
                result.type = BOOL;
                result.val.bconst = (expr1.val.iconst >= expr2.val.iconst);
                break;
            case OPB_EQ     :
                result.type = BOOL;
                result.val.bconst = (expr1.val.iconst == expr2.val.iconst);
                break;
            case OPB_DIFF   :
                result.type = BOOL;
                result.val.bconst = (expr1.val.iconst != expr2.val.iconst);
                break;
            default : 
                get_operator_str(opb, &str_op);
                handle_error("binary operator [%s] forbidden between int", 
                    str_op);
                break;
        }
    }
    else {
        result.type = BOOL;
        switch(opb) {
            case OPB_EQ     :
                result.val.bconst = (expr1.val.bconst == expr2.val.bconst);
                break;
            case OPB_DIFF   :
                result.val.bconst = (expr1.val.bconst != expr2.val.bconst);
                break;
            case OPB_AND    :
                result.val.bconst = (expr1.val.bconst && expr2.val.bconst);
                break;
            case OPB_OR     :
                result.val.bconst = (expr1.val.bconst || expr2.val.bconst);
                break;
            case OPB_XOR    :
                result.val.bconst = (expr1.val.bconst ^ expr2.val.bconst);
                break;
            default :
                get_operator_str(opb, &str_op);
                handle_error("binary operator [%s] forbidden between bool", 
                    str_op);
                break;
        }
    }
    return result;
}

struct cste_value_t compute_opu(struct cste_value_t expr, int opu) {
    if (expr.type == STRING) {
        handle_error("No operation allowed for constant string");
    }
    struct cste_value_t result;
    if (expr.type == INT) {
        result.type = INT;
        if (opu == OP_MINUS) {
            result.val.iconst = - expr.val.iconst;
        }
        else {
            handle_error("unary operator [not] forbidden for int");
        }
    }
    else {
        result.type = BOOL;
        if (opu == OPU_NOT) {
            result.val.bconst = ! expr.val.bconst;
        }
        else {
            handle_error("unary operator [-] forbidden for bool");
        }
    }
    return result;
}

/* -------------------------------------------------------------------------- */

int main (int argc, char * argv[]) {
    args_t args;
    parse_args(argc, argv, &args);

    if (args.flags & VERSION) {
        display_version();
    }
    if (!(args.flags & OUT_FILE)) {
        printf("\nno output file specified, default output file used : "
            "<output.s>\n\n");
        CHECK(args.fd = open ("output.s", O_WRONLY | O_CREAT | O_TRUNC, 0666));
    }
    fd_out = args.fd;

    init_symbol_table();
    init_quad_table();

    int c = yyparse();
    
    if (args.flags & SYM_TABLE) {
        display_symbol_table();
    }

    CHECK(close(args.fd));
    free_symbol_table();
    // TODO free quad table
    yylex_destroy();
    return c;
}

/*
J'ai des cc et d'autres projets a faire mais il faut dans un 1er temps refaire 
les expressions dans la grammaire, en fait faudra reprendre ce qu'il a dans les 
td et refaire ça proprement. Ensuite on pourra acceder au resultat de ces 
expressions pour faire des listes exprlist ou dans des if while etc.
Bon bref faut refaire proprement les expressions et generer les quadruplets en 
meme temps. Si vous avez des questions appelez moi (antoine), la j'ai pas trop 
le temps tout de suite
*/

/* TODO

IMPORTANT

NOT PRIORITY
 - table of symbol add pointer (hash table ?)
 - improve display tos function \r in display improve, print scope ?
 - expr const or not if identifier for integer in arraytype
 - const for array size in grammar
 - new test for cste and expr

Q : warning: 1 shift/reduce conflict [-Wconflicts-sr] fix ?
Q : rename linked_list by linked_list_t ?
Q : free mermory if EXIT_FAILURE or syntaxe error ?
Q : (2^3)*9 != 2^3*9 priority ?
Q : src include dir ?
Q : cste string regular expression bug for  "//" valid, "/" not valid,
    add single quote example " '"' " is a valid syntaxe
Q : getopt.h for args ?
Q : test if output file is part of the source code ?

verif :
 - yacc warning
 - valgrind
 - malloc check
 - system call check
 - remove useless command and test
 - -Wall -Werror -Wextra

Do a real README file

*/