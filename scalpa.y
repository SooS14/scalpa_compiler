%{
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include "scalpa.h"
#include "linked-list.h"

int c = 0; // DEBUG DELETE LATER

struct symbol_table_t symbol_table;

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
/*                       variable declaration                                 */
/* -------------------------------------------------------------------------- */

void init_symbol_table();

void free_symbol_table();

void display_symbol_table();

void copy_symbol(struct symbol_t *origin, struct symbol_t *dest);

/*
 * fusion of this function action of vardelc TODO, copy_symbol may be useless
 */
void add_new_symbol(struct symbol_t var);

%}
%code requires {
    #include "scalpa.h"
    #include "linked-list.h"
}

%union {
    struct cste_value_t cste;
    int ival;
    char *strval;
    struct linked_list *list_u;
    struct typename_t typename_u;
}

%token <cste> CTE
%token <strval> IDENT
%type <cste> expr
%type <ival> opb opu
%type <ival> atomictype integer
%type <typename_u> typename arraytype
%type <list_u> identlist rangelist

%token '(' ')' '[' ']' ',' ';' ':' ASSIGNMENT
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
test1 : /* empty */
    | test1 expr ';' {printf("instruction %i : \n", c);display_cste($2);c++;}

program :
    PROGRAM IDENT vardecllist {printf("TODO prgm name\n");free($2);}

vardecllist : /* empty */
    | varsdecl
    | varsdecl ';' vardecllist

varsdecl :
    VAR identlist ':' typename {
        while (list_len($2) != 0) {
            char *new_ident_name = (char *)list_get_first($2);
            struct symbol_t new_symbol;
            new_symbol.initialiazed = 0;
            new_symbol.ident_length = strlen(new_ident_name)+1;
            new_symbol.ident = new_ident_name;
            new_symbol.atomic_type = $4.atomic_type;
            new_symbol.symbol_type = $4.symbol_type;
            if (new_symbol.symbol_type == ARRAY_TYPE) {
                new_symbol.len_range_list = list_len($4.rangelist)/2;
                new_symbol.rangelist = 
                    malloc(new_symbol.len_range_list * sizeof(int[2]));
                int i = 0;
                struct node *temp_node =  $4.rangelist->first;
                while (temp_node != NULL) {
                    new_symbol.rangelist[i][0] = *(int*)temp_node->data;
                    temp_node = temp_node->next;
                    new_symbol.rangelist[i][1] = *(int*)temp_node->data;
                    temp_node = temp_node->next;
                    i++;
                }
            }
            else {
                new_symbol.len_range_list = 0;
                new_symbol.rangelist = NULL;
            }
            add_new_symbol(new_symbol);
            list_pop($2);
        }
        list_free($2);
        if ($4.symbol_type == ARRAY_TYPE) {
            list_free($4.rangelist);
        }
    }

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
      atomictype        {$$.symbol_type = ATOMIC_TYPE; $$.atomic_type = $1;}
    | arraytype         {$$ = $1;}

atomictype :
      UNIT_TYPE         {$$ = VOID_A;}
    | BOOL_TYPE         {$$ = BOOL_A;}
    | INT_TYPE          {$$ = INT_A;}

arraytype :
    ARRAY '[' rangelist ']' OF atomictype {
        $$.symbol_type = ARRAY_TYPE;
        $$.atomic_type = $6;
        $$.rangelist = $3;
    }

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

void yyerror(const char * msg) {
    fprintf(stderr, "%s\n", msg);
}

noreturn void handle_error(const char * msg, ...) {
    va_list ap;

    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    fprintf(stderr, "\n");
    va_end(ap);

    exit(EXIT_FAILURE);
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
    if (expr1.type != expr1.type) {
        handle_error("No operation allowed betwwen int constant and bool \
         constant : expr1 type %s and expr2 type %s", 
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

void init_symbol_table() {
    symbol_table.table_size = INIT_TABLE_SIZE;
    symbol_table.last_ident_index = 0;
    symbol_table.symbols = malloc(INIT_TABLE_SIZE * sizeof(struct symbol_t));
}

void free_symbol_table() {
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        free(symbol_table.symbols[i].ident);
        if (symbol_table.symbols[i].rangelist != NULL) {
            free(symbol_table.symbols[i].rangelist);
        }
    }
    free(symbol_table.symbols);
}

void display_symbol_table() {
    printf("\nTABLE OF SYMBOLS : (%i symbols in table)\n\n", 
        symbol_table.last_ident_index); 
        // 1 symbol no "s", 0 return and print that the table is empty
    int n = floor(log10(symbol_table.last_ident_index)) + 1;
    (n + 3 < 8) ? (n = 8) : (n += 3); // for column alignment
    for (int j = 0; j < n; j++) {
        printf(" ");
    }
    printf("| atomic_type | identifier | rangelist\r| index\n");
    for (int i = 0; i < symbol_table.last_ident_index; i ++) {
        char atomic_type[5];
        switch (symbol_table.symbols[i].atomic_type) {
            case VOID_A : strncpy(atomic_type, "unit", 5); break;
            case BOOL_A : strncpy(atomic_type, "bool", 5); break;
            case INT_A  : strncpy(atomic_type, "int ", 5); break;
            default        : strncpy(atomic_type, "??? ", 5); break;
        }
        for (int j = 0; j < n; j++) {
            printf(" ");
        }
        printf("| %s        | %s", atomic_type, 
            symbol_table.symbols[i].ident);

        if (symbol_table.symbols[i].symbol_type == ARRAY_TYPE) {
            printf(" ( rangelist[");
            for (int j = 0; j < symbol_table.symbols[i].len_range_list; j++) {
                printf("%i..%i,", symbol_table.symbols[i].rangelist[j][0],
                    symbol_table.symbols[i].rangelist[j][1]);
            }
            printf("] )");
        }
        printf("\r| %i\n", i);
        // display ranglist struct linked_list *rangelist;
    }
}

void copy_symbol(struct symbol_t *origin, struct symbol_t *dest){
    dest->symbol_type = origin->symbol_type;
    dest->atomic_type = origin->atomic_type;
    dest->initialiazed = origin->initialiazed;
    dest->ident_length = origin->ident_length;
    dest->ident = malloc(origin->ident_length);
    strncpy(dest->ident , origin->ident, origin->ident_length);
    dest->len_range_list = origin->len_range_list;
    dest->rangelist = origin->rangelist;
}

void add_new_symbol(struct symbol_t var) {
    // search the name in the table, error if it's already declared
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        if (var.ident_length == symbol_table.symbols[i].ident_length &&
            !strncmp(var.ident,
             symbol_table.symbols[i].ident,
             var.ident_length)) {
                handle_error("identifier [%s] already declared.", 
                    var.ident);
        }
    }
    // realloc if last_ident_index exceed the current table size
    if (symbol_table.table_size <= symbol_table.last_ident_index + 1) {
        symbol_table.table_size += INIT_TABLE_SIZE;
        symbol_table.symbols = realloc(symbol_table.symbols,
            symbol_table.table_size * sizeof(struct symbol_t));
    }
    copy_symbol(&var, &symbol_table.symbols[symbol_table.last_ident_index]);
    symbol_table.last_ident_index ++;
}

int main (void) {
    init_symbol_table();
    int c = yyparse();
    display_symbol_table(); // TODO only if option
    free_symbol_table();
    yylex_destroy();
    return c;
}

/* TODO

!IMPORTANT : varsdecl and add_symbol redondant 
!IMPORTANT : test file for all operators
!IMPORTANT : test file for var delc
!IMPORTANT : replace all strcpy by strncpy
!IMPORTANT : %union rename all type by type_u

put all function associated wit var declaration in a header file avec split code

options todo

rename linked_list by linked_list_t

@brief @param etc each function (doxygen)

scalpa comment todo (* comment *)

test files for operation (check for every operator)

alloc error CHECK macro

cste string regular expression bug for  "//" valid, "/" not valid, add single 
quote example " '"' " is a valid syntaxe

free mermory if EXIT_FAILURE or syntaxe error ???

delete DEBUG comment and printf

Do a real README file
add option

*/