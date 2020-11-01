%{
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include <stdint.h>
#include "scalpa.h"
#include "linked_list.h"
#include "var_declaration.h"

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

%}
%code requires {
    #include "scalpa.h"
    #include "linked_list.h"
    #include "var_declaration.h"
}

%union {
    struct cste_value_t cst_u;
    int int_u;
    char *str_u;
    struct linked_list *list_u;
    struct typename_t typename_u;
    struct param_t par_u;
}

%token <cst_u> CTE
%token <str_u> IDENT
%type <cst_u> expr
%type <int_u> opb opu
%type <int_u> atomictype integer
%type <typename_u> typename arraytype
%type <list_u> identlist rangelist parlist
%type <par_u> par

%token '(' ')' '[' ']' ',' ';' ':' ASSIGNMENT FUNCTION REF
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
    | test1 expr ';' {printf("instruction : \n");display_cste($2);}

program :
    PROGRAM IDENT vardecllist fundecllist {printf("TODO prgm name\n");free($2);}
/* TODO instr add later */

/* -------------------------------------------------------------------------- */
/*                         variable declaration                               */
/* -------------------------------------------------------------------------- */

vardecllist : /* empty */
    | varsdecl
    | varsdecl ';' vardecllist

varsdecl :
    VAR identlist ':' typename {add_var_symbol_list($2, $4);}

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
      atomictype {$$.symbol_type = ATOMIC_TYPE; $$.atomic_type = $1;}
    | arraytype  {$$ = $1;}

atomictype :
      UNIT_TYPE  {$$ = VOID_A;}
    | BOOL_TYPE  {$$ = BOOL_A;}
    | INT_TYPE   {$$ = INT_A;}

arraytype :
    ARRAY '[' rangelist ']' OF atomictype {
        $$.symbol_type = ARRAY_TYPE;
        $$.atomic_type = $6;
        $$.range_list = $3;
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

/* -------------------------------------------------------------------------- */
/*                         function declaration                               */
/* -------------------------------------------------------------------------- */

fundecllist : /* empty */ 
    | fundecl ';' fundecllist

fundecl : FUNCTION IDENT '(' parlist ')' ':' atomictype vardecllist {
    //add function
    if (is_symbol_in_table($2, 0)) {
        handle_error("identifier [%s] already declared.", $2);
    }
    realloc_table();

    struct symbol_t *new_symbol = 
            &symbol_table.symbols[symbol_table.last_ident_index];
    
    new_symbol->var_func_par = FUNC_T;
    new_symbol->ident_length = strlen($2)+1;
    new_symbol->ident = malloc(new_symbol->ident_length);
    strncpy(new_symbol->ident, $2, new_symbol->ident_length);
    free($2);
    new_symbol->scope = 0;
    new_symbol->type.func.atomic_type = $7;
    new_symbol->type.func.nb_param = list_len($4);
    if (list_len($4) != 0) {
        new_symbol->type.func.index_param = malloc(list_len($4) * sizeof(int));
    }
    else {
        new_symbol->type.func.index_param = NULL;
    }
    for (int i = 0; i < new_symbol->type.func.nb_param; i ++) {
        new_symbol->type.func.index_param[i] = 
            symbol_table.last_ident_index + 1 + i;
    }

    int index_function = symbol_table.last_ident_index;
    symbol_table.last_ident_index ++;
    symbol_table.cur_symbol_scope = index_function; // mabye useless
    // add function parameters
    while ($4 != NULL && list_len($4) != 0) {
        struct param_t *data = (struct param_t *)list_get_first($4);
        if (is_symbol_in_table(data->ident, symbol_table.cur_symbol_scope)) {
            handle_error("identifier [%s] already declared.", data->ident);
        }
        realloc_table();
        struct symbol_t *new_symbol = 
            &symbol_table.symbols[symbol_table.last_ident_index];
        
        new_symbol->var_func_par = PARAM_T;
        new_symbol->ident_length = strlen(data->ident)+1;
        new_symbol->ident = malloc(new_symbol->ident_length);
        strncpy(new_symbol->ident, data->ident, new_symbol->ident_length);
        free(data->ident);
        new_symbol->scope = symbol_table.cur_symbol_scope;
        new_symbol->type.param.ident = NULL;
        new_symbol->type.param.ref = data->ref;
        copy_typename_table(&new_symbol->type.param.typename, data->typename);
        symbol_table.last_ident_index ++;
        if (data->typename.symbol_type == ARRAY_TYPE) {
            list_free(data->typename.range_list);
        }
        list_pop($4);
    }
    if($4 != NULL) {
        list_free($4);
    }
}
/* TODO instr add later */

parlist : /* empty */ {$$ = NULL;}
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
        $$.ident = malloc(strlen($1)+1);
        strcpy($$.ident, $1);
        $$.ref = 0;
        $$.typename = $3;
        free($1);
    }
    | REF IDENT ':' typename {
        $$.ident = malloc(strlen($2)+1);
        strcpy($$.ident, $2);
        $$.ref = 1; 
        $$.typename = $4;
        free($2);
    }

/* -------------------------------------------------------------------------- */
/*                            expression                                      */
/* -------------------------------------------------------------------------- */

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
    vfprintf(stderr, "error : ", ap);
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

int main (void) {
    init_symbol_table();
    int c = yyparse();
    display_symbol_table(); // TODO only if option
    free_symbol_table();
    yylex_destroy();
    return c;
}

/* TODO

TODO valgrind / function for action in grammar

program options todo -version -tos -o <name>

src include dir

strncpy

print scope of var /param ??? or \n is enough to understand?

warning: 1 shift/reduce conflict [-Wconflicts-sr] to fix

-Werror -Wall -Wextra

rename linked_list by linked_list_t

scalpa comment todo (* comment *)

alloc error CHECK macro

(2^3)*9 != 2^3*9 priority ???

cste string regular expression bug for  "//" valid, "/" not valid, add single 
quote example " '"' " is a valid syntaxe

free mermory if EXIT_FAILURE or syntaxe error ???

@brief @param etc each function (doxygen)

delete DEBUG comment and printf

Do a real README file

*/