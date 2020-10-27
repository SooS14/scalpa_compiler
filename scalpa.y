%{
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include "scalpa.h"
#include "linked-list.h"

#define DEFAULT_TABLE_SIZE 1024 // define if header file later
#define MAX

int c = 0; // DEBUG DELETE LATER

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

void copy_atomic_type(struct atomic_type_t *origin, struct atomic_type_t *dest);

/*
 * IMPROVMENT TODO
 * return index of identifier in the table
 * error if var already declared
 * TODO remove static, table need to be global
 */
int add_new_identifier_atomic_type(struct atomic_type_t var);

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
}

%token <cste> CTE
%token <strval> IDENT
%type <cste> expr
%type <ival> opb opu
%type <ival> atomictype integer typename
%type <list_u> identlist

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
        // empty linked list
        // while loop create ident and call func
        //add_new_identifier_atomic_type();
        while (list_len($2) != 0) {
            printf("variable : [%s] type [%i]\n",(char *)list_get_first($2),$4);
            list_pop($2);
        }
        list_free($2);
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
      atomictype        {$$ = $1;}
    | arraytype         {printf("TODO arraytype\n");}

atomictype :
      UNIT_TYPE         {$$ = VOID_A;}
    | BOOL_TYPE         {$$ = BOOL_A;}
    | INT_TYPE          {$$ = INT_A;}

arraytype :
    ARRAY '[' rangelist ']' OF atomictype

rangelist :
      integer RANGELIST_SEPARATOR integer 
    | integer RANGELIST_SEPARATOR integer ',' rangelist

integer :
    expr {printf("TODO check if int\n");}



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

void copy_atomic_type(struct atomic_type_t *origin, struct atomic_type_t *dest){
    dest->type = origin->type;
    dest->initialiazed = origin->initialiazed;
    dest->identifier_length = origin->identifier_length;
    dest->identifier = malloc(origin->identifier_length);
    strncpy(dest->identifier , origin->identifier, origin->identifier_length);
}

int add_new_identifier_atomic_type(struct atomic_type_t var) {
    static int malloc_size = DEFAULT_TABLE_SIZE;
    static int last_identifier_index = 0;
    static struct atomic_type_t* identifier_table;
    // if the table is not initialized
    if (last_identifier_index == 0) {
        identifier_table = malloc(malloc_size * sizeof(struct atomic_type_t));
        last_identifier_index ++;
        copy_atomic_type(&var, &identifier_table[0]);
        return 0;
    }
    // search the name in the table, error if it's already declared
    for (int i = 0; i < last_identifier_index; i++) {
        if (var.identifier_length == identifier_table[i].identifier_length &&
            !strncmp(var.identifier,
            identifier_table[i].identifier,
            var.identifier_length)) {
                handle_error("identifier [%s] already declared.", 
                    var.identifier_length);
        }
    }
    // realloc if last_identifier_index exceed the current table size
    if (malloc_size <= last_identifier_index + 1) {
        malloc_size += DEFAULT_TABLE_SIZE;
        identifier_table = realloc(identifier_table,
            malloc_size * sizeof(struct atomic_type_t));
    }
    copy_atomic_type(&var, &identifier_table[last_identifier_index]);
    last_identifier_index ++;
    return last_identifier_index - 1;
}

int main (void) {
    int c = yyparse();
    yylex_destroy();
    return c;
}

/* TODO

@brief @param etc each function (doxygen)

comment todo (* comment *)

test files for operation (check for every operator)

free malloc sconst

alloc error CHECK macro

cste string regular expression bug for  "//" valid, "/" not valid, add single 
quote example " '"' " is a valid syntaxe

free mermory if exit ???

delete DEBUG comment and printf

Do a real README file
add option
*/