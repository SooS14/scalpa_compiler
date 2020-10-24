%{
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include "scalpa.h"

int c = 0; // DEBUG A SUPPRIMER

int yylex(void);

int yylex_destroy(void);

void yyerror(const char * msg);

noreturn void handle_error(const char * msg, ...);

void display_cste(struct cste_value_t cste);

struct cste_value_t compute_opb(struct cste_value_t expr1, 
                                struct cste_value_t expr2,
                                int opb);

struct cste_value_t compute_opu(struct cste_value_t expr, int opu);

%}
%code requires {
    #include "scalpa.h"
}

%union {
    struct cste_value_t cste;
    int ival;
}

%token <cste> CTE
%token IDENT
%type <cste> expr
%type <ival> opb opu
%token '(' ')' '[' ']' ',' ';' ASSIGNMENT

%left OPB
%left OPU
%left OP_MINUS OPU_NOT
%left OPB_POW
%left OPB_STAR OPB_DIVIDE OPB_AND
%left OPB_PLUS OPB_OR OPB_XOR
%left OPB_L_EQ OPB_L OPB_G_EQ OPB_G OPB_EQ OPB_DIFF

%start test

%%
test : /* empty */ 
    | test expr ';'   {printf("instruction %i : \n", c);display_cste($2);c++;}

exprlist : expr               {printf("TODO expr\n");}
    | expr ',' exprlist       {printf("TODO expr , exprlist\n");}

expr : CTE                    {$$ = $1;}
    | '(' expr ')'            {$$ = $2;}
    | expr opb expr %prec OPB {$$ = compute_opb($1, $3, $2);}
    | opu expr %prec OPU      {$$ = compute_opu($2, $1);}
    | IDENT '(' exprlist ')'  {printf("TODO IDENT ( exprlist )\n");}
    | IDENT '(' ')'           {printf("TODO IDENT ( )\n");}
    | IDENT '[' exprlist ']'  {printf("TODO IDENT [ exprlist ]\n");}
    | IDENT                   {printf("TODO IDENT\n");}

opb : OPB_PLUS   {$$ = OPB_PLUS;}
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

opu : OP_MINUS   {$$ = OP_MINUS;}
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

int main (void) {
    int c = yyparse();
    yylex_destroy();
    return c;
}

/* TODO
alloc error
cste string regular expression
free mermory error ???
// A REFAIRE
// \"(([^"])|(\\"))*[^\\]\"
// \"(((\\\")|[^\"]))*\"
*/