%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "y.tab.h"
#include "scalpa.h"

%}
%%
[0-9]+ {
    struct cste_value_t temp;
    temp.type = INT;
    temp.val.iconst = atoi(yytext);
    yylval.cst_u = temp;
    return CTE;
}
\"(((\\\")|[^\"]))*\" {
    struct cste_value_t temp;
    temp.type = STRING;
    temp.val.sconst = malloc(yyleng + 1);
    strncpy(temp.val.sconst , yytext, yyleng);
    temp.val.sconst[yyleng] = '\0';
    yylval.cst_u = temp;
    return CTE;
}
"true" {
    struct cste_value_t temp;
    temp.type = BOOL;
    temp.val.bconst = 1;
    yylval.cst_u = temp;
    return CTE;
}
"false" {
    struct cste_value_t temp;
    temp.type = BOOL;
    temp.val.bconst = 0;
    yylval.cst_u = temp;
    return CTE;
}

"("             return '(';
")"             return ')';
"["             return '[';
"]"             return ']';
","             return ',';
";"             return ';';
":"             return ':';
":="            return ASSIGNMENT;
"var"           return VAR;
"of"            return OF;
"array"         return ARRAY;
".."            return RANGELIST_SEPARATOR;
"program"       return PROGRAM;
"unit"          return UNIT_TYPE;
"int"           return INT_TYPE;
"bool"          return BOOL_TYPE;

"+"             return OPB_PLUS;
"-"             return OP_MINUS;
"*"             return OPB_STAR;
"/"             return OPB_DIVIDE;
"^"             return OPB_POW;
"<"             return OPB_L;
"<="            return OPB_L_EQ;
">"             return OPB_G;
">="            return OPB_G_EQ;
"="             return OPB_EQ;
"<>"            return OPB_DIFF;
"and"           return OPB_AND;
"or"            return OPB_OR;
"xor"           return OPB_XOR;
"not"           return OPU_NOT;

[a-zA-Z_][a-zA-Z_1-9]* {
    yylval.str_u = malloc(yyleng+1);
    strncpy(yylval.str_u , yytext, yyleng);
    yylval.str_u[yyleng] = '\0';
    return IDENT;
};

[ \t\n]+        (void)0;
.               {fprintf(stderr, "unknown char [%s]\n", yytext); return 0;}
%%