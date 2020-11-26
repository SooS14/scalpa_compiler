%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "y.tab.h"
#include "scalpa.h"

%}
%%
\(\*([^*]|[\r\n]|(\*+([^*\)]|[\r\n])))*\*+\) (void)0;

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
    MCHECK(temp.val.sconst);
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
".."            return RANGELIST_SEPARATOR;
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
"var"           return VAR;
"of"            return OF;
"array"         return ARRAY;
"program"       return PROGRAM;
"function"      return FUNCTION;
"ref"           return REF;
"unit"          return UNIT_TYPE;
"int"           return INT_TYPE;
"bool"          return BOOL_TYPE;
"read"          return READ;
"write"         return WRITE;
"begin"         return BEGIN_;
"end"           return END;
"if"            return IF;
"then"          return THEN;
"else"          return ELSE;
"return"        return RETURN;
"while"         return WHILE;
"do"            return DO;

[a-zA-Z_][a-zA-Z_1-9]* {
    yylval.str_u = malloc(yyleng+1);
    MCHECK(yylval.str_u);
    strncpy(yylval.str_u , yytext, yyleng);
    yylval.str_u[yyleng] = '\0';
    return IDENT;
};

[ \t\n]+        (void)0;
.               {fprintf(stderr, "unknown char [%s]\n", yytext); return 0;}
%%