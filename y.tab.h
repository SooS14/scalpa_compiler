/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 121 "scalpa.y"

    #include "linked_list.h"
    #include "scalpa.h"
    #include "table_of_symbol.h"

#line 54 "y.tab.h"

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    CTE = 258,
    IDENT = 259,
    ASSIGNMENT = 260,
    FUNCTION = 261,
    REF = 262,
    VAR = 263,
    UNIT_TYPE = 264,
    BOOL_TYPE = 265,
    INT_TYPE = 266,
    OF = 267,
    ARRAY = 268,
    RANGELIST_SEPARATOR = 269,
    PROGRAM = 270,
    OPB = 271,
    OPU = 272,
    OP_MINUS = 273,
    OPU_NOT = 274,
    OPB_POW = 275,
    OPB_STAR = 276,
    OPB_DIVIDE = 277,
    OPB_AND = 278,
    OPB_PLUS = 279,
    OPB_OR = 280,
    OPB_XOR = 281,
    OPB_L_EQ = 282,
    OPB_L = 283,
    OPB_G_EQ = 284,
    OPB_G = 285,
    OPB_EQ = 286,
    OPB_DIFF = 287
  };
#endif
/* Tokens.  */
#define CTE 258
#define IDENT 259
#define ASSIGNMENT 260
#define FUNCTION 261
#define REF 262
#define VAR 263
#define UNIT_TYPE 264
#define BOOL_TYPE 265
#define INT_TYPE 266
#define OF 267
#define ARRAY 268
#define RANGELIST_SEPARATOR 269
#define PROGRAM 270
#define OPB 271
#define OPU 272
#define OP_MINUS 273
#define OPU_NOT 274
#define OPB_POW 275
#define OPB_STAR 276
#define OPB_DIVIDE 277
#define OPB_AND 278
#define OPB_PLUS 279
#define OPB_OR 280
#define OPB_XOR 281
#define OPB_L_EQ 282
#define OPB_L 283
#define OPB_G_EQ 284
#define OPB_G 285
#define OPB_EQ 286
#define OPB_DIFF 287

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 127 "scalpa.y"

    struct cste_value_t cst_u;
    int int_u;
    char *str_u;
    struct linked_list *list_u;
    struct typename_t *typename_u;
    struct param_t par_u;
    struct vardecl_t *vardecl_u;
    struct fundecl_t *fundecl_u;

#line 140 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
