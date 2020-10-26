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
#line 94 "scalpa.y"

    #include "scalpa.h"

#line 52 "y.tab.h"

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    CTE = 258,
    IDENT = 259,
    ASSIGNMENT = 260,
    VAR = 261,
    UNIT_TYPE = 262,
    BOOL_TYPE = 263,
    INT_TYPE = 264,
    OF = 265,
    ARRAY = 266,
    RANGELIST_SEPARATOR = 267,
    PROGRAM = 268,
    OPB = 269,
    OPU = 270,
    OP_MINUS = 271,
    OPU_NOT = 272,
    OPB_POW = 273,
    OPB_STAR = 274,
    OPB_DIVIDE = 275,
    OPB_AND = 276,
    OPB_PLUS = 277,
    OPB_OR = 278,
    OPB_XOR = 279,
    OPB_L_EQ = 280,
    OPB_L = 281,
    OPB_G_EQ = 282,
    OPB_G = 283,
    OPB_EQ = 284,
    OPB_DIFF = 285
  };
#endif
/* Tokens.  */
#define CTE 258
#define IDENT 259
#define ASSIGNMENT 260
#define VAR 261
#define UNIT_TYPE 262
#define BOOL_TYPE 263
#define INT_TYPE 264
#define OF 265
#define ARRAY 266
#define RANGELIST_SEPARATOR 267
#define PROGRAM 268
#define OPB 269
#define OPU 270
#define OP_MINUS 271
#define OPU_NOT 272
#define OPB_POW 273
#define OPB_STAR 274
#define OPB_DIVIDE 275
#define OPB_AND 276
#define OPB_PLUS 277
#define OPB_OR 278
#define OPB_XOR 279
#define OPB_L_EQ 280
#define OPB_L 281
#define OPB_G_EQ 282
#define OPB_G 283
#define OPB_EQ 284
#define OPB_DIFF 285

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 98 "scalpa.y"

    struct cste_value_t cste;
    int ival;
    char *strval;

#line 129 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
