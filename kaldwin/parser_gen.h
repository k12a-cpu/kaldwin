/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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

#ifndef YY_KALDWIN_YY_KALDWIN_PARSER_GEN_H_INCLUDED
# define YY_KALDWIN_YY_KALDWIN_PARSER_GEN_H_INCLUDED
/* Debug traces.  */
#ifndef KALDWIN_YYDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define KALDWIN_YYDEBUG 1
#  else
#   define KALDWIN_YYDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define KALDWIN_YYDEBUG 0
# endif /* ! defined YYDEBUG */
#endif  /* ! defined KALDWIN_YYDEBUG */
#if KALDWIN_YYDEBUG
extern int kaldwin_yydebug;
#endif

/* Token type.  */
#ifndef KALDWIN_YYTOKENTYPE
# define KALDWIN_YYTOKENTYPE
  enum kaldwin_yytokentype
  {
    ELSE = 258,
    IF = 259,
    INPUT = 260,
    INTERMEDIATE = 261,
    OUTPUT = 262,
    INT = 263,
    IDENT = 264,
    SIZED_INT = 265
  };
#endif

/* Value type.  */
#if ! defined KALDWIN_YYSTYPE && ! defined KALDWIN_YYSTYPE_IS_DECLARED

union KALDWIN_YYSTYPE
{
#line 13 "kaldwin/parser.y" /* yacc.c:1909  */

    uint64_t u64;
    char *str;
    struct {
        uint64_t width;
        uint64_t value;
    } sized_int;

#line 82 "kaldwin/parser_gen.h" /* yacc.c:1909  */
};

typedef union KALDWIN_YYSTYPE KALDWIN_YYSTYPE;
# define KALDWIN_YYSTYPE_IS_TRIVIAL 1
# define KALDWIN_YYSTYPE_IS_DECLARED 1
#endif


extern KALDWIN_YYSTYPE kaldwin_yylval;

int kaldwin_yyparse (void);

#endif /* !YY_KALDWIN_YY_KALDWIN_PARSER_GEN_H_INCLUDED  */
