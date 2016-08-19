%option prefix="kaldwin_yy"
%option noyywrap
%option yylineno

%{

#include <stdint.h>
#include <stdlib.h>

#include "parser_gen.h"

%}

%%

[ \t\r\n]               ;
#.*\n                   ;

[\(\)\[\]\{\}=:;,~&\|\^x]  { return kaldwin_yytext[0]; }

case                    { return CASE; }
else                    { return ELSE; }
extern                  { return EXTERN; }
if                      { return IF; }
node                    { return NODE; }
switch                  { return SWITCH; }
transient               { return TRANSIENT; }
"=="                    { return EQ; }
"!="                    { return NE; }

[0-9]+\'b[01]+          { char *endptr;
                          kaldwin_yylval.sized_int.width = (uint64_t) strtol(kaldwin_yytext, &endptr, 10);
                          kaldwin_yylval.sized_int.value = (uint64_t) strtol(endptr + 2, NULL, 2);
                          return SIZED_INT; }

[0-9]+\'d[0-9]+         { char *endptr;
                          kaldwin_yylval.sized_int.width = (uint64_t) strtol(kaldwin_yytext, &endptr, 10);
                          kaldwin_yylval.sized_int.value = (uint64_t) strtol(endptr + 2, NULL, 10);
                          return SIZED_INT; }

[0-9]+\'h[0-9a-fA-F]+   { char *endptr;
                          kaldwin_yylval.sized_int.width = (uint64_t) strtol(kaldwin_yytext, &endptr, 10);
                          kaldwin_yylval.sized_int.value = (uint64_t) strtol(endptr + 2, NULL, 16);
                          return SIZED_INT; }

[0-9]+\'u               { kaldwin_yylval.sized_int.width = (uint64_t) strtol(kaldwin_yytext, NULL, 10);
                          return SIZED_UNDEF; }

[0-9]+                  { kaldwin_yylval.u64 = (uint64_t) strtol(kaldwin_yytext, NULL, 10);
                          return INT; }

[a-zA-Z_][a-zA-Z0-9_]*  { kaldwin_yylval.str = strdup(kaldwin_yytext);
                          return IDENT; }

.                       { char msg[24];
                          snprintf(msg, 24, "invalid character '%c'", kaldwin_yytext[0]);
                          kaldwin_yyerror(msg); }

%%
