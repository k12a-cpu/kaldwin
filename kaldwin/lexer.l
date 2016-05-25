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

\(                      { return '('; }
\)                      { return ')'; }
\[                      { return '['; }
\]                      { return ']'; }
\{                      { return '{'; }
\}                      { return '}'; }
=                       { return '='; }
:                       { return ':'; }
;                       { return ';'; }
,                       { return ','; }
~                       { return '~'; }
&                       { return '&'; }
\|                      { return '|'; }
\^                      { return '^'; }
x                       { return 'x'; }
else                    { return ELSE; }
if                      { return IF; }
input                   { return INPUT; }
output                  { return OUTPUT; }

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

[0-9]+                  { kaldwin_yylval.u64 = (uint64_t) strtol(kaldwin_yytext, NULL, 10);
                          return INT; }

[a-zA-F_][a-zA-F0-9_]*  { kaldwin_yylval.str = strdup(kaldwin_yytext);
                          return IDENT; }

%%
