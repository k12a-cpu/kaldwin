%define api.prefix {kaldwin_yy}

%{

#include <stdint.h>
#include <stdio.h>

#include "lexer_gen.h"
#include "parser.h"

%}

%union{
    uint64_t u64;
    char *str;
    struct {
        uint64_t width;
        uint64_t value;
    } sized_int;
}

%token ELSE
%token IF
%token INPUT
%token INTERMEDIATE
%token OUTPUT
%token EQ
%token NE
%token <u64> INT
%token <str> IDENT
%token <sized_int> SIZED_INT

%type <u64> statements_opt statements else_content lexprs_comma lexprs rexprs_comma rexprs

%%

compilation_unit
    : ports_opt statements_opt                  { kaldwin_yy_finish($2); }
    ;

ports_opt
    : ports
    |
    ;

ports
    : ports port
    | port
    ;

port
    : INPUT IDENT ';'                           { kaldwin_yy_add_input($2, 1); }
    | INPUT IDENT '[' INT ']' ';'               { kaldwin_yy_add_input($2, $4); }
    | INTERMEDIATE IDENT ';'                    { kaldwin_yy_add_intermediate($2, 1); }
    | INTERMEDIATE IDENT '[' INT ']' ';'        { kaldwin_yy_add_intermediate($2, $4); }
    | OUTPUT IDENT ';'                          { kaldwin_yy_add_output($2, 1); }
    | OUTPUT IDENT '[' INT ']' ';'              { kaldwin_yy_add_output($2, $4); }
    ;

statements_opt
    : statements                                { $$ = $1; }
    |                                           { $$ = 0; }
    ;

statements
    : statements statement                      { $$ = $1 + 1; }
    | statement                                 { $$ = 1; }
    ;

statement
    : assignment
    | if
    ;

assignment
    : lexpr '=' rexpr ';'                       { kaldwin_yy_construct_stmt_assignment(); }
    ;

if
    : IF rexpr '{' statements_opt '}'           { kaldwin_yy_construct_stmt_if($4, 0); }
    | IF rexpr '{' statements_opt '}'
      ELSE else_content                         { kaldwin_yy_construct_stmt_if($4, $7); }
    ;

else_content
    : '{' statements_opt '}'                    { $$ = $2; }
    | if                                        { $$ = 1; }
    ;

lexprs_comma
    : lexprs ','                                { $$ = $1; }
    | lexprs                                    { $$ = $1; }
    ;

lexprs
    : lexprs ',' lexpr                          { $$ = $1 + 1; }
    | lexpr                                     { $$ = 1; }
    ;

lexpr
    : lexpr '[' INT ']'                         { kaldwin_yy_construct_lexpr_slice($3, $3); }
    | lexpr '[' INT ':' INT ']'                 { kaldwin_yy_construct_lexpr_slice($3, $5); }
    | lexpr_nosuffix
    ;

lexpr_nosuffix
    : lexpr_atom
    ;

lexpr_atom
    : IDENT                                     { kaldwin_yy_construct_lexpr_noderef($1); }
    | '{' lexprs_comma '}'                      { kaldwin_yy_construct_lexpr_concat($2); }
    | '(' lexpr ')'
    ;

rexprs_comma
    : rexprs ','                                { $$ = $1; }
    | rexprs                                    { $$ = $1; }
    ;

rexprs
    : rexprs ',' rexpr                          { $$ = $1 + 1; }
    | rexpr                                     { $$ = 1; }
    ;

rexpr
    : rexpr '&' rexpr_atom                      { kaldwin_yy_construct_rexpr_binaryop('&'); }
    | rexpr '|' rexpr_atom                      { kaldwin_yy_construct_rexpr_binaryop('|'); }
    | rexpr '^' rexpr_atom                      { kaldwin_yy_construct_rexpr_binaryop('^'); }
    | rexpr EQ rexpr_atom                       { kaldwin_yy_construct_rexpr_binaryop('='); }
    | rexpr NE rexpr_atom                       { kaldwin_yy_construct_rexpr_binaryop('!'); }
    | rexpr_atom
    ;

rexpr_atom
    : '~' rexpr_atom                            { kaldwin_yy_construct_rexpr_not(); }
    | rexpr_noprefix
    ;

rexpr_noprefix
    : rexpr_noprefix '[' INT ']'                { kaldwin_yy_construct_rexpr_slice($3, $3); }
    | rexpr_noprefix '[' INT ':' INT ']'        { kaldwin_yy_construct_rexpr_slice($3, $5); }
    | rexpr_nosuffix
    ;

rexpr_nosuffix
    : IDENT                                     { kaldwin_yy_construct_rexpr_noderef($1); }
    | SIZED_INT                                 { kaldwin_yy_construct_rexpr_literal($1.width, $1.value); }
    | '{' rexprs_comma '}'                      { kaldwin_yy_construct_rexpr_concat($2); }
    | '{' INT 'x' rexpr_atom '}'                { kaldwin_yy_construct_rexpr_multiply($2); }
    | '(' rexpr ')'
    ;

%%

void kaldwin_parse_stdin() {
    kaldwin_yyin = stdin;
    while (!feof(kaldwin_yyin)) {
        kaldwin_yyparse();
    }
    kaldwin_yyin = NULL;
}

void kaldwin_parse_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (file != NULL) {
        kaldwin_yyin = file;
        while (!feof(kaldwin_yyin)) {
            kaldwin_yyparse();
        }
        kaldwin_yyin = NULL;
        fclose(file);
    }
}
