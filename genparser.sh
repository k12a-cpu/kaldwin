#!/bin/sh

flex --header-file=kaldwin/lexer_gen.h --outfile=kaldwin/lexer_gen.c kaldwin/lexer.l
bison --defines=kaldwin/parser_gen.h --output=kaldwin/parser_gen.c kaldwin/parser.y
