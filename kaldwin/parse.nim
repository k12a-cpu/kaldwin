import tables
from os import parentDir
from strutils import `%`

import types

{.compile: "lexer_gen.c".}
{.compile: "parser_gen.c".}
{.passC: ("-I" & parentDir(currentSourcePath())).}

type
  ParseError* = object of Exception

var currentFilename: string
var currentLineno {.header: "lexer_gen.h", importc: "kaldwin_yylineno".}: int

var unit: CompilationUnitRef
var stmtStack: seq[StmtRef] = @[]
var lexprStack: seq[LExprRef] = @[]
var rexprStack: seq[RExprRef] = @[]

proc reset() =
  unit.new()
  unit.inputWidths = initTable[string, int]()
  unit.intermediateWidths = initTable[string, int]()
  unit.outputWidths = initTable[string, int]()
  stmtStack.setLen(0)
  lexprStack.setLen(0)
  rexprStack.setLen(0)

proc popn[T](a: var seq[T], count: int): seq[T] {.noSideEffect.} =
  let length = a.len()
  result = a[(length - count) .. (length - 1)]
  a.setLen(length - count)

proc currentLoc(): Loc =
  (filename: currentFilename, lineno: currentLineno)

proc unreachable[T](): T =
  assert(false, "control should not be able to reach this point")

proc parseError(msg: string) =
  raise newException(ParseError, "parse error at $1: $2" % [$currentLoc(), msg])

proc parseError(msg: cstring) {.cdecl, exportc: "kaldwin_yyerror".} =
  parseError($msg)

proc finish(numStatements: uint64) {.cdecl, exportc: "kaldwin_yy_finish".} =
  assert(stmtStack.len() == int(numStatements), "parsing finished with a different number of stmts in the stack to what was expected")
  unit.stmts = stmtStack

proc constructStmtAssignment() {.cdecl, exportc: "kaldwin_yy_construct_stmt_assignment".} =
  let source = rexprStack.pop()
  let dest = lexprStack.pop()
  stmtStack.add(StmtRef(
    loc: currentLoc(),
    kind: stmtAssign,
    source: source,
    dest: dest,
  ))

proc constructStmtIf(numThenChildren, numElseChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_stmt_if".} =
  let elseChildren = stmtStack.popn(int(numElseChildren))
  let thenChildren = stmtStack.popn(int(numThenChildren))
  let condition = rexprStack.pop()
  stmtStack.add(StmtRef(
    loc: currentLoc(),
    kind: stmtIf,
    ifCondition: condition,
    ifThenChildren: thenChildren,
    ifElseChildren: elseChildren,
  ))

proc addInput(name: cstring, bits: uint64) {.cdecl, exportc: "kaldwin_yy_add_input".} =
  unit.inputWidths[$name] = int(bits)

proc addIntermediate(name: cstring, bits: uint64) {.cdecl, exportc: "kaldwin_yy_add_intermediate".} =
  unit.intermediateWidths[$name] = int(bits)

proc addOutput(name: cstring, bits: uint64) {.cdecl, exportc: "kaldwin_yy_add_output".} =
  unit.outputWidths[$name] = int(bits)

proc constructLExprSlice(upperBound, lowerBound: uint64) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_slice".} =
  let child = lexprStack.pop()
  lexprStack.add(LExprRef(
    loc: currentLoc(),
    kind: lexprSlice,
    sliceUpperBound: int(upperBound),
    sliceLowerBound: int(lowerBound),
    sliceChild: child,
  ))

proc constructLExprNodeRef(name: cstring) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_noderef".} =
  lexprStack.add(LExprRef(
    loc: currentLoc(),
    kind: lexprNodeRef,
    node: $name,
  ))

proc constructLExprConcat(numChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_concat".} =
  let children = lexprStack.popn(int(numChildren))
  lexprStack.add(LExprRef(
    loc: currentLoc(),
    kind: lexprConcat,
    concatChildren: children,
  ))

proc constructRExprNot() {.cdecl, exportc: "kaldwin_yy_construct_rexpr_not".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef(
    loc: currentLoc(),
    kind: rexprNot,
    notChild: child,
  ))

proc constructRExprSlice(upperBound, lowerBound: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_slice".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef(
    loc: currentLoc(),
    kind: rexprSlice,
    sliceUpperBound: int(upperBound),
    sliceLowerBound: int(lowerBound),
    sliceChild: child,
  ))

proc constructRExprBinaryOp(opChar: char) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_binaryop".} =
  let rightChild = rexprStack.pop()
  let leftChild = rexprStack.pop()
  let op =
    case opChar
    of '&': binaryOpAnd
    of '|': binaryOpOr
    of '^': binaryOpXor
    of '=': binaryOpEq
    of '!': binaryOpNe
    else:   unreachable[BinaryOp]()
  rexprStack.add(RExprRef(
    loc: currentLoc(),
    kind: rexprBinaryOp,
    op: op,
    leftChild: leftChild,
    rightChild: rightChild,
  ))

proc constructRExprNodeRef(name: cstring) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_noderef".} =
  rexprStack.add(RExprRef(
    loc: currentLoc(),
    kind: rexprNodeRef,
    node: $name,
  ))

proc constructRExprLiteral(width: uint64, value: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_literal".} =
  rexprStack.add(RExprRef(
    loc: currentLoc(),
    kind: rexprLiteral,
    literalWidth: int(width),
    literalValue: int(value),
  ))

proc constructRExprConcat(numChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_concat".} =
  let children = rexprStack.popn(int(numChildren))
  rexprStack.add(RExprRef(
    loc: currentLoc(),
    kind: rexprConcat,
    concatChildren: children,
  ))

proc constructRExprMultiply(count: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_multiply".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef(
    loc: currentLoc(),
    kind: rexprMultiply,
    multiplyCount: int(count),
    multiplyChild: child,
  ))

proc parseStdinInternal() {.cdecl, header: "parser.h", importc: "kaldwin_parse_stdin".}
proc parseFileInternal(filename: cstring) {.cdecl, header: "parser.h", importc: "kaldwin_parse_file".}

proc parseStdin*(): CompilationUnitRef =
  reset()
  currentFilename = "<stdin>"
  parseStdinInternal()
  result = unit
  reset()

proc parseFile*(filename: string): CompilationUnitRef =
  reset()
  currentFilename = filename
  parseFileInternal(filename)
  result = unit
  reset()
