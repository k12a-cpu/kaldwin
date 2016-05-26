from os import parentDir
import strutils
import tables
import kaldwin.types

{.compile: "lexer_gen.c".}
{.compile: "parser_gen.c".}
{.passC: ("-I" & parentDir(currentSourcePath())).}

type
  ParseError = object of Exception

var currentFilename: string
var currentLineno {.header: "lexer_gen.h", importc: "kaldwin_yylineno".}: int

var unit: CompilationUnitRef[string, string]
var stmtStack: seq[StmtRef[string, string]] = @[]
var lexprStack: seq[LExprRef[string]] = @[]
var rexprStack: seq[RExprRef[string]] = @[]

proc reset() =
  unit.new()
  unit.inputWidths = initTable[string, uint]()
  unit.outputWidths = initTable[string, uint]()
  stmtStack.setLen(0)
  lexprStack.setLen(0)
  rexprStack.setLen(0)

proc popn[T](a: var seq[T], count: int): seq[T] =
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
  stmtStack.add(StmtRef[string, string](
    loc: currentLoc(),
    kind: stmtAssign,
    source: source,
    dest: dest,
  ))

proc constructStmtIf(numThenChildren, numElseChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_stmt_if".} =
  let elseChildren = stmtStack.popn(int(numElseChildren))
  let thenChildren = stmtStack.popn(int(numThenChildren))
  let condition = rexprStack.pop()
  stmtStack.add(StmtRef[string, string](
    loc: currentLoc(),
    kind: stmtIf,
    ifCondition: condition,
    ifThenChildren: thenChildren,
    ifElseChildren: elseChildren,
  ))

proc addInput(name: cstring, bits: uint64) {.cdecl, exportc: "kaldwin_yy_add_input".} =
  unit.inputWidths[$name] = uint(bits)

proc addOutput(name: cstring, bits: uint64) {.cdecl, exportc: "kaldwin_yy_add_output".} =
  unit.outputWidths[$name] = uint(bits)

proc constructLExprSlice(upperBound, lowerBound: uint64) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_slice".} =
  let child = lexprStack.pop()
  lexprStack.add(LExprRef[string](
    loc: currentLoc(),
    kind: lexprSlice,
    sliceUpperBound: uint(upperBound),
    sliceLowerBound: uint(lowerBound),
    sliceChild: child,
  ))

proc constructLExprNodeRef(name: cstring) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_noderef".} =
  lexprStack.add(LExprRef[string](
    loc: currentLoc(),
    kind: lexprNodeRef,
    node: $name,
  ))

proc constructLExprConcat(numChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_concat".} =
  let children = lexprStack.popn(int(numChildren))
  lexprStack.add(LExprRef[string](
    loc: currentLoc(),
    kind: lexprConcat,
    concatChildren: children,
  ))

proc constructRExprNot() {.cdecl, exportc: "kaldwin_yy_construct_rexpr_not".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef[string](
    loc: currentLoc(),
    kind: rexprNot,
    notChild: child,
  ))

proc constructRExprSlice(upperBound, lowerBound: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_slice".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef[string](
    loc: currentLoc(),
    kind: rexprSlice,
    sliceUpperBound: uint(upperBound),
    sliceLowerBound: uint(lowerBound),
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
    else:   unreachable[BinaryOp]()
  rexprStack.add(RExprRef[string](
    loc: currentLoc(),
    kind: rexprBinaryOp,
    op: op,
    leftChild: leftChild,
    rightChild: rightChild,
  ))

proc constructRExprNodeRef(name: cstring) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_noderef".} =
  rexprStack.add(RExprRef[string](
    loc: currentLoc(),
    kind: rexprNodeRef,
    node: $name,
  ))

proc constructRExprLiteral(width: uint64, value: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_literal".} =
  rexprStack.add(RExprRef[string](
    loc: currentLoc(),
    kind: rexprLiteral,
    literalWidth: uint(width),
    literalValue: value,
  ))

proc constructRExprConcat(numChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_concat".} =
  let children = rexprStack.popn(int(numChildren))
  rexprStack.add(RExprRef[string](
    loc: currentLoc(),
    kind: rexprConcat,
    concatChildren: children,
  ))

proc constructRExprMultiply(count: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_multiply".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef[string](
    loc: currentLoc(),
    kind: rexprMultiply,
    multiplyCount: uint(count),
    multiplyChild: child,
  ))

proc parseStdinInternal() {.cdecl, header: "parser.h", importc: "kaldwin_parse_stdin".}
proc parseFileInternal(filename: cstring) {.cdecl, header: "parser.h", importc: "kaldwin_parse_file".}

proc parseStdin*(): CompilationUnitRef[string, string] =
  reset()
  currentFilename = "<stdin>"
  parseStdinInternal()
  result = unit
  reset()

proc parseFile*(filename: string): CompilationUnitRef[string, string] =
  reset()
  currentFilename = filename
  parseFileInternal(filename)
  result = unit
  reset()
