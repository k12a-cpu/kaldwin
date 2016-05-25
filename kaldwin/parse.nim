from os import parentDir
import tables
import kaldwin.types

{.compile: "lexer_gen.c".}
{.compile: "parser_gen.c".}
{.passC: ("-I" & parentDir(currentSourcePath())).}

type
  ParseError = object of Exception

var lineno {.header: "lexer_gen.h", importc: "kaldwin_yylineno".}: int

var unit: CompilationUnitRef[string, string]
var stmtStack: seq[StmtRef[string, string]] = @[]
var lexprStack: seq[LExprRef[string]] = @[]
var rexprStack: seq[RExprRef[string]] = @[]

proc reset() =
  unit.new()
  unit.inputWidths = initTable[string, int]()
  unit.outputWidths = initTable[string, int]()
  stmtStack.setLen(0)
  lexprStack.setLen(0)
  rexprStack.setLen(0)

proc popn[T](a: var seq[T], n: int): seq[T] =
  result = a[(a.len() - n) .. (a.len() - 1)]
  a.setLen(a.len() - n)

proc unreachable[T](): T =
  assert(false, "control should not be able to reach this point")

proc parseError(msg: string) =
  raise newException(ParseError, "parse error on line " & $lineno & ": " & msg)

proc parseError(msg: cstring) {.cdecl, exportc: "kaldwin_yyerror".} =
  parseError($msg)

proc finish(numStatements: uint64) {.cdecl, exportc: "kaldwin_yy_finish".} =
  assert(stmtStack.len() == int(numStatements), "parsing finished with a different number of stmts in the stack to what was expected")
  unit.stmts = stmtStack

proc constructStmtAssignment() {.cdecl, exportc: "kaldwin_yy_construct_stmt_assignment".} =
  let source = rexprStack.pop()
  let dest = lexprStack.pop()
  stmtStack.add(StmtRef[string, string](
    kind: stmtAssign,
    source: source,
    dest: dest,
  ))

proc constructStmtIf(numThenChildren, numElseChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_stmt_if".} =
  let elseChildren = stmtStack.popn(int(numElseChildren))
  let thenChildren = stmtStack.popn(int(numThenChildren))
  let condition = rexprStack.pop()
  stmtStack.add(StmtRef[string, string](
    kind: stmtIf,
    ifCondition: condition,
    ifThenChildren: thenChildren,
    ifElseChildren: elseChildren,
  ))

proc addInput(name: cstring, bits: uint64) {.cdecl, exportc: "kaldwin_yy_add_input".} =
  unit.inputWidths[$name] = int(bits)

proc addOutput(name: cstring, bits: uint64) {.cdecl, exportc: "kaldwin_yy_add_output".} =
  unit.outputWidths[$name] = int(bits)

proc constructLExprIndex(index: uint64) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_index".} =
  let child = lexprStack.pop()
  lexprStack.add(LExprRef[string](
    kind: lexprIndex,
    index: int(index),
    indexChild: child,
  ))

proc constructLExprSlice(upperBound, lowerBound: uint64) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_slice".} =
  let child = lexprStack.pop()
  lexprStack.add(LExprRef[string](
    kind: lexprSlice,
    sliceUpperBound: int(upperBound),
    sliceLowerBound: int(lowerBound),
    sliceChild: child,
  ))

proc constructLExprNodeRef(name: cstring) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_noderef".} =
  lexprStack.add(LExprRef[string](
    kind: lexprNodeRef,
    node: $name,
  ))

proc constructLExprConcat(numChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_lexpr_concat".} =
  let children = lexprStack.popn(int(numChildren))
  lexprStack.add(LExprRef[string](
    kind: lexprConcat,
    concatChildren: children,
  ))

proc constructRExprNot() {.cdecl, exportc: "kaldwin_yy_construct_rexpr_not".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef[string](
    kind: rexprNot,
    notChild: child,
  ))

proc constructRExprIndex(index: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_index".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef[string](
    kind: rexprIndex,
    index: int(index),
    indexChild: child,
  ))

proc constructRExprSlice(upperBound, lowerBound: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_slice".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef[string](
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
    else:   unreachable[BinaryOp]()
  rexprStack.add(RExprRef[string](
    kind: rexprBinaryOp,
    op: op,
    leftChild: leftChild,
    rightChild: rightChild,
  ))

proc constructRExprNodeRef(name: cstring) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_noderef".} =
  rexprStack.add(RExprRef[string](
    kind: rexprNodeRef,
    node: $name,
  ))

proc constructRExprLiteral(width: uint64, value: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_literal".} =
  rexprStack.add(RExprRef[string](
    kind: rexprLiteral,
    literalWidth: int(width),
    literalValue: value,
  ))

proc constructRExprConcat(numChildren: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_concat".} =
  let children = rexprStack.popn(int(numChildren))
  rexprStack.add(RExprRef[string](
    kind: rexprConcat,
    concatChildren: children,
  ))

proc constructRExprMultiply(count: uint64) {.cdecl, exportc: "kaldwin_yy_construct_rexpr_multiply".} =
  let child = rexprStack.pop()
  rexprStack.add(RExprRef[string](
    kind: rexprMultiply,
    multiplyCount: int(count),
    multiplyChild: child,
  ))

proc parseStdinInternal() {.cdecl, header: "parser.h", importc: "kaldwin_parse_stdin".}
proc parseFileInternal(filename: cstring) {.cdecl, header: "parser.h", importc: "kaldwin_parse_file".}

proc parseStdin*(): CompilationUnitRef[string, string] =
  reset()
  parseStdinInternal()
  result = unit
  reset()

proc parseFile*(filename: string): CompilationUnitRef[string, string] =
  reset()
  parseFileInternal(filename)
  result = unit
  reset()
