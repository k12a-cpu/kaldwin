from strutils import `%`
import hashes
import tables

type
  Loc* = tuple
    filename: string
    lineno: int

  BinaryOp* = enum
    binaryOpNand
    binaryOpAnd
    binaryOpOr
    binaryOpXor

  LExprKind* = enum
    lexprNodeRef
    lexprConcat
    lexprSlice

  LExpr*[N] = object
    loc*: Loc
    case kind*: LExprKind
    of lexprNodeRef:
      node*: N
    of lexprConcat:
      concatChildren*: seq[ref LExpr[N]]
    of lexprSlice:
      sliceUpperBound*: int
      sliceLowerBound*: int
      sliceChild*: ref LExpr[N]

  LExprRef*[N] = ref LExpr[N]

  RExprKind* = enum
    rexprNodeRef
    rexprLiteral
    rexprUndefined
    rexprNot
    rexprBinaryOp
    rexprMux
    rexprConcat
    rexprMultiply
    rexprSlice

  RExpr*[N] = object
    loc*: Loc
    case kind*: RExprKind
    of rexprNodeRef:
      node*: N
    of rexprLiteral:
      literalWidth*: int
      literalValue*: int
    of rexprUndefined:
      undefinedWidth*: int
    of rexprNot:
      notChild*: ref RExpr[N]
    of rexprBinaryOp:
      op*: BinaryOp
      leftChild*: ref RExpr[N]
      rightChild*: ref RExpr[N]
    of rexprMux:
      muxCondition*: ref RExpr[N]
      muxThen*: ref RExpr[N]
      muxElse*: ref RExpr[N]
    of rexprConcat:
      concatChildren*: seq[ref RExpr[N]]
    of rexprMultiply:
      multiplyCount*: int
      multiplyChild*: ref RExpr[N]
    of rexprSlice:
      sliceUpperBound*: int
      sliceLowerBound*: int
      sliceChild*: ref RExpr[N]

  RExprRef*[N] = ref RExpr[N]

  StmtKind* = enum
    stmtAssign
    stmtIf

  Stmt*[N] = object
    loc*: Loc
    case kind*: StmtKind
    of stmtAssign:
      source*: ref RExpr[N]
      dest*: ref LExpr[N]
    of stmtIf:
      ifCondition*: ref RExpr[N]
      ifThenChildren*: seq[ref Stmt[N]]
      ifElseChildren*: seq[ref Stmt[N]]

  StmtRef*[N] = ref Stmt[N]

  CompilationUnit*[N] = object
    inputWidths*: Table[N, int]
    intermediateWidths*: Table[N, int]
    outputWidths*: Table[N, int]
    stmts*: seq[ref Stmt[N]]

  CompilationUnitRef*[N] = ref CompilationUnit[N]

proc `$`*(loc: Loc): string =
  "$1:$2" % [loc.filename, $loc.lineno]

proc hash*[N](e: LExprRef[N]): Hash =
  result = result !& hash(e.kind)

  case e.kind
  of lexprNodeRef:
    result = result !& hash(e.node)
  of lexprConcat:
    for child in e.concatChildren:
      result = result !& hash(child)
  of lexprSlice:
    result = result !& hash(e.sliceUpperBound) !& hash(e.sliceLowerBound) !& hash(e.sliceChild)

  result = !$result

proc hash*[N](e: RExprRef[N]): Hash =
  result = result !& hash(e.kind)

  case e.kind
  of rexprNodeRef:
    result = result !& hash(e.node)
  of rexprLiteral:
    result = result !& hash(e.literalWidth) !& hash(e.literalValue)
  of rexprUndefined:
    result = result !& hash(e.undefinedWidth)
  of rexprNot:
    result = result !& hash(e.notChild)
  of rexprBinaryOp:
    result = result !& hash(e.op) !& hash(e.leftChild) !& hash(e.rightChild)
  of rexprMux:
    result = result !& hash(e.muxCondition) !& hash(e.muxThen) !& hash(e.muxElse)
  of rexprConcat:
    for child in e.concatChildren:
      result = result !& hash(child)
  of rexprMultiply:
    result = result !& hash(e.multiplyCount) !& hash(e.multiplyChild)
  of rexprSlice:
    result = result !& hash(e.sliceUpperBound) !& hash(e.sliceLowerBound) !& hash(e.sliceChild)

  result = !$result

proc hash*[N](s: StmtRef[N]): Hash =
  result = result !& hash(s.kind)

  case s.kind
  of stmtAssign:
    result = result !& hash(s.source) !& hash(s.dest)
  of stmtIf:
    result = result !& hash(s.ifCondition) !& hash(s.ifThenChildren) & hash(s.ifElseChildren)

  result = !$result

proc hash*[N](unit: CompilationUnitRef[N]): Hash =
  result = !$(result !& hash(unit.inputWidths) !& hash(unit.intermediateWidths) !& hash(unit.outputWidths) !& hash(unit.stmts))
