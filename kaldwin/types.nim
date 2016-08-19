import hashes, tables
from strutils import `%`

type
  Loc* = tuple
    filename: string
    lineno: int

  BinaryOp* = enum
    binaryOpAnd
    binaryOpOr
    binaryOpXor
    binaryOpEq
    binaryOpNe

  LExprKind* = enum
    lexprNodeRef
    lexprConcat
    lexprSlice

  LExpr* = object
    loc*: Loc
    case kind*: LExprKind
    of lexprNodeRef:
      node*: string
    of lexprConcat:
      concatChildren*: seq[ref LExpr]
    of lexprSlice:
      sliceUpperBound*: int
      sliceLowerBound*: int
      sliceChild*: ref LExpr

  LExprRef* = ref LExpr

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

  RExpr* = object
    loc*: Loc
    case kind*: RExprKind
    of rexprNodeRef:
      node*: string
    of rexprLiteral:
      literalWidth*: int
      literalValue*: int
    of rexprUndefined:
      undefinedWidth*: int
    of rexprNot:
      notChild*: ref RExpr
    of rexprBinaryOp:
      op*: BinaryOp
      leftChild*: ref RExpr
      rightChild*: ref RExpr
    of rexprMux:
      muxCondition*: ref RExpr
      muxThen*: ref RExpr
      muxElse*: ref RExpr
    of rexprConcat:
      concatChildren*: seq[ref RExpr]
    of rexprMultiply:
      multiplyCount*: int
      multiplyChild*: ref RExpr
    of rexprSlice:
      sliceUpperBound*: int
      sliceLowerBound*: int
      sliceChild*: ref RExpr

  RExprRef* = ref RExpr

  StmtKind* = enum
    stmtAssign
    stmtIf
    stmtSwitch

  RawSwitchCase* = object
    loc*: Loc
    matchWidth*: int
    matchValue*: int
    children*: seq[ref Stmt]

  Stmt* = object
    loc*: Loc
    case kind*: StmtKind
    of stmtAssign:
      source*: ref RExpr
      dest*: ref LExpr
    of stmtIf:
      ifCondition*: ref RExpr
      ifThenChildren*: seq[ref Stmt]
      ifElseChildren*: seq[ref Stmt]
    of stmtSwitch:
      switchExpr*: ref RExpr
      switchRawCases*: seq[RawSwitchCase]
      switchWidth*: int                 # populated by check pass
      switchCases*: seq[seq[ref Stmt]]  # populated by check pass

  StmtRef* = ref Stmt

  NodeInfo* = object
    loc*: Loc
    name*: string
    width*: int
    extern*: bool # declared elsewhere in the attano corpus, so no 'node' statement needs to be generated
    transient*: bool # not used outside of this file, so can be optimised away

  CompilationUnit* = object
    nodes*: Table[string, NodeInfo]
    stmts*: seq[ref Stmt]

  CompilationUnitRef* = ref CompilationUnit

proc `$`*(loc: Loc): string {.noSideEffect.} =
  "$1:$2" % [loc.filename, $loc.lineno]

proc hash*(e: LExprRef): Hash {.noSideEffect.} =
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

proc hash*(e: RExprRef): Hash {.noSideEffect.} =
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

# proc hash*(s: StmtRef): Hash {.noSideEffect.} =
#   result = result !& hash(s.kind)

#   case s.kind
#   of stmtAssign:
#     result = result !& hash(s.source) !& hash(s.dest)
#   of stmtIf:
#     result = result !& hash(s.ifCondition) !& hash(s.ifThenChildren) !& hash(s.ifElseChildren)

#   result = !$result
