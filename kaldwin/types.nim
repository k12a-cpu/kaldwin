import strutils
import tables

type
  Loc* = tuple
    filename: string
    lineno: int

  BinaryOp* = enum
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
      sliceUpperBound*: uint
      sliceLowerBound*: uint
      sliceChild*: ref LExpr[N]
  
  LExprRef*[N] = ref LExpr[N]
  
  RExprKind* = enum
    rexprNodeRef
    rexprLiteral
    rexprNot
    rexprBinaryOp
    rexprConcat
    rexprMultiply
    rexprSlice
  
  RExpr*[N] = object
    loc*: Loc
    case kind*: RExprKind
    of rexprNodeRef:
      node*: N
    of rexprLiteral:
      literalWidth*: uint
      literalValue*: uint64
    of rexprNot:
      notChild*: ref RExpr[N]
    of rexprBinaryOp:
      op*: BinaryOp
      leftChild*: ref RExpr[N]
      rightChild*: ref RExpr[N]
    of rexprConcat:
      concatChildren*: seq[ref RExpr[N]]
    of rexprMultiply:
      multiplyCount*: uint
      multiplyChild*: ref RExpr[N]
    of rexprSlice:
      sliceUpperBound*: uint
      sliceLowerBound*: uint
      sliceChild*: ref RExpr[N]
  
  RExprRef*[N] = ref RExpr[N]
  
  StmtKind* = enum
    stmtAssign
    stmtIf
  
  Stmt*[LN, RN] = object
    loc*: Loc
    case kind*: StmtKind
    of stmtAssign:
      source*: ref RExpr[RN]
      dest*: ref LExpr[LN]
    of stmtIf:
      ifCondition*: ref RExpr[RN]
      ifThenChildren*: seq[ref Stmt[LN, RN]]
      ifElseChildren*: seq[ref Stmt[LN, RN]]
  
  StmtRef*[LN, RN] = ref Stmt[LN, RN]
  
  CompilationUnit*[LN, RN] = object
    inputWidths*: Table[LN, uint]
    outputWidths*: Table[RN, uint]
    stmts*: seq[ref Stmt[LN, RN]]
  
  CompilationUnitRef*[LN, RN] = ref CompilationUnit[LN, RN]

proc `$`*(loc: Loc): string =
  "$1:$2" % [loc.filename, $loc.lineno]
