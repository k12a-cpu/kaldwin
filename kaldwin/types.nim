from strutils import `%`
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
      sliceUpperBound*: uint
      sliceLowerBound*: uint
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
      literalWidth*: uint
      literalValue*: uint64
    of rexprUndefined:
      undefinedWidth*: uint
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
    inputWidths*: Table[N, uint]
    intermediateWidths*: Table[N, uint]
    outputWidths*: Table[N, uint]
    stmts*: seq[ref Stmt[N]]
  
  CompilationUnitRef*[N] = ref CompilationUnit[N]

proc `$`*(loc: Loc): string =
  "$1:$2" % [loc.filename, $loc.lineno]
