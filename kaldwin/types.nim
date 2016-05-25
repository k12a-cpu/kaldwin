import tables

type
  BinaryOp* = enum
    binaryOpAnd
    binaryOpOr
    binaryOpXor
  
  LExprKind* = enum
    lexprNodeRef
    lexprConcat
    lexprIndex
    lexprSlice
  
  LExpr*[N] = object
    case kind*: LExprKind
    of lexprNodeRef:
      node*: N
    of lexprConcat:
      concatChildren*: seq[ref LExpr[N]]
    of lexprIndex:
      index*: int
      indexChild*: ref LExpr[N]
    of lexprSlice:
      sliceUpperBound*: int
      sliceLowerBound*: int
      sliceChild*: ref LExpr[N]
  
  LExprRef*[N] = ref LExpr[N]
  
  RExprKind* = enum
    rexprNodeRef
    rexprLiteral
    rexprNot
    rexprBinaryOp
    rexprConcat
    rexprMultiply
    rexprIndex
    rexprSlice
  
  RExpr*[N] = object
    case kind*: RExprKind
    of rexprNodeRef:
      node*: N
    of rexprLiteral:
      literalWidth*: int
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
      multiplyCount*: int
      multiplyChild*: ref RExpr[N]
    of rexprIndex:
      index*: int
      indexChild*: ref RExpr[N]
    of rexprSlice:
      sliceUpperBound*: int
      sliceLowerBound*: int
      sliceChild*: ref RExpr[N]
  
  RExprRef*[N] = ref RExpr[N]
  
  StmtKind* = enum
    stmtAssign
    stmtIf
  
  Stmt*[LN, RN] = object
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
    inputWidths*: Table[LN, int]
    outputWidths*: Table[RN, int]
    stmts*: seq[ref Stmt[LN, RN]]
  
  CompilationUnitRef*[LN, RN] = ref CompilationUnit[LN, RN]
