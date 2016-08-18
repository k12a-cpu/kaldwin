import tables, ../types
from sequtils import cycle

proc flatten(e: LExprRef, unit: CompilationUnitRef): seq[LExprRef] =
  case e.kind
  of lexprNodeRef:
    let width = unit.nodes[e.node].width
    result.newSeq(width)
    for i in 0 .. width-1:
      result[i] = LExprRef(
        loc: e.loc,
        kind: lexprSlice,
        sliceUpperBound: i,
        sliceLowerBound: i,
        sliceChild: e,
      )

  of lexprConcat:
    result = @[]
    for i in countdown(e.concatChildren.high, e.concatChildren.low):
      result.add(flatten(e.concatChildren[i], unit))

  of lexprSlice:
    let bitExprs = flatten(e.sliceChild, unit)
    result = bitExprs[e.sliceLowerBound .. e.sliceUpperBound]

proc flatten(e: RExprRef, unit: CompilationUnitRef): seq[RExprRef] =
  case e.kind
  of rexprNodeRef:
    let width = unit.nodes[e.node].width
    result.newSeq(width)
    for i in 0 .. width-1:
      result[i] = RExprRef(
        loc: e.loc,
        kind: rexprSlice,
        sliceUpperBound: i,
        sliceLowerBound: i,
        sliceChild: e,
      )

  of rexprLiteral:
    result.newSeq(e.literalWidth)
    for i in 0 .. e.literalWidth-1:
      result[i] = RExprRef(
        loc: e.loc,
        kind: rexprLiteral,
        literalWidth: 1,
        literalValue: (e.literalValue shr i) and 1,
      )

  of rexprUndefined:
    result.newSeq(e.undefinedWidth)
    for i in 0 .. e.undefinedWidth-1:
      result[i] = RExprRef(
        loc: e.loc,
        kind: rexprUndefined,
        undefinedWidth: 1,
      )

  of rexprNot:
    result = flatten(e.notChild, unit)
    for bitExpr in result.mitems():
      bitExpr = RExprRef(
        loc: e.loc,
        kind: rexprNot,
        notChild: bitExpr,
      )

  of rexprBinaryOp:
    case e.op
    of binaryOpAnd, binaryOpOr, binaryOpXor:
      let leftBitExprs = flatten(e.leftChild, unit)
      let rightBitExprs = flatten(e.rightChild, unit)
      assert(leftBitExprs.len() == rightBitExprs.len())
      result.newSeq(leftBitExprs.len())
      for i in 0 .. result.len()-1:
        result[i] = RExprRef(
          loc: e.loc,
          kind: rexprBinaryOp,
          op: e.op,
          leftChild: leftBitExprs[i],
          rightChild: rightBitExprs[i],
        )

    of binaryOpEq, binaryOpNe:
      let leftBitExprs = flatten(e.leftChild, unit)
      let rightBitExprs = flatten(e.rightChild, unit)
      assert(leftBitExprs.len() == rightBitExprs.len())
      var resultExpr =
        RExprRef(
          loc: e.loc,
          kind: rexprBinaryOp,
          op: binaryOpEq,
          leftChild: leftBitExprs[0],
          rightChild: rightBitExprs[0],
        )
      for i in 1 .. leftBitExprs.len()-1:
        resultExpr =
          RExprRef(
            loc: e.loc,
            kind: rexprBinaryOp,
            op: binaryOpAnd,
            leftChild: resultExpr,
            rightChild: RExprRef(
              loc: e.loc,
              kind: rexprBinaryOp,
              op: binaryOpEq,
              leftChild: leftBitExprs[i],
              rightChild: rightBitExprs[i],
            ),
          )
      if e.op == binaryOpNe:
        resultExpr = RExprRef(
          loc: e.loc,
          kind: rexprNot,
          notChild: resultExpr,
        )
      result = @[resultExpr]

  of rexprMux:
    let conditionBitExprs = flatten(e.muxCondition, unit)
    assert(conditionBitExprs.len() == 1)
    let conditionBitExpr = conditionBitExprs[0]

    let thenBitExprs = flatten(e.muxThen, unit)
    let elseBitExprs = flatten(e.muxElse, unit)
    assert(thenBitExprs.len() == elseBitExprs.len())
    result.newSeq(thenBitExprs.len())
    for i in 0 .. result.len()-1:
      result[i] = RExprRef(
        loc: e.loc,
        kind: rexprMux,
        muxCondition: conditionBitExpr,
        muxThen: thenBitExprs[i],
        muxElse: elseBitExprs[i],
      )

  of rexprConcat:
    result = @[]
    for i in countdown(e.concatChildren.len()-1, 0):
      result.add(flatten(e.concatChildren[i], unit))

  of rexprMultiply:
    result = cycle(flatten(e.multiplyChild, unit), e.multiplyCount)

  of rexprSlice:
    let bitExprs = flatten(e.sliceChild, unit)
    result = bitExprs[e.sliceLowerBound .. e.sliceUpperBound]

proc walk(stmts: var seq[StmtRef], unit: CompilationUnitRef)

proc walk(s: StmtRef, newStmts: var seq[StmtRef], unit: CompilationUnitRef) =
  case s.kind
  of stmtAssign:
    let sourceBits = flatten(s.source, unit)
    let destBits = flatten(s.dest, unit)
    assert(destBits.len() == sourceBits.len())
    for i in 0 .. sourceBits.len()-1:
      newStmts.add(StmtRef(
        loc: s.loc,
        kind: stmtAssign,
        source: sourceBits[i],
        dest: destBits[i],
      ))

  of stmtIf:
    let conditionBitExprs = flatten(s.ifCondition, unit)
    assert(conditionBitExprs.len() == 1)
    s.ifCondition = conditionBitExprs[0]
    walk(s.ifThenChildren, unit)
    walk(s.ifElseChildren, unit)
    newStmts.add(s)

proc walk(stmts: var seq[StmtRef], unit: CompilationUnitRef) =
  var newStmts: seq[StmtRef] = @[]
  for s in stmts:
    walk(s, newStmts, unit)
  stmts = newStmts

proc flattenNodes*(unit: CompilationUnitRef) =
  walk(unit.stmts, unit)
