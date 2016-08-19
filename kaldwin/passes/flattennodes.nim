import tables, ../types
from algorithm import reverse
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

proc reduce(exprs: seq[RExprRef], op: BinaryOp): RExprRef =
  if exprs.len == 1:
    result = exprs[0]
  else:
    let pivot = exprs.len div 2
    let left = reduce(exprs[0 .. pivot-1], op)
    let right = reduce(exprs[pivot .. exprs.len-1], op)
    result = RExprRef(
      loc: left.loc,
      kind: rexprBinaryOp,
      op: op,
      leftChild: left,
      rightChild: right
    )

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
      var bitExprsToReduce = newSeq[RExprRef](leftBitExprs.len)
      for i in 0 .. leftBitExprs.len-1:
        bitExprsToReduce[i] = RExprRef(
          loc: e.loc,
          kind: rexprBinaryOp,
          op: binaryOpEq,
          leftChild: leftBitExprs[i],
          rightChild: rightBitExprs[i],
        )
      var resultExpr = reduce(bitExprsToReduce, binaryOpAnd)
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

proc walk(stmts: seq[StmtRef], newStmts: var seq[StmtRef], unit: CompilationUnitRef)
proc walk(stmts: var seq[StmtRef], unit: CompilationUnitRef)

proc flattenSwitch(dest: var seq[StmtRef], s: StmtRef, bitExprs: seq[RExprRef], bitExprIdx, casesLow, casesHigh: int, unit: CompilationUnitRef) =
  if casesLow == casesHigh:
    assert bitExprIdx < 0
    walk(s.switchCases[casesLow], dest, unit)
  else:
    var thenChildren = newSeq[StmtRef]()
    var elseChildren = newSeq[StmtRef]()
    let casesMidLow = (casesLow + casesHigh) div 2
    let casesMidHigh = casesMidLow + 1
    assert((casesHigh - casesMidHigh) == (casesMidLow - casesLow))
    flattenSwitch(thenChildren, s, bitExprs, bitExprIdx - 1, casesMidHigh, casesHigh, unit)
    flattenSwitch(elseChildren, s, bitExprs, bitExprIdx - 1, casesLow, casesMidLow, unit)
    dest.add(StmtRef(
      loc: s.loc,
      kind: stmtIf,
      ifCondition: bitExprs[bitExprIdx],
      ifThenChildren: thenChildren,
      ifElseChildren: elseChildren,
    ))

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

  of stmtSwitch:
    let bitExprs = flatten(s.switchExpr, unit)
    flattenSwitch(newStmts, s, bitExprs, bitExprs.high, s.switchCases.low, s.switchCases.high, unit)

proc walk(stmts: seq[StmtRef], newStmts: var seq[StmtRef], unit: CompilationUnitRef) =
  for s in stmts:
    walk(s, newStmts, unit)

proc walk(stmts: var seq[StmtRef], unit: CompilationUnitRef) =
  var newStmts: seq[StmtRef] = @[]
  walk(stmts, newStmts, unit)
  stmts = newStmts

proc flattenNodes*(unit: CompilationUnitRef) =
  walk(unit.stmts, unit)
