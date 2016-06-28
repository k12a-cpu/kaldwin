import tables
from sequtils import cycle

import ../types

proc unreachable[T](): T =
  assert(false, "unreachable")

proc nodeBit(node: string, bit: int, width: int): string =
  if width == 1:
    node
  else:
    node & "__bit" & $bit

proc flatten(e: LExprRef, unit: CompilationUnitRef): seq[LExprRef] =
  case e.kind
  of lexprNodeRef:
    let width =
      if e.node in unit.inputWidths:
        unit.inputWidths[e.node]
      elif e.node in unit.intermediateWidths:
        unit.intermediateWidths[e.node]
      elif e.node in unit.outputWidths:
        unit.outputWidths[e.node]
      else:
        unreachable[int]()
    result.newSeq(width)
    for i in 0 .. width-1:
      result[i] = LExprRef(
        loc: e.loc,
        kind: lexprNodeRef,
        node: nodeBit(e.node, i, width),
      )

  of lexprConcat:
    result = @[]
    for i in countdown(e.concatChildren.len()-1, 0):
      result.add(flatten(e.concatChildren[i], unit))
  
  of lexprSlice:
    let bitExprs = flatten(e.sliceChild, unit)
    result = bitExprs[e.sliceLowerBound .. e.sliceUpperBound]

proc flatten(e: RExprRef, unit: CompilationUnitRef): seq[RExprRef] =
  case e.kind
  of rexprNodeRef:
    let width =
      if e.node in unit.inputWidths:
        unit.inputWidths[e.node]
      elif e.node in unit.intermediateWidths:
        unit.intermediateWidths[e.node]
      elif e.node in unit.outputWidths:
        unit.outputWidths[e.node]
      else:
        unreachable[int]()
    result.newSeq(width)
    for i in 0 .. width-1:
      result[i] = RExprRef(
        loc: e.loc,
        kind: rexprNodeRef,
        node: nodeBit(e.node, i, width),
      )

  of rexprLiteral:
    let zero = RExprRef(
      loc: e.loc,
      kind: rexprLiteral,
      literalWidth: 1,
      literalValue: 0,
    )
    let one = RExprRef(
      loc: e.loc,
      kind: rexprLiteral,
      literalWidth: 1,
      literalValue: 1,
    )
    result.newSeq(e.literalWidth)
    for i in 0 .. e.literalWidth-1:
      if (e.literalValue and (1 shl i)) != 0:
        result[i] = one
      else:
        result[i] = zero

  of rexprUndefined:
    let undef = RExprRef(
      loc: e.loc,
      kind: rexprUndefined,
      undefinedWidth: 1,
    )
    result.newSeq(e.undefinedWidth)
    for i in 0 .. e.undefinedWidth-1:
      result[i] = undef
  
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
    of binaryOpNand, binaryOpAnd, binaryOpOr, binaryOpXor:
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
  
  var newInputWidths = initTable[string, int]()
  var newIntermediateWidths = initTable[string, int]()
  var newOutputWidths = initTable[string, int]()
  for node, width in unit.inputWidths:
    for i in 0 .. width-1:
      newInputWidths[nodeBit(node, i, width)] = 1
  for node, width in unit.intermediateWidths:
    for i in 0 .. width-1:
      newIntermediateWidths[nodeBit(node, i, width)] = 1
  for node, width in unit.outputWidths:
    for i in 0 .. width-1:
      newOutputWidths[nodeBit(node, i, width)] = 1
  
  unit.inputWidths = newInputWidths
  unit.intermediateWidths = newIntermediateWidths
  unit.outputWidths = newOutputWidths
