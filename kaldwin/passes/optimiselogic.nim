import ../types

const generatedLoc: Loc = (filename: "<generated in optimiseLogic>", lineno: 0)

proc isZero(e: RExprRef): bool {.noSideEffect.} =
  e.kind == rexprLiteral and e.literalWidth == 1 and e.literalValue == 0

proc isOne(e: RExprRef): bool {.noSideEffect.} =
  e.kind == rexprLiteral and e.literalWidth == 1 and e.literalValue == 1

proc isUndefined(e: RExprRef): bool {.noSideEffect.} =
  e.kind == rexprUndefined

proc makeNot(a: RExprRef): RExprRef {.noSideEffect.} =
  if a.kind == rexprNot:
    a.notChild
  else:
    RExprRef(
      loc: generatedLoc,
      kind: rexprNot,
      notChild: a,
    )

proc makeAnd(a, b: RExprRef): RExprRef {.noSideEffect.} =
  RExprRef(
    loc: generatedLoc,
    kind: rexprBinaryOp,
    op: binaryOpAnd,
    leftChild: a,
    rightChild: b,
  )

proc makeOr(a, b: RExprRef): RExprRef {.noSideEffect.} =
  RExprRef(
    loc: generatedLoc,
    kind: rexprBinaryOp,
    op: binaryOpOr,
    leftChild: a,
    rightChild: b,
  )

proc walk(e: var RExprRef) =
  let zero = RExprRef(
    loc: generatedLoc,
    kind: rexprLiteral,
    literalWidth: 1,
    literalValue: 0,
  )
  let one = RExprRef(
    loc: generatedLoc,
    kind: rexprLiteral,
    literalWidth: 1,
    literalValue: 1,
  )

  case e.kind
  of rexprNodeRef, rexprLiteral, rexprUndefined:
    discard # no modification needed

  of rexprNot:
    walk(e.notChild)
    if e.notChild.kind == rexprNot: # double-not chain
      e = e.notChild.notChild
    elif e.notChild.isZero():
      e = one
    elif e.notChild.isOne():
      e = zero

  of rexprBinaryOp:
    walk(e.leftChild)
    walk(e.rightChild)
    case e.op
    of binaryOpAnd:
      if e.leftChild.isZero() or e.rightChild.isZero():
        e = zero
      elif e.leftChild.isOne():
        e = e.rightChild
      elif e.rightChild.isOne():
        e = e.leftChild
    of binaryOpOr:
      if e.leftChild.isOne() or e.rightChild.isOne():
        e = one
      elif e.leftChild.isZero():
        e = e.rightChild
      elif e.rightChild.isZero():
        e = e.leftChild
    of binaryOpXor:
      if e.leftChild.isZero():
        e = e.rightChild
      elif e.rightChild.isZero():
        e = e.leftChild
      elif e.leftChild.isOne():
        e = makeNot(e.rightChild)
      elif e.rightChild.isOne():
        e = makeNot(e.leftChild)
    of binaryOpEq, binaryOpNe:
      if e.leftChild.isZero():
        e = makeNot(e.rightChild)
      elif e.leftChild.isOne():
        e = e.rightChild
      elif e.rightChild.isZero():
        e = makeNot(e.leftChild)
      elif e.rightChild.isOne():
        e = e.leftChild

  of rexprMux:
    walk(e.muxCondition)
    walk(e.muxThen)
    walk(e.muxElse)
    if e.muxCondition.isZero() or e.muxThen.isUndefined():
      e = e.muxElse
    elif e.muxCondition.isOne() or e.muxElse.isUndefined():
      e = e.muxThen
    elif e.muxThen.isOne() and e.muxElse.isZero():
      e = e.muxCondition
    elif e.muxThen.isZero() and e.muxElse.isOne():
      e = makeNot(e.muxCondition)
    elif e.muxThen.isZero():
      e = makeAnd(makeNot(e.muxCondition), e.muxElse)
    elif e.muxThen.isOne():
      e = makeOr(e.muxCondition, e.muxElse)
    elif e.muxElse.isZero():
      e = makeAnd(e.muxCondition, e.muxThen)
    elif e.muxElse.isOne():
      e = makeOr(makeNot(e.muxCondition), e.muxElse)
    elif e.muxCondition.kind == rexprNot:
      e.muxCondition = e.muxCondition.notChild
      swap(e.muxThen, e.muxElse)

  of rexprConcat:
    for child in e.concatChildren.mitems():
      walk(child)

  of rexprMultiply:
    walk(e.multiplyChild)

  of rexprSlice:
    walk(e.sliceChild)

proc walk(s: StmtRef) =
  case s.kind
  of stmtAssign:
    walk(s.source)
  of stmtIf:
    assert(false, "stmtIf should not be present at this stage")

proc walk(unit: CompilationUnitRef) =
  for s in unit.stmts:
    walk(s)

proc optimiseLogic*(unit: CompilationUnitRef) =
  walk(unit)
