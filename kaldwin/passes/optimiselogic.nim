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
      elif e.leftChild.kind == rexprNot and e.rightChild.kind == rexprNot:
        e = makeNot(makeOr(e.leftChild.notChild, e.rightChild.notChild))
        walk(e)
    of binaryOpOr:
      if e.leftChild.isOne() or e.rightChild.isOne():
        e = one
      elif e.leftChild.isZero():
        e = e.rightChild
      elif e.rightChild.isZero():
        e = e.leftChild
      elif e.leftChild.kind == rexprNot and e.rightChild.kind == rexprNot:
        e = makeNot(makeAnd(e.leftChild.notChild, e.rightChild.notChild))
        walk(e)
    of binaryOpXor:
      if e.leftChild.isZero():
        e = e.rightChild
      elif e.rightChild.isZero():
        e = e.leftChild
      elif e.leftChild.isOne():
        e = makeNot(e.rightChild)
      elif e.rightChild.isOne():
        e = makeNot(e.leftChild)
    of binaryOpEq:
      e.op = binaryOpXor
      e = makeNot(e)
      walk(e)
    of binaryOpNe:
      e.op = binaryOpXor
      walk(e)

  of rexprMux:
    walk(e.muxCondition)
    walk(e.muxThen)
    walk(e.muxElse)
    let thenIsZero = e.muxThen.isZero()
    let thenIsOne = e.muxThen.isOne()
    let elseIsZero = e.muxElse.isZero()
    let elseIsOne = e.muxElse.isOne()
    if e.muxCondition.isZero() or e.muxThen.isUndefined():
      e = e.muxElse
    elif e.muxCondition.isOne() or e.muxElse.isUndefined():
      e = e.muxThen
    elif thenIsZero and elseIsZero:
      e = zero
    elif thenIsOne and elseIsOne:
      e = one
    elif thenIsOne and elseIsZero:
      e = e.muxCondition
    elif thenIsZero and elseIsOne:
      e = makeNot(e.muxCondition)
    elif thenIsZero:
      e = makeAnd(makeNot(e.muxCondition), e.muxElse)
      walk(e)
    elif thenIsOne:
      e = makeOr(e.muxCondition, e.muxElse)
      walk(e)
    elif elseIsZero:
      e = makeAnd(e.muxCondition, e.muxThen)
      walk(e)
    elif elseIsOne:
      e = makeOr(makeNot(e.muxCondition), e.muxElse)
      walk(e)
    elif e.muxCondition.kind == rexprNot:
      e.muxCondition = e.muxCondition.notChild
      swap(e.muxThen, e.muxElse)
    elif e.muxThen == e.muxElse:
      e = e.muxThen

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
  of stmtIf, stmtSwitch:
    assert false

proc walk(unit: CompilationUnitRef) =
  for s in unit.stmts:
    walk(s)

proc optimiseLogic*(unit: CompilationUnitRef) =
  walk(unit)
