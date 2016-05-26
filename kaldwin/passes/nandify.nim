import kaldwin.types

const generatedLoc: Loc = (filename: "<generated in nandify>", lineno: 0)

proc makeNot[RN](a: RExprRef[RN]): RExprRef[RN] =
  RExprRef[RN](
    loc: generatedLoc,
    kind: rexprNot,
    notChild: a,
  )

proc makeNand[RN](a, b: RExprRef[RN]): RExprRef[RN] =
  RExprRef[RN](
    loc: generatedLoc,
    kind: rexprBinaryOp,
    op: binaryOpNand,
    leftChild: a,
    rightChild: b,
  )

proc xorToNand[RN](a, b: RExprRef[RN]): RExprRef[RN] =
  let p = makeNand(a, b)
  result = makeNand(makeNand(a, p), makeNand(b, p))

proc muxToNand[RN](cond, then, els: RExprRef[RN]): RExprRef[RN] =
  makeNand(makeNand(cond, then), makeNand(makeNot(cond), els))

proc walk[RN](e: var RExprRef[RN]) =
  case e.kind
  of rexprNodeRef, rexprLiteral, rexprUndefined:
    discard # doesn't need modification
  of rexprNot:
    walk(e.notChild)
  of rexprBinaryOp:
    walk(e.leftChild)
    walk(e.rightChild)
    case e.op
    of binaryOpNand:
      discard # doesn't need modification
    of binaryOpAnd:
      e = makeNot(makeNand(e.leftChild, e.rightChild))
    of binaryOpOr:
      e = makeNand(makeNot(e.leftChild), makeNot(e.rightChild))
    of binaryOpXor:
      e = xorToNand(e.leftChild, e.rightChild)
  of rexprMux:
    walk(e.muxCondition)
    walk(e.muxThen)
    walk(e.muxElse)
    e = muxToNand(e.muxCondition, e.muxThen, e.muxElse)
  of rexprConcat:
    assert(false, "rexprConcat should not be present at this stage")
  of rexprMultiply:
    assert(false, "rexprMultiply should not be present at this stage")
  of rexprSlice:
    assert(false, "rexprSlice should not be present at this stage")

proc walk[LN, RN](s: StmtRef[LN, RN]) =
  case s.kind
  of stmtAssign:
    walk(s.source)
  of stmtIf:
    assert(false, "stmtIf should not be present at this stage")

proc walk[LN, RN](unit: CompilationUnitRef[LN, RN]) =
  for s in unit.stmts:
    walk(s)

proc nandify*[LN, RN](unit: CompilationUnitRef[LN, RN]) =
  walk(unit)
