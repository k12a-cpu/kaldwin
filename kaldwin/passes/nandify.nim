import kaldwin.types

const generatedLoc: Loc = (filename: "<generated in nandify>", lineno: 0)

proc makeNot[N](a: RExprRef[N]): RExprRef[N] {.noSideEffect.} =
  RExprRef[N](
    loc: generatedLoc,
    kind: rexprNot,
    notChild: a,
  )

proc makeNand[N](a, b: RExprRef[N]): RExprRef[N] {.noSideEffect.} =
  RExprRef[N](
    loc: generatedLoc,
    kind: rexprBinaryOp,
    op: binaryOpNand,
    leftChild: a,
    rightChild: b,
  )

proc xorToNand[N](a, b: RExprRef[N]): RExprRef[N] {.noSideEffect.} =
  let p = makeNand(a, b)
  result = makeNand(makeNand(a, p), makeNand(b, p))

proc muxToNand[N](cond, then, els: RExprRef[N]): RExprRef[N] {.noSideEffect.} =
  makeNand(makeNand(cond, then), makeNand(makeNot(cond), els))

proc walk[N](e: var RExprRef[N]) =
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

proc walk[N](s: StmtRef[N]) =
  case s.kind
  of stmtAssign:
    walk(s.source)
  of stmtIf:
    assert(false, "stmtIf should not be present at this stage")

proc walk[N](unit: CompilationUnitRef[N]) =
  for s in unit.stmts:
    walk(s)

proc nandify*[N](unit: CompilationUnitRef[N]) =
  walk(unit)
