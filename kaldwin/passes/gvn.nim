# Global-value numbering

import hashes
import tables
import kaldwin.types

const generatedLoc: Loc = (filename: "<generated in gvn>", lineno: 0)

type
  GVN[N] = tuple
    unit: CompilationUnitRef[N]
    exprCache: Table[Hash, RExprRef[N]]
    nextIntermediate: int

proc newIntermediate[N](gvn: var GVN[N], width: int = 1): N =
  result = "·gvn·" & $gvn.nextIntermediate
  inc gvn.nextIntermediate

  gvn.unit.intermediateWidths[result] = width

proc createAssign[N](gvn: var GVN[N], dest: LExprRef[N], source: RExprRef[N]) =
  gvn.unit.stmts.add(StmtRef[N](
    loc: generatedLoc,
    source: source,
    dest: dest,
  ))

proc walk[N](gvn: var GVN[N], e: var RExprRef[N]) =
  # Walk children
  case e.kind
  of rexprNodeRef, rexprLiteral, rexprUndefined:
    discard # no children
  of rexprNot:
    gvn.walk(e.notChild)
  of rexprBinaryOp:
    gvn.walk(e.leftChild)
    gvn.walk(e.rightChild)
  of rexprMux:
    gvn.walk(e.muxCondition)
    gvn.walk(e.muxThen)
    gvn.walk(e.muxElse)
  of rexprConcat:
    for child in e.concatChildren.mitems():
      gvn.walk(child)
  of rexprMultiply:
    gvn.walk(e.multiplyChild)
  of rexprSlice:
    gvn.walk(e.sliceChild)

  # If not a noderef or literal, reduce to a noderef.
  case e.kind
  of rexprNodeRef, rexprLiteral, rexprUndefined:
    discard # no change necessary

  of rexprNot, rexprBinaryOp, rexprMux, rexprConcat, rexprMultiply, rexprSlice:
    let hash = e.hash()
    if hash notin gvn.exprCache:
      let intermediate = gvn.newIntermediate()
      let intermediateDest = LExprRef[N](loc: generatedLoc, kind: lexprNodeRef, node: intermediate)
      let intermediateSource = RExprRef[N](loc: generatedLoc, kind: rexprNodeRef, node: intermediate)
      gvn.createAssign(intermediateDest, e)
      gvn.exprCache[hash] = intermediateSource
    e = gvn.exprCache[hash]

proc walk[N](gvn: var GVN[N], s: StmtRef[N]) =
  case s.kind
  of stmtAssign:
    assert(s.dest.kind == lexprNodeRef, "the l-expressions that should be present at this stage are lexprNodeRefs")
    gvn.walk(s.source)

  of stmtIf:
    assert(false, "stmtIf should not be present at this stage")

  gvn.unit.stmts.add(s)

proc runGVN*[N](unit: CompilationUnitRef[N]) =
  let stmts = unit.stmts
  unit.stmts = @[]

  var gvn: GVN[N]
  gvn.unit = unit
  gvn.exprCache = initTable[Hash, RExprRef[N]]()
  gvn.nextIntermediate = 1

  for s in stmts:
    gvn.walk(s)
