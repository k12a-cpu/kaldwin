# Global-value numbering

import hashes
import tables
import kaldwin.types

const generatedLoc: Loc = (filename: "<generated in gvn>", lineno: 0)

type
  GVN = tuple
    unit: CompilationUnitRef
    exprCache: Table[Hash, RExprRef]
    nextIntermediate: int

proc newIntermediate(gvn: var GVN, width: int = 1): string {.noSideEffect.} =
  result = "__gvn_" & $gvn.nextIntermediate
  inc gvn.nextIntermediate

  gvn.unit.intermediateWidths[result] = width

proc createAssign(gvn: var GVN, dest: LExprRef, source: RExprRef) {.noSideEffect.} =
  gvn.unit.stmts.add(StmtRef(
    loc: generatedLoc,
    source: source,
    dest: dest,
  ))

proc walk(gvn: var GVN, e: var RExprRef) =
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
      let intermediateDest = LExprRef(loc: generatedLoc, kind: lexprNodeRef, node: intermediate)
      let intermediateSource = RExprRef(loc: generatedLoc, kind: rexprNodeRef, node: intermediate)
      gvn.createAssign(intermediateDest, e)
      gvn.exprCache[hash] = intermediateSource
    e = gvn.exprCache[hash]

proc walk(gvn: var GVN, s: StmtRef) =
  case s.kind
  of stmtAssign:
    assert(s.dest.kind == lexprNodeRef, "the l-expressions that should be present at this stage are lexprNodeRefs")
    gvn.walk(s.source)

  of stmtIf:
    assert(false, "stmtIf should not be present at this stage")

  gvn.unit.stmts.add(s)

proc runGVN*(unit: CompilationUnitRef) =
  let stmts = unit.stmts
  unit.stmts = @[]

  var gvn: GVN
  gvn.unit = unit
  gvn.exprCache = initTable[Hash, RExprRef]()
  gvn.nextIntermediate = 1

  for s in stmts:
    gvn.walk(s)
