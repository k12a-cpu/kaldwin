# included by flattenbranches.nim

proc walk(key: ContextKey, context: var Context, walked: var HashSet[ContextKey])

proc walk(e: var RExprRef, context: var Context, walked: var HashSet[ContextKey]) =
  case e.kind
  of rexprNodeRef, rexprMultiply:
    assert false
  of rexprLiteral, rexprUndefined:
    discard
  of rexprNot:
    walk(e.notChild, context, walked)
  of rexprBinaryOp:
    walk(e.leftChild, context, walked)
    walk(e.rightChild, context, walked)
  of rexprMux:
    walk(e.muxCondition, context, walked)
    walk(e.muxThen, context, walked)
    walk(e.muxElse, context, walked)
  of rexprConcat:
    for child in e.concatChildren.mitems:
      walk(child, context, walked)
  of rexprSlice:
    assert e.sliceUpperBound == e.sliceLowerBound
    assert e.sliceChild.kind == rexprNodeRef
    let key: ContextKey = (node: e.sliceChild.node, bit: e.sliceUpperBound)
    if key in context:
      walk(key, context, walked)
      e = context[key]

proc walk(key: ContextKey, context: var Context, walked: var HashSet[ContextKey]) =
  if key notin walked:
    walk(context[key], context, walked)
    walked.incl key

proc dereference(context: var Context) =
  var walked: HashSet[ContextKey]
  walked.init()
  for key in context.keys:
    walk(key, context, walked)
  assert walked.len == context.len
