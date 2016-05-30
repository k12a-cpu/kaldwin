import sets
import tables
import kaldwin.types

const generatedLoc: Loc = (filename: "<generated in flattenBranches>", lineno: 0)

proc getOrDefault[A, B](t: OrderedTable[A, B], key: A, defaultValue: B): B =
  if key in t:
    t[key]
  else:
    defaultValue

proc walk(s: StmtRef, unit: CompilationUnitRef, context: var OrderedTable[string, RExprRef]) =
  case s.kind
  of stmtAssign:
    assert(s.dest.kind == lexprNodeRef)
    context[s.dest.node] = s.source

  of stmtIf:
    # Evaluate the contexts on each branch of the if statement.
    var thenContext = context
    for child in s.ifThenChildren:
      walk(child, unit, thenContext)
    var elseContext = context
    for child in s.ifElseChildren:
      walk(child, unit, elseContext)

    # Enumerate all output nodes in the two contexts.
    var outputNodeSet: HashSet[string]
    outputNodeSet.init()
    for node in thenContext.keys():
      outputNodeSet.incl(node)
    for node in elseContext.keys():
      outputNodeSet.incl(node)

    # For each output node, create a multiplexer between the two possible values coming from the two branches.
    for outputNode in outputNodeSet.items():
      let undefined = RExprRef(
        loc: generatedLoc,
        kind: rexprUndefined,
        undefinedWidth: unit.outputWidths[outputNode],
      )
      let thenExpr = thenContext.getOrDefault(outputNode, undefined)
      let elseExpr = elseContext.getOrDefault(outputNode, undefined)
      if thenExpr == elseExpr:
        context[outputNode] = thenExpr
      else:
        context[outputNode] = RExprRef(
          loc: generatedLoc,
          kind: rexprMux,
          muxCondition: s.ifCondition,
          muxThen: thenExpr,
          muxElse: elseExpr,
        )

proc flattenBranches*(unit: CompilationUnitRef) =
  var context = initOrderedTable[string, RExprRef]()
  for s in unit.stmts:
    walk(s, unit, context)

  unit.stmts = @[]
  for node, e in context.pairs():
    unit.stmts.add(StmtRef(
      loc: generatedLoc,
      kind: stmtAssign,
      source: e,
      dest: LExprRef(
        loc: generatedLoc,
        kind: lexprNodeRef,
        node: node,
      ),
    ))
