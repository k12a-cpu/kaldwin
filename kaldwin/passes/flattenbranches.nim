import sets, tables, ../types

const generatedLoc: Loc = (filename: "<generated in flattenBranches>", lineno: 0)

type
  ContextKey = tuple
    node: string
    bit: int
  
  Context = OrderedTable[ContextKey, RExprRef]

include dereference

proc getOrDefault[A, B](t: OrderedTable[A, B], key: A, defaultValue: B): B =
  try:
    t[key]
  except KeyError:
    defaultValue

proc walk(s: StmtRef, unit: CompilationUnitRef, context: var Context) =
  case s.kind
  of stmtAssign:
    assert s.dest.kind == lexprSlice
    assert s.dest.sliceUpperBound == s.dest.sliceLowerBound
    assert s.dest.sliceChild.kind == lexprNodeRef
    let key = (node: s.dest.sliceChild.node, bit: s.dest.sliceUpperBound)
    context[key] = s.source

  of stmtIf:
    # Evaluate the contexts on each branch of the if statement.
    var thenContext = context
    for child in s.ifThenChildren:
      walk(child, unit, thenContext)
    var elseContext = context
    for child in s.ifElseChildren:
      walk(child, unit, elseContext)

    # Enumerate all keys in the two contexts.
    var keySet: HashSet[ContextKey]
    keySet.init()
    for key in thenContext.keys():
      keySet.incl(key)
    for key in elseContext.keys():
      keySet.incl(key)

    # For each key, create a multiplexer between the two possible values coming from the two branches.
    for key in keySet.items():
      let undefined = RExprRef(
        loc: generatedLoc,
        kind: rexprUndefined,
        undefinedWidth: 1,
      )
      let thenExpr = thenContext.getOrDefault(key, undefined)
      let elseExpr = elseContext.getOrDefault(key, undefined)
      if thenExpr == elseExpr:
        context[key] = thenExpr
      else:
        context[key] = RExprRef(
          loc: generatedLoc,
          kind: rexprMux,
          muxCondition: s.ifCondition,
          muxThen: thenExpr,
          muxElse: elseExpr,
        )

proc flattenBranches*(unit: CompilationUnitRef) =
  var context = initOrderedTable[ContextKey, RExprRef]()
  for s in unit.stmts:
    walk(s, unit, context)

  dereference(context)

  unit.stmts = @[]
  for key, e in context.pairs():
    unit.stmts.add(StmtRef(
      loc: generatedLoc,
      kind: stmtAssign,
      source: e,
      dest: LExprRef(
        loc: generatedLoc,
        kind: lexprSlice,
        sliceUpperBound: key.bit,
        sliceLowerBound: key.bit,
        sliceChild: LExprRef(
          loc: generatedLoc,
          kind: lexprNodeRef,
          node: key.node,
        ),
      ),
    ))
