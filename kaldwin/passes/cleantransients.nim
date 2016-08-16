proc cleanTransients(context: var Context, unit: CompilationUnitRef) =
  for key in context.keys:
    if unit.nodes[key.node].transient:
      context.del(key)
  for name, node in unit.nodes.pairs:
    if node.transient:
      unit.nodes.del(name)
    
