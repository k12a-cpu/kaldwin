import hashes, sets, tables, ../types
from algorithm import sort
from sequtils import filterIt
from strutils import startsWith

const generatedNodePrefix = "__auto_"

type
  Node = object
    name: string
    bit: int
  
  WireKind = enum
    wireNode
    wireLiteral
  
  Wire = object
    case kind: WireKind
    of wireNode:
      pos, neg: Node
    of wireLiteral:
      value: range[0..1]
  
  GateKind = enum
    gateNot
    gateAnd
    gateOr
    gateXor
    gateNand
    gateNor
  
  Gate = object
    case kind: GateKind
    of gateNot:
      input: Wire
    of gateAnd, gateOr, gateXor, gateNand, gateNor:
      input1, input2: Wire
  
  Mapper = object
    availableGates: set[GateKind]
    gates: seq[tuple[gate: Gate, output: Node]]
    byGate: Table[Gate, int]
    assignments: seq[tuple[dest, source: Node]]
    constAssignments: seq[tuple[dest: Node, value: range[0..1]]]
    nextGeneratedNode: int
  
  ImplementationError = object of Exception

const allGates: set[GateKind] = {gateNot, gateAnd, gateOr, gateXor, gateNand, gateNor}

proc `==`(node1, node2: Node): bool {.noSideEffect.} =
  node1.bit == node2.bit and node1.name == node2.name

proc `==`(wire1, wire2: Wire): bool {.noSideEffect.} =
  if wire1.kind != wire2.kind:
    return false
  case wire1.kind
  of wireNode:
    result = wire1.pos == wire2.pos and wire1.neg == wire2.neg
  of wireLiteral:
    result = wire1.value == wire2.value

proc `==`(gate1, gate2: Gate): bool {.noSideEffect.} =
  if gate1.kind != gate2.kind:
    return false
  case gate1.kind
  of gateNot:
    result = gate1.input == gate2.input
  of gateAnd, gateOr, gateXor, gateNand, gateNor:
    result = gate1.input1 == gate2.input1 and gate1.input2 == gate2.input2

proc hash(node: Node): Hash {.noSideEffect.} =
  result = result !& hash(node.name)
  result = result !& hash(node.bit)
  result = !$result

proc hash(wire: Wire): Hash {.noSideEffect.} =
  result = result !& hash(wire.kind)
  case wire.kind
  of wireNode:
    result = result !& hash(wire.pos)
    result = result !& hash(wire.neg)
  of wireLiteral:
    result = result !& hash(wire.value)
  result = !$result

proc hash(gate: Gate): Hash {.noSideEffect.} =
  result = result !& hash(gate.kind)
  case gate.kind
  of gateNot:
    result = result !& hash(gate.input)
  of gateAnd, gateOr, gateXor, gateNand, gateNor:
    result = result !& hash(gate.input1)
    result = result !& hash(gate.input2)
  result = !$result

template have(requiredGates: set[GateKind]): bool =
  m.availableGates * requiredGates == requiredGates

proc isNil(x: Node): bool =
  x.name.isNil

proc init(m: var Mapper, availableGates: set[GateKind] = allGates) =
  m.availableGates = availableGates
  m.gates = @[]
  m.byGate = initTable[Gate, int]()
  m.assignments = @[]
  m.constAssignments = @[]
  m.nextGeneratedNode = 1

proc createNode(m: var Mapper): Node =
  let name = generatedNodePrefix & $m.nextGeneratedNode
  inc m.nextGeneratedNode
  result = Node(name: name, bit: 0)

proc invert(w: Wire): Wire =
  case w.kind
  of wireNode:
    result = Wire(kind: wireNode, pos: w.neg, neg: w.pos)
  of wireLiteral:
    result = Wire(kind: wireLiteral, value: 1 - w.value)

proc buildGate(m: var Mapper, gate: Gate): Wire

proc normaliseIP(m: var Mapper, w: var Wire) =
  if w.kind == wireNode and w.pos.isNil:
    let w1 = invert(w)
    let w2 = m.buildGate(Gate(kind: gateNot, input: w1))
    w.pos = w2.pos
  assert w.kind == wireLiteral or (not w.pos.isNil)

proc normalise(m: var Mapper, w: Wire): Wire =
  result = w
  m.normaliseIP(result)

proc addGate(m: var Mapper, gate: Gate, output: Node) =
  m.gates.add((gate: gate, output: output))
  let gateNum = m.gates.high
  m.byGate[gate] = gateNum

proc buildGate(m: var Mapper, gate: Gate): Wire =
  assert gate.kind in m.availableGates
  var normalisedGate = gate
  case normalisedGate.kind
  of gateNot:
    assert normalisedGate.input.kind == wireLiteral or (not normalisedGate.input.pos.isNil)
  of gateAnd, gateOr, gateXor, gateNand, gateNor:
    m.normaliseIP(normalisedGate.input1)
    m.normaliseIP(normalisedGate.input2)
  var outputNode: Node
  try:
    let gateNum = m.byGate[normalisedGate]
    outputNode = m.gates[gateNum].output
  except KeyError:
    outputNode = m.createNode()
    m.addGate(normalisedGate, outputNode)
  result = Wire(kind: wireNode, pos: outputNode, neg: Node())

proc buildAnd(m: var Mapper, input1, input2: Wire): Wire =
  result = m.buildGate(Gate(kind: gateAnd, input1: input1, input2: input2))

proc buildOr(m: var Mapper, input1, input2: Wire): Wire =
  result = m.buildGate(Gate(kind: gateOr, input1: input1, input2: input2))

proc buildXor(m: var Mapper, input1, input2: Wire): Wire =
  result = m.buildGate(Gate(kind: gateXor, input1: input1, input2: input2))

proc buildNand(m: var Mapper, input1, input2: Wire): Wire =
  result = m.buildGate(Gate(kind: gateNand, input1: input1, input2: input2))

proc buildNor(m: var Mapper, input1, input2: Wire): Wire =
  result = m.buildGate(Gate(kind: gateNor, input1: input1, input2: input2))

proc implementAnd(m: var Mapper, w1, w2: Wire): Wire =
  if have({gateAnd}):
    result = m.buildAnd(w1, w2)
  elif have({gateNand}):
    result = invert(m.buildNand(w1, w2))
  elif have({gateNor}):
    result = m.buildNor(invert(w1), invert(w2))
  elif have({gateOr}):
    result = invert(m.buildOr(invert(w1), invert(w2)))
  else:
    raise newException(ImplementationError, "unable to implement AND operation")

proc implementOr(m: var Mapper, w1, w2: Wire): Wire =
  if have({gateOr}):
    result = m.buildOr(w1, w2)
  elif have({gateNor}):
    result = invert(m.buildNor(w1, w2))
  elif have({gateNand}):
    result = m.buildNand(invert(w1), invert(w2))
  elif have({gateAnd}):
    result = invert(m.buildAnd(invert(w1), invert(w2)))
  else:
    raise newException(ImplementationError, "unable to implement OR operation")

proc implementXor(m: var Mapper, w1, w2: Wire): Wire =
  if have({gateXor}):
    result = m.buildXor(w1, w2)
  elif have({gateNand}):
    let x = m.buildNand(w1, w2)
    result = m.buildNand(m.buildNand(w1, x), m.buildNand(x, w2))
  else:
    raise newException(ImplementationError, "unable to implement XOR operation")

proc implementNand(m: var Mapper, w1, w2: Wire): Wire =
  if have({gateNand}):
    result = m.buildNand(w1, w2)
  elif have({gateAnd}):
    result = invert(m.buildAnd(w1, w2))
  elif have({gateOr}):
    result = m.buildOr(invert(w1), invert(w2))
  else:
    raise newException(ImplementationError, "unable to implement NAND operation")

proc implementNor(m: var Mapper, w1, w2: Wire): Wire =
  if have({gateNor}):
    result = m.buildNor(w1, w2)
  elif have({gateOr}):
    result = invert(m.buildOr(w1, w2))
  elif have({gateAnd}):
    result = m.buildAnd(invert(w1), invert(w2))
  else:
    raise newException(ImplementationError, "unable to implement NOR operation")

proc implementMux(m: var Mapper, cond, then, els: Wire): Wire =
  result = m.implementOr(m.implementAnd(cond, then), m.implementAnd(invert(cond), els))

proc walk(m: var Mapper, e: RExprRef): Wire =
  case e.kind
  of rexprNodeRef:
    assert false
  of rexprLiteral:
    assert e.literalWidth == 1
    result = Wire(kind: wireLiteral, value: e.literalValue)
  of rexprUndefined:
    assert false
  of rexprNot:
    if e.notChild.kind == rexprBinaryOp and e.notChild.op in {binaryOpAnd, binaryOpOr}:
      if e.notChild.op == binaryOpAnd:
        result = m.implementNand(m.walk(e.notChild.leftChild), m.walk(e.notChild.rightChild))
      else:
        result = m.implementNor(m.walk(e.notChild.leftChild), m.walk(e.notChild.rightChild))
    else:
      result = invert(m.walk(e.notChild))
  of rexprBinaryOp:
    case e.op
    of binaryOpAnd:
      result = m.implementAnd(m.walk(e.leftChild), m.walk(e.rightChild))
    of binaryOpOr:
      result = m.implementOr(m.walk(e.leftChild), m.walk(e.rightChild))
    of binaryOpXor:
      result = m.implementXor(m.walk(e.leftChild), m.walk(e.rightChild))
    of binaryOpEq, binaryOpNe:
      assert false
  of rexprMux:
    result = m.implementMux(m.walk(e.muxCondition), m.walk(e.muxThen), m.walk(e.muxElse))
  of rexprConcat:
    assert false
  of rexprMultiply:
    assert false
  of rexprSlice:
    assert e.sliceChild.kind == rexprNodeRef
    assert e.sliceUpperBound == e.sliceLowerBound
    let node = Node(name: e.sliceChild.node, bit: e.sliceUpperBound)
    result = Wire(kind: wireNode, pos: node, neg: Node())

proc map(m: var Mapper, s: StmtRef) =
  case s.kind
  of stmtAssign:
    assert s.dest.kind == lexprSlice
    assert s.dest.sliceChild.kind == lexprNodeRef
    assert s.dest.sliceUpperBound == s.dest.sliceLowerBound
    let destNode = Node(name: s.dest.sliceChild.node, bit: s.dest.sliceUpperBound)
    let sourceWire = m.normalise(m.walk(s.source))
    case sourceWire.kind
    of wireNode:
      m.assignments.add((dest: destNode, source: sourceWire.pos))
    of wireLiteral:
      m.constAssignments.add((dest: destNode, value: sourceWire.value))
  of stmtIf, stmtSwitch:
    assert false

proc map(m: var Mapper, unit: CompilationUnitRef) =
  for s in unit.stmts:
    m.map(s)

proc reorderTransients(m: var Mapper, nodeInfos: Table[string, NodeInfo]) =
  proc isTransient(node: Node): bool =
    try:
      nodeInfos[node.name].transient
    except KeyError:
      true # generated nodes are transient
  
  ## Move assignments to transient nodes to the end of the list, so that
  ## non-transient nodes will take precedence in the dealias stage.
  m.assignments.sort do (x, y: tuple[dest, source: Node]) -> int:
    result = cmp(x.dest.isTransient, y.dest.isTransient)

proc isGenerated(node: Node): bool =
  node.name.startsWith(generatedNodePrefix)

iterator revItems[T](arr: openarray[T]): T =
  for i in countdown(arr.high, arr.low):
    yield arr[i]

proc dealias(m: var Mapper) =
  var replacements = initTable[Node, Node]()
  var indicesToDelete = newSeq[int]()
  for i, assignment in m.assignments.mpairs:
    if assignment.source.isGenerated():
      try:
        assignment.source = replacements[assignment.source]
      except KeyError:
        replacements[assignment.source] = assignment.dest
        indicesToDelete.add(i)
  for i in indicesToDelete.revItems:
    m.assignments.del(i)
  
  template replace(node: var Node) =
    try:
      node = replacements[node]
    except KeyError:
      discard
  
  template replace(wire: var Wire) =
    if wire.kind == wireNode:
      assert(not wire.pos.isNil)
      replace(wire.pos)
  
  for t in m.gates.mitems:
    replace(t.output)
    case t.gate.kind
    of gateNot:
      replace(t.gate.input)
    of gateAnd, gateOr, gateXor, gateNand, gateNor:
      replace(t.gate.input1)
      replace(t.gate.input2)
  for t in m.assignments.mitems:
    replace(t.dest)
    replace(t.source)
  for t in m.constAssignments.mitems:
    replace(t.dest)

proc prune(m: var Mapper, nodeInfos: Table[string, NodeInfo]) =
  var requiredNodes = initSet[Node]()
  
  proc isNotTransient(node: Node): bool =
    try:
      not nodeInfos[node.name].transient
    except KeyError:
      false # generated nodes are transient
  
  proc require(node: Node) =
    requiredNodes.incl(node)
  
  proc require(wire: Wire) =
    if wire.kind == wireNode:
      assert(not wire.pos.isNil)
      require(wire.pos)
  
  proc require(gate: Gate) =
    case gate.kind
    of gateNot:
      require(gate.input)
    of gateAnd, gateOr, gateXor, gateNand, gateNor:
      require(gate.input1)
      require(gate.input2)
  
  for t in m.gates:
    if t.output.isNotTransient:
      requiredNodes.incl(t.output)
  for t in m.assignments:
    if t.dest.isNotTransient:
      requiredNodes.incl(t.dest)
  for t in m.constAssignments:
    if t.dest.isNotTransient:
      requiredNodes.incl(t.dest)
  
  var x = 0
  var y = requiredNodes.card
  
  while x != y:
    for t in m.gates:
      if t.output in requiredNodes:
        require(t.gate)
    for t in m.assignments:
      if t.dest in requiredNodes:
        require(t.source)
    
    x = y
    y = requiredNodes.card
  
  m.gates = filterIt(m.gates, it.output in requiredNodes)
  m.assignments = filterIt(m.assignments, it.dest in requiredNodes)
  m.constAssignments = filterIt(m.constAssignments, it.dest in requiredNodes)

proc map*(unit: CompilationUnitRef) =
  var m: Mapper
  m.init()
  m.map(unit)
  m.reorderTransients(unit.nodes)
  m.dealias()
  m.prune(unit.nodes)
  
  proc `$`(node: Node): string =
    node.name & "[" & $node.bit & "]"
  proc `$`(wire: Wire): string =
    case wire.kind
    of wireNode:
      assert(not wire.pos.isNil)
      result = $wire.pos
    of wireLiteral:
      result = $wire.value
  proc `$`(gate: Gate): string =
    case gate.kind
    of gateNot:
      result = "NOT " & $gate.input
    of gateAnd:
      result = $gate.input1 & " AND " & $gate.input2
    of gateOr:
      result = $gate.input1 & " OR " & $gate.input2
    of gateXor:
      result = $gate.input1 & " XOR " & $gate.input2
    of gateNand:
      result = $gate.input1 & " NAND " & $gate.input2
    of gateNor:
      result = $gate.input1 & " NOR " & $gate.input2
  for t in m.gates:
    echo $t.output & " = " & $t.gate
  for t in m.assignments:
    echo $t.dest & " = " & $t.source
  for t in m.constAssignments:
    echo $t.dest & " = " & $t.value
  # for aliasSet in m.aliasGroups:
  #   var s = "alias:"
  #   for node in aliasSet:
  #     s.add(" ")
  #     s.add($node)
  #   echo s
