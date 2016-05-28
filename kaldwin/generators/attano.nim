from strutils import startsWith
from tables import keys
import ropes
import kaldwin.types

type
  Wire = string
  Literal = bool
  WireOrLiteral = object
    case isLiteral: bool
    of false:
      wire: Wire
    of true:
      literal: Literal
  
  Buffer = tuple
    a: WireOrLiteral
    q: Wire
  
  Nand = tuple
    a, b: WireOrLiteral
    q: Wire
  
  Generator = tuple
    intermediates: seq[Wire]
    buffers: seq[Buffer]
    nands: seq[Nand]

iterator chunks[T](sequence: seq[T], chunkSize: int): seq[T] =
  var start = 0
  var finish = chunkSize - 1
  while finish < sequence.len():
    yield sequence[start .. finish]
    start.inc(chunkSize)
    finish.inc(chunkSize)
  if start < sequence.len():
    yield sequence[start .. sequence.len() - 1]

converter toWireOrLiteral(w: Wire): WireOrLiteral =
  WireOrLiteral(isLiteral: false, wire: w)

converter toWireOrLiteral(lit: Literal): WireOrLiteral =
  WireOrLiteral(isLiteral: true, literal: lit)

proc addBuffer(g: var Generator, q: Wire, a: WireOrLiteral) =
  g.buffers.add((a: a, q: q))

proc addNand(g: var Generator, q: Wire, a, b: WireOrLiteral) =
  g.nands.add((a: a, b: b, q: q))

proc reduce[N](g: var Generator, s: StmtRef[N]) =
  assert(s.kind == stmtAssign, "only stmtAssign should be present at this stage")
  assert(s.dest.kind == lexprNodeRef, "only lexprNodeRef should be present at this stage")
  
  case s.source.kind
  of rexprNodeRef:
    g.addBuffer(s.dest.node, s.source.node.toWireOrLiteral())
  
  of rexprLiteral:
    assert(s.source.literalWidth == 1, "only literals of width 1 should be present at this stage")
    if s.source.literalValue == 0:
      g.addBuffer(s.dest.node, false.toWireOrLiteral())
    elif s.source.literalValue == 1:
      g.addBuffer(s.dest.node, true.toWireOrLiteral())
    else:
      assert(false, "literals should only have values of 0 or 1 at this stage")
  
  of rexprUndefined:
    assert(false, "rexprUndefined should not be present at this stage")
  
  of rexprNot:
    assert(s.source.notChild.kind == rexprNodeRef, "only rexprNodeRef should be present as the child of a rexprNot at this stage")
    g.addNand(s.dest.node, s.source.notChild.node.toWireOrLiteral(), true.toWireOrLiteral())
  
  of rexprBinaryOp:
    assert(s.source.op == binaryOpNand, "only binaryOpNand should be present at this stage")
    assert(s.source.leftChild.kind == rexprNodeRef, "only rexprNodeRef should be present as the child of a rexprNand at this stage")
    assert(s.source.rightChild.kind == rexprNodeRef, "only rexprNodeRef should be present as the child of a rexprNand at this stage")
    g.addNand(s.dest.node, s.source.leftChild.node.toWireOrLiteral(), s.source.rightChild.node.toWireOrLiteral())
  
  of rexprMux:
    assert(false, "rexprMux should not be present at this stage")
  
  of rexprConcat:
    assert(false, "rexprConcat should not be present at this stage")
  
  of rexprMultiply:
    assert(false, "rexprMultiply should not be present at this stage")
  
  of rexprSlice:
    assert(false, "rexprSlice should not be present at this stage")

proc reduce[N](g: var Generator, unit: CompilationUnitRef[N]) =
  for node in unit.intermediateWidths.keys():
    g.intermediates.add(node)
  for s in unit.stmts:
    g.reduce(s)

proc rope(wol: WireOrLiteral, namespace: Rope): Rope =
  if wol.isLiteral:
    if wol.literal:
      rope("1'h1")
    else:
      rope("1'h0")
  else:
    if wol.wire.startsWith("·"):
      # internally generated node; prepend with namespace
      &[namespace, rope("·generatednode"), rope(wol.wire)]
    else:
      rope(wol.wire)

proc rope(intermediates: seq[Wire], namespace: Rope): Rope =
  for node in intermediates:
    result = &[
      result,
      rope("node "),
      rope(node, namespace),
      rope(" : bit;\n"),
    ]

proc rope(buffers: seq[Buffer], namespace: Rope): Rope =
  for buffer in buffers:
    result = &[
      result,
      rope("alias "),
      rope(buffer.q, namespace),
      rope(" = "),
      rope(buffer.a, namespace),
      rope(";\n")
    ]

proc rope(nands: seq[Nand], namespace: Rope): Rope =
  var i = 0
  for group in nands.chunks(4):
    result = &[
      result,
      rope("create "),
      namespace,
      rope("·nand"),
      rope(i),
      rope(" : NAND[4] (\n    in0 => {"),
      rope(group[0].a, namespace),
    ]
    for nand in group[1 .. len(group) - 1]:
      result = &[result, rope(", "), rope(nand.a, namespace)]
    result = &[result, rope("},\n    in1 => {"), rope(group[0].b, namespace)]
    for nand in group[1 .. len(group) - 1]:
      result = &[result, rope(", "), rope(nand.b, namespace)]
    result = &[result, rope("},\n    out => {"), rope(group[0].q, namespace)]
    for nand in group[1 .. len(group) - 1]:
      result = &[result, rope(", "), rope(nand.q, namespace)]
    result = &[result, rope("},\n)\n")]
    inc i

proc rope(g: Generator, namespace: Rope): Rope =
  &[rope(g.intermediates, namespace), rope(g.buffers, namespace), rope(g.nands, namespace)]

proc generateAttano*[N](unit: CompilationUnitRef[N], namespace: string = "kaldwin_out"): string =
  var g: Generator = (intermediates: @[], buffers: @[], nands: @[])
  g.reduce(unit)
  result = $rope(g, rope(namespace))
