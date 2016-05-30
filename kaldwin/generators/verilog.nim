from tables import pairs
import ropes
import kaldwin.types

proc rope(e: LExprRef): Rope =
  case e.kind
  of lexprNodeRef:
    result = rope(e.node)
  of lexprConcat:
    assert(false, "lexprConcat should not be present at this stage")
  of lexprSlice:
    assert(false, "lexprSlice should not be present at this stage")

proc rope(e: RExprRef): Rope =
  case e.kind
  of rexprNodeRef:
    result = rope(e.node)
  of rexprLiteral:
    result = &[
      rope(e.literalWidth),
      rope("'d"),
      rope(e.literalValue),
    ]
  of rexprUndefined:
    result = &[rope(e.undefinedWidth), rope("'b")]
    for i in 1u .. e.undefinedWidth:
      result = result & rope("x")
  of rexprNot:
    result = &[rope("~"), rope(e.notChild)]
  of rexprBinaryOp:
    if e.op == binaryOpNand:
      result = &[rope("~("), rope(e.leftChild), rope(" & "), rope(e.rightChild), rope(")")]
    else:
      assert(false, "only NAND binaryOps should be present at this stage")
  of rexprMux:
    assert(false, "rexprMux should not be present at this stage")
  of rexprConcat:
    assert(false, "rexprConcat should not be present at this stage")
  of rexprMultiply:
    assert(false, "rexprMultiply should not be present at this stage")
  of rexprSlice:
    assert(false, "rexprSlice should not be present at this stage")

proc rope(s: StmtRef): Rope =
  case s.kind
  of stmtAssign:
    result = &[rope("assign "), rope(s.dest), rope(" = "), rope(s.source), rope(";\n")]
  of stmtIf:
    assert(false, "stmtIf should not be present at this stage")

proc rope(unit: CompilationUnitRef, moduleName: string = "kaldwin_out"): Rope =
  result = &[rope("module "), rope(moduleName)]

  var portLines: seq[Rope] = @[]
  for node, width in unit.inputWidths.pairs():
    var portLine = rope("    input logic ")
    if width != 1:
      portLine = &[portLine, rope("["), rope(width-1), rope(":0] ")]
    portLine = &[portLine, rope(node)]
    portLines.add(portLine)
  for node, width in unit.outputWidths.pairs():
    var portLine = rope("    output logic ")
    if width != 1:
      portLine = &[portLine, rope("["), rope(width-1), rope(":0] ")]
    portLine = &[portLine, rope(node)]
    portLines.add(portLine)

  if len(portLines) > 0:
    result = &[result, rope("(\n")]
    for portLine in portLines[0 .. len(portLines)-2]:
      result = &[result, portLine, rope(",\n")]
    result = &[result, portLines[len(portLines)-1], rope("\n)")]

  result = &[result, rope(";\n\n")]

  for node, width in unit.intermediateWidths.pairs():
    result = &[result, rope("logic ")]
    if width != 1:
      result = &[result, rope("["), rope(width-1), rope(":0] ")]
    result = &[result, rope(node), rope(";\n")]

  for s in unit.stmts:
    result = &[result, rope(s)]

  result = &[result, rope("\nendmodule\n")]

proc generateVerilog*(unit: CompilationUnitRef, moduleName: string = "kaldwin_out"): string =
  $rope(unit, moduleName)
