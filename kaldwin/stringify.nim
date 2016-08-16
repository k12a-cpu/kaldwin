import ropes
from tables import len, values

import types

proc rope*(loc: Loc): Rope =
  rope($loc)

proc rope*(op: BinaryOp): Rope =
  case op
  of binaryOpAnd:
    rope("&")
  of binaryOpOr:
    rope("|")
  of binaryOpXor:
    rope("^")
  of binaryOpEq:
    rope("==")
  of binaryOpNe:
    rope("!=")

proc rope*(e: LExprRef): Rope =
  case e.kind
  of lexprNodeRef:
    result = rope(e.node)
  of lexprConcat:
    result = rope("{")
    if e.concatChildren.len() > 0:
      result = result & rope(e.concatChildren[0])
      for child in e.concatChildren[1 .. len(e.concatChildren)-1]:
        result = result & rope(", ") & rope(child)
    result = result & rope("}")
  of lexprSlice:
    result = &[
      rope(e.sliceChild),
      rope("["),
      rope(e.sliceUpperBound),
      rope(":"),
      rope(e.sliceLowerBound),
      rope("]"),
    ]

proc rope*(e: RExprRef): Rope =
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
    result = &[
      rope(e.undefinedWidth),
      rope("'u"),
    ]
  of rexprNot:
    result = &[
      rope("~"),
      rope(e.notChild),
    ]
  of rexprBinaryOp:
    result = &[
      rope("("),
      rope(e.leftChild),
      rope(" "),
      rope(e.op),
      rope(" "),
      rope(e.rightChild),
      rope(")"),
    ]
  of rexprMux:
    result = &[
      rope("("),
      rope(e.muxCondition),
      rope(" ? "),
      rope(e.muxThen),
      rope(" : "),
      rope(e.muxElse),
      rope(")"),
    ]
  of rexprConcat:
    result = rope("{")
    if e.concatChildren.len() > 0:
      result = result & rope(e.concatChildren[0])
      for child in e.concatChildren[1 .. len(e.concatChildren)-1]:
        result = result & rope(", ") & rope(child)
    result = result & rope("}")
  of rexprMultiply:
    result = &[
      rope("{"),
      rope(e.multiplyCount),
      rope(" x "),
      rope(e.multiplyChild),
      rope("}"),
    ]
  of rexprSlice:
    result = &[
      rope(e.sliceChild),
      rope("["),
      rope(e.sliceUpperBound),
      rope(":"),
      rope(e.sliceLowerBound),
      rope("]"),
    ]

proc rope*(s: StmtRef, indent: Rope = nil): Rope =
  case s.kind
  of stmtAssign:
    result = &[
      indent,
      rope(s.dest),
      rope(" = "),
      rope(s.source),
      rope(";\n"),
    ]
  of stmtIf:
    result = &[indent, rope("if "), rope(s.ifCondition), rope(" {\n")]
    let newIndent = indent & rope("  ")
    for child in s.ifThenChildren:
      result = result & rope(child, newIndent)
    result = &[result, indent, rope("}")]
    if len(s.ifElseChildren) > 0:
      result = result & rope(" else {\n")
      for child in s.ifElseChildren:
        result = result & rope(child, newIndent)
      result = &[result, indent, rope("}\n")]
    else:
      result = &[result, rope("\n")]

proc rope*(unit: CompilationUnitRef): Rope =
  for node in unit.nodes.values:
    if node.extern:
      result = &[result, rope("extern ")]
    if node.transient:
      result = &[result, rope("transient ")]
    result = &[result, rope("node "), rope(node.name)]
    if node.width != 1:
      result = &[result, rope("["), rope(node.width), rope("]")]
    result = &[result, rope(";\n")]
  if len(unit.nodes) > 0:
    result = result & rope("\n")

  for s in unit.stmts:
    result = result & rope(s)

proc `$`*(unit: CompilationUnitRef): string =
  $rope(unit)
