from strutils import `%`
import tables
import kaldwin.types

type
  Checker[LN, RN] = tuple
    unit: CompilationUnitRef[LN, RN] not nil
    messages: seq[string]

proc error[LN, RN](c: var Checker[LN, RN], loc: Loc, msg: string) =
  c.messages.add("$1: $2" % [$loc, msg])

# Check the given r-expression, and returns the bit-width of the expression.
proc walk[LN, RN](c: var Checker[LN, RN], e: RExprRef[RN]): uint =
  case e.kind
  of rexprNodeRef:
    if e.node in c.unit.inputWidths:
      result = c.unit.inputWidths[e.node]
    else:
      when compiles($e.node):
        c.error(e.loc, "undefined reference to input node '$1'" % [$e.node])
      else:
        c.error(e.loc, "undefined reference to input node")
      result = 1 # fallback
  
  of rexprLiteral:
    # 64 will probably work, but don't want to take the risk
    if e.literalWidth > 63u:
      c.error(e.loc, "literal widths greater than 63 are not supported")
    let max = (1u64 shl e.literalWidth) - 1u64
    if e.literalValue > max:
      c.error(e.loc, "literal value is greater than the largest representable $1-bit value ($2)" % [$e.literalWidth, $max])
    result = e.literalWidth
  
  of rexprNot:
    result = c.walk(e.notChild)
  
  of rexprBinaryOp:
    let leftWidth = c.walk(e.leftChild)
    let rightWidth = c.walk(e.rightChild)
    if leftWidth != rightWidth:
      c.error(e.loc, "left and right operands have different widths ($1 and $2)" % [$leftWidth, $rightWidth])
    result = leftWidth
  
  of rexprConcat:
    for child in e.concatChildren:
      let childWidth = c.walk(child)
      result += childWidth
  
  of rexprMultiply:
    let childWidth = c.walk(e.multiplyChild)
    result = e.multiplyCount * childWidth
  
  of rexprSlice:
    let childWidth = c.walk(e.sliceChild)
    if e.sliceUpperBound >= childWidth:
      c.error(e.loc, "upper bound '$1' out of range (must be in range 0..$2 inclusive)" % [$e.sliceUpperBound, $(childWidth-1u)])
    if e.sliceLowerBound >= childWidth:
      c.error(e.loc, "lower bound '$1' out of range (must be in range 0..$2 inclusive)" % [$e.sliceLowerBound, $(childWidth-1u)])
    if e.sliceUpperBound < e.sliceLowerBound:
      c.error(e.loc, "upper bound '$1' must be greater than or equal to lower bound '$2'" % [$e.sliceUpperBound, $e.sliceLowerBound])
    result = e.sliceUpperBound - e.sliceLowerBound + 1u
  
  assert result != 0

# Check the given l-expression, and returns the bit-width of the expression.
proc walk[LN, RN](c: var Checker[LN, RN], e: LExprRef[LN]): uint =
  case e.kind
  of lexprNodeRef:
    if e.node in c.unit.outputWidths:
      result = c.unit.outputWidths[e.node]
    else:
      when compiles($e.node):
        c.error(e.loc, "undefined reference to output node '$1'" % [$e.node])
      else:
        c.error(e.loc, "undefined reference to output node")
  
  of lexprConcat:
    for child in e.concatChildren:
      let childWidth = c.walk(child)
      result += childWidth
  
  of lexprSlice:
    let childWidth = c.walk(e.sliceChild)
    if e.sliceUpperBound >= childWidth:
      c.error(e.loc, "upper bound '$1' out of range (must be in range 0..$2 inclusive)" % [$e.sliceUpperBound, $(childWidth-1u)])
    if e.sliceLowerBound >= childWidth:
      c.error(e.loc, "lower bound '$1' out of range (must be in range 0..$2 inclusive)" % [$e.sliceLowerBound, $(childWidth-1u)])
    if e.sliceUpperBound < e.sliceLowerBound:
      c.error(e.loc, "upper bound '$1' must be greater than or equal to lower bound '$2'" % [$e.sliceUpperBound, $e.sliceLowerBound])
    result = e.sliceUpperBound - e.sliceLowerBound + 1u
  
  assert result != 0

proc walk[LN, RN](c: var Checker[LN, RN], s: StmtRef[LN, RN]) =
  case s.kind
  of stmtAssign:
    let destWidth = c.walk(s.dest)
    let sourceWidth = c.walk(s.source)
    if destWidth != sourceWidth:
      c.error(s.dest.loc, "source and destination have different bit widths ($1 and $2 respectively)" % [$sourceWidth, $destWidth])
  of stmtIf:
    let condWidth = c.walk(s.ifCondition)
    if condWidth != 1:
      c.error(s.ifCondition.loc, "if-statement condition does not have a bit width of 1")
    for child in s.ifThenChildren:
      c.walk(child)
    for child in s.ifElseChildren:
      c.walk(child)

proc walk[LN, RN](c: var Checker[LN, RN]) =
  for s in c.unit.stmts:
    c.walk(s)

proc check*[LN, RN](unit: CompilationUnitRef[LN, RN]): seq[string] =
  var c: Checker[LN, RN] = (
    unit: unit,
    messages: @[],
  )
  c.walk()
  result = c.messages
