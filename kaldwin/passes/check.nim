from strutils import `%`
import tables
import kaldwin.types

type
  Checker[N] = tuple
    unit: CompilationUnitRef[N]
    messages: seq[string]

proc error[N](c: var Checker[N], loc: Loc, msg: string) {.noSideEffect.} =
  c.messages.add("$1: $2" % [$loc, msg])

# Check the given r-expression, and returns the bit-width of the expression.
proc walk[N](c: var Checker[N], e: RExprRef[N]): int =
  case e.kind
  of rexprNodeRef:
    if e.node in c.unit.inputWidths:
      result = c.unit.inputWidths[e.node]
    elif e.node in c.unit.intermediateWidths:
      result = c.unit.intermediateWidths[e.node]
    elif e.node in c.unit.outputWidths:
      result = c.unit.outputWidths[e.node]
    else:
      when compiles($e.node):
        c.error(e.loc, "undefined reference to node '$1'" % [$e.node])
      else:
        c.error(e.loc, "undefined reference to node")
      result = 1 # fallback

  of rexprLiteral:
    # 64 will probably work, but don't want to take the risk
    if e.literalWidth > 63:
      c.error(e.loc, "literal widths greater than 63 are not supported")
    let max = (1 shl e.literalWidth) - 1
    if e.literalValue > max:
      c.error(e.loc, "literal value is greater than the largest representable $1-bit value ($2)" % [$e.literalWidth, $max])
    result = e.literalWidth

  of rexprUndefined:
    result = e.undefinedWidth

  of rexprNot:
    result = c.walk(e.notChild)

  of rexprBinaryOp:
    let leftWidth = c.walk(e.leftChild)
    let rightWidth = c.walk(e.rightChild)
    if leftWidth != rightWidth:
      c.error(e.loc, "left and right operands have different widths ($1 and $2)" % [$leftWidth, $rightWidth])
    result = leftWidth

  of rexprMux:
    let condWidth = c.walk(e.muxCondition)
    if condWidth != 1:
      c.error(e.muxCondition.loc, "mux condition does not have a bit width of 1")
    let thenWidth = c.walk(e.muxThen)
    let elseWidth = c.walk(e.muxElse)
    if thenWidth != elseWidth:
      c.error(e.muxThen.loc, "mux branches have different widths ($1 and $2)" % [$thenWidth, $elseWidth])
    result = thenWidth

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
      c.error(e.loc, "upper bound '$1' out of range (must be in range 0..$2 inclusive)" % [$e.sliceUpperBound, $(childWidth-1)])
    if e.sliceLowerBound >= childWidth:
      c.error(e.loc, "lower bound '$1' out of range (must be in range 0..$2 inclusive)" % [$e.sliceLowerBound, $(childWidth-1)])
    if e.sliceUpperBound < e.sliceLowerBound:
      c.error(e.loc, "upper bound '$1' must be greater than or equal to lower bound '$2'" % [$e.sliceUpperBound, $e.sliceLowerBound])
    result = e.sliceUpperBound - e.sliceLowerBound + 1

  assert result > 0

# Check the given l-expression, and returns the bit-width of the expression.
proc walk[N](c: var Checker[N], e: LExprRef[N]): int =
  case e.kind
  of lexprNodeRef:
    if e.node in c.unit.intermediateWidths:
      result = c.unit.intermediateWidths[e.node]
    elif e.node in c.unit.outputWidths:
      result = c.unit.outputWidths[e.node]
    else:
      when compiles($e.node):
        c.error(e.loc, "undefined reference to node '$1'" % [$e.node])
      else:
        c.error(e.loc, "undefined reference to node")

  of lexprConcat:
    for child in e.concatChildren:
      let childWidth = c.walk(child)
      result += childWidth

  of lexprSlice:
    let childWidth = c.walk(e.sliceChild)
    if e.sliceUpperBound >= childWidth:
      c.error(e.loc, "upper bound '$1' out of range (must be in range 0..$2 inclusive)" % [$e.sliceUpperBound, $(childWidth-1)])
    if e.sliceLowerBound >= childWidth:
      c.error(e.loc, "lower bound '$1' out of range (must be in range 0..$2 inclusive)" % [$e.sliceLowerBound, $(childWidth-1)])
    if e.sliceUpperBound < e.sliceLowerBound:
      c.error(e.loc, "upper bound '$1' must be greater than or equal to lower bound '$2'" % [$e.sliceUpperBound, $e.sliceLowerBound])
    result = e.sliceUpperBound - e.sliceLowerBound + 1

  assert result > 0

proc walk[N](c: var Checker[N], s: StmtRef[N]) =
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

proc walk[N](c: var Checker[N]) =
  for s in c.unit.stmts:
    c.walk(s)

proc check*[N](unit: CompilationUnitRef[N]): seq[string] =
  var c: Checker[N] = (
    unit: unit,
    messages: @[],
  )
  c.walk()
  result = c.messages
