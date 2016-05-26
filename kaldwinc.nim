import kaldwin.parse
import kaldwin.stringify
import kaldwin.passes.check
import kaldwin.passes.flattenbranches

let unit = parseStdin()

let messages = check(unit)
if messages.len() > 0:
  for message in messages:
    echo message
  quit(1)

echo $unit
echo()
echo()
echo()

flattenBranches(unit)
echo $unit
echo()
echo()
echo()
