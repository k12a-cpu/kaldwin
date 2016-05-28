import kaldwin.parse
import kaldwin.stringify
import kaldwin.passes.check
import kaldwin.passes.flattenbranches
import kaldwin.passes.optimiselogic
import kaldwin.passes.nandify
import kaldwin.passes.gvn
import kaldwin.generators.attano

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

optimiseLogic(unit)
echo $unit
echo()
echo()
echo()

nandify(unit)
echo $unit
echo()
echo()
echo()

optimiseLogic(unit)
echo $unit
echo()
echo()
echo()

runGVN(unit)
echo $unit
echo()
echo()
echo()

echo generateAttano(unit)
