import kaldwin.parse
import kaldwin.stringify
import kaldwin.passes.check

let unit = parseStdin()
echo $unit
echo()
echo()

let messages = check(unit)
if messages.len() > 0:
  for message in messages:
    echo message
else:
  echo "OK"
