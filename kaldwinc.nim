import kaldwin.parse
import kaldwin.passes.check
import marshal

let unit = parseStdin()
# echo $$unit

let messages = check(unit)
if messages.len() > 0:
  for message in messages:
    echo message
else:
  echo "OK"
