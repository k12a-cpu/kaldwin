import docopt
import kaldwin.parse
import kaldwin.stringify
import kaldwin.passes.check
import kaldwin.passes.flattenbranches
import kaldwin.passes.optimiselogic
import kaldwin.passes.nandify
import kaldwin.passes.gvn
import kaldwin.generators.attano

const doc = """
kaldwinc - Compiler for the Kaldwin logic specification language.

Usage:
  kaldwinc [options] [<infile>]

Options:
  -h, --help                          Print this help text.
  -o <outfile>, --output <outfile>    Write output to <outfile>, instead of to
                                      standard output.
  -p <prefix>, --prefix <prefix>      Use <prefix> as the prefix for
                                      automatically generated gates and nodes.
                                      [default: kaldwin_out]
"""

let args = docopt(doc)

let unit =
  try:
    if args["<infile>"]:
      parseFile($args["<infile>"])
    else:
      parseStdin()
  except ParseError:
    let msg = getCurrentExceptionMsg()
    echo msg
    quit(1)
    nil # the compiler requires that all branches return a value, even though this line is unreachable.

let messages = check(unit)
if messages.len() > 0:
  for message in messages:
    echo message
  quit(1)

flattenBranches(unit)
optimiseLogic(unit)
nandify(unit)
optimiseLogic(unit)
runGVN(unit)

let output = generateAttano(unit, namespace = $args["--prefix"])

if args["--output"]:
  writeFile($args["--output"], output)
else:
  echo output
