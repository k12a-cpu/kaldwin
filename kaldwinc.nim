import docopt
import kaldwin.parse
import kaldwin.stringify
import kaldwin.passes.check
import kaldwin.passes.flattennodes
import kaldwin.passes.flattenbranches
import kaldwin.passes.optimiselogic
# import kaldwin.generators.attano

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
  -d <stage>, --dump <stage>          Halt and dump intermediate representation
                                      after <stage>, where <stage> is one of
                                      'check', 'fn', 'fb', 'opt1', 'nand',
                                      'opt2' or 'gvn'.
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

let dump = $args["--dump"]

let messages = check(unit)
if messages.len() > 0:
  for message in messages:
    echo message
  quit(1)
if dump == "check":
  echo $unit
  quit(0)

flattenNodes(unit)
if dump == "fn":
  echo $unit
  quit(0)

flattenBranches(unit)
if dump == "fb":
  echo $unit
  quit(0)

optimiseLogic(unit)
if dump == "opt1":
  echo $unit
  quit(0)

# nandify(unit)
# if dump == "nand":
#   echo $unit
#   quit(0)

# optimiseLogic(unit)
# if dump == "opt2":
#   echo $unit
#   quit(0)

# runGVN(unit)
# if dump == "gvn":
#   echo $unit
#   quit(0)

# let output = generateAttano(unit, namespace = $args["--prefix"])

# if args["--output"]:
#   writeFile($args["--output"], output)
# else:
#   echo output
