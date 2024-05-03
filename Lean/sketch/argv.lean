-- An entry script should not be defined in a namespace

def main (args : List String) : IO UInt32 := do
  let self <- IO.appPath

  IO.println s!"running: {self}"
  IO.println s!"  received {args.length} argument{if args.length == 1 then "" else "s"} from user:"

  for arg in args,
      idx in List.range args.length
  do
    IO.println s!"    args[{idx + 1}] = {arg}"

  return 0
