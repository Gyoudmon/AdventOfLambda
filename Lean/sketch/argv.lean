import Lean

/-- argv
  echo the command line arguments,
    as well as the name of current running script
    just as `argv[0]` in C and alike.
-/

---------------------------------------------------------------
syntax "module_name" : term
elab_rules : term
 | `(module_name) => do
    return Lean.mkStrLit s!"{(<- Lean.getEnv).header.mainModule}"

---------------------------------------------------------------
def main (args : List String) : IO UInt32 := do
  let pwd <- IO.currentDir
  let arguments := s!"argument{if args.length == 1 then "" else "s"}"

  IO.println s!"running: <{pwd}> {module_name}"
  IO.println s!"  received {args.length} {arguments} from user:"

  for arg in args,
      idx in List.range args.length
  do
    IO.println s!"    args[{idx + 1}] = {arg}"

  return 0
