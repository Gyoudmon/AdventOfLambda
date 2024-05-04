import Lean.CoreM
import Lean.MonadEnv
import Lean.Environment

---------------------------------------------------------------
#eval show Lean.CoreM Unit from do
  dbg_trace s!"running: {(<- Lean.getEnv).header.mainModule}"

---------------------------------------------------------------
def main (args : List String) : IO UInt32 := do
  let pwd <- IO.currentDir
  let arguments := s!"argument{if args.length == 1 then "" else "s"}"

  IO.println s!"  in: {pwd}"
  IO.println s!"  received {args.length} {arguments} from user:"

  for arg in args,
      idx in List.range args.length
  do
    IO.println s!"    args[{idx + 1}] = {arg}"

  return 0
