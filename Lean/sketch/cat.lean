---------------------------------------------------------------
partial def cat (stdin : IO.FS.Stream) (stdout : IO.FS.Stream) : IO Unit := do
  let line ← stdin.getLine

  -- `line` contains the newline character
  if line.length > 0 then
    stdout.putStr line
    cat stdin stdout

---------------------------------------------------------------
def main (args : List String) : IO UInt32 := do
  let stdout ← IO.getStdout

  if args.length > 0 then
    for path in (List.map System.FilePath.mk args) -- string->path
    do
      if (← System.FilePath.pathExists path) && !(← System.FilePath.isDir path) then
        -- open input handle from file
        let handle ← IO.FS.Handle.mk path IO.FS.Mode.read
        -- work with a stream instead of the handle directly
        cat (IO.FS.Stream.ofHandle handle) stdout
  else
    cat (← IO.getStdin) stdout

  return 0
