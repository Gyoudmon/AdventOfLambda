-- An entry script should not be defined in a namespace

-- Any IO action must be wrapped in another IO action
def readLine (prompt : String) : IO String := do
  IO.print prompt

  -- use `<-` to obtain the value from the IO monad
  let stdin <- IO.getStdin
  let line <- stdin.getLine

  -- use `pure` to put a value into the IO monad,
  -- and use `()` correctly... 🤦‍♀️
  pure (line.dropRightWhile Char.isWhitespace)

---------------------------------------------------------------
def main : IO Unit := do
  let name <- readLine "喊出来你要踢谁: "
  IO.println s!"你刚刚踢了{name}一脚! {name} 很气愤。"

  let maybeN <- readLine "你打算给几块糖讲和？"
  match maybeN.toNat? with
  | none    => IO.println s!"你毫无诚意，{name}跟你绝交了！"
  | some n  =>
    if n >= 4 then
      IO.println s!"哇，有{n}块糖啊！你是有诚意的，可以愉快地玩耍。"
    else
      IO.println s!"哼，才{n}块糖。我记住你了，你给我等着。"
