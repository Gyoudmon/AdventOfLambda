/- **这里接题目的简短名称, 可省略** 例如: _测试驱动计算_

**这里可以以多个段落给出完整的问题, 不建议省略**

**问题的输入-输出说明**
1. 使用 `@arg` 声明程序输入
    1. 每个参数都要有一个 `@arg`
      2. 一般不会出现“无输入参数”的情况。
        1. 如果有, 那也用不着费这个劲请测试程序出马了
        3. 输入参数必须说清楚参数的类型
    2. 使用 `@result` 声明程序输出
      1. 每个结果都要有一个 `@result`
      2. 如果输出不重要可省略
        1. 考虑对话类的程序, 它们说了一堆给用户看的人话, 此时你的眼睛比测试程序更靠谱

`@arg` **输入参数1的简短说明** 例如: _type 正整数 测试类型编号_
`@arg` **输入参数2的简短说明** 例如: _data 自定义类型 本测试实际用到的数据_
`@result` **输出结果的简短说明** 例如: _将输入数据转化为浮点数, 保留一位小数_

**测试用例书写规范**
1. 输入参数跟在 input 行后面, 直到碰到其他可以指示注释结构的行
2. 输出参数跟在 output 行后面, 直到碰到其他可以指示注释结构的行
3. 测试用例可以保存在别的文件里
    1. 文件用 `@file` 或 `@include` 声明
    2. 相当于把文件内容复制插入到声明它们的那一行
    3. 文件的相对路径相对于声明它的源码文件, 与 `#include ""`相同
    4. 如果有多个 input, 它们的内容将合并到一起, 相当于只有一个 input
      1. 最终的输入内容【不】包含它们前后的空行, 中间的空行会合并成一个
    5. 如果有多个 output, 说明程序的输出不止一种情况, 满足其中任意一个即可
      1. 每一个输出内容【不】包含它们前后的空行, 中间的空行会合并成一个
    6. 如果省略 input 和 output, 则输入和输出以空行分隔
      1. 没有空行时默认所有内容均为输入
      2. 不建议学生省略 input 和 output
    7. 测试用例的输入参数和输出参数应当符合上述 `@arg` 和 `@result` 的说明
    8. 可以通过 timeout 行指定测试用例的时间限制, 单位毫秒

`@test` 这是题目给出的标准测试用例
input:  0 4
output: 4.0

`@test` 这是学生纸笔推演出的测试用例
input:  0 -2
output: -2.0

`@test` 这个测试用例有多种输出可能(而且 input 和 output 交错出现了)
output: -2.0
input:  0 -2
output: -2.00

`@test` 这个测试用例有 output, 但是要求“不输出任何东西”
input: 1
output:

`@test` 话唠测试用例从不需要说明 output
input: 2

`@test` 懒惰的测试用例会省略 input 和 output。你得去猜, 哎~, 就是玩!
0 128

128.0

`@test` 懒惰的外部测试用例。其内容来自两个文件, 中间的空行说明这两个文件分别提供输入和输出数据
`@file` stone/readme.in

`@file` stone/readme.ans

`@test` 超时的测试用例。不小心写了个死循环！
timeout: 100
3

`@test` 懒惰又易错的测试用例。它省略了 input 和 output, 但又企图只给 output, 结果翻车了吧？

4

`@test` 空测试用例算做“待测用例”, 不计入失败的测试
-/

import Lean

---------------------------------------------------------------------------------------------------
partial def forever (wc : Bool) : IO Unit := do
  forever wc

def read_arguments (stdin : IO.FS.Stream) : IO (Option (UInt8 × Int)) := do
  let line ← stdin.getLine

  if line.length > 0 then
    -- TODO: match pattern with predicate function?
    match (List.filterMap String.toInt? line.trim.splitOn) with
    | [t, d] => pure (Prod.mk (UInt8.ofNat t.natAbs) d)
    | [t] => pure (Prod.mk (UInt8.ofNat t.natAbs) (0 : Int))
    | _ => pure none
  else
    pure none

-- TODO: this function should not exist
def print_flonum (fl : Float) (precision : Nat) : IO Unit := do
  let fxscale : Nat := 10 ^ precision
  let fxabs : Nat := (Float.round (fl.abs * Float.ofInt fxscale)).toUInt64.toNat

  if fl < 0.0 then IO.print "-"
  IO.print s!"{fxabs / fxscale}"

  if precision > 0 then
    let r := s!"{fxabs % fxscale}"
    let diff := precision - r.length

    if diff == 0 then
      IO.println s!".{r}"
    else
      IO.print "."
      for _ in (List.range diff) do IO.print "0"
      IO.println s!"{r}"

---------------------------------------------------------------------------------------------------
def main (_args : List String) : IO UInt32 := do
  match (← read_arguments (← IO.getStdin)) with
  | none => return 255
  | some (0, datum) => /- 主测试, 整数转浮点数 -/ print_flonum (Float.ofInt datum) 1
  | some (1, _) => /- “沉默”咒术 -/ IO.println "哼, 战斗力只有5的渣渣!"
  | some (2, _) => /- 打断"沉默", 全异常解除 -/ IO.println "毁灭吧, 赶紧的, 累了!"
  | some (3, _) => /- 时间监狱 -/ forever true
  | some (n, _) =>
    if n == 4 then
      (← IO.getStderr).putStrLn "不规范的输出被当成了输入！此类错误无法通过测试发现, 只好让程序自己报错了！"
    return n.toUInt32 -- TODO: check Init.Coe to make this implicitly

  return 0
