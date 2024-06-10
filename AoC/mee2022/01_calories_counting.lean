/- -- Day 1: Calorie Counting

Santa's reindeer typically eat regular reindeer food,
but they need a lot of magical energy to deliver presents on Christmas.
For that, their favorite snack is a special type of star fruit that only grows deep in the jungle.
The Elves have brought you on their annual expedition to the grove where the fruit grows.

To supply enough magical energy,
the expedition needs to retrieve a minimum of fifty stars by December 25th.
Although the Elves assure you that the grove has plenty of fruit,
you decide to grab any fruit you see along the way, just in case.

The jungle must be too overgrown and difficult to navigate in vehicles or access from the air;
the Elves' expedition traditionally goes on foot.
As your boats approach land, the Elves begin taking inventory of their supplies.
One important consideration is food - in particular,
the number of Calories each Elf is carrying.

The Elves take turns writing down the number of Calories contained by the various
meals, snacks, rations, etc. that they've brought with them, one item per line.
Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.

`@arg` lists of calories carried by all elves
`@result` total of the Elf carrying the most Calories

***************************************************************************************************

By the time you calculate the answer to the Elves' question,
they've already realized that the Elf carrying the most
Calories of food might eventually run out of snacks.

To avoid this unacceptable situation,
the Elves would instead like to know the total Calories carried by
the top three Elves carrying the most Calories.
That way, even if one of those Elves runs out of snacks, they still have two backups.

`@result` total of the top three Elves carrying the most Calories

`@test` samples
input:
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000

output:
24000
45000

`@test` collected calories
input:
`@file` ../tamer/mee/01_cc.aoc
output:
69281
201524
--/

import Lean

---------------------------------------------------------------
def find_peak_calories (tops : List Nat) (cal : Nat) : List Nat :=
  List.take tops.length
    ((List.filter (· >= cal) tops) ++ [cal] ++ (List.filter (· < cal) tops))

---------------------------------------------------------------
partial def read_elf_calories (stdin : IO.FS.Stream) : IO (List Nat) := do
  let rec read_calories (self_cal : Nat) (calories : (List Nat)) : IO (List Nat) := do
    let line ← stdin.getLine

    if line.length > 0 then
      match line.trim.toNat? with
        | some item_cal => read_calories (self_cal + item_cal) calories
        | none => read_calories 0 (calories ++ [self_cal]) -- switch to next Elf
    else -- EOF
      if self_cal > 0 then
        pure (calories ++ [self_cal])
      else
        pure calories

  read_calories 0 []

---------------------------------------------------------------
def main (_args : List String) : IO UInt32 := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let calories ← read_elf_calories stdin
  let top_cal := List.foldl max 0 calories
  let top3_cals := List.foldl find_peak_calories [0, 0, 0] calories
  let top3_cal := List.foldl (· + ·) 0 top3_cals

  stdout.putStrLn s!"{top_cal}"
  stdout.putStrLn s!"{top3_cal}"

  return 0
