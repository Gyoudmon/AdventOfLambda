/- -- Day 2: Rucksack Reorganization

One Elf has the important job of loading all of the rucksacks with supplies for the jungle journey.
Unfortunately, that Elf didn't quite follow the packing instructions,
and so a few items now need to be rearranged.

Each rucksack has two large compartments.
All items of a given type are meant to go into exactly one of the two compartments.
The Elf that did the packing failed to follow this rule for exactly one item type per rucksack.

The Elves have made a list of all of the items currently in each rucksack,
but they need your help finding the errors.
Every item type is identified by a single lowercase or uppercase letter.

The list of items for each rucksack is given as characters all on a single line.
A given rucksack always has the same number of items in each of its two compartments,
so the first half of the characters represent items in the first compartment,
while the second half of the characters represent items in the second compartment.

To help prioritize item rearrangement, every item type can be converted to a priority:
  - Lowercase item types a through z have priorities 1 through 26.
  - Uppercase item types A through Z have priorities 27 through 52.

`@arg` lists of packaged items that represented with a single letter
`@result` sum of the priorities of misplaced items

For safety, the Elves are divided into groups of three.
Every Elf carries a badge that identifies their group.
For efficiency, within each group of three Elves,
the badge is the only item type carried by all three Elves.
That is, if a group's badge is item type B,
then all three Elves will have item type B somewhere in their rucksack,
and at most two of the Elves will be carrying any other item type.

The problem is that someone forgot to put this year's updated authenticity sticker on the badges.
All of the badges need to be pulled out of the rucksacks so the new authenticity stickers can be attached.

Additionally, nobody wrote down which item type corresponds to each group's badges.
The only way to tell which item type is the right one is by
finding the one item type that is common between all three Elves in each group.

Priorities for these items must still be found to organize the sticker attachment efforts.
`@result` sum of the priorities of badges of 3-Elf groups

`@test` samples
input:
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
output:
157
70

`@test` all items in each rucksack
input:
`@file` ../tamer/mee/03_rr.aoc
output:
8185
2817

-/

import Lean

---------------------------------------------------------------
def ItemGroup : Type := List String

---------------------------------------------------------------
def item_priority (item : Char) : UInt32 :=
  match (item >= 'a') && (item <= 'z'), (item >= 'A') && (item <= 'Z') with
  | true, false => item.val - 97 + 1
  | false, true => item.val - 65 + 27
  | _, _ => 0

def filter_shared_item (bank : List Char) (items : List Char) : List Char :=
  List.filter (· ∈ bank) items

def find_misplaced_item (items : String) : List Char :=
  let midpos := items.length / 2
  filter_shared_item (List.take midpos items.data) (List.drop midpos items.data)

def priority_fold (sum : UInt32) (unique_items : List Char) : UInt32 :=
  List.foldl (· + ·) sum (List.map item_priority (List.eraseDups unique_items))

def misplaced_item_fold (sum : UInt32) (items : ItemGroup) : UInt32 :=
  List.foldl priority_fold sum (List.map find_misplaced_item items)

def find_group_badge : ItemGroup -> List Char
| head::tail => List.foldl filter_shared_item head.data (List.map String.data tail)
| [] => []

---------------------------------------------------------------
partial def read_item_groups (stdin : IO.FS.Stream) : IO (List ItemGroup) := do
  let rec read_groups (groups : List ItemGroup) : IO (List ItemGroup) := do
    let fst ← stdin.getLine
    let snd ← stdin.getLine
    let trd ← stdin.getLine

    if trd.length > 0 then
      read_groups ([fst.trim, snd.trim, trd.trim]::groups)
    else
      pure groups.reverse

  read_groups []

---------------------------------------------------------------
def main (_args : List String) : IO UInt32 := do
  let item_groups ← read_item_groups (← IO.getStdin)
  let sum_of_misplaced_items := (List.foldl misplaced_item_fold 0 item_groups)
  let sum_of_group_badges := (List.foldl priority_fold 0 (List.map find_group_badge item_groups))

  IO.println s!"{sum_of_misplaced_items}"
  IO.println s!"{sum_of_group_badges}"

  return 0
