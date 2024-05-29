/- -- Day 2: Rock Paper Scissors

The Elves begin to set up camp on the beach.
To decide whose tent gets to be closest to the snack storage,
a giant Rock Paper Scissors tournament is already in progress.

Rock Paper Scissors is a game between two players.
Each game contains many rounds; in each round,
the players each simultaneously choose one of Rock, Paper, or Scissors using a hand shape.
Then, a winner for that round is selected:
Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.
If both players choose the same shape, the round instead ends in a draw.

Appreciative of your help yesterday,
one Elf gives you an encrypted strategy guide that they say will be sure to help you win.
"The first column is what your opponent is going to play:
A for Rock, B for Paper, and C for Scissors. The second column--"
Suddenly, the Elf is called away to help with someone's tent.

The second column, you reason, must be what you should play in response:
X for Rock, Y for Paper, and Z for Scissors.
Winning every time would be suspicious,
so the responses must have been carefully chosen.

The winner of the whole tournament is the player with the highest score.
Your total score is the sum of your scores for each round.
The score for a single round is the score for the shape you selected
(1 for Rock, 2 for Paper, and 3 for Scissors)
plus the score for the outcome of the round
(0 if you lost, 3 if the round was a draw, and 6 if you won).

Since you can't be sure if the Elf is trying to help you or trick you,
you should calculate the score you would get if you were to follow the strategy guide.

`@arg` lists of encrypted strategy guide
`@result` final score based on the strategy but you guess

The Elf finishes helping with the tent and sneaks back over to you.
"Anyway, the second column says how the round needs to end:
X means you need to lose,
Y means you need to end the round in a draw,
and Z means you need to win. Good luck!"

`@result` final score based on the exact strategy

`@test` samples
input:
A Y
B X
C Z

output:
15
12

`@test` rock, paper, scissor competition
input:
`@file` ../stone/aoc2022/02_rps.aoc
output:
14827
13889
--/

import Lean

---------------------------------------------------------------
inductive Shape where
  | Rock : Shape
  | Paper : Shape
  | Scissor : Shape
  deriving Repr

inductive Outcome where
  | Win : Outcome
  | Draw : Outcome
  | Lose : Outcome
  deriving Repr

def shape_score : Shape -> Nat
  | Shape.Rock => 1
  | Shape.Paper => 2
  | Shape.Scissor => 3

def outcome_score : Outcome -> Nat
  | Outcome.Win => 6
  | Outcome.Draw => 3
  | Outcome.Lose => 0

---------------------------------------------------------------
def round_score (sf_play : Shape) (outcome : Outcome) : Nat :=
  shape_score sf_play + outcome_score outcome

def round_end (op_play : Shape) (sf_play : Shape) : Outcome :=
  match op_play with
  | Shape.Rock =>
      match sf_play with
      | Shape.Rock => Outcome.Draw
      | Shape.Scissor => Outcome.Lose
      | Shape.Paper => Outcome.Win
  | Shape.Paper =>
      match sf_play with
      | Shape.Paper => Outcome.Draw
      | Shape.Rock => Outcome.Lose
      | Shape.Scissor => Outcome.Win
  | Shape.Scissor =>
      match sf_play with
      | Shape.Scissor => Outcome.Draw
      | Shape.Paper => Outcome.Lose
      | Shape.Rock => Outcome.Win

def smart_shape (op_play : Shape) (sf_end : Outcome) : Shape :=
  match sf_end with
  | Outcome.Draw => op_play
  | Outcome.Win =>
      match op_play with
      | Shape.Rock => Shape.Paper
      | Shape.Scissor => Shape.Rock
      | Shape.Paper => Shape.Scissor
  | Outcome.Lose =>
      match op_play with
      | Shape.Paper => Shape.Rock
      | Shape.Rock => Shape.Scissor
      | Shape.Scissor => Shape.Paper

---------------------------------------------------------------
def char_to_shape (ch : Char) : Option Shape :=
  match ch with
  | 'A' | 'X' => Shape.Rock
  | 'B' | 'Y' => Shape.Paper
  | 'C' | 'Z' => Shape.Scissor
  | _ => none

def char_to_outcome (ch : Char) : Option Outcome :=
  match ch with
  | 'X' => Outcome.Lose
  | 'Y' => Outcome.Draw
  | 'Z' => Outcome.Win
  | _ => none

partial def read_strategy (stdin : IO.FS.Stream) : IO (List (Shape × Char)) := do
  let rec read_guide (instructions : List (Shape × Char)) : IO (List (Shape × Char)) := do
    let line ← stdin.getLine

    match line.data with
    | op::_::sf::_ =>
      match char_to_shape op with
        | some shape => read_guide ((shape, sf)::instructions)
        | none => read_guide instructions
    | _EOF => pure instructions

  read_guide ([] : List (Shape × Char))

def guessed_strategy_round (strategy : (Shape × Char)) (score : Nat) : Nat :=
  match char_to_shape strategy.snd with
  | some sf_play => score + (round_score sf_play (round_end strategy.fst sf_play))
  | none => score

def designed_strategy_round (strategy : (Shape × Char)) (score : Nat) : Nat :=
  match char_to_outcome strategy.snd with
  | some sf_out => score + (round_score (smart_shape strategy.fst sf_out) sf_out)
  | none => score

---------------------------------------------------------------
def main (_args : List String) : IO UInt32 := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let strategies ← read_strategy stdin
  let guessed_score := List.foldr guessed_strategy_round 0 strategies
  let designed_score := List.foldr designed_strategy_round 0 strategies

  stdout.putStrLn s!"{guessed_score}"
  stdout.putStrLn s!"{designed_score}"

  return 0
