#lang info

(define collection "Advent of Lambda")
(define pkg-desc "函数降临节")

(define version "0.1")

(define deps '("digimon"))
(define build-deps '("digimon" "scribble-lib" "racket-doc"))

(define pkg-authors '("WarGrey Gyoudmon Ju"))
(define test-omit-paths 'all)

(define typesettings '(["literacy/AdventOfCode.scrbl" xelatex]))
