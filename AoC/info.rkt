#lang info

(define collection "Advent of Lambda")
(define pkg-desc "函数降临节")

(define version "0.1")

(define deps '("digimon"))
(define build-deps '("digimon" "scribble-lib" "racket-doc"))

(define pkg-authors '("WarGrey Gyoudmon Ju"))
(define test-omit-paths 'all)

(define sdl2-config
  '((config SDL2)
    (lib gydm pltmos stemos)
    (include "C:\\opt\\GYDMstem\\include")
    (libpath "C:\\opt\\GYDMstem\\lib")))

(define native-compiled-subpath '())
(define native-compiled-bindir '("bin"))
(define native-compiled-libdir '())
(define native-compiled-release '())
(define native-compiled-debug '("debug"))

(define typesettings
  '(["literacy/AdventOfCode.scrbl" xelatex]))

(define native-launcher-names
  `(["village/cpp/MagicalEnergyExpedition.cpp" console ,@sdl2-config]))
