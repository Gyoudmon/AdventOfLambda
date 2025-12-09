#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mark-forklift-path : (-> Input-Port Integer)
  (lambda [/dev/aocin]
    (define grid : (Vectorof String) (list->vector (port->lines /dev/aocin)))

    (define row (vector-length grid))
    (define col (string-length (vector-ref grid 0)))

    (for*/sum : Natural ([r (in-range row)]
                         [c (in-range col)])
      (if (and (eq? (string-ref (vector-ref grid r) c) #\@)
               (cell-removable? grid r c row col))
          1 0))))

(define remove-rolls-of-paper : (-> Input-Port Integer)
  (lambda [/dev/aocin]
    (define grid : (Vectorof String) (list->vector (port->lines /dev/aocin)))

    (define row (vector-length grid))
    (define col (string-length (vector-ref grid 0)))

    (let remove ([sum : Natural 0])
      (define marked-rolls
        (for*/list : (Listof (Pairof Integer Integer)) ([r (in-range row)]
                                                        [c (in-range col)]
                                                        #:when (and (eq? (string-ref (vector-ref grid r) c) #\@)
                                                                    (cell-removable? grid r c row col)))
          (cons r c)))

      (if (pair? marked-rolls)
          (begin (for ([cell (in-list marked-rolls)])
                   (string-set! (vector-ref grid (car cell)) (cdr cell) #\.))
                 (remove (+ sum (length marked-rolls))))
          sum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define roll-of-paper-ref : (-> (Vectorof String) Integer Integer Index Index (U One Zero))
  (lambda [grid r c row col]
    (if (and (< -1 r row) (< -1 c col)
             (eq? (string-ref (vector-ref grid r) c) #\@))
        1 0)))

(define cell-removable? : (-> (Vectorof String) Integer Integer Index Index Boolean)
  (lambda [grid r c row col]
    (< (+ (roll-of-paper-ref grid (- r 1) (- c 1) row col)
          (roll-of-paper-ref grid (- r 1)       c row col)
          (roll-of-paper-ref grid (- r 1) (+ c 1) row col)
          (roll-of-paper-ref grid       r (- c 1) row col)
          (roll-of-paper-ref grid       r (+ c 1) row col)
          (roll-of-paper-ref grid (+ r 1) (- c 1) row col)
          (roll-of-paper-ref grid (+ r 1)       c row col)
          (roll-of-paper-ref grid (+ r 1) (+ c 1) row col))
       4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)

  (define-type Test-Case-Datum Symbol)
  
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define testcases : (Listof Test-Case-Datum)
    '(..@@.@@@@.
      @@@.@.@.@@
      @@@@@.@.@@
      @.@@@@..@.
      @@.@@@@.@@
      .@@@@@@@.@
      .@.@.@.@@@
      @.@@@.@@@@
      .@@@@@@@@.
      @.@.@@@.@.))

  (define example : String (string-join ((inst map String Test-Case-Datum) symbol->string testcases) (string #\newline)))
  (define test-ans1 : Integer 13)
  (define test-ans2 : Integer 43)
  (define pzzl-ans1 : Integer 1419)
  (define pzzl-ans2 : Integer 8739)
  
  (define-feature AoC2025::Day04::Printing-Department #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "How many rolls of paper can be accessed by a forklift?" #:do
        (it ["should produce ~a for the example" test-ans1] #:do
          (expect-= (call-with-input-string example mark-forklift-path)
                    test-ans1))
        (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
          (expect-= (call-with-input-file input.aoc mark-forklift-path)
                    pzzl-ans1)))
      (describe "How many rolls of paper in total can be removed by the Elves and their forklifts?" #:do
        (it ["should produce ~a for the example" test-ans2] #:do
          (expect-= (call-with-input-string example remove-rolls-of-paper)
                    test-ans2))
        (it ["should produce ~a for the puzzle" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc remove-rolls-of-paper)
                    pzzl-ans2)))))
    
  (void (spec-prove AoC2025::Day04::Printing-Department)))
