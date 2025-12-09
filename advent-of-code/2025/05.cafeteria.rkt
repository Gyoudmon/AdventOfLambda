#lang typed/racket/base

(require racket/set)
(require racket/list)

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define filter-fresh-ingredient : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define database : (Listof (Pairof Index Index)) (read-ingredient-database /dev/aocin))

    (for/sum : Natural ([id (in-port read /dev/aocin)]
                        #:when (index? id))
      (if (ingredient-fresh? database id)
          1 0))))

(define merge-fresh-ingredient-set : (-> Input-Port Integer)
  (lambda [/dev/aocin]
    (define database : (Listof (Pairof Index Index))
      (sort (read-ingredient-database /dev/aocin)
            (λ [[r1 : (Pairof Index Index)] [r2 : (Pairof Index Index)]]
              (or (< (car r1) (car r2))
                  (and (= (car r1) (car r2))
                       (< (cdr r1) (cdr r2)))))))

    (for/fold ([sum : Integer 0]
               [self : (Pairof Index Index) (car database)]
               #:result (+ sum 1 (- (cdr self) (car self))))
              ([ranges (in-list (cdr database))])
      (cond [(> (car ranges) (cdr self)) (values (+ sum 1 (- (cdr self) (car self))) ranges)]
            [(> (cdr ranges) (cdr self)) (values sum (cons (car self) (cdr ranges)))]
            [else (values sum self)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-ingredient-database : (-> Input-Port (Listof (Pairof Index Index)))
  (lambda [/dev/aocin]
    (let read-database ([ranges : (Listof (Pairof Index Index)) null])
      (define maybe-ranges (read-maybe-range /dev/aocin))
      
      (if (pair? maybe-ranges)
          (read-database (cons maybe-ranges ranges))
          ranges))))

(define ingredient-fresh? : (-> (Listof (Pairof Index Index)) Index Boolean)
  (lambda [database id]
    (for/or ([ranges (in-list database)]) 
       (<= (car ranges) id (cdr ranges)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)

  (define-type Test-Case-Datum String)
  
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define testcases : (Listof Test-Case-Datum)
    (list "3-5"
          "10-14"
          "16-20"
          "12-18"
          ""
          "1"
          "5"
          "8"
          "11"
          "17"
          "32"))

  (define example : String (string-join ((inst map String Test-Case-Datum) values testcases) (string #\newline)))
  (define test-ans1 : Integer 3)
  (define test-ans2 : Integer 14)
  (define pzzl-ans1 : Integer 840)
  (define pzzl-ans2 : Integer 359913027576322)
  
  (define-feature AoC2025::Day05::Cafeteria #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "How many of the available ingredient IDs are fresh?" #:do
        (it ["should produce ~a for the example" test-ans1] #:do
          (expect-= (call-with-input-string example filter-fresh-ingredient)
                    test-ans1))
        (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
          (expect-= (call-with-input-file input.aoc filter-fresh-ingredient)
                    pzzl-ans1)))
      (describe "How many ingredient IDs are considered to be fresh according to the fresh ingredient ID ranges?" #:do
        (it ["should produce ~a for the example" test-ans2] #:do
          (expect-= (call-with-input-string example merge-fresh-ingredient-set)
                    test-ans2))
        (it ["should produce ~a for the puzzle" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc merge-fresh-ingredient-set)
                    pzzl-ans2)))))
    
  (void (spec-prove AoC2025::Day05::Cafeteria)))
