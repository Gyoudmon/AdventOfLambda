#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define calculate-grand-total : (-> Input-Port Integer)
  (lambda [/dev/aocin]
    (define-values (operators operands) (read-grand-problems /dev/aocin))

    (for/sum : Integer ([idx (in-range (vector-length operators))])
      (apply (vector-ref operators idx)
             (for/list : (Listof Integer) ([v (in-list operands)])
               (vector-ref v idx))))))

(define calculate-grand-total-from-right-to-left : (-> Input-Port Integer)
  (lambda [/dev/aocin]
    (define problems : (Vectorof String) (read-grand-problems/char /dev/aocin))

    (if (> (vector-length problems) 0)
        (let calc ([idx : Nonnegative-Fixnum 0]
                   [integers : (Listof Integer) null]
                   [total : Integer 0])
          (if (< idx (string-length (vector-ref problems 0)))
              (let-values ([(idx++) (+ idx 1)]
                           [(n op) (column->number problems idx)])
                (cond [(zero? n) #;#:column (calc idx++ integers total)]
                      [(not op) (calc idx++ (cons n integers) total)]
                      [else (calc idx++ null (+ total (apply op n integers)))]))
              total))
        0)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-grand-problems : (-> Input-Port (Values (Vectorof (-> Integer * Integer)) (Listof (Vectorof Integer))))
  (lambda [/dev/aocin]
    (let read-problem ([operands : (Listof (Vectorof Integer)) null])
      (if (regexp-match-peek #px"\\s*\\d" /dev/aocin)
          (let ([line (read-line /dev/aocin)])
            (if (string? line)
                (let ([op (filter exact-integer? (map string->number (string-split line)))])
                  (read-problem (cons (list->vector op) operands)))
                (read-problem operands)))
          (let ([line (read-line /dev/aocin)])
            (values (if (string? line)
                        (for/vector : (Vectorof (-> Integer * Integer)) ([op (string-split line)])
                          (op->operator (string-ref op 0)))
                        (vector))
                    operands))))))

(define read-grand-problems/char : (-> Input-Port (Vectorof String))
  (lambda [/dev/aocin]
    (for/vector : (Vectorof String) ([line (in-port read-line /dev/aocin)])
      (list->string (reverse (string->list line))))))

(define column->number : (-> (Vectorof String) Index (Values Natural (Option (-> Integer * Integer))))
  (lambda [problems idx]
    (for/fold ([n : Natural 0]
               [op : (Option (-> Integer * Integer)) #false])
              ([sheet (in-vector problems)])
      (define digit (string-ref sheet idx))
      (cond [(char<=? #\0 digit #\9) (values (+ (* n 10) (char->octadecimal digit)) op)]
            [(char=? digit #\space) (values n op)]
            [else (values n (op->operator digit))]))))

(define op->operator : (-> Char (-> Integer * Integer))
  (lambda [op]
    (if (char=? op #\+)
        + *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)

  (define-type Test-Case-Datum String)
  
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))

  (define testcases : (Listof Test-Case-Datum)
    (list "123 328  51 64 "
          " 45 64  387 23 "
          "  6 98  215 314"
          "*   +   *   +  "))

  (define example : String (string-join ((inst map String Test-Case-Datum) values testcases) (string #\newline)))
  (define test-ans1 : Integer 4277556)
  (define test-ans2 : Integer 3263827)
  (define pzzl-ans1 : Integer 6378679666679)
  (define pzzl-ans2 : Integer 11494432585168)
  
  (define-feature AoC2025::Day06::Trash.Compactor #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "What is the grand total found by adding together all of the answers to the individual problems?" #:do
        (it ["should produce ~a for the example" test-ans1] #:do
          (expect-= (call-with-input-string example calculate-grand-total)
                    test-ans1))
        (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
          (expect-= (call-with-input-file input.aoc calculate-grand-total)
                    pzzl-ans1)))
      (describe "What is the grand total found by adding together all of the answers to the individual problems? [by reading right to left char by char]" #:do
        (it ["should produce ~a for the example" test-ans2] #:do
          (expect-= (call-with-input-string example calculate-grand-total-from-right-to-left)
                    test-ans2))
        (it ["should produce ~a for the puzzle" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc calculate-grand-total-from-right-to-left)
                    pzzl-ans2)))))
    
  (void (spec-prove AoC2025::Day06::Trash.Compactor)))
