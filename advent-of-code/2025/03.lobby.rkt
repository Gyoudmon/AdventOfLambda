#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-turn-on-batteries : (-> (-> String Integer) (-> Input-Port Integer))
  (lambda [turn-on]
    (λ [/dev/aocin]
      (for/sum : Integer ([line (in-lines /dev/aocin)])
        (turn-on line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-largest-two-joltages : (-> String Integer)
  (lambda [bank]
    (define size : Index (string-length bank))
    (define top : Index (find-largest-joltage-index bank 0 size))
    (define top+1 : Nonnegative-Fixnum (+ top 1))

    (if (< top+1 size)
        (bank-joltage bank top (find-largest-joltage-index bank top+1 size))
        (bank-joltage bank (find-largest-joltage-index bank 0 top) top))))

(define find-largest-twelve-joltages : (-> String Integer)
  (lambda [bank]
    (define indices : (Listof Index)
      (let subfind ([required : Index 12]
                    [start : Index 0]
                    [stop : Index (string-length bank)])
        (if (> required 0)
            (let* ([top (find-largest-joltage-index bank start stop)]
                   [rest+1 (unsafe-idx- stop top)])
              (cond [(<= required rest+1) (cons top (subfind (sub1 required) (unsafe-idx+ top 1) stop))]
                    [(<= rest+1 1) (append (subfind (sub1 required) start top) (list top))]
                    [else (append (subfind (unsafe-idx- required rest+1) start top)
                                  (list top)
                                  (subfind (sub1 rest+1) (unsafe-idx+ top 1) stop))]))
            null)))
    
    (bank-joltage bank indices)))

(define find-largest-joltage-index : (-> String Index Index Index)
  (lambda [bank start stop]
    (let find ([pos : Nonnegative-Fixnum (+ start 1)]
               [peak : Index start])
      (if (< pos stop)
          (find (+ pos 1)
                (if (char>? (string-ref bank pos)
                            (string-ref bank peak))
                    pos peak))
          peak))))

(define bank-joltage : (case-> [String Index Index -> Natural]
                              [String (Listof Index) -> Natural])
  (case-lambda
    [(bank tens ones)
     (+ (* (char->octadecimal (string-ref bank tens)) 10)
        (char->octadecimal (string-ref bank ones)))]
    [(bank indices)
     (for/fold ([joltage : Natural 0])
               ([idx (in-list indices)])
       (+ (* joltage 10)
          (char->octadecimal (string-ref bank idx))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)
  
  (define-type Test-Case-Datum (List String Integer Integer))
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define testcases : (Listof Test-Case-Datum)
    (list (list "987654321111111" 98 987654321111)
          (list "811111111111119" 89 811111111119)
          (list "234234234234278" 78 434234234278)
          (list "818181911112111" 92 888911112111)))

  (define example : String (string-join ((inst map String Test-Case-Datum) car testcases) (string #\newline)))
  (define test-ans1 : Integer (apply + ((inst map Integer Test-Case-Datum) cadr testcases)))
  (define test-ans2 : Integer (apply + ((inst map Integer Test-Case-Datum) caddr testcases)))
  (define pzzl-ans1 : Integer 17074)
  (define pzzl-ans2 : Integer 169512729575727)
  
  (define-feature AoC2025::Day03::Lobby #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "what is the total output joltage for two batteries?" #:do
        (it ["should produce ~a for the example" test-ans1] #:do
          (expect-= (call-with-input-string example (make-turn-on-batteries find-largest-two-joltages))
                    test-ans1))
        (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
          (expect-= (call-with-input-file input.aoc (make-turn-on-batteries find-largest-two-joltages))
                    pzzl-ans1)))
      (describe "what is the total output joltage for twelve batteries?" #:do
        (it ["should produce ~a for the example" test-ans2] #:do
          (expect-= (call-with-input-string example (make-turn-on-batteries find-largest-twelve-joltages))
                    test-ans2))
        (it ["should produce ~a for the puzzle" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc (make-turn-on-batteries find-largest-twelve-joltages))
                    pzzl-ans2))))
    (context "details in the solving process" #:do
      (describe "Turn on exact two batteries" #:do
        (for/spec ([t (in-list testcases)])
          (it ["[~a] the largest joltage should be ~a" (car t) (cadr t)] #:do
            (expect-= (find-largest-two-joltages (car t)) (cadr t)))))
      (describe "Turn on exact twelve batteries" #:do
        (for/spec ([t (in-list testcases)])
          (it ["[~a] the largest joltage should be ~a" (car t) (caddr t)] #:do
            (expect-= (find-largest-twelve-joltages (car t)) (caddr t)))))))
    
  (void (spec-prove AoC2025::Day03::Lobby)))
