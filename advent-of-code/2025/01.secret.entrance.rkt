#lang typed/racket/base

;;; @link https://adventofcode.com/2025/day/1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dial-the-password : (-> (-> Natural Integer (Values Natural Natural)) (-> Input-Port Natural))
  (lambda [dial]
    (λ [/dev/aocin]
      (for/fold ([ptr : Natural 50] [pwd : Natural 0] #:result pwd)
                ([distance (in-port read-distance /dev/aocin)])
        (define-values (ptr++ round) (dial ptr distance))
        
        (values ptr++ (+ pwd round))))))

(define dial-the-password/434C49434B* : (-> Input-Port (Values (Listof Natural) (Listof Natural)))
  (lambda [/dev/aocin]
    (for/fold ([ptrs : (Pairof Natural (Listof Natural)) (list 50)]
               [rounds : (Pairof Natural (Listof Natural)) (list 0)]
               #:result (values (reverse ptrs) (reverse rounds)))
              ([distance (in-port read-distance /dev/aocin)])
      (define-values (ptr++ round) (dial/434C49434B (car ptrs) distance))

      (values (cons ptr++ ptrs)
              (cons round rounds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-distance : (-> Input-Port (U EOF Integer))
  (lambda [/dev/aocin]
    (define direction (read-char /dev/aocin))

    (if (char? direction)
        (let ([distance (read /dev/aocin)])
          (read-line /dev/aocin)
          (if (exact-nonnegative-integer? distance)
              (* distance (if (eq? direction #\L) -1 +1))
              eof))
        eof)))

(define dial/0 : (-> Natural Integer (Values Natural Natural))
  (lambda [ptr distance]
    (define ptr++ (modulo (+ ptr distance) 100))

    (values ptr++ (if (zero? ptr++) 1 0))))

(define dial/434C49434B : (-> Natural Integer (Values Natural Natural))
  (lambda [ptr distance]
    (define pos (+ ptr distance))
    (define round
      (cond [(zero? ptr) (quotient (abs distance) 100)]
            [(>= pos 100) (quotient pos 100)]
            [(<= pos 0) (add1 (quotient (abs pos) 100))]
            [else 0]))
    
    (values (modulo pos 100) round)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)
  (require racket/string)
  (require racket/port)

  (define-type Test-Case-Datum (List String Integer Integer))
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define testcases : (Listof Test-Case-Datum)
    (list (list "L168" 82 2)
          (list "L130" 52 1)
          (list "R148"  0 2)
          (list "L105" 95 1)
          (list "R160" 55 2)
          (list "L155"  0 2)
          (list "L101" 99 1)
          (list "L199"  0 2)
          (list "R114" 14 1)
          (list "L182" 32 2)))

  (define example : String (string-join ((inst map String Test-Case-Datum) car testcases) (string #\newline)))
  (define test-ans1 : Integer (length (filter zero? ((inst map Integer Test-Case-Datum) cadr testcases))))
  (define test-ans2 : Integer (apply + ((inst map Integer Test-Case-Datum) caddr testcases)))
  (define pzzl-ans1 : Integer 1023)
  (define pzzl-ans2 : Integer 5899)
  
  (define-feature AoC2025::Day01::Secret-Password #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "the number of times the dial is left pointing at 0 after any rotation in the sequence" #:do
        (it ["should use the password ~a to open the example door" test-ans1] #:do
          (expect-= (call-with-input-string example (make-dial-the-password dial/0))
                    test-ans1))
        (it ["should use the password ~a to open the door" pzzl-ans1] #:do
          (expect-= (call-with-input-file input.aoc (make-dial-the-password dial/0))
                    pzzl-ans1)))
      (describe "the password method 0x434C49434B counts the number of times any click causes the dial to point at 0" #:do
        (it ["should use the new password ~a to open the example door" test-ans2] #:do
          (expect-= (call-with-input-string example (make-dial-the-password dial/434C49434B))
                    test-ans2))
        (it ["should use the new password ~a to open the door" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc (make-dial-the-password dial/434C49434B))
                    pzzl-ans2))))
    (context "details in the solving process" #:do
      (let-values ([(dials rounds) (call-with-input-string example dial-the-password/434C49434B*)])
        (for/spec ([gdial (in-list (cdr dials))]
                   [ground (in-list (cdr rounds))]
                   [op (in-list testcases)])
          (it ["[~a] the dial should point at ~a" (car op) (cadr op)] #:do
            (expect-=  gdial  (cadr op) "wrong dial")
            (expect-= ground (caddr op) "wrong round"))))))

  (void (spec-prove AoC2025::Day01::Secret-Password)))
