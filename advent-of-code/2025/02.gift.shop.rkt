#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-sum-all-invalid-IDs : (-> (-> Index Any) (-> Input-Port Integer))
  (lambda [invalid?]
    (λ [/dev/aocin]
      (for/sum : Integer ([range (in-port read-range /dev/aocin)])
        (define stop (cdr range))
        (let for-sum : Natural ([id : Natural (car range)]
                                [sum : Natural 0])
          (cond [(> id stop) sum]
                [(invalid? id) (for-sum (add1 id) (+ sum id))]
                [else (for-sum (add1 id) sum)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define id-split : (-> Index (Listof Byte))
  (lambda [id]
    (let split ([id : Integer id]
                [digits : (Listof Byte) null])
      (if (> id 0)
          (let-values ([(q r) (quotient/remainder id 10)])
            (split q (cons r digits)))
          digits))))

(define repeated-twice? : (-> Index Boolean)
  (lambda [id]
    (define digits (id-split id))

    (and (even? (length digits))
         (let ([half (quotient (length digits) 2)])
           (for/and : Boolean ([l (in-range half)])
             (eq? (list-ref digits l)
                  (list-ref digits (+ l half))))))))

(define repeated-at-least-twice? : (-> Index Boolean)
  (lambda [id]
    (define digits (id-split id))
    (define pool (apply bytes (reverse digits)))
    
    (let ormap ([span : Index (quotient (bytes-length pool) 2)])
      (and (> span 0)
           (or (repeated-by-span? pool span)
               (ormap (sub1 span)))))))

(define repeated-by-span? : (-> Bytes Positive-Index Boolean)
  (lambda [digits span]
    (define size (bytes-length digits))
    (define r (remainder size span))

    (and (zero? r)
         (let and-span ([idx : Nonnegative-Fixnum 0])
           (if (< idx span)
               (and (let and-segment : Boolean ([distance : Nonnegative-Fixnum span])
                      (if (< distance size)
                          (and (= (bytes-ref digits idx)
                                  (bytes-ref digits (unsafe-fx+ idx distance)))
                               (and-segment (+ distance span)))
                          #true))
                    (and-span (+ idx 1)))
               #true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)
  
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define example : String
    (string-append "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
                   "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
                   "824824821-824824827,2121212118-2121212124"))
  
  (define test-ans1 : Integer 1227775554)
  (define test-ans2 : Integer 4174379265)
  (define pzzl-ans1 : Integer 12599655151)
  (define pzzl-ans2 : Integer 20942028255)
  
  (define-feature AoC2025::Day02::Gift-Shop #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "What do you get if you add up all of the invalid IDs? [twice repeated]" #:do
        (it ["should produce ~a for the example" test-ans1] #:do
          (expect-= (call-with-input-string example (make-sum-all-invalid-IDs repeated-twice?))
                    test-ans1))
        (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
          (expect-= (call-with-input-file input.aoc (make-sum-all-invalid-IDs repeated-twice?))
                    pzzl-ans1)))
      (describe "What do you get if you add up all of the invalid IDs? [at least twice repeated]" #:do
        (it ["should produce ~a for the example" test-ans2] #:do
          (expect-= (call-with-input-string example (make-sum-all-invalid-IDs repeated-at-least-twice?))
                    test-ans2))
        (it ["should produce ~a for the puzzle" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc (make-sum-all-invalid-IDs repeated-at-least-twice?))
                    pzzl-ans2)))))

  (void (spec-prove AoC2025::Day02::Gift-Shop)))
