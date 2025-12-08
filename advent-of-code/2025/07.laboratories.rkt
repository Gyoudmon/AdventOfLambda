#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define drive-classical-beam-splitting : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define manifold (port->lines /dev/aocin))

    (or (and (pair? manifold)
             (for/fold ([beams : (Listof Index) (for/list : (Listof Index) ([chr (in-string (car manifold))]
                                                                            [idx (in-naturals)] #:when (char=? chr #\S))
                                                  (assert idx index?))]
                        [count : Natural 0]
                        #:result count)
                       ([line (in-list (cdr manifold))])
               (beam-classical-movedown beams line count)))
        0)))

(define drive-quantum-beam-splitting : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define manifold (port->lines /dev/aocin))

    (or (and (pair? manifold)
             (apply +
                    (for/list : (Listof Natural) ([chr (in-string (car manifold))]
                                                  [beam (in-naturals)] #:when (char=? chr #\S))
                      (beam-quantum-movedown (assert beam index?) (cdr manifold)))))
        0)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define beam-classical-movedown : (-> (Listof Index) String Natural (Values (Listof Index) Natural))
  (lambda [beams line count]
    (define boundary (string-length line))
    
    (for/fold ([beams++ : (Listof Index) null]
               [count++ : Natural count])
              ([beam (in-list beams)])
      (if (char=? (string-ref line beam) #\^)
          (let* ([b++ (classical-beam-cons (classical-beam-cons beams++ (sub1 beam) boundary)
                                           (add1 beam) boundary)]
                 [delta (- (length b++) (length beams++))])
            (values b++ (+ count++ (if (zero? delta) 0 1))))
          (values (cons beam beams++) count++)))))

(define classical-beam-cons : (-> (Listof Index) Fixnum Index (Listof Index))
  (lambda [beams idx boundary]
    (if (and (index? idx) (< idx boundary)
             (not (memq idx beams)))
        (cons idx beams)
        beams)))

(define beam-quantum-movedown : (-> Index (Listof String) Natural)
  (lambda [beam manifold]
    (define beams : (Mutable-HashTable Integer Natural) (make-hasheq (list (cons beam 1))))
    (for ([line (in-list manifold)])
      (for ([self (in-list (hash-keys beams))])
        (when (char=? (string-ref line self) #\^)
          (define delta (hash-ref beams self (λ [] 0)))
          
          (hash-set! beams (sub1 self) (+ (hash-ref beams (sub1 self) (λ [] 0)) delta))
          (hash-set! beams (add1 self) (+ (hash-ref beams (add1 self) (λ [] 0)) delta))
          (hash-remove! beams self))))
    (apply + (hash-values beams))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)

  (define-type Test-Case-Datum Symbol)
  
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))

  (define testcases : (Listof Test-Case-Datum)
    '(.......S.......
      ...............
      .......^.......
      ...............
      ......^.^......
      ...............
      .....^.^.^.....
      ...............
      ....^.^...^....
      ...............
      ...^.^...^.^...
      ...............
      ..^...^.....^..
      ...............
      .^.^.^.^.^...^.
      ...............))

  (define example : String (string-join ((inst map String Test-Case-Datum) symbol->string testcases) (string #\newline)))
  (define test-ans1 : Integer 21)
  (define test-ans2 : Integer 40)
  (define pzzl-ans1 : Integer 1543)
  (define pzzl-ans2 : Integer 3223365367809)
  
  (define-feature AoC2025::Day07::Laboratories #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "How many times will the beam be split?" #:do
        (it ["should produce ~a for the example" test-ans1] #:do
          (expect-= (call-with-input-string example drive-classical-beam-splitting)
                    test-ans1))
        (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
          (expect-= (call-with-input-file input.aoc drive-classical-beam-splitting)
                    pzzl-ans1)))
      (describe "In total, how many different timelines would a single tachyon particle end up on?" #:do
        (it ["should produce ~a for the example" test-ans2] #:do
          (expect-= (call-with-input-string example drive-quantum-beam-splitting)
                    test-ans2))
        (it ["should produce ~a for the puzzle" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc drive-quantum-beam-splitting)
                    pzzl-ans2)))))
    
  (void (spec-prove AoC2025::Day07::Laboratories)))
