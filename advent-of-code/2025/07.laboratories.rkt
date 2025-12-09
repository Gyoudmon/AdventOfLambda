#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define drive-classical-beam-splitting : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define-values (beam manifold) (read-manifold /dev/aocin))

    (or (and (pair? manifold)
             (for/fold ([beams : (Listof Integer) (list beam)]
                        [count : Natural 0]
                        #:result count)
                       ([line (in-list manifold)])
               (beam-classical-movedown beams line count)))
        0)))

(define drive-quantum-beam-splitting : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define-values (beam manifold) (read-manifold /dev/aocin))

    (or (and (pair? manifold)
             (beam-quantum-movedown beam manifold))
        0)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-manifold : (-> Input-Port (Values Index (Listof String)))
  (lambda [/dev/aocin]
    (let scan-start ([idx : Natural 0])
      (define ch (read-char /dev/aocin))

      (cond [(eof-object? ch) (values 0 null)]
            [(char=? ch #\S) (read-line /dev/aocin) (values (assert idx index?) (port->lines /dev/aocin))]
            [else (scan-start (add1 idx))]))))

(define beam-classical-movedown : (-> (Listof Integer) String Natural (Values (Listof Integer) Natural))
  (lambda [beams line count]
    (define boundary (string-length line))
    
    (for/fold ([beams++ : (Listof Integer) null]
               [count++ : Natural count])
              ([beam (in-list beams)])
      (if (char=? (string-ref line beam) #\^)
          (let* ([b++ (set-union beams++ (list (sub1 beam) (add1 beam)))]
                 [delta (- (length b++) (length beams++))])
            (values b++ (+ count++ (if (zero? delta) 0 1))))
          (values (cons beam beams++) count++)))))

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
