#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Device-Graph (Immutable-HashTable Symbol (Listof Symbol)))
(define-type Numerical-Graph (Immutable-HashTable Index (Listof Index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define count-all-device-pathways : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (length (collect-pathways (read-device-graph /dev/aocin) 'you 'out))))

(define count-all-numerical-pathways : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define-values (G indices) (read-numerical-graph /dev/aocin))
    (define you-id (hash-ref indices 'you (λ [] #false)))
    (define out-id (hash-ref indices 'out (λ [] #false)))

    (if (and you-id out-id)
        (length (collect-pathways G you-id out-id))
        0)))

(define count-all-pathway-through-dac/fft : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define master : Device-Graph (read-device-graph /dev/aocin))
    (define all-pathways : (Listof (Listof Symbol)) (collect-pathways master 'svr 'out))

    (for/sum : Natural ([pathway (in-list all-pathways)]
                        #:when (and (memq 'dac pathway)
                                    (memq 'fft pathway)))
      1)))

(define count-all-through-dac/fft-via-dynamic-programming : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define-values (G indices) (read-numerical-graph /dev/aocin))
    (define start (hash-ref indices 'svr (λ [] #false)))
    (define end (hash-ref indices 'out (λ [] #false)))
    (define anchors : (Listof (Option Index))
      (for/list ([anchor (in-list '(fft dac))])
        (hash-ref indices anchor (λ [] #false))))
    
    (if (and start end (andmap index? anchors))
        (count-anchored-pathways G (hash-count indices) start end anchors)
        0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-device-graph : (-> Input-Port Device-Graph)
  (lambda [/dev/aocin]
    (for/fold ([devices : Device-Graph (hasheq)])
              ([line (in-port read-line /dev/aocin)])
      (define tokens : (Listof String) (string-split line #px":\\s*"))

      (if (pair? tokens)
          (hash-set devices (string->symbol (car tokens))
                    (map string->symbol (string-split (cadr tokens) #px"\\s+")))
          devices))))

(define read-numerical-graph : (-> Input-Port (Values Numerical-Graph (HashTable Symbol Index)))
  (lambda [/dev/aocin]
    (define indices : (HashTable Symbol Index) (make-hasheq))
    (define (create-index) : Index (hash-count indices))
    
    (values (for/fold ([graph : Numerical-Graph (hasheq)])
                      ([line (in-port read-line /dev/aocin)])
              (define tokens : (Listof String) (string-split line #px":\\s*"))
              
              (if (pair? tokens)
                  (hash-set graph (hash-ref! indices (string->symbol (car tokens)) create-index)
                            (for/list : (Listof Index) ([outlet (string-split (cadr tokens) #px"\\s+")])
                              (hash-ref! indices (string->symbol outlet)
                                         create-index)))
                  graph))
            indices)))

(define #:forall (Node) collect-pathways : (-> (HashTable Node (Listof Node)) Node Node (Listof (Listof Node)))
  (lambda [G start end]
    (let visit ([device : Node start]
                [tracks : (Listof Node) null])
      (for/fold ([pathways : (Listof (Listof Node)) null])
                ([outlet (in-list (hash-ref G device (inst list Node)))])
        (cond [(eq? outlet end) (cons (append tracks (list device outlet)) pathways)]
              [else (append pathways
                            (visit outlet (append tracks (list device))))])))))

(define count-anchored-pathways : (-> Numerical-Graph Index Index Index (Listof Index) Natural)
  (lambda [G n start end anchors]
    (define mask-count : Index (unsafe-idxlshift 1 (length anchors)))
    (define dp-states : (Vectorof Index) (make-vector (unsafe-idx* mask-count n) 0))
    (define anchor-indices : (Immutable-HashTable Index Index)
      (for/hasheq : (Immutable-HashTable Index Index) ([anchor (in-list anchors)]
                                                       [idx (in-naturals)])
        (values anchor (assert idx index?))))

    (define (dp-ref  [mask : Index] [device : Index]) : Index (vector-ref dp-states (+ device (unsafe-idx* mask n))))
    (define (dp-set! [mask : Index] [device : Index] [v : Index]) : Void (vector-set! dp-states (+ device (unsafe-idx* mask n)) v))

    (if (memq start anchors)
        (dp-set! (unsafe-idxlshift 1 (hash-ref anchor-indices start)) start 1)
        (dp-set! 0 start 1))

    (for* ([mask (in-range mask-count)]
           [device (in-list (topological-sort G n))])
      (with-asserts ([mask index?]
                     [device index?])
        (when (> (dp-ref mask device) 0)
          (for ([outlet (in-list (hash-ref G device (inst list Index)))])
            (define anchor-idx (hash-ref anchor-indices outlet (λ [] #false)))
            (define parent-count (dp-ref mask device))
            (if (and anchor-idx)
                (unless (bitwise-bit-set? mask anchor-idx)
                  (define this-mask (bitwise-ior mask (unsafe-idxlshift 1 anchor-idx)))
                  (dp-set! this-mask outlet (unsafe-idx+ (dp-ref this-mask outlet) parent-count)))
                (dp-set! mask outlet (unsafe-idx+ (dp-ref mask outlet) parent-count)))))))
    
    (dp-ref (unsafe-idx- mask-count 1) end)))

(define topological-sort : (-> Numerical-Graph Index (Listof Index))
  (lambda [G n]
    (define in:deg : (Vectorof Index) (make-vector n 0))
    
    (for* ([(device outlets) (in-hash G)]
           [outlet (in-list outlets)])
        (vector-set! in:deg outlet (unsafe-idx+ (vector-ref in:deg outlet) 1)))
    
    (define root-devices
      (for/list : (Listof Index) ([i (in-range n)]
                                  #:when (zero? (vector-ref in:deg i)))
        (assert i index?)))
    
    (let topo-sort ([secived : (Listof Index) null] 
                    [devices : (Listof Index) root-devices])
      (if (pair? devices)
          (let-values ([(device rest) (values (car devices) (cdr devices))])
            (topo-sort (cons device secived)
                  (for/fold ([erds : (Listof Index) rest])
                            ([outlet (in-list (hash-ref G device (inst list Index)))])
                    (vector-set! in:deg outlet (unsafe-idx- (vector-ref in:deg outlet) 1))
                    (if (zero? (vector-ref in:deg outlet))
                        (append erds (list outlet))
                        erds))))
          (reverse secived)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)

  (define-type Test-Case-Datum String)
  
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define testcase1 : (Listof Test-Case-Datum)
    '("aaa: you hhh"
      "you: bbb ccc"
      "bbb: ddd eee"
      "ccc: ddd eee fff"
      "ddd: ggg"
      "eee: out"
      "fff: out"
      "ggg: out"
      "hhh: ccc fff iii"
      "iii: out"))

  (define testcase2 : (Listof Test-Case-Datum)
    '("svr: aaa bbb"
      "aaa: fft"
      "fft: ccc"
      "bbb: tty"
      "tty: ccc"
      "ccc: ddd eee"
      "ddd: hub"
      "hub: fff"
      "eee: dac"
      "dac: fff"
      "fff: ggg hhh"
      "ggg: out"
      "hhh: out"))

  (define example1 : String (string-join ((inst map String Test-Case-Datum) values testcase1) (string #\newline)))
  (define example2 : String (string-join ((inst map String Test-Case-Datum) values testcase2) (string #\newline)))
  (define test1-ans1 : Integer 5)
  (define test1-ans2 : Integer 0)
  (define test2-ans1 : Integer 0)
  (define test2-ans2 : Integer 2)
  (define pzzl-ans1 : Integer 511)
  (define pzzl-ans2 : Integer 458618114529380)

  (define-feature AoC2025::Day11::Reactor #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "How many different paths lead from you to out?" #:do
        (context "device graph" #:do
          (it ["should produce ~a for the 1st example" test1-ans1] #:do
            (expect-= (call-with-input-string example1 count-all-device-pathways)
                      test1-ans1))
          (it ["should produce ~a for the 2nd example" test2-ans1] #:do
            (expect-= (call-with-input-string example2 count-all-device-pathways)
                      test2-ans1))
          (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
            (expect-= (call-with-input-file input.aoc count-all-device-pathways)
                      pzzl-ans1)))
        (context "numerical graph" #:do
          (it ["should produce ~a for the 1st example" test1-ans1] #:do
            (expect-= (call-with-input-string example1 count-all-numerical-pathways)
                      test1-ans1))
          (it ["should produce ~a for the 2nd example" test2-ans1] #:do
            (expect-= (call-with-input-string example2 count-all-numerical-pathways)
                      test2-ans1))
          (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
            (expect-= (call-with-input-file input.aoc count-all-numerical-pathways)
                      pzzl-ans1))))
      (describe "How many of those paths visit both dac and fft?" #:do
        (it ["should produce ~a for the 1st example via collecting" test1-ans2] #:do
          (expect-= (call-with-input-string example1 count-all-pathway-through-dac/fft)
                    test1-ans2))
        (it ["should produce ~a for the 2nd example via collecting" test2-ans2] #:do
          (expect-= (call-with-input-string example2 count-all-pathway-through-dac/fft)
                    test2-ans2))
        (it ["should produce ~a for the 2nd example via dynamic programming" test2-ans2] #:do
          (expect-= (call-with-input-string example2 count-all-through-dac/fft-via-dynamic-programming)
                    test2-ans2))
        (it ["should produce ~a for the puzzle via dynamic programming" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc count-all-through-dac/fft-via-dynamic-programming)
                    pzzl-ans2)))))
    
  (void (spec-prove AoC2025::Day11::Reactor)))
