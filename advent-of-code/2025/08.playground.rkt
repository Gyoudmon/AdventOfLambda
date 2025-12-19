#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type JBox Point3D)

(struct playground
  ([jboxes : (Vectorof JBox)]
   [roots : (Vectorof Index)]
   [ranks : (Vectorof Index)]
   [sorted-edges : (Vectorof (Pairof Index Index))])
  #:transparent
  #:type-name Playground)

(define playground-root-ref : (-> Playground Index Index)
  (lambda [master id]
    (define roots (playground-roots master))

    (let find ([id : Index id])
      (define parent (vector-ref roots id))

      (cond [(= parent id) id]
            [else (let ([parent (find parent)])
                    (vector-set! roots id parent)
                    parent)]))))

(define playground-connect! : (-> Playground (Pairof Index Index) Boolean)
  (lambda [master edge]
    (define roots (playground-roots master))
    (define ranks (playground-ranks master))
    
    (define root-a (playground-root-ref master (car edge)))
    (define root-b (playground-root-ref master (cdr edge)))
    
    (and (not (= root-a root-b))
         (let ([merged-rank (assert (+ (vector-ref ranks root-a) (vector-ref ranks root-b)) index?)])      
           (if (< (vector-ref ranks root-a) (vector-ref ranks root-b))
               (begin (vector-set! roots root-a root-b)
                      (vector-set! ranks root-b merged-rank))
               (begin (vector-set! roots root-b root-a)
                      (vector-set! ranks root-a merged-rank)))
           #true))))

(define playground-circuit-okay? : (-> Playground Boolean)
  (lambda [master]
    (define roots : (Vectorof Index) (playground-roots master))
    (define size : Index (vector-length roots))

    ;;; WARNING: ensure all paths are compressed
    (for ([idx (in-range (vector-length roots))])
      (playground-root-ref master (assert idx index?)))

    (= size (vector-count (λ [[v : Index]] (= (vector-ref roots 0) v)) roots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define kruskal-circuits-solve : (-> Input-Port Index Natural)
  (lambda [/dev/aocin n]
    (define master (read-playground /dev/aocin))
    
    (for ([edge (in-vector (playground-sorted-edges master))]
          [idx (in-range n)])
      (playground-connect! master edge))
    
    (for/product : Natural ([circuit (in-list (playground-largest-circuits master 3))])
      (length circuit))))

(define the-kruskal-circuit-solve : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define master (read-playground /dev/aocin))
    (define jboxes (playground-jboxes master))
    
    (or (for/or : (Option Natural) ([edge (playground-sorted-edges master)])
          (and (playground-connect! master edge)
               (playground-circuit-okay? master)
               (* (point3d-x (vector-ref jboxes (car edge)))
                  (point3d-x (vector-ref jboxes (cdr edge))))))
        0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-playground : (-> Input-Port Playground)
  (lambda [/dev/aocin]
    (define jboxes
      (for/vector : (Vectorof JBox) ([pos (in-port read-point3d /dev/aocin)])
        pos))

    (define indices (generate-matices jboxes))

    (playground jboxes
                (build-vector (vector-length jboxes) (inst values Index))
                (make-vector (vector-length jboxes) 1)
                indices)))

(define generate-matices : (-> (Vectorof JBox) (Values (Vectorof (Pairof Index Index))))
  (lambda [jboxes]
    (define N (vector-length jboxes))
    (define edge-indices : (Listof (Pairof Index Index))
      (for*/list : (Listof (Pairof Index Index)) ([r (in-range N)]
                                                  [c (in-range r)])
        (cons (assert r index?) (assert c index?))))

    (define sorted-edges : (Listof (Pairof Index Index))
      ((inst sort (Pairof Index Index) Real)
       edge-indices <
       #:key (λ [[edge : (Pairof Index Index)]] : Real
               (squared-distance (vector-ref jboxes (car edge))
                                 (vector-ref jboxes (cdr edge))))
       #:cache-keys? #true))

    (list->vector sorted-edges)))

(define playground-largest-circuits : (-> Playground Index (Listof (Listof Index)))
  (lambda [master n]
    (define roots : (Vectorof Index) (playground-roots master))

    (define circuits : (HashTable Index (Listof Index))
      (for/fold ([circuits : (HashTable Index (Listof Index)) (hasheq)])
                ([id (in-range (vector-length roots))])
        (with-asserts ([id index?])
          (define root (playground-root-ref master id))
          (hash-set circuits root
                    (cons id (hash-ref circuits root
                                       (inst list Index)))))))

    (define largest-circuits : (Listof (Listof Index))
      ((inst sort (Listof Index) Index) (hash-values circuits) > #:key length))
    
    (if (> (length largest-circuits) n)
        (take largest-circuits n)
        largest-circuits)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define-type Test-Case-Datum String)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define testcases : (Listof Test-Case-Datum)
    '("162,817,812"
      "57,618,57"
      "906,360,560"
      "592,479,940"
      "352,342,300"
      "466,668,158"
      "542,29,236"
      "431,825,988"
      "739,650,466"
      "52,470,668"
      "216,146,977"
      "819,987,18"
      "117,168,530"
      "805,96,715"
      "346,949,466"
      "970,615,88"
      "941,993,340"
      "862,61,35"
      "984,92,344"
      "425,690,689"))

  (define example : String (string-join ((inst map String Test-Case-Datum) values testcases) (string #\newline)))
  (define test-ans1 : Integer 40)
  (define test-ans2 : Integer 25272)
  (define pzzl-ans1 : Integer 54600)
  (define pzzl-ans2 : Integer 107256172)
  
  (define-feature AoC2025::Day08::Playground #:do
    (describe "what do you get if you multiply together the sizes of the three largest circuits?" #:do
      (it ["should produce ~a for the example" test-ans1] #:do
        ($ kruskal-circuits-solve 10 #:< example #:=> test-ans1))
      (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
        ($ kruskal-circuits-solve 1000 #:=> pzzl-ans1)))
    (describe "What do you get if you multiply together the X coordinates of the last two junction boxes you need to connect?" #:do
      (it ["should produce ~a for the example" test-ans2] #:do
        (expect-= (call-with-input-string example the-kruskal-circuit-solve)
                  test-ans2))
      (it ["should produce ~a for the puzzle" pzzl-ans2] #:do
        ($ the-kruskal-circuit-solve #:=> pzzl-ans2))))
    
  (void (spec-prove AoC2025::Day08::Playground)))
