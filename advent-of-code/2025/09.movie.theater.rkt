#lang typed/racket/base

(require "../aoc.rkt")

;;; @link https://adventofcode.com/2025/day/9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Scan-Line-Database (Immutable-HashTable Integer Scan-Line))

(struct scan-line
  ([min : Natural]
   [max : Natural]
   [intermediates : (Listof Natural)])
  #:type-name Scan-Line
  #:transparent)

(struct sentry-line
  ([start : Natural]
   [span : Natural]
   [position : Natural])
  #:type-name Sentry-Line
  #:transparent)

(struct rect
  ([area : Natural]
   [vertices : (Listof Complex)])
  #:type-name Rect
  #:transparent)

(define scanline0 (scan-line 0 0 null))
(define λscanline0 (λ [] scanline0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-largest-red-rectangle : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define vertices
      (for/vector : (Vectorof Point2D) ([pos (in-port read-point2d /dev/aocin)])
        pos))

    (define N (vector-length vertices))
    (define areas : (Listof Natural)
      (sort (for*/list : (Listof Natural) ([r (in-range N)]
                                           [c (in-range r)])
              (red-tile-area (vector-ref vertices r)
                             (vector-ref vertices c)))
            >))

    (if (pair? areas) (car areas) 0)))

(define find-largest-inscribed-rectangle-for-convect-polygon : (-> Input-Port Natural)
  (lambda [/dev/aocin]
    (define-values (vertices xscnls yscnls sentries) (read-red-vertices /dev/aocin))
    (define N (vector-length vertices))

    (define areas : (Listof Natural)
      (sort (for*/list : (Listof Natural) ([r (in-range N)]
                                           [c (in-range r)])
              (or (inscribed-area (vector-ref vertices r)
                                                        (vector-ref vertices c)
                                                        xscnls yscnls)
                  0))
            >))
    
    (if (pair? areas) (car areas) 0)))

(define find-largest-inscribed-rectangle-for-the-puzzle : (->* (Input-Port)
                                                               ((Option (Boxof (Listof Complex)))
                                                                (Option (Boxof (Vectorof Rect)))
                                                                (Option (Boxof Scan-Line-Database))
                                                                (Option (Boxof Scan-Line-Database)))
                                                               Natural)
  (lambda [/dev/aocin [&vertices #false] [&rectangles #false] [&xline #false] [&yline #false]]
    (define-values (vertices xscnls yscnls sentries) (read-red-vertices /dev/aocin))
    (define N (vector-length vertices))

    (define rects : (Listof Rect)
      ((inst sort Rect Natural)
       (for*/fold ([rects : (Listof Rect) null])
                  ([r (in-range N)]
                   [c (in-range r)])
         (define rpt (vector-ref vertices r))
         (define cpt (vector-ref vertices c))
         (define-values (lx ty) (values (point2d-x rpt) (point2d-y rpt)))
         (define-values (rx by) (values (point2d-x cpt) (point2d-y cpt)))
         (define area : (Option Natural)
           (and (vertices-okay? lx ty rx by sentries)
                (inscribed-area rpt cpt xscnls yscnls)))

         (if (and area)
             (cons (rect area
                         (let-values ([(lx ty) (values (point2d-x rpt) (point2d-y rpt))]
                                      [(rx by) (values (point2d-x cpt) (point2d-y cpt))])
                           (list (make-rectangular lx ty) (make-rectangular rx ty)
                                 (make-rectangular rx by) (make-rectangular lx by))))
                   rects)
             rects))
       >
       #:key rect-area))

    (when (and &vertices)
      (set-box! &vertices
                (for/list : (Listof Complex) ([pos (in-vector vertices)])
                     (make-rectangular (point2d-x pos)
                                       (point2d-y pos)))))

    (when (and &rectangles) (set-box! &rectangles (list->vector rects)))
    (when (and &xline) (set-box! &xline xscnls))
    (when (and &yline) (set-box! &yline yscnls))
    
    (if (pair? rects) (rect-area (car rects)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define red-tile-area : (case-> [Point2D Point2D -> Natural]
                                [Integer Integer Integer Integer -> Natural])
  (case-lambda
    [(lt rb) (red-tile-area (point2d-x lt) (point2d-y lt) (point2d-x rb) (point2d-y rb))]
    [(lx ty rx by) (* (+ (abs (- rx lx)) 1)
                      (+ (abs (- by ty)) 1))]))

(define inscribed-area : (case-> [Point2D Point2D Scan-Line-Database Scan-Line-Database -> (Option Natural)]
                                 [Integer Integer Integer Integer Scan-Line-Database Scan-Line-Database -> (Option Natural)])
  (case-lambda
    [(lt rb xscnls yscnls)
     (inscribed-area (point2d-x lt) (point2d-y lt) (point2d-x rb) (point2d-y rb) xscnls yscnls)]
    [(lx ty rx by xscnls yscnls)
     (and (vertex-inscribed? lx by xscnls)
          (vertex-inscribed? rx ty xscnls)
          (edge-inscribed? ty by lx rx yscnls)
          (red-tile-area lx ty rx by))]))

(define vertex-inscribed? : (case-> [Integer Integer Scan-Line-Database -> Boolean])
  (lambda [v pos scnls]
    (let ([span (hash-ref scnls v λscanline0)])
      (<= (scan-line-min span) pos (scan-line-max span)))))

(define edge-inscribed? : (case-> [Integer Integer Integer Scan-Line-Database -> Boolean]
                                  [Integer Integer Integer Integer Scan-Line-Database -> Boolean])
  (case-lambda
    [(v pmin pmax scnls)
     (if (<= pmin pmax)
         (let ([yrng (hash-ref scnls v λscanline0)])
           (<= (scan-line-min yrng) pmin pmax (scan-line-max yrng)))
         (edge-inscribed? v pmax pmin scnls))]
    [(start end pmin pmax scnls)
     (cond [(> start end) (edge-inscribed? end start pmin pmax scnls)]
           [else (for/and : Boolean ([x (in-range (+ start 1) end)]
                                     #:when (hash-has-key? scnls x))
                   (edge-inscribed? x pmin pmax scnls))])]))

(define read-red-vertices : (-> Input-Port (Values (Vectorof Point2D) Scan-Line-Database Scan-Line-Database (Listof Sentry-Line)))
  (lambda [/dev/aocin]
    (for/fold ([head : (Option Point2D) #false]
               [px : Natural 0]
               [py : Natural 0]
               [xscnls : Scan-Line-Database (hasheq)]
               [yscnls : Scan-Line-Database (hasheq)]
               [vertices : (Listof Point2D) null]
               [sentries : (Listof Sentry-Line) null]
               #:result (let ([vs (list->vector (reverse vertices))]
                              [ss (sentry-line-filter sentries)])
                          (if (and head)
                              (let-values ([(xsl ysl) (scan-line-move-to px py head xscnls yscnls)])
                                (values vs xsl ysl ss))
                              (values vs xscnls yscnls ss))))
              ([pos (in-port read-point2d /dev/aocin)])
      (define-values (cx cy) (values (point2d-x pos) (point2d-y pos)))
      (define-values (xsl ysl)
        (cond [(not head) (values xscnls yscnls)]
              [else (scan-line-move-to px py cx cy xscnls yscnls)]))

      (values (or head pos) cx cy xsl ysl
              (cons pos vertices)
              (cond [(null? vertices) sentries]
                    [else (let* ([span (- cx px)])
                            (cond [(not (= py cy)) sentries]
                                  [(< span 0) (cons (sentry-line cx (abs span) cy) sentries)]
                                  [else (cons (sentry-line px span cy) sentries)]))])))))

(define scan-line-move-to : (case-> [Natural Natural Point2D Scan-Line-Database Scan-Line-Database -> (Values Scan-Line-Database Scan-Line-Database)]
                                    [Natural Natural Natural Natural Scan-Line-Database Scan-Line-Database -> (Values Scan-Line-Database Scan-Line-Database)]
                                    [Natural Natural Natural Scan-Line-Database -> Scan-Line-Database])
  (case-lambda
    [(px py cx cy xscnls yscnls)
     (values (if (= py cy)
                 (scan-line-move-to px cx cy xscnls)
                 xscnls)
             (if (= px cx)
                 (scan-line-move-to py cy cx yscnls)
                 yscnls))]
    [(px py dot xscnls yscnls)
     (scan-line-move-to px py (point2d-x dot) (point2d-y dot) xscnls yscnls)]
    [(pv cv pos scnls)
     (if (< pv cv)
         (let move ([v : Natural pv]
                    [scnls : Scan-Line-Database scnls])
           (if (<= v cv)
               (move (+ v 1) (scan-line-update scnls v pos))
               scnls))
         (scan-line-move-to cv pv pos scnls))]))

(define scan-line-update : (case-> [Scan-Line Natural -> Scan-Line]
                                   [Scan-Line-Database Natural Natural -> Scan-Line-Database])
  (case-lambda
    [(scnls v pos)
     (hash-set scnls v
               (let ([self (hash-ref scnls v (λ [] #false))])
                 (if (or self)
                     (scan-line-update self pos)
                     (scan-line pos pos null))))]
    [(self pos)
     (scan-line (min pos (scan-line-min self))
                (max pos (scan-line-max self))
                null)]))

(define sentry-line-filter : (-> (Listof Sentry-Line) (Listof Sentry-Line))
  (lambda [lines]
    (define sorted-lines ((inst sort Sentry-Line Natural) lines > #:key sentry-line-span))

    (if (> (length sorted-lines) 2)
        (take sorted-lines 2)
        sorted-lines)))

(define vertices-okay? : (-> Natural Natural Natural Natural (Listof Sentry-Line) Boolean)
  (lambda [lx ty rx by sentries]
    (if (<= ty by)
        (for/and : Boolean ([sentry (in-list sentries)])
          (define y (sentry-line-position sentry))
          
          (or (and (< ty y) (<= by y))
              (and (>= ty y) (> by y))))
        (vertices-okay? rx by lx ty sentries))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/spec)
  (require syntax/location)
  (require plotfun)

  (define-type Test-Case-Datum String)
  
  (define input.aoc (path-replace-suffix (quote-source-file #'this) #".aoc"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define testcases : (Listof Test-Case-Datum)
    '("7,1"
      "11,1"
      "11,7"
      "9,7"
      "9,5"
      "2,5"
      "2,3"
      "7,3"))

  (define example : String (string-join ((inst map String Test-Case-Datum) values testcases) (string #\newline)))
  (define test-ans1 : Integer 50)
  (define test-ans2 : Integer 24)
  (define pzzl-ans1 : Integer 4744899849)
  (define pzzl-ans2 : Integer 1540192500)

  (define make-puzzle-visualize : (-> Index Index Byte Byte Boolean Boolean (-> Input-Port Plot:Cartesian))
    (lambda [width range start end display-xscnls? display-yscnls?]
      (λ [/dev/aocin]
        (define &dots : (Boxof (Listof Complex)) (box null))
        (define &rects : (Boxof (Vectorof Rect)) (box (vector)))
        (define &xscanline : (Boxof Scan-Line-Database) (box ((inst hasheq Integer Scan-Line))))
        (define &yscanline : (Boxof Scan-Line-Database) (box ((inst hasheq Integer Scan-Line))))
        
        (find-largest-inscribed-rectangle-for-the-puzzle /dev/aocin &dots &rects &xscanline &yscanline)

        (plot-cartesian #:width width #:screen? #true #:background 'White
                        #:x-range (cons 0 range) #:y-range (cons 0 range)
                        (list (lines (unbox &dots) #:close? #true)
                              (for/list : (Listof Plot-Visualizer) ([self (in-vector (unbox &rects) start end)])
                                (displayln (rect-area self))
                                (lines #:close? #true #:label (number->string (rect-area self))
                                       (rect-vertices self)))
                              (and display-xscnls?
                                   (for/list : (Listof Plot-Visualizer) ([(x line) (in-hash (unbox &xscanline))])
                                     (lines #:close? #true #:label (format "x=~a" x)
                                            (list (make-rectangular x (scan-line-min line))
                                                  (make-rectangular x (scan-line-max line))))))
                              (and display-yscnls?
                                   (for/list : (Listof Plot-Visualizer) ([(y line) (in-hash (unbox &yscanline))])
                                     (lines #:close? #true #:label (format "y=~a" y)
                                            (list (make-rectangular (scan-line-min line) y)
                                                  (make-rectangular (scan-line-max line) y))))))))))

  (call-with-input-string example (make-puzzle-visualize 400     12 0  4 #false #false))
  (call-with-input-file input.aoc (make-puzzle-visualize 800 100000 0 16 #false #false))
  
  (define-feature AoC2025::Day09::Movie.Theata #:do
    (describe "collect stars by solving puzzles" #:do
      (describe "what is the largest area of any rectangle you can make?" #:do
        (it ["should produce ~a for the example" test-ans1] #:do
          (expect-= (call-with-input-string example find-largest-red-rectangle)
                    test-ans1))
        (it ["should produce ~a for the puzzle" pzzl-ans1] #:do
          (expect-= (call-with-input-file input.aoc find-largest-red-rectangle)
                    pzzl-ans1)))
      (describe "what is the largest area of any rectangle you can make using only red and green tiles?" #:do
        (it ["should produce ~a for the example" test-ans2] #:do
          (expect-= (call-with-input-string example find-largest-inscribed-rectangle-for-convect-polygon)
                    test-ans2))
        (it ["should produce ~a for the puzzle" pzzl-ans2] #:do
          (expect-= (call-with-input-file input.aoc find-largest-inscribed-rectangle-for-the-puzzle)
                    pzzl-ans2)))))
    
  (void (spec-prove AoC2025::Day09::Movie.Theata)))
