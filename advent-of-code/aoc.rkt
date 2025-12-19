#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/spec))
(provide (all-from-out digimon/digitama/unsafe/ops))
(provide (all-from-out digimon/character digimon/format))
(provide (all-from-out racket/string racket/list racket/vector racket/set))
(provide (all-from-out racket/port))

(require digimon/digitama/unsafe/ops)
(require digimon/character)
(require digimon/format)
(require digimon/spec)

(require racket/string)
(require racket/vector)
(require racket/port)
(require racket/list)
(require racket/set)

(require syntax/location)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (/dev/aocin stx)
  (syntax-case stx []
    [(_ ext) #`(path-replace-suffix (quote-source-file #,stx) ext)]
    [(_ here ext) #'(path-replace-suffix (quote-source-file here) ext)]))

(define-syntax ($ stx)
  (syntax-parse stx #:datum-literals [< > >>]
    [(_ f argv:expr ... (~optional (~seq #:< in) #:defaults ([in #'#".aoc"])) (~seq #:=> answer))
     (syntax/loc stx
       (expect-= ($ f argv ... #:< in) answer))]
    [(_ f argv:expr ... (~optional (~seq #:< in) #:defaults ([in #'#".aoc"])))
     (syntax/loc stx
       (if (string? in)
           (call-with-input-string in
             (lambda [[/dev/stdin : Input-Port]]
               (f /dev/stdin argv ...)))
           (call-with-input-file* (/dev/aocin f in)
             (lambda [[/dev/stdin : Input-Port]]
               (f /dev/stdin argv ...)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct point2d
  ([x : Natural]
   [y : Natural])
  #:transparent
  #:type-name Point2D)

(struct point3d
  ([x : Natural]
   [y : Natural]
   [z : Natural])
  #:transparent
  #:type-name Point3D)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-point2d : (-> Input-Port (U EOF Point2D))
  (lambda [/dev/aocin]
    (define x (read-natural /dev/aocin))

    (or (and (exact-nonnegative-integer? x)
             (let ([y (read-natural /dev/aocin)])
               (and (exact-nonnegative-integer? y)
                    (point2d x y))))
        eof)))

(define read-point3d : (-> Input-Port (U EOF Point3D))
  (lambda [/dev/aocin]
    (define x (read-natural /dev/aocin))

    (or (and (exact-nonnegative-integer? x)
             (let ([y (read-natural /dev/aocin)])
               (and (exact-nonnegative-integer? y)
                    (let ([z (read-natural /dev/aocin)])
                      (and (exact-nonnegative-integer? z)
                           (point3d x y z))))))
        eof)))

(define point->string : (-> (U Point2D Point3D) String)
  (lambda [v]
    (if (point3d? v)
        (format "(~a, ~a, ~a)" (point3d-x v) (point3d-y v) (point3d-z v))
        (format "(~a, ~a)" (point2d-x v) (point2d-y v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-range : (-> Input-Port (U EOF (Pairof Index Index)))
  (lambda [/dev/aocin]
    (define start (read-natural /dev/aocin))

    (cond [(eof-object? start) eof]
          [(index? start)
           (let ([stop (read-natural /dev/aocin)])
             (if (index? stop)
                 (cons start stop)
                 eof))]
          [else eof])))

(define read-maybe-range : (-> Input-Port (U False EOF (Pairof Index Index)))
  (lambda [/dev/aocin]
    (define start (read-natural /dev/aocin))

    (cond [(eof-object? start) eof]
          [(index? start)
           (let ([stop (read-natural /dev/aocin)])
             (if (index? stop)
                 (cons start stop)
                 eof))]
          [else #false])))

(define read-natural : (-> Input-Port (U EOF Natural False))
  (lambda [/dev/aocin]
    (define digit : (U EOF Char) (peek-char /dev/aocin))

    (if (char? digit)
        (let read-id ([id : (Option Natural) #false])
          (define digit : (U EOF Char) (read-char /dev/aocin))
          
          (if (and (char? digit) (char<=? #\0 digit #\9))
              (read-id (+ (* (or id 0) 10)
                          (char->octadecimal digit)))
              id))
        eof)))

(define read-text : (-> Input-Port Char Char (U EOF String))
  (lambda [/dev/aocin open close]
    (regexp-match #px"\\s*" /dev/aocin)

    (define ch (peek-char /dev/aocin))

    (if (and (char? ch) (char=? open ch))
        (begin (read-char /dev/aocin)
               (let read-text ([chars : (Listof Char) null])
                 (define ch (read-char /dev/aocin))
                 (if (char? ch)
                     (if (char=? close ch)
                         (apply string (reverse chars))
                         (read-text (cons ch chars)))
                     eof)))
        eof)))

(define read-consecutive-lines : (-> Input-Port (U EOF (Pairof String (Listof String))))
  (lambda [/dev/aocin]
    (define head-line (read-line /dev/aocin))

    (if (string? head-line)
        (let read-next ([senil : (Pairof String (Listof String)) (list head-line)])
          (define line (read-line /dev/aocin))

          (if (or (eof-object? line) (regexp-match? #px"^\\s*$" line))
              (let ([lines (reverse senil)])
                (assert lines pair?))
              (read-next (cons line senil))))
        eof)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Z) string->numbers : (-> String (U String Regexp) (-> (Option Complex) Boolean : Z) (Listof Z))
  (lambda [s px z?]
    (filter z? (map string->number (string-split s px)))))

(define string->byte-numbers : (-> String (U String Regexp) (Listof Byte))
  (lambda [s px]
    (string->numbers s px byte?)))

(define string->index-numbers : (->* (String) ((U String Regexp)) (Listof Index))
  (lambda [s [px #px"\\s*,\\s*"]]
    (string->numbers s px index?)))

(define string->natural-numbers : (->* (String) ((U String Regexp)) (Listof Natural))
  (lambda [s [px #px"\\s*,\\s*"]]
    (string->numbers s px exact-nonnegative-integer?)))

(define squared-distance : (-> Point3D Point3D Natural)
  (lambda [p1 p2]
    (define dx (- (point3d-x p2) (point3d-x p1)))
    (define dy (- (point3d-y p2) (point3d-y p1)))
    (define dz (- (point3d-z p2) (point3d-z p1)))
    
    (abs (+ (* dx dx) (* dy dy) (* dz dz)))))
