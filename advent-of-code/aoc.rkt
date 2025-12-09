#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/digitama/unsafe/ops))
(provide (all-from-out digimon/character))
(provide (all-from-out racket/string racket/list racket/vector racket/set))
(provide (all-from-out racket/port))

(require digimon/digitama/unsafe/ops)
(require digimon/character)

(require racket/string)
(require racket/vector)
(require racket/port)
(require racket/list)
(require racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct point3d
  ([x : Natural]
   [y : Natural]
   [z : Natural])
  #:transparent
  #:type-name Point3D)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-point2d : (-> Input-Port (U EOF Complex))
  (lambda [/dev/aocin]
    (define x (read-natural /dev/aocin))

    (or (and (exact-nonnegative-integer? x)
             (let ([y (read-natural /dev/aocin)])
               (and (exact-nonnegative-integer? y)
                 (make-rectangular x y))))
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

(define squared-distance : (-> Point3D Point3D Natural)
  (lambda [p1 p2]
    (define dx (- (point3d-x p2) (point3d-x p1)))
    (define dy (- (point3d-y p2) (point3d-y p1)))
    (define dz (- (point3d-z p2) (point3d-z p1)))
    
    (abs (+ (* dx dx) (* dy dy) (* dz dz)))))

(define point->string : (-> (U Complex Point3D) String)
  (lambda [v]
    (if (point3d? v)
        (format "(~a, ~a, ~a)" (point3d-x v) (point3d-y v) (point3d-z v))
        (format "(~a, ~a)" (real-part v) (imag-part v)))))

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
