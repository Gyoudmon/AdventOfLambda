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
(define read-range : (-> Input-Port (U EOF (Pairof Index Index)))
  (lambda [/dev/aocin]
    (define start (read-id /dev/aocin))

    (cond [(eof-object? start) eof]
          [(index? start)
           (let ([stop (read-id /dev/aocin)])
             (if (index? stop)
                 (cons start stop)
                 eof))]
          [else eof])))

(define read-maybe-range : (-> Input-Port (U False EOF (Pairof Index Index)))
  (lambda [/dev/aocin]
    (define start (read-id /dev/aocin))

    (cond [(eof-object? start) eof]
          [(index? start)
           (let ([stop (read-id /dev/aocin)])
             (if (index? stop)
                 (cons start stop)
                 eof))]
          [else #false])))

(define read-id : (-> Input-Port (U EOF Natural False))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
