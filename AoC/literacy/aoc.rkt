#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out typed/racket/base))

(require digimon/digitama/system)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (with-aoc-data-from stx)
  (syntax-case stx []
    [(_ path #:do f argv ...)
     (syntax/loc stx
       (call-with-input-file* (digimon-path 'tamer path)
         (lambda [[/dev/stdin : Input-Port]]
           (f /dev/stdin argv ...))))]))

(define-syntax (read-aoc-data stx)
  (syntax-case stx []
    [(_ #:from path #:with read-datum #:for-each-do f argv ...)
     (syntax/loc stx
       (call-with-input-file* (digimon-path 'tamer path)
         (lambda [[/dev/stdin : Input-Port]]
           (for/list : (Listof Out) ([datum (in-port read-datum /dev/stdin)])
             (f datum argv ...)))))]
    [(_ #:from path #:for-each-do f argv ...)
     (syntax/loc stx
       (read-aoc-data #:from path #:with read-single-line
                      #:for-each-do f argv ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-single-line : (-> Input-Port (U String EOF))
  (lambda [/dev/stdin]
    (read-line /dev/stdin 'any)))

(define make-read-lines : (-> Byte (-> Input-Port (U EOF (Listof String))))
  (lambda [n]
    (λ [[/dev/stdin : Input-Port]]
      (let read-n-lines ([ls : (Listof String) null]
                         [rest : Integer n])
        (cond [(<= rest 0) (if (null? ls) eof (reverse ls))]
              [else (let ([line (read-single-line /dev/stdin)])
                      (cond [(string? line) (read-n-lines (cons line ls) (sub1 rest))]
                            [(null? ls) eof]
                            [else (reverse ls)]))])))))
