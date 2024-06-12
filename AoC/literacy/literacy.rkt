#lang racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))

(require digimon/tamer)
(require digimon/syntax)
(require digimon/collection)

(require scribble/manual)

(require (for-syntax racket/list))
(require (for-syntax racket/symbol))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Just in case for README.md
(enter-digimon-zone!)

;(tamer-indexed-block-hide-chapter-index #true)

(tamer-default-figure-label "图")
(tamer-default-figure-label-separator #false)
(tamer-default-figure-label-tail ". ")
(tamer-default-figure-label-style 'bold)

(tamer-story-submodule-name 'advent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (aoc-task stx)
  (syntax-parse stx #:datum-literals []
    [(_ year day title ...)
     (syntax/loc stx
       (handbook-story (hyperlink (format "https://adventofcode.com/~a/day/~a" year day)
                                  title ...)))]))

(define-syntax (aoc-desc stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~once (~seq #:keywords [kw ...]))
              (~once (~seq #:edition [edition date])))
        ...)
     (syntax/loc stx
       (itemlist #:style 'compact
                 (item (add-between #:splice? #true
                                    #:before-first (list (emph "关键词") ":" ~)
                                    (for/list ([key (in-list (list kw ...))])
                                      (racketkeywordfont (tech key)))
                                    (list "," ~)))
                 (item (list (emph "时间戳") ": " (tt date) ~ (format "第~a版" 'edition)))))]))

(define-syntax ($argv stx)
  (syntax-case stx []
    [(_ [aname desc ...] [aname:rest desc:rest ...] ...)
     (with-syntax* ([name (datum->syntax #'aname (symbol->immutable-string (syntax-e #'aname)))]
                    [(name:rest ...) (for/list ([<name> (in-list (syntax->list #'(aname:rest ...)))])
                                       (datum->syntax <name> (symbol->immutable-string (syntax-e <name>))))]
                    [maxlength (apply max (map string-length (map syntax-e (syntax->list #'(name name:rest ...)))))])
       (syntax/loc stx
         (list (elem (hspace (- maxlength (string-length name)))
                     (racketvarfont name) ~ desc ...)
               (elem (linebreak) "; " (hspace (- maxlength (string-length name:rest)))
                     (racketvarfont name:rest) ~ desc:rest ...) ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define story
  (lambda argv
    (inset-flow (apply racketplainfont (list* (hspace 4) argv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desc
  (lambda argv
    (apply racketmetafont argv)))

(define name
  (lambda [arg]
    (tamer-deftech arg)))

(define term-name
  (lambda [arg]
    (defterm arg)))

(define variable
  (lambda argv
    (apply racketvarfont argv)))

(define sign
  (lambda argv
    (apply racketparenfont argv)))

(define id
  (lambda argv
    (apply racketidfont argv)))

(define type
  (lambda body
    (apply racketvalfont body)))

(define form
  (lambda argv
    (apply racketkeywordfont argv)))

(define idea
  (lambda argv
    (apply racketoutput argv)))

(define focus
  (lambda argv
    (apply racketvalfont argv)))

(define thus
  (lambda argv
    (apply racketresultfont argv)))

(define note
  (lambda argv
    (apply racketcommentfont argv)))

(define fallacy
  (lambda argv
    (apply racketerror argv)))

(define question
  (lambda argv
    (apply racketparenfont argv)))
