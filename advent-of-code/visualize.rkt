#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out geofun/vector))

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (V L) aoc-array : (->* ((Vectorof V) (Vectorof L))
                                        ((Option (-> Index V (U String (Pairof (Option String) (Option Color)) False)))
                                         (Option (-> Index L (Option String))))
                                        Geo)
  (lambda [array label-descs [desc-value #false] [desc-label #false]]
    (define labels : (Listof Geo)
      (for/list : (Listof Geo) ([d (in-vector label-descs)]
                                [idx (in-naturals)])
        (cond [(not desc-label) (geo-text d #:alignment 'center)]
              [else (geo-text (or (desc-label (assert idx index?) d) d) #:alignment 'center)])))

    (define cell
      (for/list : (Listof Geo) ([col (in-vector array)]
                                [c (in-naturals)])
        (define-values (value color)
          (cond [(not desc-value) (values col #false)]
                [else (let ([v (desc-value (assert c index?) col)])
                        (cond [(string? v) (values v #false)]
                              [(not v) (values col #false)]
                              [else (values (or (car v) col) (cdr v))]))]))
        
        (cond [(not color) (geo-text value #:alignment 'center)]
              [else (geo-text value #:color color #:alignment 'center)])))

    (geo-table* (cons labels (list cell))
                'cc 'cc 4.0 32.0)))

(define #:forall (V L) aoc-matrix : (->* ((Vectorof (Vectorof V)) (Vectorof L))
                                         ((Option (-> Index Index V (U String (Pairof (Option String) (Option Color)) False)))
                                          (Option (-> Index L (Option String))))
                                         Geo)
  (lambda [matrix label-descs [desc-value #false] [desc-label #false]]
    (define labels : (Listof Geo)
      (for/list : (Listof Geo) ([d (in-vector label-descs)]
                                [idx (in-naturals)])
        (cond [(not desc-label) (geo-text d #:alignment 'center)]
              [else (geo-text (or (desc-label (assert idx index?) d) d) #:alignment 'center)])))

    (define cell
      (for/list : (Listof (Listof Geo)) ([row (in-vector matrix)]
                                         [lbl (in-list labels)]
                                         [r (in-naturals)])
        (cons lbl
              (for/list : (Listof Geo) ([col (in-vector row)]
                                        [c (in-naturals)])
                (define-values (value color)
                  (cond [(not desc-value) (values col #false)]
                        [else (let ([v (desc-value (assert r index?) (assert c index?) col)])
                                (cond [(string? v) (values v #false)]
                                      [(not v) (values col #false)]
                                      [else (values (or (car v) col) (cdr v))]))]))
                
                (cond [(not color) (geo-text value #:alignment 'center)]
                      [else (geo-text value #:color color #:alignment 'center)])))))

    (geo-table* (cons (cons #false labels) cell)
                'cc 'cc 4.0 32.0)))
