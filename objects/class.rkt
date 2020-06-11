#lang racket/base
(require (rename-in "with-method.rkt"               
                    [class raw:class])
         syntax/parse/define
         racket/stxparam
         (for-syntax racket/base))

(provide class
         make-object
         send
         with-method)

;; >>> define `class` <<<
;;  where the expansion uses `raw:class`
#;
(define point-class
  (class [x y] ; fields
    this ; name that refers back to self
    (define (get-x) x)
    (define (get-y) y)
    (define (set-x v) (set! x v))
    (define (set-y v) (set! y v))
    (define (rotate degrees)
      (define pt (make-rectangular x y))
      (define new-pt (make-polar
                      (magnitude pt)
                      (+ (angle pt) (* pi (/ degrees 180)))))
      (set! x (real-part new-pt))
      (set! y (imag-part new-pt)))))

#;
(define point-class
  (class
      (hash 'get-x
            (lambda (this) (get-field this 'x))
            'get-y
            (lambda (this) (get-field this 'y))
            'set-x
            (lambda (this v) (set-field! this 'x v))
            'set-y
            (lambda (this v) (set-field! this 'y v))
            'rotate
            (lambda (this degrees)
              (define pt (make-rectangular
                          (get-field this 'x)
                          (get-field this 'y)))
              (define new-pt (make-polar
                              (magnitude pt)
                              (+ (angle pt) (* pi (/ degrees 180)))))
              (set-field! this 'x (real-part new-pt))
              (set-field! this 'y (imag-part new-pt))))
    (hash 'x 0
          'y 1)))

(define-syntax-parser class
  #:literals (define)
  [(_ [fields ...]
      (define (function-names args ...) body ...) ...)
   #'(raw:class (for/hash ([fn-name (list 'function-names ...)]
                           [fn-body (list (lambda(this args ...)
                                            (define-field fields this) ...
                                            body ...) ...)])
                  (values fn-name fn-body))
                (for/hash ([name '(fields ...)]
                           [pos (in-naturals)])
                  (values name pos)))])

; shamelessly copied from solutions
; instead of 'field-name in expanded code I could use 'id
; eg. [id (get-field this-id 'id)]
(define-syntax-rule (define-field field-name this-id)
  (define-syntax field-name
    (syntax-id-rules (set!)
      [(set! id v)
       (set-field! this-id 'field-name v)]
      [(id arg (... ...))
       ((get-field this-id 'field-name) arg (... ...))]
      [id
       (get-field this-id 'field-name)])))
