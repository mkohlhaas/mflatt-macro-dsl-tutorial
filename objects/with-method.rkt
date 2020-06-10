#lang racket/base
(require (for-syntax syntax/parse
                     racket/base)
         syntax/parse/define
         "send.rkt"
         (only-in "raw.rkt" lookup-method))

(provide class
         make-object
         get-field
         set-field!
         send
         with-method
         with-methods)

;; >>> define `with-method` <<<
#;
(with-method ([rot (a-pt rotate)])
  (for ([i (in-range N)])
    (rot 1)))

#;
(let ([rot' (lookup a-pt 'rotate)])
  (let-syntax ([rot (syntax-rules () 
                      [(rot args ...)
                       (rot' a-pt args ...)])])
    (for ([i (in-range N)])
      (rot 1))))

(define-syntax-rule (with-method ([method-id (obj-expr method-name)]) body ...)
  (let* ([obj obj-expr]
         [orig-method (lookup-method obj 'method-name)])
    (let-syntax ([method-id (syntax-rules ()
                              [(method-id args (... ...))
                               (orig-method obj args (... ...))])])
      body ...)))

#;
(define-syntax (with-methods stx)
  (syntax-parse stx
    [(_ ([new-method (obj-expr orig-method-name)] ...) body ...)
     #:with (obj:id ...) (generate-temporaries #'(obj-expr ...))
     #:with (orig-method:id ...) (generate-temporaries #'(orig-method-name ...))
     #'(let* ([obj obj-expr] ...
              [orig-method (lookup-method obj 'orig-method-name)] ...)
         (let-syntax ([new-method (syntax-rules ()
                                    [(new-method args (... ...))
                                     (orig-method obj args (... ...))])] ...)
           body ...))]))

(define-syntax-parser with-methods
  [(_ ([new-method (obj-expr orig-method-name)] ...) body ...)
   #:with (obj:id ...) (generate-temporaries #'(obj-expr ...))
   #:with (orig-method:id ...) (generate-temporaries #'(orig-method-name ...))
   #'(let* ([obj obj-expr] ...
            [orig-method (lookup-method obj 'orig-method-name)] ...)
       (let-syntax ([new-method (syntax-rules ()
                                  [(new-method args (... ...))
                                   (orig-method obj args (... ...))])] ...)
         body ...))])
