#lang racket/base
(require "raw.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide class
         make-object
         get-field
         set-field!
         send)

; (send a-pt set-x 10)
; ((lookup-method a-pt 'set-x) a-pt 10)

;; >>> define `send` <<<
#;
(define-syntax-rule (send obj-expr method args ...)
  (let ([obj obj-expr])
    ((lookup-method obj 'method) obj args ...)))


#;
(define-syntax (send stx)
  (syntax-parse stx
    ([_ obj-expr method args ...]
     (unless (identifier? #'method)
       (raise-syntax-error #f "method is not an identifier" stx #'method))
     #'(let ([obj obj-expr])
         ((lookup-method obj 'method) obj args ...)))))

(define-syntax (send stx)
  (syntax-parse stx
    ([_ obj-expr method:id args ...]
     #'(let ([obj obj-expr])
         ((lookup-method obj 'method) obj args ...)))))

