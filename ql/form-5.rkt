#lang racket

(require "gui.rkt"
         "ops.rkt"
         syntax/parse/define)

(provide form
         (rename-out [boolean-widget boolean]
                     [money-widget money]
                     [-/coerce -]
                     [>/coerce >]
                     [</coerce <]
                     [=/coerce =]
                     [and/coerce and]
                     [or/coerce or])
         when
         #%app
         #%datum
         #%module-begin)

(define-syntax-parser form
  ([_ form-name clause ...]
   #'(begin
       (define form-name (make-gui 'form-name))
       (form-clause form-name #t clause) ...
       (send form-name start))))

(define-syntax-parser form-clause
  #:literals (when)
  [(_ form-name guard-expr (when cond-expr clause ...))
   #'(begin
       ; Nested when's are not working:
       ; (define (new-guard-expr) (and guard-expr (?/coerce cond-expr)))
       (form-clause form-name (and guard-expr (?/coerce cond-expr)) clause ) ...)]
  [(_ form-name guard-expr [local-id label widget-type])
   #'(form-clause* form-name guard-expr [local-id label widget-type #f])]
  [(_ form-name guard-expr [local-id label widget-type calculated-field-fn-or-false])
   #'(form-clause* form-name guard-expr [local-id label widget-type (lambda() calculated-field-fn-or-false)])])

(define-syntax-parser form-clause*
  [(_ form-name guard-expr [local-id label widget-type calculated-field-fn-or-false])
   #'(begin
       (define local-id undefined)
       (gui-add! form-name
                 widget-type
                 label
                 (lambda() guard-expr)
                 (lambda (v) (set! local-id v))
                 calculated-field-fn-or-false))])
