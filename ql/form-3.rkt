#lang racket

(require "gui.rkt"
         "ops.rkt"
         syntax/parse/define)

;; >>> define `form` macro with a conditional clause <<<

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

(form Box1HouseOwning
      [hasSoldHouse "Did you sell a house in 2010?" boolean-widget]
      [hasBoughtHouse "Did you buy a house in 2010?" boolean-widget]
      (when hasSoldHouse
        [sellingPrice "Price the house was sold for:" money-widget]
        (when hasBoughtHouse
          [privateDebt "Private debts for the sold house:" money-widget]
          [valueResidue "Value residue:" money-widget (-/coerce sellingPrice privateDebt)])))

#|
(define Box1HouseOwning
  (make-gui 'Box1HouseOwning))

(define hasSoldHouse undefined)
(gui-add! Box1HouseOwning
          boolean-widget
          "Did you sell a house in 2010?"
          (lambda () #t) ; guard
          (lambda (v) (set! hasSoldHouse v))
          #f) ; not a computed field

(define sellingPrice undefined)
(gui-add! Box1HouseOwning
          money-widget
          "Price the house was sold for:"
          (lambda () (?/coerce hasSoldHouse)) ; <---------------
          (lambda (v) (set! sellingPrice v))
          #f)

(define privateDebt undefined)
(gui-add! Box1HouseOwning
          money-widget
          "Private debts for the sold house:"
          (lambda () (?/coerce hasSoldHouse)) ; <---------------
          (lambda (v) (set! privateDebt v))
          #f)

(define valueResidue undefined)
(gui-add! Box1HouseOwning
          money-widget
          "Value residue:"
          (lambda () (?/coerce hasSoldHouse)) ; <---------------
          (lambda (v) (set! valueResidue v))
          (lambda () (-/coerce sellingPrice privateDebt)))

(send Box1HouseOwning start)
|#