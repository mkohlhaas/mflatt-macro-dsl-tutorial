#lang racket

(require "gui.rkt"
         "ops.rkt"
         syntax/parse/define)

;; >>> define `form` macro with computed fields <<<

(define-syntax-parser form
  ([_ form-name clause ...]
   #'(begin
       (define form-name (make-gui 'form-name))
       (form-clause* form-name clause) ...
       (send form-name start))))

(define-syntax-parser form-clause
  [(_ form-name [local-id label widget-type calculated-field-fn-or-false])
   #'(begin
       (define local-id undefined)
       (gui-add! form-name
                 widget-type
                 label
                 (lambda () #t) ; guard
                 (lambda (v) (set! local-id v))
                 calculated-field-fn-or-false))])

(define-syntax-parser form-clause*
  [(_ form-name [local-id label widget-type])
   #'(form-clause form-name [local-id label widget-type #f])]
  [(_ form-name [local-id label widget-type calculated-field-fn-or-false])
   #'(form-clause form-name [local-id label widget-type (lambda() calculated-field-fn-or-false)])])

(form Box1HouseOwning
      [hasSoldHouse "Did you sell a house in 2010?" boolean-widget]
      [sellingPrice "Price the house was sold for:" money-widget]
      [privateDebt "Private debts for the sold house:" money-widget]
      [valueResidue "Value residue:" money-widget (-/coerce sellingPrice privateDebt)])

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
          (lambda () #t)
          (lambda (v) (set! sellingPrice v))
          #f)

(define privateDebt undefined)
(gui-add! Box1HouseOwning
          money-widget
          "Private debts for the sold house:"
          (lambda () #t)
          (lambda (v) (set! privateDebt v))
          #f)

(define valueResidue undefined)
(gui-add! Box1HouseOwning
          money-widget
          "Value residue:"
          (lambda () #t)
          (lambda (v) (set! valueResidue v))
          (lambda () (-/coerce sellingPrice privateDebt)))

(send Box1HouseOwning start)
|#