#lang racket

(require "gui.rkt"
         "ops.rkt"
         syntax/parse/define)

;; >>> define a basic `form` macro <<<
(define-syntax-parser form
  ([_ form-name clause ...]
   #'(begin
       (define form-name (make-gui 'form-name))
       (form-clause form-name clause) ...
       (send form-name start))))

(define-syntax-parser form-clause
  [(_ form-name [local-id label widget-type])
   #'(begin
       (define local-id undefined)
       (gui-add! form-name
                 widget-type
                 label
                 (lambda () #t) ; guard
                 (lambda (v) (set! local-id v))
                 #f))])

(form Box1HouseOwning
      [hasSoldHouse "Did you sell a house in 2010?" boolean-widget]
      [sellingPrice "Price the house was sold for:" money-widget])

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
          (lambda () #t) ; guard
          (lambda (v) (set! sellingPrice v))
          #f) ; not a computed field

(send Box1HouseOwning start)
|#
