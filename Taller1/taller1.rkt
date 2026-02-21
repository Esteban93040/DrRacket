#lang eopl

(define invert 
    (lambda (list procedure?)
     map procedure? list
     )
    )
