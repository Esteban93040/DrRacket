#lang eopl

(define fnc-interpreter
    '((white-sp
        (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (number
        (digit (arbno digit)) number)
    )


    )

(define grammar-for-FNC
    '(
        ()
        ()
    )
)