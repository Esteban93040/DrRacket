#lang eopl

;Ejercicio 10
(define auxbalanced
    (lambda (lst n1 n2)
        (if (null? lst)
            (= n1 n2)
            (if (> n2 n1)
                #f
                (if (equal? (car lst) 'O)
                    (auxbalanced (cdr lst) (+ n1 1) n2)
                    (if (equal? (car lst) 'C)
                        (auxbalanced (cdr lst) n1 (+ n2 1))
                        (auxbalanced (cdr lst) n1 n2)
                    )
                )
            )
        )
    )   
)

(define balanced-parentheses? 
    (lambda (lst)
        (if (null? lst)
            #f
            (auxbalanced lst 0 0)
        )
    )
)