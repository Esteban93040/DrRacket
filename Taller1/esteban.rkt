#lang eopl

;car = primer elemento
;cdr = todo menos el primer elemento
;cadr = el primer elemento del resto de la lista
;caddr = el segundo elemento del resto de la lista

;Taller 1 - Ejercicio 1
(define invert
    (lambda (lst predicate)
        (if (null? lst)
            '() 
            (if (predicate (car(car lst))) 
                (if (predicate (car(cadr lst)))
                    (cons (car lst cadr lst)
                    (cons (car lst) (invert (cdr lst)))
                )
                (cons (car lst) (invert (cdr lst)))
                )
            (cons (car lst) (invert (cdr lst)))
        )   
    )
)
)


;Taller 1 - Ejercicio 3

(define list-set
    (lambda (lst n x predicate) ;Recibe 4 elementos, una lista, un número, un elemento y un predicado
        (if (null? lst) ; si la lista es nula se reporta un error
            (eopl:error "List too short" )   ;lista muy corta                                      
            (if (zero? n) ; se verifica si n es 0 para determinar en que momento hacer la verificacion del predicado
                (if (predicate (car lst)) ; si el predicado se cumple se hace el cambio en la lista original
                    (cons x (cdr lst)) ; cons crea una nueva lista con el nuevo elemento , cdr lst mantiene el resto de la lista original
                    (cons (car lst) (cdr lst)) ; cons crea una nueva lista con el mismo elemento, cdr lst mantiene el resto de la lista original
                )
                (cons (car lst) (list-set (cdr lst) (- n 1) x predicate)) ; si n no es 0 se mantiene el mismo elemento y se llama recursivamente a list-set con el resto de la lista y n-1
            )
            
        )
    )
)
 