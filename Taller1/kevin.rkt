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

;; inversions :
;; Proposito:
;; L -> Int : Procedimiento que recibe una lista de números diferentes L
;; y retorna el número de inversiones presentes en la lista.
;;
;; <lista> ::= ()
;;          ::= (<numero> <lista>)

(define inversions
  (lambda (L)
    (cond
      ((null? L) 0)
      (else
       (+ (count-smaller (car L) (cdr L))
          (inversions (cdr L)))))))

;; count-smaller :
;; Proposito:
;; numero lista -> Int : Cuenta cuántos elementos de la lista son menores que el número dado.
;;
;; <lista> ::= ()
;;          ::= (<numero> <lista>)

(define count-smaller
  (lambda (x L)
    (cond
      ((null? L) 0)
      ((> x (car L))
       (+ 1 (count-smaller x (cdr L))))
      (else
       (count-smaller x (cdr L))))))
