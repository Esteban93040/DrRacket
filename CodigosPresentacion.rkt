#lang eopl
(define A 5)
(define B (* 2 A))

(define (multiplicacion1 a b) (* a b))
(multiplicacion1 5 6)

(lambda (x y) (if (> x y) (* x y) (-  x y )))
((lambda (x y) (if (> x y) (* x y) (-  x y ))) 6 8)

(lambda (x y) (* x y));retorna un procedimiento
((lambda (x y) (* x y)) 2 3); retorna un valor

; Las funciones las definiremos en torno a lambdas, para representar funciones como valores

(define multiplicacion2
  (lambda (a b)
    (* a b)
   )
)
(multiplicacion2 5 6)

(+ (* 2 2) (* 3 5) (expt (/ 1 4) 2))

((lambda (x y) (if (> x  y) x y)) 5 9)

((lambda (x y) (if (or (> 0 x y) (< 0 x y)) (* x y) (+ x y))) 5 -6)
((lambda (x y) (if (>= (* x y) 0) (* x y) (+ x y))) 5 -6)


(define verificar
   (lambda (n s) (cond
                   [(<= n 0) 'error]
                   [(eqv? s 'm) 'hombre]
                   [(eqv? s 'f) 'mujer]
                   [else 'error]
                   ))
   )


;Definiciones locales

; función no recursiva con let
(define f1
  (lambda (n)
    (let
        (
         (p 2)
         (f (lambda (a b) (* a b)))
         )
         (f n p)
      )
    )
)

(f1 10)

;función recursiva con letrec

(define f2
  (lambda (n)
    (letrec
        (
         (p 2)
         (sume (lambda (a b)
                 (if (= b 0) 0 (+ a (sume a (- b 1))))))
          )
         
         (sume n p)
      )
    )
)

(f2 10)

; el let* permite conocer valores que fueron declarados antes, ejemplo r conoce a p y a q
; q conoce a p, p no conoce a nadie

(define f3
  (lambda (n)
    (let*
        (
         (p 2)
         (q 3)
         (r (+ p q n))
         )
         r
      )
    )
)

(f3 5)


;lista factorial. Sólo puede recibir un argumento, entonces se necesita
;una función auxiliar que cuente de 0 a n, por eso se utiliza una función
;auxiliar recursiva con un letrec. Eso se llama abstracción de datos.
;la recursión tiene criterio de parada y condición de recursión...

(define factorial
  (lambda (n)
    ;(write n)
    (if
     (= n 0)
     1
     (* n (factorial (- n 1))))))

(define listaFactorial
  (lambda (x)
    (letrec
        (
        (lista
         (lambda (a b)
                 (if (= a b)
                     (cons (factorial b) empty)
                     (cons (factorial a) (lista (+ a 1) b))
                 )
                )
               )
        )
      (lista 0 x)
      )))


;Otra posible implementación


(define listaFactorial2
  (lambda (x)
    (letrec
        (
        (lista
         (lambda (a b acc)
                 (cond
                   [(= a b)  (cons (* acc b) empty)]
                   [(= a 0) (cons 1 (lista (+ a 1) b (* 1 acc) ))]
                   [else  (cons (* a acc) (lista (+ a 1) b (* a acc) )) ]
                 )
                )
               )
        )
      (lista 0 x 1)
      )))


;Funciones como ciudadanos de primera clase, se puede utilizar como argumento en un llamado

(define mayor
  (lambda (a b)
    (cond
      [(> a b) #T]
      [else #F]
     )
    )
  )

(define filtro
  (lambda (lst f n)
    (cond
      [(eqv? lst '()) empty]
      [(f (car lst) n) (cons (car lst) (filtro (cdr lst) f n))]
      [else (filtro (cdr lst) f n)]
      )
    )
  )

(filtro (list 3 5 6 2 90 1) mayor 4)

; funciones como ciudadanos de primera clase, pueden retornarse funciones

(define funcionM
  (lambda ( x y )
    (lambda ( a b )
      (+ x y a b))))

(funcionM 1 3)
((funcionM 1 3) 5 6)


; función que reemplaza el elemento n por un valor x

(define list-set
  (lambda (lst n x)
    (letrec
        (
         (list-aux
          (lambda (lstaux naux xaux acc)
            (cond
              [(eqv? lstaux '()) empty]
              [(= acc naux) (cons xaux (list-aux (cdr lstaux) naux xaux (+ 1 acc)))]
              [else (cons (car lstaux) (list-aux (cdr lstaux) naux xaux (+ 1 acc)))]
              )
            )
          )
         )
      (list-aux lst n x 0))
    )
  )
            
(list-set '(perro q 4 6 7) 2 'b)

;Función que cambia los elementos n por x en una lista
(define cambiar
    (lambda (l n x)
      (cond
        [(eqv? l '()) empty]
        [(eqv? (car l) n) (cons x (cambiar (cdr l) n x))]
        [else  (cons (car l) (cambiar (cdr l) n x))])))



(define prodCartesiano
  (lambda (l1 l2)
    (if (null? l1)
        empty
        (append (lista2 (car l1) l2) (prodCartesiano (cdr l1) l2))
    )))

(define lista2
  (lambda (l1 l2)
    (if (null? l2)
        empty
        (cons (cons l1 (cons (car l2) empty))
        (lista2 l1 (cdr l2))))))

    