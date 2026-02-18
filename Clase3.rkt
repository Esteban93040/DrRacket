#lang eopl

;in-S? : N -> Bool
;usage: (in-S? n) = #t if n is in S, #f otherwise
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))

;Potencia n de un número x, calculado de forma recursiva.
;programa : Int x Int -> Int
;usage: (programa n x) = x^n	
(define programa
  (lambda (n x)
    (if (zero? n)
        1
        (* x (programa (- n 1) x))
        )
    )
  )


;Programa que calcula la longitud de una lista
;list-length : List -> Int
;usage: (list-length l) = the length of l
(define list-length
  (lambda (lst)
   (if (null? lst)
   0
   (+ 1 (list-length (cdr lst))))))



;nth-element : List x Int -> SchemeVal
;usage: (nth-element lst n) = the n-th element of lst

(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))
           
(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element "List too short by ~s elements.~%" (+ n 1))))


;sum-arbol : arbol-binario -> Int
;usage: (sum-arbol arbol) = suma de las hojas del árbol
		
(define sum-arbol
  (lambda (arbol)
    (if (number? arbol)
        arbol
        (+  (sum-arbol (cadr arbol))
            (sum-arbol (caddr arbol))
            )
        )
    )
  )

(define arbol '(k (h 5 3) (t (s 10 11) 12)))
(sum-arbol arbol)


;cantidad-int : arbol-binario -> Int
;usage: (cantidad-int arbol) = Cuenta la cantidad de enteros en un árbol binario

(define cantidad-int
  (lambda (arbol)
    (if (number? arbol)
        1
        (+  (cantidad-int (cadr arbol))
            (cantidad-int (caddr arbol))
            )
        )
    )
  )

(cantidad-int arbol)



(define cantidad-symbol
  (lambda (arbol)
    (if (number? arbol)
        0
        (+ 1 (cantidad-symbol (cadr arbol))
            (cantidad-symbol (caddr arbol))
            )
        )
    )
  )


(define lista-symbol
  (lambda (arbol)
    (if (number? arbol)
        empty
        (append (list (car arbol))
                (lista-symbol (cadr arbol))
                (lista-symbol (caddr arbol))
                )
        )
    )
  )
    

(define lista-numero
  (lambda (arbol)
    (if (number? arbol)
        (list arbol)
        (append (lista-numero (cadr arbol))
                (lista-numero (caddr arbol))
                )
        )
    )
  )


(define lista-numero-predicado
  (lambda (arbol P)
    (if (number? arbol)
        (if (P arbol) (list arbol) empty)
        (append (lista-numero-predicado (cadr arbol) P)
                (lista-numero-predicado (caddr arbol) P)
                )
        )
    )
  )

(lista-numero-predicado arbol (lambda (x) (> x 10)))


;;BNF
;;<lista-numeros> ::= '() | <int> <lista-numeros>
(define suma-lista-numeros
  (lambda (lst)
    (if (eqv? lst empty)
        0
        (+ (car lst) (suma-lista-numeros (cdr lst))))
    )
  )

(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda) 
       (and (not (eqv? (caadr exp) var))
            (occurs-free? var (caddr exp))))
      (else (or (occurs-free? var  (car exp))
                (occurs-free? var (cadr exp)))))))


(define exp1 '(lambda (x) ((lambda (a) (x (a y))) x)))
(occurs-free? 'x exp1)

(define exp2 '((lambda (x) x) y))
(occurs-free? 'y exp2)

(define exp4 '(lambda (y) ((lambda (x) x) y)))
(occurs-free? 'y exp4)

(define exp5 '(lambda (x) (a b)))
(occurs-free? 'x exp5)


(define x						
	(lambda (x)				
		(map
			(lambda (x)		
				(+ x 1))	
			x)))			
(x '(1 2 3))


(lambda (z)
  ((lambda (a b c)
     (a (lambda (a)
          (+ a c))
        b))
   (lambda (f x)
     (f (z x)))))


(let ((x 3) (y 4))
  (+ (let ((x (+ y 5)))
       (* x y))
     x)
  )



(let ((x 6)
      (y 7))
  (+ (let ((x (- y 6)))
       (* x y))
     x)
  )


(let ((x 6)(y 7))
  (*
   (let ((y 8)) 
     (+
      (let ((x 6) (y x))
        (+ x
           (let ((y 3) (x y)) (+ x (+ 2 y)))
           )
        )
      y)
     )
   (let ((x 4)) (- y x))
   )
  )