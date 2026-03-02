#lang eopl

;;-----------------------------------------------------------------

;; auxbalanced : (funcion auxiliar)
;; Proposito:
;; lst n1 n2 -> Bool : Procedimiento auxiliar que recorre la lista lst
;; contando paréntesis de apertura en n1 y de cierre en n2. Retorna #t
;; si al finalizar el recorrido n1 = n2 y en ningún punto n2 supera a n1.
;; Utilizado internamente por balanced-parentheses?.
;;
;; <lista> ::= ()
;;          ::= (<simbolo> <lista>)
;; <simbolo> ::= O | C | <otro>
;; <n1> ::= <int-no-negativo>
;; <n2> ::= <int-no-negativo>

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

;;-----------------------------------------------------------------

;; balanced-parentheses? :
;; Proposito:
;; lst -> Bool : Procedimiento que recibe una lista de símbolos lst cuyos
;; elementos pueden ser O (apertura) o C (cierre), y determina si los
;; paréntesis están correctamente balanceados. Retorna #t si están
;; balanceados y #f en caso contrario.
;;
;; <lista> ::= ()
;;          ::= (<simbolo> <lista>)
;; <simbolo> ::= O | C | <otro>

;;Pruebas
;(balanced-parentheses? '(O C))
;(balanced-parentheses? '(O O C C))
;(balanced-parentheses? '(O C C O))
;(balanced-parentheses? '(O O C))

(define balanced-parentheses? 
    (lambda (lst)
        (if (null? lst)
            #f
            (auxbalanced lst 0 0)
        )
    )
)

;;-----------------------------------------------------------------

;; inversions :
;; Proposito:
;; L -> Int : Procedimiento que recibe una lista de números diferentes L
;; y retorna el número de inversiones presentes en la lista.
;;
;; <lista> ::= ()
;;          ::= (<numero> <lista>)

;;Pruebas
;(inversions '(2 3 8 6 1))
;(inversions '(1 2 3 4 5))

(define inversions
  (lambda (L)
    (cond
      ((null? L) 0)
      (else
       (+ (count-smaller (car L) (cdr L))
          (inversions (cdr L)))))))

;;-----------------------------------------------------------------

;; count-smaller : (funcion auxiliar)
;; Proposito:
;; numero lista -> Int : Cuenta cuántos elementos de la lista son menores que el número dado.
;; Utilizado internamente por inversions.
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

;;-----------------------------------------------------------------

;; zip :
;; Proposito:
;; F L1 L2 -> L' : Procedimiento que recibe una función binaria F y dos
;; listas de igual tamaño L1 y L2. Retorna una lista donde el elemento
;; n-ésimo es el resultado de aplicar F sobre los elementos n-ésimos de
;; L1 y L2. Si las listas no tienen el mismo tamaño, lanza un error.
;;
;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>
;; <funcion-binaria> ::= <procedimiento>

;;Pruebas
;(zip + '(1 4) '(6 2))
;(zip * '(11 5 6) '(10 9 8))

(define zip
    (lambda (f l1 l2)
        (if (and (null? l1) (null? l2))
            '()
            (if (or (null? l1) (null? l2))
                (eopl:error "zip: las listas no son del mismo tamaño")
                (cons (f (car l1) (car l2))
                      (zip f (cdr l1) (cdr l2))
                )
            )
        )
    )
)
;;-----------------------------------------------------------------

;; filter-acum :
;; Proposito:
;; a b F acum filter -> acum' : Procedimiento que recibe dos enteros a y b
;; que definen un intervalo [a,b], una función binaria F, un valor inicial
;; acum y un predicado unario filter. Aplica F acumulativamente sobre los
;; elementos del intervalo que cumplan el predicado filter y retorna el
;; valor acumulado final.
;;
;; <a> ::= <int>
;; <b> ::= <int>
;; <funcion-binaria> ::= <procedimiento>
;; <funcion-unaria> ::= <procedimiento-booleano>

;;Pruebas
;(filter-acum 1 10 + 0 odd?)
;(filter-acum 1 10 + 0 even?)

(define filter-acum
    (lambda (a b f acum filter)
        (if (> a b)
            acum
            (if (filter a)
                (filter-acum (+ a 1) b f (f acum a) filter)
                (filter-acum (+ a 1) b f acum filter)
            )
        )
    )
)

;;-----------------------------------------------------------------

;; coin-change :
;; Proposito:
;; monto monedas -> Int : Procedimiento que recibe un número entero no
;; negativo monto y una lista de denominaciones de monedas. Retorna el
;; número total de formas distintas de obtener exactamente el monto
;; usando las denominaciones disponibles.
;;
;; <monto> ::= <int-no-negativo>
;; <monedas> ::= ()
;;            ::= (<int-positivo> <monedas>)

;;Pruebas
;(coin-change 5 '(1 5))
;(coin-change 5 '(1 2 5))
;(coin-change 10 '(2 5 3 6))

(define coin-change
    (lambda (monto monedas)
        (cond
            ((= monto 0) 1)
            ((or (< monto 0) (null? monedas)) 0)
            (else
                (+ (coin-change (- monto (car monedas)) monedas)
                   (coin-change monto (cdr monedas))
                )
            )
        )
    )
)

;;-----------------------------------------------------------------

;; sum-rows : (funcion auxiliar)
;; Proposito:
;; L1 L2 -> L' : Procedimiento auxiliar que suma elemento a elemento
;; dos listas de igual tamaño. Usado internamente por pascal.
;;
;; <lista> ::= ()
;;          ::= (<numero> <lista>)

(define sum-rows
    (lambda (l1 l2)
        (if (null? l1)
            '()
            (cons (+ (car l1) (car l2))
                  (sum-rows (cdr l1) (cdr l2))
            )
        )
    )
)

;;-----------------------------------------------------------------

;; pascal :
;; Proposito:
;; N -> L : Procedimiento que recibe un número entero positivo N
;; y retorna la fila N del triángulo de Pascal.
;;
;; <N> ::= <int-positivo>
;; <lista> ::= ()
;;          ::= (<numero> <lista>)

;;Pruebas
;(pascal 1)
;(pascal 2)
;(pascal 3)
;(pascal 4)
;(pascal 5)

(define pascal
    (lambda (n)
        (if (= n 1)
            '(1)
            (let ((prev (pascal (- n 1))))
                (sum-rows (cons 0 prev)
                          (append prev '(0))
                )
            )
        )
    )
)