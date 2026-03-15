#lang eopl

;Esteban Andres Espinosa - 2610114
;Kevin Andres Giron  - 2510102
;Juan Sebastian Oviedo - 2510104

;;-----------------------------------------------------------------

;; list-set :
;; Proposito:
;; L N X Pred -> L' : Procedimiento que recibe una lista L, un índice N, un elemento X
;; y un predicado Pred. Retorna una lista igual a L pero con el elemento en la posición N
;; reemplazado por X, solo si el elemento original cumple el predicado Pred.
;;
;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>
;; <predicado> ::= <procedimiento-booleano>

(define list-set
    (lambda (L n x Pred)
        (if (null? L)
            (eopl:error "List too short")
            (if (zero? n)
                (if (Pred (car L))
                    (cons x (cdr L))
                    (cons (car L) (cdr L))
                )
                (cons (car L) (list-set (cdr L) (- n 1) x Pred))
            )
        )
    )
)

;;Pruebas
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)
 
;;-----------------------------------------------------------------

;; filter-in :
;; Proposito:
;; Pred L -> L' : Procedimiento que recibe un predicado Pred y una lista L.
;; Retorna una nueva lista con los elementos de L que cumplen el predicado Pred.
;;
;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>
;; <predicado> ::= <procedimiento-booleano>

(define filter-in 
    (lambda (pred L)
        (if (null? L)
            '()
            (if (pred (car L))
                (cons (car L) (filter-in pred (cdr L)))
                (filter-in pred (cdr L))
            )
        )
    )
)

;;Pruebas
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
;;-----------------------------------------------------------------

;; invertPalindrome : (funcion auxiliar)
;; Proposito:
;; lst acc -> lst' : Procedimiento auxiliar que invierte la lista lst
;; usando un acumulador acc. Utilizado internamente por palindrome?.
;;
;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>

(define invertPalindrome
    (lambda (lst n)
        (if (null? lst)
            n
            (invertPalindrome (cdr lst) (cons (car lst) n))
        )
    )
)

;; palindrome? :
;; Proposito:
;; lst -> Bool : Procedimiento que recibe una lista lst y determina si
;; es un palíndromo, es decir, si se lee igual de izquierda a derecha
;; que de derecha a izquierda.
;;
;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>

(define palindrome?
    (lambda (lst)
        (if (null? lst)
            (eopl:error "empty list")
            (if (equal? lst (invertPalindrome lst '()))
                #t
                #f
            )
        )
    )
)

;;Pruebas
(palindrome? '(r a d a r))
(palindrome? '(n e u q u e n))
(palindrome? '(h o l a))
;;-----------------------------------------------------------------

;; mapping :
;; Proposito:
;; F L1 L2 -> L' : Procedimiento que recibe una función unaria F y dos listas de
;; números L1 y L2 (de igual tamaño). Retorna una lista de pares (a b)
;; tales que a pertenece a L1, b pertenece a L2 y se cumple que F(a) = b.
;;
;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>
;; <par> ::= (<valor> <valor>)
;; <funcion-unaria> ::= <procedimiento>

(define mapping 
    (lambda (f l1 l2)
        (if (null? l1)
            '()
            (if (equal? (f (car l1)) (car l2))
                (cons (list (car l1) (car l2)) (mapping f (cdr l1) (cdr l2)))
                (mapping f (cdr l1) (cdr l2))
            )
        )
    )
)

;;Pruebas
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

;;-----------------------------------------------------------------

;; path :
;; Proposito:
;; N BST -> L : Procedimiento que recibe un número entero N y un árbol binario
;; de búsqueda BST (representado con listas). Retorna una lista con la ruta
;; de símbolos 'left y 'right necesaria para llegar al nodo que contiene N
;; desde la raíz. Si N es la raíz, retorna una lista vacía.
;;
;; <BST> ::= ()
;;        ::= (<int> <BST> <BST>)
;; <ruta> ::= ()
;;         ::= (left <ruta>)
;;         ::= (right <ruta>)

(define path
    (lambda (n bst)
        (if (null? bst)
            (eopl:error "Number not found in tree")
            (if (= n (car bst))
                '()
                (if (< n (car bst))
                    (cons 'left  (path n (cadr bst)))
                    (cons 'right (path n (caddr bst)))
                )
            )
        )
    )
)

;;Pruebas
(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))

;;-----------------------------------------------------------------

;; concat : (funcion auxiliar)
;; Proposito:
;; L1 L2 -> L' : Procedimiento auxiliar que recibe dos listas L1 y L2
;; y retorna una nueva lista con los elementos de L1 seguidos por los de L2,
;; sin usar la función append.
;;
;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>

(define concat
    (lambda (l1 l2)
        (if (null? l1)
            l2
            (cons (car l1) (concat (cdr l1) l2))
        )
    )
)

;; hanoi :
;; Proposito:
;; N Origen Auxiliar Destino -> L : Procedimiento que resuelve el problema clásico
;; de las Torres de Hanoi. Recibe un entero positivo N (cantidad de discos) y tres
;; símbolos que representan las torres origen, auxiliar y destino. Retorna una lista
;; con la secuencia de movimientos (desde hacia) necesarios para trasladar los N
;; discos desde la torre origen hasta la torre destino usando la torre auxiliar.
;;
;; <movimiento> ::= (<torre> <torre>)
;; <torre> ::= <symbol>
;; <lista-movimientos> ::= ()
;;                      ::= (<movimiento> <lista-movimientos>)

(define hanoi
    (lambda (n origen auxiliar destino)
        (if (zero? n)
            '()
            (concat
                (hanoi (- n 1) origen destino auxiliar)
                (concat
                    (list (list origen destino))
                    (hanoi (- n 1) auxiliar origen destino)
                )
            )
        )
    )
)

;;Pruebas
(hanoi 1 'A 'B 'C)
(hanoi 2 'A 'B 'C)
(hanoi 3 'A 'B 'C)

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

; multiplo5? : Int -> Bool
; usage: (multiplo5? n) = #t si n es múltiplo de 5

(define multiplo5?
  (lambda (n)
    (= (remainder n 5) 0)))

;; invert :
;; Proposito:
;; L P -> L’ lista de pares invertidos que cumplen P

;; <lista> ::= ()
;;          ::= (<par> <lista>)
;; <par> ::= (<valor> <valor>)
;; <valor> ::= <valor-de-scheme>
;; <predicado> ::= <procedimiento-booleano>

(define invert
  (lambda (L P)
    (cond
      ((null? L) '())
      (else
       (let ((x (car (car L)))
             (y (cadr (car L))))
         (if (and (P x) (P y))
             (cons (list y x)
                   (invert (cdr L) P))
             (invert (cdr L) P)))))))

;; Pruebas
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((6 9) (10 90) (82 7)) odd?)
(invert '((5 9) (10 90) (82 7)) multiplo5?)
;;-----------------------------------------------------------------

;; down :
;; Proposito:
;; L -> L’ : Procedimiento que recibe una lista L y retorna una nueva lista donde cada elemento de L se encuentra asociado a un nivel adicional de paréntesis con respecto a su estado original.

;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>
;;              | <lista>

(define down
  (lambda (L)
    (cond
      ((null? L) '())
      (else
       (cons (list (car L))
             (down (cdr L)))))))

;;Pruebas
(down '(1 2 3))
(down '((una) (buena) (idea)))
(down '(un (objeto (mas)) complicado))
;;-----------------------------------------------------------------

;; swapper:
;; Proposito:
;; E1 E2 L -> L’ :Procedimiento que recibe dos elementos E1 y E2, y una lista L.
;; Retorna una lista similar a L, donde cada ocurrencia de E1 es reemplazada por E2 y cada ocurrencia de E2 es reemplazada por E1. 
;; Los elementos E1 y E2 pertenecen a la lista L.

;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;;
;; <elemento> ::= <valor-de-scheme>
;;              | <lista>

(define swapper
  (lambda (E1 E2 L)
    (cond
      ((null? L) '())
      (else
       (cons (swap-elem E1 E2 (car L))
             (swapper E1 E2 (cdr L)))))))

;; swap-elem : (Funcion auxliar)
;; Proposito:
;; E1 E2 X -> X’ : Procedimiento auxiliar que intercambia X por E2 si X es E1,
;; intercambia X por E1 si X es E2, o retorna X en otro caso.

(define swap-elem
  (lambda (E1 E2 X)
    (cond
      ((eqv? X E1) E2)
      ((eqv? X E2) E1)
      (else X))))

;;Pruebas
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))
;;-----------------------------------------------------------------

;; operate :
;; Proposito:
;; Lrators Lrands -> Int : Procedimiento que recibe una lista de funciones binarias lrators de tamaño n y una lista de números lrands de tamaño n + 1.
;; Retorna el resultado de aplicar sucesivamente las operaciones de lrators sobre los valores de lrands, de izquierda a derecha.
;;
;; <lista-operadores> ::= ()
;;                     ::= (<operador-binario> <lista-operadores>)
;; <lista-numeros> ::= (<numero> <lista-numeros>)
;; <operador-binario> ::= <procedimiento>
;; <numero> ::= <valor-de-scheme>

(define operate
  (lambda (lrators lrands)
    (cond
      ;; Caso base: solo quedan dos operandos y un operador
      ((null? (cdr lrators))
       ((car lrators) (car lrands) (cadr lrands)))
      (else
       (operate
        (cdr lrators)
        (cons ((car lrators) (car lrands) (cadr lrands))
              (cddr lrands)))))))

;;Pruebas
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))
;;-----------------------------------------------------------------

;; count-odd-and-even :
;; Proposito:
;; arbol -> List : Procedimiento que recibe un árbol binario con números en los nodos y retorna una lista (pares impares).
;;
;; <arbol> ::= ()
;;          ::= (<numero> <arbol> <arbol>)

(define count-odd-and-even
  (lambda (arbol)
    (cond
      ((null? arbol) '(0 0))
      (else
       (combinar
        (car arbol)
        (count-odd-and-even (cadr arbol))
        (count-odd-and-even (caddr arbol)))))))

;; combinar :
;; numero lista lista -> lista : Combina el valor del nodo con los resultados de los subárboles

(define combinar
  (lambda (valor res1 res2)
    (let ((pares (+ (car res1) (car res2)))
          (impares (+ (cadr res1) (cadr res2))))
      (if (even? valor)
          (list (+ pares 1) impares)
          (list pares (+ impares 1))))))

;;Prueba
 (count-odd-and-even '(14 (7 () (12 () ()))
                        (26 (20 (17 () ())
                        ())
                        (31 () ()))))

(count-odd-and-even
                    '(8
                      (3
                        (2 () ())
                        (5 () ()))
                      (10
                        ()
                        (7 () ()))))
