#lang eopl

;car = primer elemento
;cdr = todo menos el primer elemento
;cadr = el primer elemento del resto de la lista
;caddr = el segundo elemento del resto de la lista

;; Esteban
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




    