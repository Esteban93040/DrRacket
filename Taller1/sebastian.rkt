#lang eopl

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

;; mapping :
;; Proposito:
;; F L1 L2 -> L’ : Procedimiento que recibe una función unaria F y dos listas de
;; números L1 y L2 (de igual tamaño). Retorna una lista de pares (a b)
;; tales que a pertenece a L1, b pertenece a L2 y se cumple que F(a) = b.
;;
;; <lista> ::= ()
;;          ::= (<elemento> <lista>)
;; <elemento> ::= <valor-de-scheme>
;; <par> ::= (<valor> <valor>)
;; <funcion-unaria> ::= <procedimiento>

(define mapping
  (lambda (F L1 L2)
    (cond
      ((null? L1) '())
      ((= (F (car L1)) (car L2))
       (cons (list (car L1) (car L2))
             (mapping F (cdr L1) (cdr L2))))
      (else
       (mapping F (cdr L1) (cdr L2))))))

;;Pruebas
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

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


;; Sebastián: 15