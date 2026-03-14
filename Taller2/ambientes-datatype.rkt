#lang eopl

;Ambientes para múltiples variables (listas de simbolos y listas de valores)
(define-datatype environment environment?
   (empty-env-record)
   (extended-env-record (syms (list-of symbol?))
                        (vals (list-of scheme-value?))
                        (env environment?)))

(define scheme-value? (lambda (v) #t))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))



; Creación de ambientes con múltiples variables
(define init-env
    (extended-env-record '(i v x) '(1 5 10)
     (extended-env-record '(y z) '(1 5)
     (empty-env-record))))

(apply-env init-env 'x) ;Buscar una variable


;;Otros ejemplos de datatypes
;;;;;;;;
;Interfaz: Definición de tipo de dato lambda con define-datatype
;<Lc-exp> ::= <var-exp> (<identifier>)
;         ::= <lambda-exp> (lambda (<identifier>) <Lc-exp>)
;         ::= <app-exp> (<Lc-exp> <Lc-exp>)

(define-datatype lc-exp lc-exp?
   (var-exp (id symbol?) )
   (lambda-exp (bound-var symbol?) (body lc-exp?) )
   (app-exp (rator lc-exp?) (rand lc-exp?) ) )


(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (id)
               (eqv? id search-var))
      (lambda-exp (bound-var body)
                  (and (not ( eqv? search-var bound-var) )
                       (occurs-free? search-var body ) ) )
      (app-exp (rator rand )
               (or(occurs-free? search-var rator)
                  (occurs-free? search-var rand ) ) ) ) ) )

(define e
  (lambda-exp 'x (var-exp 'y)))

(occurs-free? 'x  e)
(occurs-free? 'y  e)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tipo de dato s-list
;<S-list> ::= ({<S-exp>}*)
;<S-exp> ::= <Symbol> | <S-list>
		
(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list (first s-exp?) (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp (sym symbol?))
  (s-list-s-exp (slst s-list?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tipo de dato binary tree
;<arbol-binario> ::= <int>
;                ::= (<simbolo> <arbol-binario> <arbol-binario>)
(define-datatype bintree bintree?
   (leaf-node (datum number?) )
   (interior-node (key symbol?)
                  (left bintree?)
                  (right bintree?) ) )
;Definir un procedimiento que permita encontrar la suma de los
;enteros en las hojas de un  arbol

(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right)
                     (+ (leaf-sum left) (leaf-sum right ) ) ) ) ) )






