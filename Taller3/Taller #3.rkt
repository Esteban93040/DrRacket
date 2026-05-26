#lang eopl

;******************************************************************************************
;;;;; Interpretador Simple extendido con recursión

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)
;;
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;
;;                  ::= declararRec ( id ( ids ) = exp ; ) { exp }
;;                      <rec-exp (proc-name ids proc-body body)>
;;
;;  <primitivey>     ::= + | - | * | add1 | sub1
;;
;******************************************************************************************

;******************************************************************************************
; Especificación Léxica

(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)


    (comment
     ("%" (arbno (not #\newline))) skip)

    (identifier
     (letter (arbno (or letter digit "?")))
     symbol)

    (number
     (digit (arbno digit))
     number)

    (number
     ("-" digit (arbno digit))
     number)))

;******************************************************************************************
; Especificación Sintáctica

(define grammar-simple-interpreter
  '((program
     (expression)
     a-program)

    (expression
     (number)
     lit-exp)

    (expression
     (identifier)
     var-exp)

    (expression
     (primitive "(" (separated-list expression ",") ")")
     primapp-exp)

    (expression
     ("declararRec"
      "("
      identifier
      "("
      (separated-list identifier ",")
      ")"
      "="
      expression
      ";"
      ")"
      "{"
      expression
      "}")
     rec-exp)

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)))

;******************************************************************************************
; show-the-datatypes:
; Muestra en consola los datatypes generados automáticamente
; por sllgen a partir del scanner y la gramática.

(sllgen:make-define-datatypes
 scanner-spec-simple-interpreter
 grammar-simple-interpreter)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes
     scanner-spec-simple-interpreter
     grammar-simple-interpreter)))

;******************************************************************************************
; scan&parse:
; Recibe un string del lenguaje y realiza
; análisis léxico + análisis sintáctico.
; Retorna el AST correspondiente.
(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-simple-interpreter
   grammar-simple-interpreter))

; just-scan:
; Ejecuta únicamente el scanner (análisis léxico)
; para verificar los tokens generados.
(define just-scan
  (sllgen:make-string-scanner
   scanner-spec-simple-interpreter
   grammar-simple-interpreter))

; interpretador:
; Inicia el REPL del lenguaje.
; Permite escribir expresiones y evaluarlas.
(define interpretador
  (sllgen:make-rep-loop
   "--> "
   (lambda (pgm)
     (eval-program pgm))
   (sllgen:make-stream-parser
    scanner-spec-simple-interpreter
    grammar-simple-interpreter)))

;******************************************************************************************
; datatype de procedimientos (cerraduras)

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expression?)
   (amb environment?)))

;******************************************************************************************
; eval-program:
; evalúa un programa completo.
; Inicializa el ambiente y delega
; la evaluación a eval-expression.
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

;******************************************************************************************
; init-env:
; construye el ambiente inicial
; con variables predefinidas.
(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

;******************************************************************************************
; eval-expression:
; evalúa una expresión del lenguaje
; dentro de un ambiente dado.
(define eval-expression
  (lambda (exp env)
    (cases expression exp

      (lit-exp (datum)
               datum)

      (var-exp (id)
               (apply-env env id))

      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))

      ;; NUEVO: declararRec
      (rec-exp (proc-name ids proc-body body)
               (eval-expression
                body
                (extend-env-rec
                 proc-name
                 ids
                 proc-body
                 env))))))

;******************************************************************************************
; eval-rands:
; evalúa una lista de operandos.
(define eval-rands
  (lambda (rands env)
    (map
     (lambda (x)
       (eval-rand x env))
     rands)))

; eval-rand:
; evalúa un único operando.
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;******************************************************************************************
; apply-primitive:
; aplica una operación primitiva
; sobre una lista de argumentos.
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim ()
                (+ (car args) (cadr args)))

      (substract-prim ()
                       (- (car args) (cadr args)))

      (mult-prim ()
                 (* (car args) (cadr args)))

      (incr-prim ()
                 (+ (car args) 1))

      (decr-prim ()
                 (- (car args) 1)))))

;******************************************************************************************
; apply-procedure:
; aplica una cerradura (procedimiento)
; extendiendo el ambiente con los argumentos.
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body saved-env)
                 (eval-expression
                  body
                  (extend-env ids args saved-env))))))

;******************************************************************************************
; AMBIENTES

(define-datatype environment environment?

  (empty-env-record)

  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?))

  (recursively-extended-env-record
   (proc-name symbol?)
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

(define scheme-value?
  (lambda (v) #t))

;-----------------------------------
; empty-env:
; crea un ambiente vacío.
(define empty-env
  (lambda ()
    (empty-env-record)))

;-----------------------------------
; extend-env:
; crea un ambiente extendido agregando
; símbolos y valores al ambiente actual.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;-----------------------------------
; extend-env-rec:
; crea un ambiente recursivo que permite
; que un procedimiento pueda llamarse a sí mismo.
(define extend-env-rec
  (lambda (proc-name ids body env)
    (recursively-extended-env-record
     proc-name
     ids
     body
     env)))

;-----------------------------------
; apply-env:
; busca una variable dentro de un ambiente.
; si no existe genera error.
(define apply-env
  (lambda (env sym)
    (cases environment env

      (empty-env-record ()
                        (eopl:error
                         'apply-env
                         "No binding for ~s"
                         sym))

      (extended-env-record (syms vals old-env)
                           (let ((pos
                                  (list-find-position
                                   sym
                                   syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))

      (recursively-extended-env-record
       (proc-name ids body old-env)
       (if (eqv? sym proc-name)
           (cerradura ids body env)
           (apply-env old-env sym))))))

;******************************************************************************************
; FUNCIONES AUXILIARES

; list-find-position:
; busca la posición de un símbolo
; dentro de una lista.
(define list-find-position
  (lambda (sym los)
    (list-index
     (lambda (sym1)
       (eqv? sym1 sym))
     los)))

; list-index:
; retorna el índice del primer elemento
; que cumple un predicado.
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)

      ((pred (car ls)) 0)

      (else
       (let ((list-index-r
              (list-index pred (cdr ls))))
         (if (number? list-index-r)
             (+ list-index-r 1)
             #f))))))


;******************************************************************************************
; INCISOS DEL TALLER
;******************************************************************************************


(show-the-datatypes)

(just-scan "add1(x)")
(scan&parse "add1(x)")

;--------------------------------------------------
; inciso c)
; procedimiento recursivo que calcula potencia
; potencia(4,2)=16

(scan&parse
 "declararRec
  (
    potencia(base,exp)=
      *(base,
        potencia(base,sub1(exp)));
  )
  {
    potencia(4,2)
  }")

;--------------------------------------------------
; inciso d)
; procedimiento recursivo que suma
; números en un rango [a,b]
; sumaRango(2,5)=14

(scan&parse
 "declararRec
  (
    sumaRango(a,b)=
      +(a,
        sumaRango(add1(a),b));
  )
  {
    sumaRango(2,5)
  }")