#lang eopl

;******************************************************************************************
;;;;; Interpretador Propio

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-interpreter
  '((white-sp  ;Espacios en blanco
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip) ;comentarios
    (identificador
     ("@" (arbno (or letter digit))) symbol) ;delcaración de variables, una variable se crea con @ seguido de una letra o digito y luego cualquier combinación de letras o dígitos
    (texto
     (letter (arbno (or letter digit "_"))) string);declaracion de texto, un texto se crea con una letra seguida de cualquier cantidad de letras o numeros o guiones bajos, los guiones bajos se usan para seprar palabras
    (numero
     (digit (arbno digit)) number) ;declaración de numeros, un numero se crea con un digito seguido de cualquier cantidad de numeros
    (numero
     ("-" digit (arbno digit)) number);declaración de numeros negativos, un numero negativo se crea con un guion seguido de un digito y luego cualquier cantidad de numeros
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number);declaración de numeros decimales, un numero decimal se crea con un digito seguido de cualquier cantidad de numeros, luego un punto y luego cualquier cantidad de numeros
    (numero
     ("-" digit (arbno digit) "." digit (arbno digit)) number)));declaración de numeros decimales negativos, un numero decimal negativo se crea con un guion seguido de un digito y luego cualquier cantidad de numeros luego un punto y luego cualquier cantidad de numeros

;Especificación Sintáctica (gramática)

(define grammar-interpreter ;gramtica
  '((programa (expresion) un-programa) ;un programa es una expresión
    (expresion (numero) numero-lit) ;una expresión puede ser un numero, se representa con numero-lit
    (expresion ("\"" texto "\"") texto-lit) ;una expresión puede ser un texto, se representa con texto-lit
    (expresion (identificador) var-exp) ; una expresión puede ser una variable, se representa con var-exp
    (expresion ;una expresión puede ser una aplicación de una primitiva binaria, se representa con primapp-bin-exp
     ("(" expresion primitiva-binaria expresion ")");una primapp-bin-exp se representa con un parentesis seguido de una expresión seguido de una primitiva binaria segudo de una expresión y se cierran parentesis
     primapp-bin-exp)
    (expresion ;una expresión puede ser una aplicación de una primitiba unaria, se representa con primapp-un-exp
     (primitiva-unaria "(" expresion ")");una primapp-un-exp se representa con una primitiva unaria seguida de un parentesis seguido de una expresión y se cierran parentesis
     primapp-un-exp)
    (expresion
     ("if" expresion "{" expresion "}" "else" "{" expresion "}");una expresión puede ser un condicional, se representa con condicional-exp, un condicional se representa con la palabra if seguida de una expresión seguida de un bloque de código entre llaves seguido de la palabra else seguida de otro bloque de código entre llaves
     condicional-exp)
    (expresion ;una expresión puede ser una declaración de variables locales, se representa con variableLocal-exp, una declaración de variables locales se representa con la palabra declarar seguida de un parentesis seguido de cualquier cantidad de declaraciones de variables seguidas de un bloque de código entre llaves
     ("declarar" "(" (arbno identificador "=" expresion ";") ")" "{" expresion "}")
     variableLocal-exp)
    (expresion ;una expresión puede ser la declaración de un procedimiento, se representa con procedimiento-exp, una declaración de procedimiento se representa con la palabra procedimiento seguida de un parentesis seguido de cualquier cantidad de identificadores separados por comas seguidos de un bloque de código entre llaves
     ("procedimiento" "(" (separated-list identificador ",") ")" "{" expresion "}")
     procedimiento-ex)
    (expresion
     ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")
     app-exp)
    (expresion
    ("declararRec" "(" identificador "(" (separated-list identificador ",") ")" "=" expresion ";" ")" "{" expresion "}") 
    rec-exp)
    (primitiva-binaria ("+") primitiva-suma) ;primitiva de suma, se representa con un + y se llama primitiva-suma
    (primitiva-binaria ("~") primitiva-resta) ;primitiva de resta, se representa con un ~ y se llama primitiva-resta
    (primitiva-binaria ("/") primitiva-div) ;primitiva de division, se representa con un / y se llama primitiva-div
    (primitiva-binaria ("mod") primitiva-mod) ;primitiva de modulo, se representa con la palabra mod y se llama primitiva-mod
    (primitiva-binaria ("div") primitiva-div-entera) ;primitiva de division entera, se representa con la palabra div y se llama primitiva-div
    (primitiva-binaria ("*") primitiva-multi) ;primitiva de multiplicacion, se representa con un * y se llama primitiva-multi
    (primitiva-binaria ("concat") primitiva-concat);primitiva de concatenacion, se representa con la palabra concat y se llama primitiva-concat
    (primitiva-binaria (">") primitiva-mayor);primitiva de mayor que, se representa con un > y se llama primitiva-mayor
    (primitiva-binaria ("<") primitiva-menor);primitiva de menor que, se representa con un < y se llama primitiva-menor
    (primitiva-binaria (">=") primitiva-mayor-igual);primitiva de mayor o igual que, se representa con un >= y se llama primitiva-mayor-igual
    (primitiva-binaria ("<=") primitiva-menor-igual);primitiva de menor o igual que, se representa con un <= y se llama primitiva-menor-igual
    (primitiva-binaria ("!=") primitiva-diferente);primitiva de diferente, se representa con != y se llama primitiva-diferente
    (primitiva-binaria ("==") primitiva-comparador-igual);primitiva de comparador de igualdad, se representa con == y se llama primitiva-comparador-igual 
    (primitiva-unaria ("longitud") primitiva-longitud);primitiva de longitud, se representa con la palabra longitud y se llama primitiva-longitud
    (primitiva-unaria ("add1") primitiva-add1);primitiva de add1, se representa con la palabra add1 y se llama primitiva-add1
    (primitiva-unaria ("sub1") primitiva-sub1);primitiva de sub1, se representa con la palabra sub1 y se llama primitiva-sub1
    (primitiva-unaria ("neg") primitiva-negacion-booleana)));primitiva de negacion booleana, se representa con la palabra neg y se llama primitiva-negacion-booleana

;Construidos automáticamente (se copia del archivo interpretador_simple.rkt):

(sllgen:make-define-datatypes 
scanner-spec-interpreter 
grammar-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes 
  scanner-spec-interpreter 
  grammar-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz (se copia del archivo interpretador_simple.rkt):

(define scan&parse
  (sllgen:make-string-parser 
  scanner-spec-interpreter 
  grammar-interpreter))

(define just-scan
  (sllgen:make-string-scanner 
  scanner-spec-interpreter 
  grammar-interpreter))

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-interpreter
      grammar-interpreter)))

;*******************************************************************************************
;El Interprete

(define eval-program
  (lambda (pgm) ;recube un programa y devuelve un numero
    (cases programa pgm ;evalua los casos de programa
      (un-programa (body) ;si el programa es un un-programa, se evalua el cuerpo del programa con el ambiente inicial
                (eval-expression body (init-env)))))) ;evalua el cuerpo del programa con el ambiente inicial

; Ambiente inicial
(define init-env ;se declara el ambiente inicial siguiendo las indicaciones del taller
  (lambda ()
    (extend-env
      '(@a @b @c @d @e)
      '(1 2 3 "hola" "FLP")
      (empty-env))))

(define valor-verdad?
  (lambda (x)
    (not (and (number? x) (= x 0)))))

(define bool->int
  (lambda (b)
    (if b 1 0)))

;eval-expression: <expresion> <enviroment> -> valor expresado
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable id env))
      (primapp-bin-exp (exp1 prim-bin exp2)
                  (let ((arg1 (eval-expression exp1 env))
                        (arg2 (eval-expression exp2 env)))
                     (apply-primitiva-binaria prim-bin arg1 arg2)))
      (primapp-un-exp (prim-un exp1)
                  (let ((arg (eval-expression exp1 env)))
                     (apply-primitiva-unaria prim-un arg)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (eval-expression test-exp env))
                           (eval-expression true-exp env)
                           (eval-expression false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                         (let ((vals (map (lambda (e) (eval-expression e env)) exps)))
                           (eval-expression cuerpo (extend-env ids vals env))))
      (procedimiento-ex (ids cuerpo)
                        (cerradura ids cuerpo env))
      (rec-exp (proc-name ids proc-body body)
               (eval-expression body (extend-env-rec proc-name ids proc-body env)))
      (app-exp (exp exps)
               (let ((proc (eval-expression exp env))
                     (args (map (lambda (e) (eval-expression e env)) exps)))
                 (if (procVal? proc)
                     (cases procVal proc
                       (cerradura (ids cuerpo env-proc)
                                  (if (= (length ids) (length args))
                                      (eval-expression cuerpo (extend-env ids args env-proc))
                                      (eopl:error 'eval-expression "El número de argumentos no coincide con el esperado."))))
                     (eopl:error 'eval-expression "Intentando evaluar algo que no es un procedimiento.")))))))

(define apply-primitiva-binaria
  (lambda (prim arg1 arg2)
    (cases primitiva-binaria prim
    
      (primitiva-suma () (+ arg1 arg2))
      (primitiva-resta () (- arg1 arg2))
      (primitiva-div () (/ arg1 arg2))
      (primitiva-mod () (modulo arg1 arg2))
      (primitiva-div-entera () (quotient arg1 arg2))
      (primitiva-multi () (* arg1 arg2))
      (primitiva-concat () (string-append arg1 arg2))
      (primitiva-mayor () (bool->int (> arg1 arg2)))
      (primitiva-menor () (bool->int (< arg1 arg2)))
      (primitiva-mayor-igual () (bool->int (>= arg1 arg2)))
      (primitiva-menor-igual () (bool->int (<= arg1 arg2)))
      (primitiva-diferente () (bool->int (not (equal? arg1 arg2))))
      (primitiva-comparador-igual () (bool->int (equal? arg1 arg2))))))

(define apply-primitiva-unaria
  (lambda (prim arg)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length arg))
      (primitiva-add1 () (+ arg 1))
      (primitiva-sub1 () (- arg 1))
      (primitiva-negacion-booleana () (if (valor-verdad? arg) 0 1)))))

;*******************************************************************************************
;Ambientes

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record 
  (syms (list-of symbol?))         
  (vals (list-of scheme-value?))
                      (env environment?))
  (recursively-extended-env-record (proc-name symbol?)
                                   (ids (list-of symbol?))
                                   (body expresion?)
                                   (env environment?)))

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb environment?)))

(define scheme-value? (lambda (v) #t))

(define empty-env  
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

(define extend-env-rec
  (lambda (proc-name ids body env)
    (recursively-extended-env-record proc-name ids body env)))

(define buscar-variable
  (lambda (sym env)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'buscar-variable "Error, la variable no existe"))
      (extended-env-record (syms vals env-next)
                          (let ((pos (list-find-position sym syms)))
                            (if (number? pos)
                                (list-ref vals pos)
                                (buscar-variable sym env-next))))
      (recursively-extended-env-record (proc-name ids body env-next)
                                       (if (eqv? sym proc-name)
                                           (cerradura ids body env)
                                           (buscar-variable sym env-next))))))

;****************************************************************************************
;Funciones Auxiliares

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

;--------------------------------------------------
; inciso a)
; procedimiento recursivo que la suma de digitos
; @sumarDigitos(147)=12

;se declara un procedimiento recursivo que tendra el nombre de sumarDigitos y recibira un parametro n
;si n es igual a 0 entonces el resultado es 0, sino entonces evalua el resultado de sumar el resultado de n mod 10 con el resultado de llamar nuevamente a sumarDigitos con un parametro div 10
; luego se evalua el resultado de llamar a sumarDigitos con el parametro 147, se espera que el resultado sea 12
(eval-program
      (scan&parse
   "declararRec (
      @sumarDigitos(@n) =
        if (@n == 0) { 0 }
        else { ((@n mod 10) + evaluar @sumarDigitos( (@n div 10) ) finEval) };
    )
    {
      evaluar @sumarDigitos(147) finEval
    }")
)

;--------------------------------------------------
; inciso b)
; procedimiento recursivo que calcula el factorial
; factorial(5)=120
; factorial(10)=3628800

;se declara un procedimiento recursivo que tendra el nombre de factorial y recibira un parametro n
;si n es igual a 0 entonces el resultado es 1, sino entonces evalua el resultado de multiplicar n por el resultado de llamar nuevamente a factorial con un parametro -1
; luego se evalua el resultado de llamar a factorial con el parametro 5 y luego con el parametro 10, se espera que el resultado sea 120 y 3628800 
(eval-program
      (scan&parse
   "declararRec ( 
      @factorial(@n) =
        if (@n == 0) { 1 }
        else { (@n * evaluar @factorial(sub1(@n)) finEval) };
    )
    {
      evaluar @factorial(5) finEval
    }")
)

(eval-program
      (scan&parse
   "declararRec (
      @factorial(@n) =
        if (@n == 0) { 1 }
        else { (@n * evaluar @factorial(sub1(@n)) finEval) };
    )
    {
      evaluar @factorial(10) finEval
    }")
)

;--------------------------------------------------
; inciso c)
; procedimiento recursivo que calcula potencia
; potencia(4,2)=16

;se declara un procedimiento recursivo que tendra el nombre de potencia y recibira dos parametros base y exp
;si exp es igual a 0 entonces el resultado es 1, sino entonces evalua
; el resultado de multiplicar base por el resultado de llamar nuevamente a potencia con el mismo parametro base y un parametro exp -1
; luego se evalua el resultado de llamar a potencia con los parametros 4 y 2, se espera que el resultado sea 16
  (eval-program
    (scan&parse
     "declararRec (
        @potencia(@base, @exp) =
          if (@exp == 0) { 1 } 
          else { (@base * evaluar @potencia(@base, sub1(@exp)) finEval) };
      )
      {
        evaluar @potencia(4, 2) finEval
      }"))

;--------------------------------------------------
; inciso d)
; procedimiento recursivo que suma
; numeros en un rango [a,b]
; sumaRango(2,5)=14

;se declara un procedimiento recursivo que tendra el nombre de sumaRango y recibira dos parametros a y b
;si a es mayor que b entonces el resultado es 0, sino entonces evalua el resultado de sumar a con el resultado de llamar nuevamente a sumaRango con un parametro add1(a) y el mismo parametro b
; luego se evalua el resultado de llamar a sumaRango con los parametros 2 y 5, se espera que el resultado sea 14
(eval-program
      (scan&parse
   "declararRec (
      @sumaRango(@a, @b) =
        if (@a > @b) { 0 }
        else { (@a + evaluar @sumaRango(add1(@a), @b) finEval) };
    )
    {
      evaluar @sumaRango(2, 5) finEval
    }")
)





