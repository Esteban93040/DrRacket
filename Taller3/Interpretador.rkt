#lang eopl

;******************************************************************************************
; Interpretador
; Kevin Andrés Girón Villegas

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter

'(

  (white-sp
   (whitespace) skip)

  (comment
   ("%" (arbno (not #\newline))) skip)

  ; identificadores
  (identifier
   ("@" letter (arbno (or letter digit "_"))) symbol)

  ;  enteros positivos
  (number
   (digit (arbno digit)) number)

  ;  enteros negativos
  (number
   ("-" digit (arbno digit)) number)

  ;  decimales positivos
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)

  ;  decimales negativos
  (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)

  ; textos con aceptación de varios caracteres
  (texto
   ((or letter "_")
    (arbno (or letter digit "_" ":")))
   symbol)
  ))


(define valor-verdad?
  (lambda (x)
    (not (zero? x))))


; Gramatica que acepta el interpretador

(define grammar-simple-interpreter

'((program (expression) un-programa)

  ; numero
  (expression (number) numero-lit)

  ; texto
  (expression ("\"" texto "\"") texto-lit)

  ; variables
  (expression (identifier) var-exp)

  ; operaciones binarias
  (expression ("(" expression primitive-binaria expression ")")
              primapp-bin-exp)

  ; operaciones unarias
  (expression (primitive-unaria "(" expression ")")
              primapp-un-exp)
  
  ; condicionales
  (expression ("Si" expression "{" expression "}" "sino"
                    "{" expression "}")
              condicional-exp)

  ; permitir variables locales
  (expression ("declarar" "(" (arbno identifier "=" expression ";") ")"
                          "{" expression "}") variableLocal-exp)

  ; recursiva
  (expression ("declararRec" "(" identifier "=" expression ";" ")"
                             "{" expression "}") recursivo-exp)
  ; funciones
  (expression ("procedimiento" "(" (separated-list identifier ",") ")"
                               "{" expression "}") procedimiento-exp)
  
  ; procedimientos aplicados
  (expression ("evaluar" expression "(" (separated-list expression ",") ")"
                         "finEval") app-exp)


  ; Catalogo primitivas de mi lenguaje 

  (primitive-binaria ("+") primitiva-suma)

  (primitive-binaria ("~") primitiva-resta)

  (primitive-binaria ("*") primitiva-multi)

  (primitive-binaria ("/") primitiva-div)

  (primitive-binaria ("concat") primitiva-concat)

  (primitive-binaria (">") primitiva-mayor)

  (primitive-binaria ("<") primitiva-menor)

  (primitive-binaria (">=") primitiva-mayor-igual)

  (primitive-binaria ("<=") primitiva-menor-igual)

  (primitive-binaria ("==") primitiva-comparador-igual)

  (primitive-binaria ("!=") primitiva-diferente)

  (primitive-unaria ("longitud") primitiva-longitud)

  (primitive-unaria ("add1") primitiva-add1)

  (primitive-unaria ("sub1") primitiva-sub1)

  (primitive-unaria ("neg") primitiva-negacion-booleana)

))


;datatypes

(sllgen:make-define-datatypes
 scanner-spec-simple-interpreter
 grammar-simple-interpreter)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes
     scanner-spec-simple-interpreter
     grammar-simple-interpreter)))

(define-datatype procVal procVal?

  (cerradura

   (lista-ID (list-of symbol?))

   (exp expression?)

   (amb environment?)))

(define scheme-value?
  (lambda (v) #t))


;Scanner y Parser

(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-simple-interpreter
   grammar-simple-interpreter))

(define just-scan
  (sllgen:make-string-scanner
   scanner-spec-simple-interpreter
   grammar-simple-interpreter))


;Ambiente

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

   (saved-env environment?)))


;Ambiente vacio
(define empty-env
  (lambda ()
    (empty-env-record)))


; Extensión de ambiente

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-rec

  (lambda (proc-name ids body env)

    (recursively-extended-env-record
     proc-name
     ids
     body
     env)))


; Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))




; funciones que me permiten buscar la variable

(define list-find-position
  (lambda (sym los)
    (list-index
     (lambda (sym1)
       (eqv? sym1 sym))
     los)))


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


(define buscar-variable
  (lambda (sym env)
    (cases environment env

      (empty-env-record ()
        "Error, la variable no existe")

      (extended-env-record (syms vals old-env)

        (let ((pos (list-find-position sym syms)))

          (if (number? pos)
              (list-ref vals pos)
              (buscar-variable sym old-env))))


      (recursively-extended-env-record
       (proc-name ids body saved-env)

       (if (eqv? sym proc-name)

           (cerradura
            ids
            body
            env)

           (buscar-variable sym saved-env))))))



; Ejecutar

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (exp)
        (eval-expression exp (init-env))))))



; Permite procesar las operaciones binarias
(define apply-primitive-binaria
  (lambda (prim arg1 arg2)

    (cases primitive-binaria prim

      (primitiva-suma ()
        (+ arg1 arg2))

      (primitiva-resta ()
        (- arg1 arg2))

      (primitiva-multi ()
        (* arg1 arg2))

      (primitiva-div ()
        (/ arg1 arg2))

      (primitiva-concat ()
         (string-append
          (if (symbol? arg1)
              (symbol->string arg1)
              arg1)
          (if (symbol? arg2)
              (symbol->string arg2)
              arg2)))

      (primitiva-mayor ()
        (if (> arg1 arg2) 1 0))

      (primitiva-menor ()
        (if (< arg1 arg2) 1 0))

      (primitiva-mayor-igual ()
        (if (>= arg1 arg2) 1 0))

      (primitiva-menor-igual ()
        (if (<= arg1 arg2) 1 0))

      (primitiva-comparador-igual ()
        (if (= arg1 arg2) 1 0))

      (primitiva-diferente ()
        (if (not (= arg1 arg2)) 1 0)))))


; Permite procesar las operaciones unarias
(define apply-primitive-unaria

  (lambda (prim arg)

    (cases primitive-unaria prim

      (primitiva-longitud ()
        (string-length
         (if (symbol? arg)
             (symbol->string arg)
             arg)))

      (primitiva-add1 ()
        (+ arg 1))

      (primitiva-sub1 ()
        (- arg 1))

      (primitiva-negacion-booleana ()
        (if (valor-verdad? arg)
            0
            1)))))


; permite ejecutar los procedimientos

(define apply-procedure

  (lambda (proc args)

    (cases procVal proc

      (cerradura
       (ids cuerpo amb)

       (eval-expression
        cuerpo
        (extend-env ids args amb))))))

; permite evaluar cada expresión
(define eval-expression
  (lambda (exp env)

    (cases expression exp

      (numero-lit (num)
        num)

      (texto-lit (txt)
        txt)

      (var-exp (id)
        (buscar-variable id env))

      (primapp-bin-exp (exp1 prim exp2)
        (apply-primitive-binaria
         prim
         (eval-expression exp1 env)
         (eval-expression exp2 env)))

      (primapp-un-exp (prim exp)

                      (apply-primitive-unaria

                       prim

                       (eval-expression exp env)))

      (condicional-exp
       (test-exp true-exp false-exp)

       (if (valor-verdad?
            (eval-expression test-exp env))

           (eval-expression true-exp env)

           (eval-expression false-exp env)))

      (variableLocal-exp
       (ids exps cuerpo)

       (letrec
           ((crear-ambiente-secuencial
             (lambda (ids exps env-actual)

               (if (null? ids)

                   env-actual

                   (let* ((valor
                           (eval-expression
                            (car exps)
                            env-actual))

                          (nuevo-env
                           (extend-env
                            (list (car ids))
                            (list valor)
                            env-actual)))

                     (crear-ambiente-secuencial
                      (cdr ids)
                      (cdr exps)
                      nuevo-env))))))

         (eval-expression
          cuerpo
          (crear-ambiente-secuencial
           ids
           exps
           env))))


      (procedimiento-exp
       (ids cuerpo)

       (cerradura
        ids
        cuerpo
        env))

      (app-exp
       (exp exps)

       (let ((proc
              (eval-expression exp env))

             (args
              (map
               (lambda (e)
                 (eval-expression e env))
               exps)))

         (apply-procedure proc args)))


      (recursivo-exp
       (id proc-exp cuerpo)

       (eval-expression

        cuerpo

        (extend-env-rec

         id

         (cases expression proc-exp

           (procedimiento-exp (ids body)
                              ids)

           (else
            (eopl:error
             'recursivo-exp
             "Se esperaba un procedimiento")))

         (cases expression proc-exp

           (procedimiento-exp (ids body)
                              body)

           (else
            (eopl:error
             'recursivo-exp
             "Se esperaba un procedimiento")))

         env))))))


;; NOTA: SE TOMA DE REFERENCIA TODOS LOS ARCHIVOS DADOS PARA LA IMPLEMENTACIÓN DEL INTERPRETADOR DADOS COMO EJEMPLOS POR PARTE DEL PROFEROS
;; SIGUIENDO EL LIBRO
;;;;;; PRUEBAS E Y F

(eval-program
 (scan&parse

"declarar (

 @x=2;

 @y=3;

 @a=procedimiento (@x,@y,@z)
 {
   ((@x + @y) + @z)
 };

)

{

 evaluar @a (1,2,@x) finEval

}"))

(eval-program
 (scan&parse

"declarar(

 @f=
 procedimiento()
 {
   10
 };

 @g=
 procedimiento(@x)
 {
   procedimiento()
   {
      evaluar @x() finEval
   }
 };

)

{
 declarar(
   @h=
   evaluar @g(@f) finEval;
 )
 {
   evaluar @h() finEval
 }
}"))

(eval-program
 (scan&parse

"declarar(

 @integrantes=
 procedimiento()
 {
   \"Robinson_y_Sara\"
 };

 @saludar=
 procedimiento(@f)
 {
   procedimiento()
   {
      (\"Hola:\" concat evaluar @f() finEval)
   }
 };

 @decorate=
 evaluar @saludar(@integrantes) finEval;

)

{
 evaluar @decorate() finEval
}"))

(eval-program
 (scan&parse

"declarar(

 @integrantes=
 procedimiento()
 {
   \"Robinson_y_Sara\"
 };

 @saludar=
 procedimiento(@f)
 {
   procedimiento(@msg)
   {
      ((\"Hola:\" concat evaluar @f() finEval)
       concat
       @msg)
   }
 };

 @decorate=
 evaluar @saludar(@integrantes) finEval;

)

{
 evaluar @decorate(\"_ProfesoresFLP\") finEval
}"))




