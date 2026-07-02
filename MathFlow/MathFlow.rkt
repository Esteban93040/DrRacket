#lang eopl

(require (only-in racket/base 
                  make-hash 
                  hash-set! 
                  hash-ref 
                  hash-has-key? 
                  hash-keys 
                  hash-values 
                  hash?))

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= var {<identifier> = <expression>}* in <expression>
;;                      <var-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'(
  ;; espacios
  (white-sp
   (whitespace) skip)
  ;; comentarios
  (comment
   ("%" (arbno (not #\newline))) skip)
  ;; identificadores
  (identifier
   (letter (arbno (or letter digit "?" "_"))) symbol)
  ;; números reales
  (real
  (digit (arbno digit) "." digit (arbno digit))
   number)
  (real
  ("-" digit (arbno digit) "." digit (arbno digit))
   number)
  ;; enteros positivos
  (number
   (digit (arbno digit)) number)
  ;; enteros negativos
  (number
   ("-" digit (arbno digit)) number)
  ;; cadenas
  (text
    ("\"" (arbno (not #\")) "\"") string)
))


;Especificación Sintáctica (gramática)
(define grammar-simple-interpreter
  '((program
      (declaration (arbno ";" declaration))
      a-program)

    ; --- declaraciones (var, const) con coma permitida ---
    (declaration
      ("var" identifier "=" expression
            (arbno "," identifier "=" expression))
      var-decl-exp)
    (declaration
      ("const" identifier "=" expression
              (arbno "," identifier "=" expression))
      const-decl-exp)
    (declaration
      (expression)
      expr-decl)
    (declaration
      ("symbol" (separated-list identifier ","))
      symbol-decl-exp)

    ; --- expresiones ---
    (expression (number) lit-exp)
    (expression (real) real-exp)
    (expression (text) text-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("null") null-exp)
    (expression
      ("print" "(" expression ")")
      print-exp)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("(" expression primitive expression ")")
      infix-primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      ("func"
        identifier
        "("
        (separated-list identifier ",")
        ")"
        "{"
        (arbno declaration ";")
        "return"
        expression
        "}")
      func-exp)
    (expression
      (identifier (arbno "(" (separated-list expression ",") ")"))
      var-or-app-exp)
    (expression
      ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
      letrec-exp)
    (expression
      ("begin" declaration (arbno ";" declaration) "end")
      begin-exp)
    (expression
      ("set" identifier "=" expression)
      set-exp)
    (expression ("[" (separated-list expression ",") "]") list-exp)
    (expression ("{" identifier ":" expression (arbno "," identifier ":" expression) "}") dict-exp)
    (expression
      ("while" expression "do" declaration (arbno ";" declaration) "done")
      while-exp)
    (expression ("for" identifier "in" expression "do" expression (arbno ";" expression) "done") for-exp)
    (expression ("switch" expression "{" 
                 (arbno "case" expression ":" expression ";") 
                 "default" ":" expression ";" 
                 "}") 
                switch-exp)
    
    (primitive ("<")  menor-prim)
    (primitive (">")  mayor-prim)
    (primitive ("<=") menor-igual-prim)
    (primitive (">=") mayor-igual-prim)
    (primitive ("==") igual-prim)
    (primitive ("<>") diferente-prim)
    (primitive ("and") and-prim)
    (primitive ("or")  or-prim)
    (primitive ("not") not-prim)
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("mod") modulo-prim)
    (primitive ("/") division-prim)
    (primitive ("longitud")   longitud-prim)
    (primitive ("concatenar") concatenar-prim)
    (primitive ("vacio?") vacio-pred-prim)
    (primitive ("vacio")  vacio-prim)
    (primitive ("crear-lista") crear-lista-prim)
    (primitive ("lista?") lista-pred-prim)
    (primitive ("cabeza") cabeza-prim)
    (primitive ("cola")   cola-prim)
    (primitive ("append") append-prim)
    (primitive ("ref-list") ref-list-prim)
    (primitive ("set-list") set-list-prim)
    (primitive ("diccionario?") diccionario-pred-prim)
    (primitive ("crear-diccionario") crear-diccionario-prim)
    (primitive ("ref-diccionario") ref-diccionario-prim)
    (primitive ("set-diccionario") set-diccionario-prim)
    (primitive ("claves") claves-prim)
    (primitive ("valores") valores-prim)
    
    ))


;Tipos de datos para la sintaxis abstracta de la gramática
;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (primera-decl resto-decls)
        (let loop ((cur-decl  primera-decl)
                   (cur-env   (init-env))
                   (restando  resto-decls))
          (let* ((resultado  (eval-declaration cur-decl cur-env))
                 (nuevo-env  (if (environment? resultado)
                                 resultado
                                 cur-env)))
            (if (null? restando)
                resultado
                (loop (car restando)
                      nuevo-env
                      (cdr restando)))))))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     (list 1 5 10)   ; ← valores simples, sin direct-target
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target
    (expval expval?)
    (constante? boolean?)
    (simbolo? boolean?))

  (indirect-target
    (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************
(define eval-declaration
  (lambda (decl env)
    (cases declaration decl
      (var-decl-exp (id rand ids rands)
        (let loop ((all-ids   (cons id ids))
                   (all-rands (cons rand rands))
                   (cur-env   env))
          (cond
            ((null? all-ids) cur-env)
            ((conflicts-with-symbol-binding? cur-env (car all-ids))
             (eopl:error 'eval-declaration
                         "El identificador ~s ya está reservado como símbolo"
                         (car all-ids)))
            (else
             (let ((val (eval-expression (car all-rands) cur-env)))
               (loop (cdr all-ids)
                     (cdr all-rands)
                     (extended-env-record
                      (list (car all-ids))
                      (vector (direct-target val #f #f))
                      cur-env)))))))
      (const-decl-exp (id rand ids rands)
        (let loop ((all-ids   (cons id ids))
                   (all-rands (cons rand rands))
                   (cur-env   env))
          (cond
            ((null? all-ids) cur-env)
            ((conflicts-with-symbol-binding? cur-env (car all-ids))
             (eopl:error 'eval-declaration
                         "El identificador ~s ya está reservado como símbolo"
                         (car all-ids)))
            (else
             (let ((val (eval-expression (car all-rands) cur-env)))
               (loop (cdr all-ids)
                     (cdr all-rands)
                     (extended-env-record
                      (list (car all-ids))
                      (vector (direct-target val #t #f))
                      cur-env)))))))
      (symbol-decl-exp (ids)
        (let loop ((all-ids ids)
                   (cur-env env))
          (cond
            ((null? all-ids) cur-env)
            ((conflicts-with-non-symbol-binding? cur-env (car all-ids))
             (eopl:error 'eval-declaration
                         "El identificador ~s ya está reservado como variable"
                         (car all-ids)))
            (else
             (loop (cdr all-ids)
                   (extended-env-record
                    (list (car all-ids))
                    (vector (direct-target (car all-ids) #t #t))
                    cur-env))))))
      (expr-decl (exp)
        (cases expression exp
          (func-exp (nombre ids exps return-exp)
            (let* ((vec (make-vector 1))
                  (nuevo-env (extended-env-record (list nombre) vec env)))
              (vector-set! vec 0
                (direct-target
                  (closure-with-body nombre ids exps return-exp nuevo-env)
                  #f
                  #f))
              nuevo-env))
          (else
          (eval-expression exp env))))
    )))

(define conflicts-with-symbol-binding?
  (lambda (env id)
    (let ((kind (binding-kind env id)))
      (and kind (eq? kind 'symbol)))))

(define conflicts-with-non-symbol-binding?
  (lambda (env id)
    (let ((kind (binding-kind env id)))
      (and kind (not (eq? kind 'symbol))))))

(define decl-env?
  (lambda (resultado decl)
    (environment? resultado)))


(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (real-exp (num)
        num)
      (text-exp (txt)
        (if (and (string? txt) (> (string-length txt) 1))
            (substring txt 1 (- (string-length txt) 1))
            txt))
      (true-exp ()
        #t)
      (false-exp ()
        #f)
      (null-exp ()
        'null)
      (var-or-app-exp (id args-lists)
        (if (null? args-lists)
            (apply-env env id)
            (let ((proc (apply-env env id))
                  (args (eval-rands (car args-lists) env)))
              (if (procval? proc)
                  (apply-procedure proc args)
                  (eopl:error
                   'eval-expression
                   "No es una función: ~s"
                   id)))))
      (primapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-primitive prim args)))
      (infix-primapp-exp (exp1 prim exp2)
        (apply-primitive prim
                         (list (eval-expression exp1 env)
                               (eval-expression exp2 env))))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (func-exp (nombre ids exps return-exp)
        (closure-with-body nombre ids exps return-exp env))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (begin-exp (primera-decl resto-decls)
        (let loop ((cur-decl  primera-decl)
                  (cur-env   env)
                  (restando  resto-decls))
          (let* ((resultado  (eval-declaration cur-decl cur-env))
                (nuevo-env  (if (decl-env? resultado cur-decl)
                                resultado
                                cur-env)))
            (if (null? restando)
                resultado
                (loop (car restando)
                      nuevo-env
                      (cdr restando))))))
      (print-exp (exp1)
           (let ((valor (eval-expression exp1 env)))
             (display valor)
             (newline)
             valor))
      (while-exp (condicion primera-decl resto-decls)
        (let loop ((cur-env env))
          (if (true-value? (eval-expression condicion cur-env))
              (let inner ((cur-decl  primera-decl)
                          (inner-env cur-env)
                          (restando  resto-decls))
                (let* ((resultado  (eval-declaration cur-decl inner-env))
                      (nuevo-env  (if (environment? resultado)
                                      resultado
                                      inner-env)))
                  (if (null? restando)
                      (loop nuevo-env)              ; ← usa el ambiente actualizado
                      (inner (car restando) nuevo-env (cdr restando)))))
              'null)))
      (for-exp (id lista-exp primera-exp resto-exps)
        (let ((lista-val (eval-expression lista-exp env)))
          (if (and (vector? lista-val) (list? (vector-ref lista-val 0)))
              (let ((lista-real (vector-ref lista-val 0)))
                (for-each
                  (lambda (elemento)
                    (let ((cur-env (extended-env-record
                                      (list id)
                                      (vector (direct-target elemento #f #f))
                                      env)))
                      (eval-expression primera-exp cur-env)
                      (for-each (lambda (ex) (eval-expression ex cur-env)) resto-exps)))
                  lista-real)
                'null)
              (eopl:error 'for-exp "Se esperaba una lista de MathFlow, pero se obtuvo: ~s" lista-val))))
      (switch-exp (exp-principal exps-casos exps-cuerpos exp-default)
        (let ((valor-principal (eval-expression exp-principal env)))
          (let iterar-casos ((casos exps-casos)
                             (cuerpos exps-cuerpos))
            (if (null? casos)
                (eval-expression exp-default env)
                (let ((valor-caso-actual (eval-expression (car casos) env)))
                  (if (equal? valor-principal valor-caso-actual)
                      (eval-expression (car cuerpos) env)
                      (iterar-casos (cdr casos) (cdr cuerpos))))))))
      (list-exp (exps)
        (let ((valores-evaluados (map (lambda (e) (eval-expression e env)) exps)))
          (vector valores-evaluados)))  
      (dict-exp (id1 exp1 ids exps)
        (let ((h (make-hash))
              (claves (map symbol->string (cons id1 ids)))
              (valores (map (lambda (e) (eval-expression e env)) (cons exp1 exps))))
          (for-each (lambda (k v) (hash-set! h k v)) claves valores)
          h))

  
      )))
      

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand

      (var-or-app-exp (id args-lists)
        (if (null? args-lists)
            (indirect-target
             (let ((ref (apply-env-ref env id)))
               (cases target (primitive-deref ref)
                 (direct-target (expval constante? simbolo?)
                   ref)
                 (indirect-target (ref1)
                   ref1))))
            (direct-target
             (eval-expression rand env)
             #f
             #f)))

      (else
       (direct-target
        (eval-expression rand env)
        #f
        #f)))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-var-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-var-exp-rand x env))
         rands)))

(define eval-var-exp-rand
  (lambda (rand env)
    (direct-target
      (eval-expression rand env)
      #f
      #f)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      ; aritméticas existentes
      (add-prim ()        (symbolic-bin-op '+ + (car args) (cadr args)))
      (substract-prim ()  (symbolic-bin-op '- - (car args) (cadr args)))
      (mult-prim ()       (symbolic-bin-op '* * (car args) (cadr args)))
      (incr-prim ()       (symbolic-un-op (string->symbol "add1") (lambda (x) (+ x 1)) (car args)))
      (decr-prim ()       (symbolic-un-op (string->symbol "sub1") (lambda (x) (- x 1)) (car args)))
      ; relacionales
      (menor-prim ()        (apply-primitive-relacion '< < (car args) (cadr args)))
      (mayor-prim ()        (apply-primitive-relacion '> > (car args) (cadr args)))
      (menor-igual-prim ()  (apply-primitive-relacion '<= <= (car args) (cadr args)))
      (mayor-igual-prim ()  (apply-primitive-relacion '>= >= (car args) (cadr args)))
      (igual-prim ()        (apply-primitive-relacion '== equal? (car args) (cadr args)))
      (diferente-prim ()    (apply-primitive-relacion '<> (lambda (a b) (not (equal? a b))) (car args) (cadr args)))
      ; booleanos
      (and-prim () (and (true-value? (car args)) (true-value? (cadr args))))
      (or-prim ()  (or  (true-value? (car args)) (true-value? (cadr args))))
      (not-prim () (not (true-value? (car args))))
      (modulo-prim ()      (symbolic-bin-op 'mod modulo (car args) (cadr args)))
      (division-prim ()    (symbolic-bin-op '/ / (car args) (cadr args)))
      (longitud-prim ()    (string-length (car args)))
      (concatenar-prim ()  (string-append (car args) (cadr args)))
      ; --- Primitivas de Listas ---
      (vacio-pred-prim () (null? (vector-ref (car args) 0)))
      (vacio-prim ()      (vector '()))
      (crear-lista-prim () (vector args))
      (lista-pred-prim () (and (vector? (car args)) (list? (vector-ref (car args) 0))))
      (cabeza-prim ()     (car (vector-ref (car args) 0)))
      (cola-prim ()       (vector (cdr (vector-ref (car args) 0))))
      (append-prim ()     (vector (append (vector-ref (car args) 0) (vector-ref (cadr args) 0))))
      (ref-list-prim ()   (list-ref (vector-ref (car args) 0) (cadr args)))
      (set-list-prim ()   
        (let* ((vec (car args))
               (lst (vector-ref vec 0))
               (idx (cadr args))
               (val (caddr args)))
          (vector-set! vec 0 (reemplazar-en-lista lst idx val))
          val))
      (diccionario-pred-prim () (hash? (car args)))
      (crear-diccionario-prim () (make-hash))
      (ref-diccionario-prim ()
        (let ((h (car args))
              (k (cadr args)))
          (if (hash-has-key? h k)
              (hash-ref h k)
              (eopl:error 'ref-diccionario "Clave no encontrada: ~s" k))))    
      (set-diccionario-prim ()
        (let ((h (car args))
              (k (cadr args))
              (v (caddr args)))
          (hash-set! h k v)
          v))
      (claves-prim ()
        (vector (hash-keys (car args)))) 
      (valores-prim ()
        (vector (hash-values (car args))))
    )))

(define symbolic-operator?
  (lambda (op)
    (memq op (list '+ '- '* '/ 'mod (string->symbol "add1") (string->symbol "sub1")))))

(define symbolic-expression?
  (lambda (v)
    (or (symbol? v)
     (and (list? v)
       (or (and (= (length v) 2)
          (symbolic-operator? (car v)))
        (and (= (length v) 3)
          (symbolic-operator? (cadr v))))))))

(define symbolic-value?
  (lambda (v)
    (symbolic-expression? v)))

(define normalize-symbolic-value
  (lambda (v)
    (cond
      ((symbolic-value? v) v)
      ((number? v) v)
      (else
       (eopl:error 'normalize-symbolic-value
                   "Se esperaba un número o una expresión simbólica, se obtuvo: ~s"
                   v)))))

(define symbolic-bin-op
  (lambda (op numeric-op lhs rhs)
    (cond
      ((and (number? lhs) (number? rhs))
       (numeric-op lhs rhs))
      ((or (symbolic-value? lhs) (symbolic-value? rhs))
       (list (normalize-symbolic-value lhs)
             op
             (normalize-symbolic-value rhs)))
      (else
       (eopl:error 'symbolic-bin-op
                   "Operación aritmética no soportada con valores: ~s y ~s"
                   lhs rhs)))))

(define symbolic-un-op
  (lambda (op numeric-op value)
    (cond
      ((number? value)
       (numeric-op value))
      ((symbolic-value? value)
       (list op (normalize-symbolic-value value)))
      (else
       (eopl:error 'symbolic-un-op
                   "Operación aritmética no soportada con valor: ~s"
                   value)))))

(define apply-primitive-relacion
  (lambda (op pred lhs rhs)
    (if (and (number? lhs) (number? rhs))
        (pred lhs rhs)
        (eopl:error 'apply-primitive-relacion
                    "La primitiva relacional ~s requiere operandos numéricos: ~s y ~s"
                    op lhs rhs))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (v)
    (cond
      ((number? v)
       (not (zero? v)))
      ((boolean? v)
       v)
      ((string? v)
       (not (string=? v "")))   ; "" es falsy
      ((eq? v 'null)
       #f)
      (else
       #t))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?))
  (closure-with-body
   (nombre symbol?)
   (ids (list-of symbol?))
   (exps (list-of expression?))
   (return-exp expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body (extend-env ids args env)))
      
      (closure-with-body (nombre ids exps return-exp env)
        (let ((new-env (extend-env ids args env)))
          (let loop ((cur-env  new-env)
                     (restando exps))
            (if (null? restando)
                (eval-expression return-exp cur-env)
                (let* ((cur-decl  (car restando))
                       (resultado (eval-declaration cur-decl cur-env))
                       (nuevo-env (if (environment? resultado)
                                      resultado
                                      cur-env)))
                  (loop nuevo-env (cdr restando)))))))
      )))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record
      syms
      (list->vector
       (map
        (lambda (valor)
          (if (target? valor)
              valor                      ; ya es un target, no envolver
              (direct-target valor #f #f))) ; valor simple, envolver
        vals))
      env)))

; extend-env-const
; Crea un ambiente extendido donde todas las variables declaradas
; son constantes.
(define extend-env-const
  (lambda (syms vals env)

    (extended-env-record
      syms
      (list->vector
       (map
        (lambda (valor)
          (direct-target valor #t #f))
        vals))
      env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set!
              vec
              pos
              (direct-target
               (closure ids body env)
               #f
               #f)))
           (iota len)
           idss
           bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    ;(begin
     ; (display env)
      ;(display "jajajaj ")
      (deref (apply-env-ref env sym))))
    ;)

(define binding-kind
  (lambda (env sym)
    (let loop ((cur-env env))
      (cases environment cur-env
        (empty-env-record () #f)
        (extended-env-record (syms vals rest-env)
          (let ((pos (rib-find-position sym syms)))
            (if (number? pos)
                (target-kind (primitive-deref (a-ref pos vals)))
                (loop rest-env))))))))

(define target-kind
  (lambda (tgt)
    (cases target tgt
      (direct-target (valor constante? simbolo?)
        (cond
          (simbolo? 'symbol)
          (constante? 'const)
          (else 'var)))
      (indirect-target (ref1)
        (target-kind (primitive-deref ref1))))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x)
        (symbol? x)
        (boolean? x)
        (string? x)
        (procval? x)
        (pair? x)
        (null? x)
        (eq? x 'null)
        (vector? x)
        (hash? x))
        ))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
             (cases target (vector-ref vec pos)
               (direct-target (v constante? simbolo?) #t)
               (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)

      (direct-target (expval constante? simbolo?)
        expval)

      (indirect-target (ref1)
        (cases target (primitive-deref ref1)

          (direct-target (expval constante? simbolo?)
            expval)

          (indirect-target (p)
            (eopl:error
             'deref
             "Illegal reference: ~s"
             ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)

    (cases target (primitive-deref ref)

      (direct-target (valor constante? simbolo?)

        (if (or constante? simbolo?)

            (eopl:error
             'setref!
             "No es posible modificar una constante")

            (primitive-setref!
             ref
             (direct-target expval #f #f))))

      (indirect-target (ref1)

        (setref! ref1 expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

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

; reemplazar-en-lista: <lista> <entero> <valor> -> <lista>
; Función auxiliar para simular la mutación de una lista en una posición específica
(define reemplazar-en-lista
  (lambda (lst n val)
    (cond
      ((null? lst) (eopl:error 'set-list "Índice fuera de rango"))
      ((zero? n) (cons val (cdr lst)))
      (else (cons (car lst) (reemplazar-en-lista (cdr lst) (- n 1) val))))))

;******************************************************************************************
;Pruebas

(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x)")
(just-scan "add1(   x   )%cccc")
(just-scan "add1(  +(5, x)   )%cccc")
(scan&parse "add1(x)")
(scan&parse "add1(   x   )%cccc")
(scan&parse "add1(  +(5, x)   )%cccc")
(scan&parse "5")
(scan&parse "3.14")
(scan&parse "\"hola\"")
(scan&parse "true")
(scan&parse "false")
(scan&parse "null")
(scan&parse "add1(  +(5, %cccc
x)) ")
(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
(scan&parse "begin
  var x = -(y,1);
  var x = +(x,2);
  add1(x)
end")
(scan&parse "print(5)")
(scan&parse "print(x)")


(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
(define exp-numero (lit-exp 8))
(define exp-app (primapp-exp (add-prim) (list exp-numero (lit-exp 5))))
(define programa (a-program (expr-decl exp-app) '()))
(define una-expresion-dificil 
                              (primapp-exp (mult-prim)
                                          (list (primapp-exp (incr-prim)
                                                              (list (lit-exp 1)
                                                                    (lit-exp 5)))
                                                (lit-exp 3)
                                                (lit-exp 200))))
(define un-programa-dificil
    (a-program (expr-decl una-expresion-dificil) '()))

; (interpretador)

(provide (all-defined-out))