#lang eopl
;; ======================================================================
;; PUNTO 3:
;; ======================================================================

;; agregar-a-todas :
;; Proposito:
;; val lista-listas -> lista-listas'
;; Procedimiento que agrega el valor val al inicio de cada lista dentro de lista-listas.

;; <lista-listas> ::= ()
;;                 ::= (<lista> <lista-listas>)
;; <lista> ::= ()
;;         ::= (<valor-de-scheme> <lista>)

(define agregar-a-todas
  (lambda (val lista-listas)
    (if (null? lista-listas)
        '()
        (cons (cons val (car lista-listas))
              (agregar-a-todas val (cdr lista-listas))))))

;; generar-combinaciones :
;; Proposito:
;; n -> lista-listas
;; Procedimiento que genera todas las combinaciones posibles de valores booleanos (#t, #f) para n variables.
;;
;; <lista-listas> ::= ()
;;                 ::= (<lista-booleana> <lista-listas>)
;; <lista-booleana> ::= ()
;;                   ::= (<booleano> <lista-booleana>)

(define generar-combinaciones
  (lambda (n)
    (if (= n 0)
        '(())
        (let ((sub-combinaciones (generar-combinaciones (- n 1))))
          (append (agregar-a-todas #t sub-combinaciones)
                  (agregar-a-todas #f sub-combinaciones))))))

;; eval-formula :
;; Proposito:
;; clausulas env -> Bool
;; Procedimiento que evalúa una fórmula lógica en CNF. La fórmula es verdadera si todas las cláusulas son verdaderas.
;;
;; <formula> ::= ()
;;            ::= (<clausula> <formula>)

(define eval-formula
  (lambda (clausulas env)
    (cond
      ((null? clausulas) #t)
      ((eqv? (eval-clausula (car clausulas) env) #f) #f)
      (else (eval-formula (cdr clausulas env))))))

;; poblar-ambiente :
;; Proposito:
;; valores num-var env -> env'
;; Procedimiento que construye un ambiente asignando valores booleanos a las variables.

;; <valores> ::= ()
;;            ::= (<booleano> <valores>)
;; <booleano> ::= #t | #f
;; <num-var> ::= <numero>
;; <env> ::= <ambiente-de-variables>

(define poblar-ambiente
  (lambda (valores num-var env)
    (if (null? valores)
        env
        (poblar-ambiente (cdr valores)
                         (+ num-var 1)
                         (extend-env num-var (car valores) env)))))

;; buscar-solucion :
;; Proposito:
;; clausulas combinaciones -> resultado
;; Procedimiento que recibe una lista de cláusulas y una lista de combinaciones booleanas (tabla de verdad). Evalúa cada combinación construyendo un ambiente y verifica si la fórmula es satisfactible.
;; Si encuentra una combinación que satisface la fórmula retorna (satisfactible combinacion). Si ninguna combinación satisface la fórmula retorna (insatisfactible ()).
;;
;; <clausulas> ::= ()
;;              ::= (<clausula> <clausulas>)
;; <clausula> ::= ()
;;             ::= (<literal> <clausula>)
;; <literal> ::= <numero> | -<numero>
;; <combinaciones> ::= ()
;;                  ::= (<lista-booleana> <combinaciones>)
;; <lista-booleana> ::= ()
;;                   ::= (<booleano> <lista-booleana>)

(define buscar-solucion
  (lambda (clausulas combinaciones)
    (if (null? combinaciones)
        '(insatisfactible ())
        (let* ((combinacion-actual (car combinaciones))
               (env-actual (poblar-ambiente combinacion-actual 1 (empty-env))))
          (if (eval-formula clausulas env-actual)
              (list 'satisfactible combinacion-actual)
              (buscar-solucion clausulas (cdr combinaciones)))))))

;; evaluar-sat :
;; Proposito:
;; ast -> resultado
;; Procedimiento principal que determina si una fórmula en forma normal conjuntiva (CNF) es satisfactible.
;;
;; <ast> ::= (cnf <numero-variables> <clausulas>)
;; <numero-variables> ::= <numero>
;; <clausulas> ::= ()
;;              ::= (<clausula> <clausulas>)
;; <clausula> ::= ()
;;             ::= (<literal> <clausula>)
;; <literal> ::= <numero> | -<numero>

;; <resultado> ::= (satisfactible <lista-booleana>)
;;               | (insatisfactible ())

(define evaluar-sat
  (lambda (ast)
    (let ((n (cadr ast))
          (clausulas (caddr ast)))
      (let ((tabla-de-verdad (generar-combinaciones n)))
        (buscar-solucion clausulas tabla-de-verdad)))))


;; ======================================================================
;; BLOQUE DE PRUEBAS PARA EVALUAR-SAT
;; ======================================================================

;; 1. El caso más básico (1 variable, debe ser verdadera)
(evaluar-sat '(cnf 1 ((1))))
;; -> (satisfactible (#t))
;; 2. Contradicción directa (1 variable exigiendo ser #t y #f a la vez)
(evaluar-sat '(cnf 1 ((1) (-1))))
;; -> (insatisfactible ())
;; 3. Dos variables condicionadas 
(evaluar-sat '(cnf 2 ((1 2) (-1 2))))
;; -> (satisfactible (#t #t)) 
;; 4. Callejón sin salida lógico (Bucle de contradicciones con 3 variables)
(evaluar-sat '(cnf 3 ((1 2) (-1 2) (1 -2) (-1 -2))))
;; -> (insatisfactible ())
;; 5. El reto clásico de 4 variables (El que analizamos paso a paso)
(evaluar-sat '(cnf 4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2))))
;; -> (satisfactible (#f #t #t #t))
;; 6. Exigencia absoluta (3 variables, todas obligadas a ser verdaderas)
(evaluar-sat '(cnf 3 ((1) (2) (3))))
;; -> (satisfactible (#t #t #t))
;; 7. Reacción en cadena fallida (2 variables, insatisfactible)
;; La cláusula 1 exige que la variable 1 sea #t. 
;; La cláusula 2 exige que si la 1 es #t, la 2 también debe serlo.
;; Pero la cláusula 3 exige que la 2 sea #f. ¡Contradicción!
(evaluar-sat '(cnf 2 ((1) (-1 2) (-2))))
;; -> (insatisfactible ())
;; 8. Efecto dominó de 5 variables (Satisfactible)
;; Cada variable obliga a la siguiente a ser verdadera en cascada.
(evaluar-sat '(cnf 5 ((1) (-1 2) (-2 3) (-3 4) (-4 5))))
;; -> (satisfactible (#t #t #t #t #t))

;; ======================================================================
;; Se hace uso de la IA para la generación de los casos de prueba, buscando cubrir distintos escenarios lógicos y niveles de complejidad. Se han incluido casos básicos, contradicciones directas, 
;; condiciones encadenadas, bucles de contradicciones, y casos con múltiples variables para asegurar una evaluación exhaustiva del procedimiento SAT implementado.
;; ======================================================================
