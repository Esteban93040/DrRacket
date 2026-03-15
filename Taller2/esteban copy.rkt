#lang eopl

; =====================================================
; EJERCICIO 1.2: Representación de Ambientes (Datatypes)
; =====================================================
(define-datatype ambiente ambiente?
  (empty-env)
  (extend-env 
   (var integer?)
   (val boolean?)
   (env ambiente?)))

(define apply-env
  (lambda (env search-var)
    (cases ambiente env
      (empty-env ()
        (eopl:error 'apply-env "Variable no encontrada: ~s" search-var))
      (extend-env (var val saved-env)
        (if (= search-var var)
            val
            (apply-env saved-env search-var))))))

; =====================================================
; PROBLEMA SAT - Forma Normal Conjuntiva (FNC / CNF)
; =====================================================

; Evalúa un literal bajo un ambiente dado (Reemplaza get-val)
; literal positivo k  -> usa el valor de la variable k en el ambiente
; literal negativo -k -> niega el valor de la variable k en el ambiente
(define eval-literal
  (lambda (literal env)
    (if (> literal 0)
        (apply-env env literal)
        (not (apply-env env (- literal))))))

; Evalúa una cláusula (OR de literales)
; basta con que UNO sea #t para que la cláusula sea #t
(define eval-clausula
  (lambda (clausula env)
    (if (null? clausula)
        #f
        (or (eval-literal (car clausula) env)
            (eval-clausula (cdr clausula) env)))))

; Evalúa la FNC completa (AND de cláusulas)
; TODAS deben ser #t para que la fórmula sea #t
(define eval-FNC
  (lambda (clausulas env)
    (if (null? clausulas)
        #t
        (and (eval-clausula (car clausulas) env)
             (eval-FNC (cdr clausulas) env)))))

; Genera la asignación inicial: todos #f  (equivale a 00...0 en binario)
(define asignacion-inicial
  (lambda (n)
    (if (= n 0)
        '()
        (cons #f (asignacion-inicial (- n 1))))))

; Genera la siguiente asignación (como sumar 1 en binario)
; #f = 0, #t = 1, el primer elemento es el bit menos significativo
(define siguiente-asignacion
  (lambda (asignacion)
    (if (null? asignacion)
        '()
        (if (car asignacion)
            (cons #f (siguiente-asignacion (cdr asignacion)))  ; carry
            (cons #t (cdr asignacion))))))                     ; flip y parar

; Verifica si todos los valores son #t (última asignación posible: 11...1)
(define todos-verdad?
  (lambda (asignacion)
    (if (null? asignacion)
        #t
        (and (car asignacion)
             (todos-verdad? (cdr asignacion))))))

; --- FUNCIÓN PUENTE: Convierte la lista binaria al Datatype de Ambiente ---
(define crear-ambiente
  (lambda (asignacion num-var)
    (if (null? asignacion)
        (empty-env)
        (extend-env num-var 
                    (car asignacion) 
                    (crear-ambiente (cdr asignacion) (+ num-var 1))))))

; Busca recursivamente una asignación que satisfaga la FNC
; Prueba desde la asignación actual hasta agotar todas las posibilidades
(define resolver-SAT
  (lambda (clausulas asignacion)
    (let ((env-actual (crear-ambiente asignacion 1))) ; Convertimos la lista a ambiente
      (cond
        [(eval-FNC clausulas env-actual) asignacion]    ; encontró solución
        [(todos-verdad? asignacion) #f]                 ; agotó posibilidades
        [else (resolver-SAT clausulas
                            (siguiente-asignacion asignacion))]))))

; Función principal
(define FNC
  (lambda (n clausulas)
    (let ((resultado (resolver-SAT clausulas (asignacion-inicial n))))
      (if resultado
          resultado
          'no-satisfactible))))

; =====================================================
; EJEMPLOS:
; (FNC 3 '((1 -2 3) (-1 2) (2 -3)))  -> '(#f #f #f)
; (FNC 1 '((1) (-1)))                -> 'no-satisfactible
; =====================================================