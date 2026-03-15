#lang eopl

; =====================================================
; PROBLEMA SAT - Forma Normal Conjuntiva (FNC / CNF)
; =====================================================
; Entrada: (FNC n clausulas)
;   n         -> número de variables
;   clausulas -> lista de cláusulas
;                cada cláusula es una lista de literales:
;                  literal  k -> variable k debe ser #t
;                  literal -k -> variable k debe ser #f
;
; Salida: lista de #t/#f si es satisfactible
;         'no-satisfactible si no existe solución
; =====================================================


; Obtiene el valor de la variable i-ésima en la asignación
(define get-val
  (lambda (asignacion i)
    (if (= i 1)
        (car asignacion)
        (get-val (cdr asignacion) (- i 1)))))


; Evalúa un literal bajo una asignación dada
; literal positivo k  -> usa el valor de la variable k
; literal negativo -k -> niega el valor de la variable k
(define eval-literal
  (lambda (literal asignacion)
    (if (> literal 0)
        (get-val asignacion literal)
        (not (get-val asignacion (- literal))))))


; Evalúa una cláusula (OR de literales)
; basta con que UNO sea #t para que la cláusula sea #t
(define eval-clausula
  (lambda (clausula asignacion)
    (if (null? clausula)
        #f
        (or (eval-literal (car clausula) asignacion)
            (eval-clausula (cdr clausula) asignacion)))))


; Evalúa la FNC completa (AND de cláusulas)
; TODAS deben ser #t para que la fórmula sea #t
(define eval-FNC
  (lambda (clausulas asignacion)
    (if (null? clausulas)
        #t
        (and (eval-clausula (car clausulas) asignacion)
             (eval-FNC (cdr clausulas) asignacion)))))


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


; Busca recursivamente una asignación que satisfaga la FNC
; Prueba desde la asignación actual hasta agotar todas las posibilidades
(define resolver-SAT
  (lambda (clausulas asignacion)
    (cond
      [(eval-FNC clausulas asignacion) asignacion]    ; encontró solución
      [(todos-verdad? asignacion) #f]                 ; agotó posibilidades
      [else (resolver-SAT clausulas
                          (siguiente-asignacion asignacion))])))


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
; (FNC 1 '((1) (-1)))                 -> 'no-satisfactible
; =====================================================

