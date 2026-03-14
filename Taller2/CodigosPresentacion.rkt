#lang eopl

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;REPRESENTACIÓN UNARIA
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;
; ;Representación: Definición de funciones para representación de datos como listas de trues
; (define zero (lambda () '()))
; (define is-zero? (lambda (n) (null? n)))
; (define successor (lambda (n) (cons #t n)))
; (define predecessor (lambda (n) (cdr n)))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;REPRESENTACIÓN NÚMEROS DE SCHEME
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;
; ;Representación: Definición de funciones para representación de datos como números de scheme
; (define zero (lambda () 0))
; (define is-zero? (lambda (n) (zero? n)))
; (define successor (lambda (n) (+ n 1)))
; (define predecessor (lambda (n) (- n 1)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;CÓDIGO CLIENTE: Operaciones sobre datos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; (define suma
;   (lambda (x y)
;     (if (is-zero? x)
;         y
;         (successor (suma (predecessor x) y)))))
; 
; (define resta
;   (lambda (x y)
;     (if (is-zero? y)
;         x
;         (predecessor (resta  x (predecessor y))))))
; 
; (define multiplicacion
;   (lambda (x y)
;     (if (is-zero? x)
;         (zero)
;         (suma (multiplicacion (predecessor x) y) y))
;     ))
;     
; (define potencia
;   (lambda (x y)
;     (if (is-zero? y)
;         (successor y)
;         (multiplicacion (potencia x (predecessor y)) x))))
; 
; (define factorial
;   (lambda (n)
;     (if (is-zero? n)
;         (successor n)
;         (multiplicacion n (factorial (predecessor n))))))




; ;Invocación de funciones utilizando representación unaria
; (suma '(#t #t #t) '(#t #t))
; (resta '(#t #t #t) '(#t #t))
; (multiplicacion '(#t #t #t) '(#t #t #t))
; (multiplicacion '(#t #t #t) '())
; (potencia '() '(#t #t))
; (potencia '(#t #t #t) '())
; (potencia '(#t #t #t) '(#t #t))
; (factorial '())
; (factorial '(#t))
; (factorial '(#t #t #t))



; ;Invocación de funciones utilizando representación Números de scheme
; (suma 3 2)
; (resta 3 2)
; (multiplicacion 3 3)
; (multiplicacion 3 0)
; (potencia 0 2)
; (potencia 3 0)
; (potencia 3 2)
; (factorial 0)
; (factorial 1)
; (factorial 3)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ambientes representados como listas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;(extend-env 'x 4 (empty-env))
;(extend-env 'y 7 (extend-env 'x 4 (empty-env)) )


(define apply-env
  (lambda (env search-var)
    (cond
      [(eqv? (car env) 'empty-env)
       (eopl:error 'apply-env "No binding for ~s" search-var)]
      [(eqv? (car env) 'extend-env)
       (let ( (saved-var (cadr env))
              (saved-val (caddr env))
              (saved-env (cadddr env))
             )
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))
         )
       ]
      [else (eopl:error 'apply-env "Expecting environment, given ~s" env)])))

;(apply-env '(extend-env y 7 (extend-env x 4 (empty-env)) ) 'p)
;(apply-env '(extend-env y 7 (extend-env x 4 (empty-env)) ) 'x)
;(apply-env '(extend-env y 7 (extend-env x 4 (empty-env)) ) 'y)

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))
;(apply-env e 'x) ;e es una lista
;(apply-env e 'c)
;(apply-env '(x 8 9 7) 'x)

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
