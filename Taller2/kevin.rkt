#lang eopl

;; Estudiante: Kevin Giron
;; Código: 202510102
;; Estudiante: Sebastian Oviedo
;; Código: 202510104
;; Estudiante: Esteban Espinosa
;; Código: 202610114
;; Taller 2 - Fundamentos de Lenguajes de Programación
;; PUNTO 2: Funciones Parse y Unparse


;; parse-clausula :
;; cl -> lista
;; Recibe una clausula en representación concreta
;; y elimina los símbolos 'or para obtener solo los literales.

;; <clausula-concreta> ::= ()
;;                      ::= (<literal> or <literal> ...)

(define procesar-cl
  (lambda (cl)
    (cond
      [(null? cl) '()]
      [(eqv? (car cl) 'or)
       (procesar-cl (cdr cl))]
      [else
       (cons (car cl)
             (procesar-cl (cdr cl)))])))


;; parse-clausulas :
;; clausulas -> lista-clausulas
;; Recibe la lista de clausulas de la fórmula
;; y elimina los símbolos 'and para obtener solo las clausulas.

;; <clausulas-concretas> ::= ()
;;                        ::= (<clausula> and <clausulas>)

(define procesar-cls
  (lambda (clausulas)
    (cond
      [(null? clausulas) '()]
      [(eqv? (car clausulas) 'and)
       (procesar-cls (cdr clausulas))]
      [(list? (car clausulas))
       (cons (procesar-cl (car clausulas))
             (procesar-cls (cdr clausulas)))]
      [else
       (procesar-cls (cdr clausulas))])))


;; PARSEBNF :
;; exp -> ast
;; Recibe una instancia SAT
;; construye el árbol de sintaxis
;; abstracta basado en listas.

;; <exp> ::= (FNC <numero-variables> <clausulas>)

(define PARSEBNF
  (lambda (exp)
    (let ((n (cadr exp))
          (clausulas (caddr exp)))
      (list 'FNC
            n
            (procesar-cls clausulas)))))


;; unparse-clausula :
;; cl -> clausula-concreta
;; Reconstruye una clausula agregando los
;; símbolos 'or entre los literales.

;; <clausula-ast> ::= (<literal> ...)

(define cl-concreta
  (lambda (cl)
    (cond
      [(null? cl) '()]
      [(null? (cdr cl))
       (list (car cl))]
      [else
       (cons (car cl)
             (cons 'or
                   (cl-concreta (cdr cl))))])))


;; unparse-clausulas :
;; clausulas -> clausulas-concretas
;; Reconstruye la lista de clausulas
;; agregando los símbolos 'and entre ellas.

(define cl-concretas
  (lambda (clausulas)
    (cond
      [(null? clausulas) '()]
      [(null? (cdr clausulas))
       (list (cl-concreta (car clausulas)))]
      [else
       (cons (cl-concreta (car clausulas))
             (cons 'and
                   (cl-concretas (cdr clausulas))))])))


;; UNPARSEBNF :
;; exp -> representacion-concreta
;; Recibe un árbol de sintaxis abstracta
;; y reconstruye la representación concreta de la instancia SAT.

(define UNPARSEBNF
  (lambda (exp)
    (let ((n (cadr exp))
          (clausulas (caddr exp)))
      (list 'FNC
            n
            (cl-concretas clausulas)))))


;; Ejemplo representación concreta
(define ejemplo
 '(FNC 4 ((1 or -2 or 3 or 4)
          and (-2 or 3)
          and (-1 or -2 or -3)
          and (3 or 4)
          and (2))))

(PARSEBNF ejemplo)
;; -> (FNC 4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2)))


;; Ejemplo árbol de sintaxis abstracta
(define exp
 '(FNC 4 ((1 -2 3 4)
          (-2 3)
          (-1 -2 -3)
          (3 4)
          (2))))

(UNPARSEBNF exp)
;; -> (FNC 4 ((1 or -2 or 3 or 4)
;;            and (-2 or 3)
;;            and (-1 or -2 or -3)
;;            and (3 or 4)
;;            and (2)))


;; ======================================================================
;; Declaración de uso de IA
;; Se utilizó IA como apoyo para revisar el comportamiento de algunas
;; funciones, entender posibles errores y validar que las pruebas
;; funcionaran correctamente. También se usó como ayuda para verificar
;; algunos casos de prueba.
;; ======================================================================