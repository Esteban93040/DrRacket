#lang eopl

(require "MathFlow.rkt")


;******************************************************************************************
;Pruebas de eval-program

;--- Literales ---
(eval-program (scan&parse "5"))                  ; 5
(eval-program (scan&parse "3.14"))                ; 3.14
(eval-program (scan&parse "\"hola\""))             ; "hola"
(eval-program (scan&parse "true"))                ; #t
(eval-program (scan&parse "false"))               ; #f
(eval-program (scan&parse "null"))                ; 'null

;--- Aritméticas ---
(eval-program (scan&parse "+(3, 4)"))             ; 7
(eval-program (scan&parse "-(10, 4)"))            ; 6
(eval-program (scan&parse "*(3, 5)"))             ; 15
(eval-program (scan&parse "add1(9)"))             ; 10
(eval-program (scan&parse "sub1(9)"))             ; 8
(eval-program (scan&parse "mod(10, 3)"))            ; 1
(eval-program (scan&parse "/(10, 2)"))            ; 5

;--- Relacionales y booleanos ---
(eval-program (scan&parse "<(3, 5)"))             ; #t
(eval-program (scan&parse ">(3, 5)"))             ; #f
(eval-program (scan&parse "<=(5, 5)"))            ; #t
(eval-program (scan&parse ">=(6, 5)"))            ; #t
(eval-program (scan&parse "==(5, 5)"))            ; #t
(eval-program (scan&parse "<>(3, 5)"))            ; #t
(eval-program (scan&parse "and(true, false)"))    ; #f
(eval-program (scan&parse "or(true, false)"))     ; #t
(eval-program (scan&parse "not(false)"))          ; #t

;--- Cadenas ---
(eval-program (scan&parse "longitud(\"hola\")"))                 ; 4
(eval-program (scan&parse "concatenar(\"hola\", \"mundo\")"))     ; "holamundo"

;--- Condicional if ---
(eval-program (scan&parse "if <(3, 5) then 100 else 200"))        ; 100
(eval-program (scan&parse "if >(3, 5) then 100 else 200"))        ; 200

;--- Variables (var) ---
(eval-program (scan&parse "begin
  var x = 5;
  x
end"))
; 5

(eval-program (scan&parse "begin
  var x = 5;
  var y = 10;
  +(x, y)
end"))
; 15

(eval-program (scan&parse "begin
  var x = 5, y = 10, z = 3;
  +(x, +(y, z))
end"))
; 18

;--- Constantes (const) ---
(eval-program (scan&parse "begin
  const pi = 3, e = 2;
  +(pi, e)
end"))
; 5

; const inmutable (debe lanzar error)
(eval-program (scan&parse "begin
  const pi = 3;
  set pi = 5
end"))
; ERROR: No es posible modificar una constante

;--- set (mutación) ---
(eval-program (scan&parse "begin
  var x = 1;
  set x = +(x, 9);
  x
end"))
; 10

;--- print ---
(eval-program (scan&parse "print(5)"))            ; imprime 5, retorna 5
(eval-program (scan&parse "print(\"hola\")"))      ; imprime hola, retorna "hola"

;--- Funciones simples ---
(eval-program (scan&parse "begin
  func sumar(a, b) {
    return +(a, b)
  };
  sumar(3, 4)
end"))
; 7

(eval-program (scan&parse "begin
  func doble(n) {
    var resultado = *(n, 2);
    return resultado
  };
  doble(5)
end"))
; 10

;--- Funciones recursivas ---
(eval-program (scan&parse "begin
  func factorial(n) {
    return if <=(n, 1) then 1 else *(n, factorial(-(n,1)))
  };
  factorial(5)
end"))
; 120

(eval-program (scan&parse "begin
  func fib(n) {
    return if <=(n, 1) then n else +(fib(-(n,1)), fib(-(n,2)))
  };
  fib(10)
end"))
; 55

(eval-program (scan&parse "begin
  func suma(n) {
    return if <=(n, 0) then 0 else +(n, suma(-(n,1)))
  };
  suma(10)
end"))
; 55

;--- while ---
(eval-program (scan&parse "begin
  var x = 0;
  while <(x, 5) do
    set x = +(x, 1)
  done;
  x
end"))
; 5

;--- for ---
(eval-program (scan&parse "begin
  var lista = [1, 2, 3];
  for i in lista do
    print(i)
  done
end"))
; imprime 1, 2, 3 en líneas separadas

;--- switch ---
(eval-program (scan&parse "begin
  var nota = 5;
  switch nota {
    case 1: \"Muy mal\";
    case 5: \"Excelente\";
    default: \"Sin clasificar\";
  }
end"))
; "Excelente"

(eval-program (scan&parse "begin
  var nota = 99;
  switch nota {
    case 1: \"Muy mal\";
    case 5: \"Excelente\";
    default: \"Sin clasificar\";
  }
end"))
; "Sin clasificar"

;--- Listas ---
(eval-program (scan&parse "vacio?(vacio)"))                       ; #t
(eval-program (scan&parse "lista?(crear-lista(1, 2, 3))"))         ; #t
(eval-program (scan&parse "cabeza(crear-lista(1, 2, 3))"))         ; 1
(eval-program (scan&parse "cola(crear-lista(1, 2, 3))"))           ; vector con (2 3)
(eval-program (scan&parse "append(crear-lista(1, 2), crear-lista(3, 4))"))  ; vector con (1 2 3 4)
(eval-program (scan&parse "ref-list(crear-lista(10, 20, 30), 1)")) ; 20

(eval-program (scan&parse "begin
  var mi_lista = crear-lista(10, 20, 30);
  set-list(mi_lista, 1, 99);
  ref-list(mi_lista, 1)
end"))
; 99

;--- Diccionarios ---
(eval-program (scan&parse "begin
  var dic = crear-diccionario();
  set-diccionario(dic, \"nombre\", \"Robinson\");
  ref-diccionario(dic, \"nombre\")
end"))
; "Robinson"

(eval-program (scan&parse "diccionario?(crear-diccionario())"))   ; #t

;--- Expresiones simbólicas ---
; Variable tradicional: evalúa a un valor concreto.
(eval-program (scan&parse "begin
  var x = 5;
  +(x, 2)
end"))
; 7

; Símbolos: conservan la forma algebraica y no se reducen a un valor numérico.
(eval-program
 (scan&parse "symbol x, y;
              var expr1 = +(x, 2);
              var expr2 = +(y, 5);
              var expr3 = *(expr1, expr2);
              var z = 9;
              var w = +(z, 5);
              var q = +(x, z);
              print(x);
              print(expr1);
              print(expr2);
              print(z);
              print(w);
              print(q);
              null"))
; Resultado esperado:
; x
; (x + 2)
; (y + 5)
; 9
; 14
; (x + 9)

; Expresiones simbólicas: solo participan en operaciones aritméticas.
; Los operadores booleanos y relacionales generan error semántico.
; (eval-program (scan&parse "symbol x; >(x, 3)"))
; (eval-program (scan&parse "symbol x; and(x, true)"))

;--- Listas literales y diccionarios literales (sintaxis [ ] y { }) ---
(eval-program (scan&parse "[1, 2, 3]"))
; vector con (1 2 3)

(eval-program (scan&parse "{ nombre: \"Ana\", edad: 25 }"))
; hash con claves "nombre" y "edad"

;--- simplificar ---
(eval-program (scan&parse "symbol x; simplificar((x + 0))"))
; x

(eval-program (scan&parse "symbol x; simplificar(((x * 1) + 0))"))
; x

(eval-program (scan&parse "symbol x; var y = ((x + 2) + 3); simplificar(y)"))
; (x + 5)

(eval-program (scan&parse "symbol x; simplificar(((x * 0) + 10))"))
; 10

(eval-program (scan&parse "symbol x; simplificar(((x * 5) * 6))"))
; (x * 30)