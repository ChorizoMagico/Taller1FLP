#lang eopl

;; Integrantes:
;; Juan Esteban Arias Saldaña (2417915)
;; Valeria Zamudio Arevalo (2415210)

;;1
;; invert :
;; Proposito:
;; L x P -> L’: Recibe una lista L y un predicado P.
;; Devuelve una lista con las sublistas de L cuyos dos primeros
;; elementos cumplen el predicado P, pero invertidos.
;; <lista> := ()
;;         := ((<valor-de-scheme>) <lista>)

;; aplicarPredicado :
;; Proposito:
;; L x P -> L’: Recibe una sublista L y un predicado P.
;; Si los dos primeros elementos de L cumplen P,
;; devuelve una lista con esos dos elementos invertidos.
;; En caso contrario devuelve la lista vacia.
;;
;; <lista> := ()
;;         := ((<valor-de-scheme>) <lista>)

(define invert (lambda (L P)
                 (letrec (
                          (aplicarPredicado (lambda (L P)
                                             (if
                                              (and (P (car L)) (P (cadr L))) (cons (cadr L) (cons (car L) '()))
                                              '()
                                              ))))
                   (cond
                     [(null? L) '()]
                     [(null? (aplicarPredicado (car L) P)) (invert (cdr L) P)]
                     [else (cons (aplicarPredicado (car L) P) (invert (cdr L) P))]
                     ))))

;; Pruebas invert
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((6 9) (10 90) (82 7)) odd?)
(invert '((2 4) (6 8)) number?)

;; Pruebas aplicarPredicado
;;(aplicarPredicado '(4 6) even?)
;;(aplicarPredicado '(3 5) even?)
;;(aplicarPredicado '(7 9) odd?)


;;2
;; down :
;; Proposito:
;; L -> L’: Transforma cada elemento de la lista L en una lista unitaria.
;;
;; <lista> := ()
;;         := ((<valor-de-scheme>) <lista>)
(define down (lambda (L) 
                        (if
                          (null? L) '()
                          (cons (cons (car L) '()) (down (cdr L)))
                         )))

;; Pruebas down
(down '(1 2 3))
(down '((a) (b) (c)))
(down '(x (y z) w))

;;3
;; list-set :
;; Proposito:
;; L x n x x x P -> L’: Reemplaza el
;; elemento en la posicion n de la lista L por el valor x,
;; solo si dicho elemento cumple el predicado P.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

;; acumulador :
;; Proposito:
;; L x n x x x P x acum -> L’: Recorre la lista con un contador (acum).
;; Cuando el contador coincide con n,
;; reemplaza el elemento por x si este elemento cumple P.
;; En caso contrario, continua el recorrido sin modificar.
;;
;; <lista> := ()
;;         := ((<valor-de-scheme>) <lista>)

(define list-set (lambda (L n x P)
                   (letrec (
                            (acumulador (lambda (L n x P acum)
                                          (if
                                           (eq? acum n)
                                           (if (P (car L))
                                               (cons x (cdr L))
                                               L)
                                           (cons (car L) (acumulador (cdr L) n x P (+ 1 acum)))
                                           ))))
                     (cond
                       [(null? L) '()]
                       [else (acumulador L n x P 0)]
                       ))))

;; Pruebas list-set
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)
(list-set '(2 4 6 8) 1 'a even?)

;; Pruebas acumulador
;;(acumulador '(5 8 7 6) 2 '(1 2) odd? 0)
;;(acumulador '(2 4 6 8) 1 'a even? 0)


;;4
;; filter-in :
;; Proposito:
;; P x L -> L’: Devuelve una nueva lista con los elementos de L que cumplen el predicado P.
;;
;; <lista> := ()
;;         := ((<valor-de-scheme>) <lista>)

(define filter-in (lambda (P L)
                                (cond
                                 [(null? L) '()]
                                 [(P (car L)) (cons (car L) (filter-in P (cdr L)))]
                                 [else (filter-in P (cdr L))]
                                 )))


;; Pruebas filter-in
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 d))
(filter-in even? '(1 2 3 4 5 6))


;;5
;; palindrome? :
;; Proposito:
;; palabra -> boolean: Determina si una palabra (lista) es un palindromo.
;;
;; <lista> := ()
;;         := ((<valor-de-scheme>) <lista>)

;; invertirPalabra :
;; Proposito:
;; L x acum -> L’: Invierte recursivamente la lista L usando un acumulador para construir la lista invertida.
;;
;; <lista> := ()
;;         := ((<valor-de-scheme>) <lista>)

(define palindrome? (lambda (palabra)
                      (letrec (
                               (invertirPalabra (lambda (L acum)
                                 (if
                                  (null? L) acum
                                  (invertirPalabra (cdr L) (cons (car L) acum))
                                  ))))
                        (if
                         (null? palabra) '()
                         (equal? palabra (invertirPalabra palabra '()))
                         ))))

;; Pruebas palindrome?
(palindrome? '(r a p a r))
(palindrome? '(s a n a s))
(palindrome? '(c a m b i o))

;; Pruebas invertirPalabra
;;(invertirPalabra '(1 2 3) '())
;;(invertirPalabra '(a b c d) '())


;;6
;; swapper :
;; Proposito:
;; E1 x E2 x L -> L’: Intercambia todas las ocurrencias
;; de E1 por E2 y de E2 por E1 dentro de la lista L.
;;
;; <lista> := ()
;;         := ((<valor-de-scheme>) <lista>)
(define swapper (lambda (E1 E2 L)
                                (cond
                                 [(null? L) '()]
                                 [(eq? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]
                                 [(eq? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]
                                 [else (cons (car L) (swapper E1 E2 (cdr L)))]
                                 )))

;; Pruebas swapper
(swapper 'a 'd '(a b c d))
(swapper 'x 'y '(y y x y x))
(swapper 1 2 '(1 2 3 2 1))



;; unirListas :
;; Proposito:
;; L1 x L2 -> L’: Une dos listas en una sola.
;;
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

(define unirListas (lambda (L1 L2)
  (if (null? L1) L2
      (cons (car L1) (unirListas (cdr L1) L2)))))

;; Pruebas unirListas
(unirListas '(1 2 3) '(4 5))
(unirListas '() '(a b))
(unirListas '(x y) '())

;; 7
;; cartesian-product :
;; Proposito:
;; L1 x L2 -> L’: Devuelve el producto cartesiano
;; entre las listas L1 y L2.
;;
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

;; unirListas :
;; Proposito:
;; L1 x L2 -> L’: Une dos listas en una sola.
;;
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

;; combinacion :
;; Proposito:
;; L1 x L2 -> L’: Genera las combinaciones entre el primer
;; elemento de L1 y todos los elementos de L2.
;;
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

(define cartesian-product (lambda (L1 L2)
                      (letrec (
                               (unirListas (lambda (L1 L2)
                                 (if (null? L1) L2
                                     (cons (car L1) (unirListas (cdr L1) L2)))))
                               (combinacion (lambda (L1 L2)
                                 (cond
                                  [(null? L1) '()]
                                  [(null? L2) '()]
                                  [else (cons (list (car L1) (car L2)) (combinacion L1 (cdr L2)))]
                                  ))))
                        (cond
                         [(null? L1) '()]
                         [(null? L2) '()]
                         [else (unirListas (combinacion L1 L2) (cartesian-product (cdr L1) L2))]
                         ))))

;; Pruebas cartesian-product
(cartesian-product '(a b c) '(x y))
(cartesian-product '(1 2) '(3 4))
(cartesian-product '() '(a b))

;; Pruebas combinacion
;;(combinacion '(a) '(x y))
;;(combinacion '(1) '(2 3 4))

;;8
;; mapping :
;; Proposito:
;; F x L1 x L2 -> L’ : Procedimiento que recibe una función F y dos listas numéricas L1 y L2,
;; y retorna una lista de pares (sim valor) tales que para cada elemento sim en L1,
;; se incluye el par (sim valor) si (F sim) es igual a algún elemento valor en L2.
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

;; mapearUno :
;; Proposito:
;; F x sim x L2 -> L’ : Procedimiento que recibe una función F,
;; un elemento sim y una lista L2, y retorna una lista de pares
;; (sim valor) tales que (F sim) es igual a algún elemento valor en L2.
;;
;; <listas> := ()
;; := (<valor-de-scheme> <listas>)

;; concatenarAlFinal :
;; Proposito:
;; L x sim -> L’ : Procedimiento que agrega el elemento sim
;; al final de la lista L.
;;
;; <listas> := ()
;; := (<valor-de-scheme> <listas>)

;; unirListas :
;; Proposito:
;; L1 x L2 -> L’ : Procedimiento que une los elementos
;; de L2 al final de L1 utilizando concatenación sucesiva.
;;
;; <listas> := ()
;; := (<valor-de-scheme> <listas>)

(define mapping (lambda (F L1 L2)
  (letrec (
           (mapearUno (lambda (F sim L2)
                        (cond
                          [(null? L2) '()]
                          [(= (F sim) (car L2)) (cons (list sim (car L2)) (mapearUno F sim (cdr L2)))]
                          [else (mapearUno F sim (cdr L2))]
                          )
                        )
           )
           (concatenarAlFinal (lambda (L sim)
                            (if (null? L) (cons sim L) (cons (car L) (concatenarAlFinal (cdr L) sim))))
                            )
           (unirListas (lambda (L1 L2)
                         (if (null? L2) L1 (unirListas (concatenarAlFinal L1 (car L2)) (cdr L2)))
                         )))
    (if (null? L1) '() (unirListas (mapearUno F (car L1) L2) (mapping F (cdr L1) L2))))))

;; Pruebas

(mapping (lambda (x) (* x 2)) '(1 2 3) '(2 4 6))
(mapping (lambda (x) (+ x 1)) '(1 2 3) '(3 4 5))
(mapping (lambda (x) (* x x)) '(1 2 3 4) '(1 4 9))
(mapping (lambda (x) (- x 1)) '(5 6 7) '(1 2 3))


;; Pruebas mapearUno

;;(mapearUno (lambda (x) (* x 2)) 2 '(2 4 6))
;;(mapearUno (lambda (x) (+ x 1)) 3 '(1 4 5 6))
;;(mapearUno (lambda (x) (* x x)) 3 '(9 3 1))
;;(mapearUno (lambda (x) (- x 1)) 5 '(1 2 3))


;; Pruebas concatenarAlFinal

;;(concatenarAlFinal '(1 2 3) 4)
;;(concatenarAlFinal '() 'a)
;;(concatenarAlFinal '(x y) 'z)
;;(concatenarAlFinal '(a) '(b c))


;; Pruebas

;;(unirListas '(1 2) '(3 4))
;;(unirListas '() '(a b))
;;(unirListas '(x y) '())
;;(unirListas '(a) '(b c d))


;;9
;; inversions :
;; Proposito:
;; L -> N : Procedimiento que recibe una lista numérica L y retorna
;; la cantidad de inversiones en la lista, es decir, el número de pares
;; (i, j) tales que i < j y el elemento en la posición i es mayor
;; que el elemento en la posición j.
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

;; comparadorSucesivo :
;; Proposito:
;; num x L x acum -> N : Procedimiento que cuenta cuántos
;; elementos en la lista L son menores que num, acumulando
;; el resultado en acum.
;;
;; <listas> := ()
;; := (<valor-de-scheme> <listas>)




(define inversions (lambda (L)
                      (letrec (
                               (comparadorSucesivo  (lambda (num L acum) 
                                                  (cond
                                                    [(null? L) acum]
                                                    [(> num (car L)) (comparadorSucesivo num (cdr L) (+ acum 1))]
                                                    [else (comparadorSucesivo num (cdr L) acum)]
                                                    )))
                               )
                        (if
                        (null? L) 0
                        (+ (comparadorSucesivo (car L) (cdr L) 0) (inversions (cdr L))))
                        )               
                      ))
;; Pruebas

(inversions '())
(inversions '(1 2 3 4))
(inversions '(4 3 2 1))
(inversions '(3 1 2))

;; Pruebas comparadorSucesivo

;;(comparadorSucesivo 3 '(1 2 4 0) 0)
;;(comparadorSucesivo 5 '(6 7 8) 0)
;;(comparadorSucesivo 4 '(1 2 3) 2)
;;(comparadorSucesivo 2 '() 0)

;;10
;; balanced-parentheses? :
;; Proposito:
;; L -> B : Procedimiento que recibe una lista de símbolos L que puede
;; contener los símbolos 'O (paréntesis que abre) y 'C (paréntesis que cierra),
;; y retorna #t si los paréntesis están balanceados y correctamente anidados,
;; y #f en caso contrario.
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

;; contadorParéntesis :
;; Proposito:
;; counter x L -> B : Procedimiento que recorre la lista L
;; contando las apariciones de 'O como +1 y 'C como -1,
;; verificando que nunca el contador sea negativo y que
;; al final sea igual a 0. Retorna #t si está balanceado
;; y #f en caso contrario.
;;
;; <listas> := ()
;; := (<valor-de-scheme> <listas>)

(define balanced-parentheses? (lambda (L)
                                (letrec (
                                         (contadorParéntesis (lambda (counter L)
                             (cond
                               [(null? L) (if (= counter 0) #t #f) ]
                               [(< counter 0) #f]
                               [(eq? (car L) 'O) (contadorParéntesis (+ counter 1) (cdr L))]
                               [(eq? (car L) 'C) (contadorParéntesis (- counter 1) (cdr L))]
                               [else (contadorParéntesis counter (cdr L))]
                             )))
                                         )

                                  (contadorParéntesis 0 L)
                                  )
                                ))
;; Pruebas

(balanced-parentheses? '())
(balanced-parentheses? '(O C O C))
(balanced-parentheses? '(O O C C))
(balanced-parentheses? '(O C C O))

;; Pruebas contadorParéntesis

;;(contadorParéntesis 0 '(O C))
;;(contadorParéntesis 0 '(O O C C))
;;(contadorParéntesis 0 '(O C C))
;;(contadorParéntesis 0 '())

;;11
;; zip :
;; Proposito:
;; F x L1 x L2 -> L’ : Procedimiento que recibe una función F y dos listas L1 y L2
;; de igual longitud, y retorna una nueva lista cuyos elementos son el resultado
;; de aplicar F a los elementos correspondientes de L1 y L2.
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

(define zip (lambda (F L1 L2)
              (if (null? L1) '() (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2))
              ))))

;; Pruebas

(zip + '(1 2 3) '(4 5 6))
(zip * '(2 3 4) '(5 6 7))
(zip (lambda (x y) (- x y)) '(10 8 6) '(1 2 3))
(zip (lambda (x y) (list x y)) '(a b c) '(1 2 3))

;;12
;; filter-acum :
;; Proposito:
;; a x b x F x acum x filter -> R : Procedimiento que recibe dos números
;; a y b que determinan un intervalo [a, b], una función F, un acumulador inicial
;; acum y un predicado filter. Recorre los valores desde a hasta b (inclusive)
;; y acumula con F únicamente aquellos valores que satisfacen el predicado filter.
;; Retorna el valor final del acumulador.
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

(define filter-acum (lambda (a b F acum filter)
                      (cond
                        [(> a b) acum]
                        [(filter a) (filter-acum (+ a 1) b F (F a acum) filter)]
                        [else (filter-acum (+ a 1) b F acum filter)]
                        )
                      ))
;; Pruebas

(filter-acum 1 5 + 0 (lambda (x) #t))
(filter-acum 1 5 + 0 (lambda (x) (even? x)))
(filter-acum 1 4 * 1 (lambda (x) (odd? x)))
(filter-acum 3 2 + 0 (lambda (x) #t))

;;13
;; operate :
;; Proposito:
;; lrators x lrands -> R : Procedimiento que recibe una lista de operadores
;; binarios lrators y una lista de operandos lrands, y evalúa la operación
;; de manera secuencial (asociatividad izquierda), aplicando cada operador
;; acumulativamente sobre los operandos.
;;
;; <listas> := ()
;; := (<valor-de-scheme> <listas>)

;; operarConAcum :
;; Proposito:
;; lrators x lrands x acum -> R : Procedimiento auxiliar que aplica
;; recursivamente los operadores de la lista lrators sobre los operandos
;; lrands, acumulando el resultado parcial en acum hasta procesar
;; el último operador.
;;
;; <listas> := ()
;; := (<valor-de-scheme> <listas>)

(define operate (lambda (lrators lrands)
                  (letrec
                      ((operarConAcum (lambda (lrators lrands acum)
                                     (cond
                                       [(null? (cdr lrators)) ((car lrators) acum (car lrands) )]
                                       [else (operarConAcum (cdr lrators) (cdr lrands) ((car lrators) acum (car lrands)))]
                                       )
                                     )))
                    (operarConAcum lrators (cdr lrands) (car lrands) ))))

;; Pruebas
(operate (list +) '(3 4))
(operate (list + *) '(2 3 4))
(operate (list - +) '(10 2 3))
(operate (list * + -) '(2 3 4 5))

;; Pruebas operarConAcum
;;(operarConAcum (list +) '(4) 3)
;;(operarConAcum (list + *) '(3 4) 2)
;;(operarConAcum (list - +) '(2 3) 10)
;;(operarConAcum (list *) '(5) 4)

;;14
;; path :
;; Proposito:
;; n x BTS -> L’ : Procedimiento que recibe un número n y un árbol binario
;; de búsqueda BTS representado como lista, y retorna una lista con las
;; direcciones "left" y "right" que indican el camino desde la raíz
;; hasta el nodo que contiene n. Si el número no se encuentra, retorna #f.
;;
;; <BTS> := ()
;; := (<numero> <BTS> <BTS>)


;; verNúmero :
;; Proposito:
;; BTS -> N : Procedimiento auxiliar que retorna el número
;; almacenado en la raíz del árbol binario BTS.
;;
;; <BTS> := ()
;; := (<numero> <BTS> <BTS>)

;; irIzquierda :
;; Proposito:
;; BTS -> BTS’ : Procedimiento auxiliar que retorna
;; el subárbol izquierdo del árbol binario BTS.
;;
;; <BTS> := ()
;; := (<numero> <BTS> <BTS>)

;; irDerecha :
;; Proposito:
;; BTS -> BTS’ : Procedimiento auxiliar que retorna
;; el subárbol derecho del árbol binario BTS.
;;
;; <BTS> := ()
;; := (<numero> <BTS> <BTS>)


(define path (lambda (n BTS)
               (letrec
                   (
                    (verNúmero (lambda (BTS) (car BTS)))

                     (irIzquierda (lambda (BTS) (car (cdr BTS))))

                     (irDerecha (lambda (BTS) (car (cdr (cdr BTS)))))
               )
                 (cond
                   [(null? BTS) #f]
                   [(= (verNúmero BTS) n) '()]
                   [else (let (
                             (caminoIzquierda (path n (irIzquierda BTS))))
                           (if (not (eq? caminoIzquierda #f)) (cons "left" caminoIzquierda)
                               (let
                                   ((caminoDerecha (path n (irDerecha BTS))))
                                 (if (not (eq? caminoDerecha #f)) (cons "right" caminoDerecha) #f))
                           ))]
                   ))))

;; Pruebas
(path 5 '(5 () ()))
(path 3 '(5 (3 () ()) (7 () ())))
(path 7 '(5 (3 () ()) (7 () ())))
(path 9 '(5 (3 () ()) (7 () ())))

;; Pruebas verNúmero
;;(verNúmero '(5 (3 () ()) (7 () ())))
;;(verNúmero '(10 () ()))
;;(verNúmero '(1 (0 () ()) (2 () ())))
;;(verNúmero '(8 (4 () ()) (12 () ())))

;; Pruebas irIzquierda
;;(irIzquierda '(5 (3 () ()) (7 () ())))
;;(irIzquierda '(10 () ()))
;;(irIzquierda '(8 (4 () ()) (12 () ())))
;;(irIzquierda '(1 (0 () ()) (2 () ())))

;; Pruebas irDerecha
;;(irDerecha '(5 (3 () ()) (7 () ())))
;;(irDerecha '(10 () ()))
;;(irDerecha '(8 (4 () ()) (12 () ())))
;;(irDerecha '(1 (0 () ()) (2 () ())))

;;15
;; count-odd-and-even :
;; Proposito:
;; arbol -> L’ : Procedimiento que recibe un árbol binario representado
;; como lista y retorna una lista (pares impares) donde el primer
;; elemento indica la cantidad de números pares y el segundo
;; la cantidad de números impares presentes en el árbol.
;;
;; <arbol> := ()
;; := (<numero> <arbol> <arbol>)

;; verNúmero :
;; Proposito:
;; arbol -> N : Procedimiento auxiliar que retorna el número
;; almacenado en la raíz del árbol.
;;
;; <arbol> := ()
;; := (<numero> <arbol> <arbol>)

;; irIzquierda :
;; Proposito:
;; arbol -> arbol’ : Procedimiento auxiliar que retorna
;; el subárbol izquierdo del árbol dado.
;;
;; <arbol> := ()
;; := (<numero> <arbol> <arbol>)

;; irDerecha :
;; Proposito:
;; arbol -> arbol’ : Procedimiento auxiliar que retorna
;; el subárbol derecho del árbol dado.
;;
;; <arbol> := ()
;; := (<numero> <arbol> <arbol>)

;; sumador :
;; Proposito:
;; L1 x L2 -> L’ : Procedimiento auxiliar que recibe dos listas
;; de la forma (pares impares) y retorna una nueva lista
;; con la suma componente a componente.
;;
;; <listas> := ()
;; := (<valor-de-scheme> <listas>)

(define count-odd-and-even (lambda (arbol)
               (letrec
                   (
                    (verNúmero (lambda (arbol) (car arbol)))

                     (irIzquierda (lambda (arbol) (car (cdr arbol))))

                     (irDerecha (lambda (arbol) (car (cdr (cdr arbol)))))

                     (sumador (lambda (L1 L2) (list (+ (car L1) (car L2)) (+ (car (cdr L1)) (car (cdr L2))))))
               )
                 (cond
                   [(null? arbol) '(0 0)]
                   [else (let (
                               (nodo (if (= (modulo (verNúmero arbol) 2) 0) '(1 0) '(0 1)))
                              (caminoIzquierda (if (not (null? (irIzquierda arbol))) (count-odd-and-even (irIzquierda arbol)) '(0 0)))
                              (caminoDerecha (if (not (null? (irDerecha arbol))) (count-odd-and-even (irDerecha arbol)) '(0 0)))
                              )
                           (sumador (sumador nodo caminoIzquierda) caminoDerecha))
                         ]))))
;; Pruebas
(count-odd-and-even '())
(count-odd-and-even '(2 () ()))
(count-odd-and-even '(2 (3 () ()) (4 () ())))
(count-odd-and-even '(5 (2 (1 () ()) ()) (8 () ())))
                   
;; Pruebas verNúmero
;;(verNúmero '(5 (2 () ()) (8 () ())))
;;(verNúmero '(1 () ()))
;;(verNúmero '(10 (4 () ()) (12 () ())))
;;(verNúmero '(7 (3 () ()) (9 () ())))

;; Pruebas irIzquierda
;;(irIzquierda '(5 (2 () ()) (8 () ())))
;;(irIzquierda '(1 () ()))
;;(irIzquierda '(10 (4 () ()) (12 () ())))
;;(irIzquierda '(7 (3 () ()) (9 () ())))

;; Pruebas irDerecha
;;(irDerecha '(5 (2 () ()) (8 () ())))
;;(irDerecha '(1 () ()))
;;(irDerecha '(10 (4 () ()) (12 () ())))
;;(irDerecha '(7 (3 () ()) (9 () ())))

;; Pruebas sumador
;;(sumador '(1 0) '(0 1))
;;(sumador '(2 3) '(4 5))
;;(sumador '(0 0) '(1 1))
;;(sumador '(5 2) '(3 4))

;;16
;; hanoi :
;; Proposito:
;; n x origen x auxiliar x destino -> L : Devuelve la lista de movimientos necesarios
;; para trasladar n discos de la torre origen a la torre destino.
;; Cada movimiento se representa como (origen destino).
;;
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

(define hanoi (lambda (n origen auxiliar destino)
                (letrec (
                         (unirListas (lambda (L1 L2)
                                 (if (null? L1) L2
                                     (cons (car L1) (unirListas (cdr L1) L2))))))
                                                 (cond
                                                  [(= n 0) '()]
                                                  [(= n 1) (cons (list origen destino) '())]
                                                  [else (unirListas (hanoi (- n 1) origen destino auxiliar) (unirListas (list (list origen destino)) (hanoi (- n 1) auxiliar origen destino)))]
                                                  ))))
;; Pruebas hanoi
(hanoi 1 'A 'B 'C)
(hanoi 2 'A 'B 'C)
(hanoi 3 'A 'B 'C)

;;17
;; coin-change :
;; Proposito:
;; monto x monedas -> numero: Devuelve la cantidad de formas distintas
;; de formar el monto usando las monedas dadas.
;; Usa recursión considerando tomar la moneda actual o ignorarla y pasar a la siguiente
;;
;; <listas> := ()
;;           := (<valor-de-scheme> <listas>)

(define coin-change (lambda (monto monedas)
                                          (cond
                                            [(= monto 0) 1]
                                            [(< monto 0) 0]
                                            [(null? monedas) 0]
                                            [else (+ (coin-change (- monto (car monedas)) monedas) (coin-change monto (cdr monedas)))]
                                            )))

;; Pruebas coin-change
(coin-change 5 '(1 5))
(coin-change 5 '(1 2 5))
(coin-change 10 '(2 5 3 6))

 ;;18
;; pascal :
;; Proposito:
;; N -> L’ : Procedimiento que recibe un número natural N
;; y retorna la fila N del triángulo de Pascal representada
;; como una lista de números.
;;
;; <listas> := ()
;; := (<numero> <listas>)

;; concatenarAlFinal :
;; Proposito:
;; L x sim -> L’ : Procedimiento auxiliar que agrega el elemento sim
;; al final de la lista L.
;;
;; <listas> := ()
;; := (<numero> <listas>)

;; añadirExtremosUnos :
;; Proposito:
;; L -> L’ : Procedimiento auxiliar que recibe una fila del
;; triángulo de Pascal y retorna una nueva lista agregando
;; un 1 al inicio y un 1 al final.
;;
;; <listas> := ()
;; := (<numero> <listas>)

;; sumarPares :
;; Proposito:
;; L -> L’ : Procedimiento auxiliar que recibe una lista L
;; y retorna una nueva lista con la suma de cada par consecutivo
;; de elementos de L.
;;
;; <listas> := ()
;; := (<numero> <listas>)

;; sumatoria :
;; Proposito:
;; L x N -> L’ : Procedimiento auxiliar que aplica iterativamente
;; la construcción de la siguiente fila del triángulo de Pascal
;; N veces a partir de la lista L.
;;
;; <listas> := ()
;; := (<numero> <listas>)

(define pascal (lambda (N)
                 (letrec (

                          (concatenarAlFinal (lambda (L sim)
                              (if (null? L) (cons sim L) (cons (car L) (concatenarAlFinal (cdr L) sim)))) )

                           (añadirExtremosUnos (lambda (L)
                                                 (cons 1 (concatenarAlFinal L 1)) ))

                           (sumarPares (lambda (L)
                                         (cond [(null? L) '()]
                                               [(null? (cdr L)) '()]
                                               [(null? (cdr (cdr L))) (list (+ (car L) (car (cdr L))))]
                                               [else (cons (+ (car L) (car (cdr L))) (sumarPares (cdr L)))])))
                           (sumatoria (lambda (L N)
                                        (cond [(= N 0) L] [else (sumatoria (añadirExtremosUnos (sumarPares L)) (- N 1))] ))) )
                   (sumatoria (list 1) (- N 1)) )))

;; Pruebas
(pascal 1)
(pascal 2)
(pascal 5)
(pascal 7)

;; Pruebas concatenarAlFinal
;;(concatenarAlFinal '(1 2 3) 4)
;;(concatenarAlFinal '() 1)
;;(concatenarAlFinal '(5 6) 7)
;;(concatenarAlFinal '(9) 10)

;; Pruebas añadirExtremosUnos
;;(añadirExtremosUnos '(1))
;;(añadirExtremosUnos '(1 1))
;;(añadirExtremosUnos '(1 2 1))
;;(añadirExtremosUnos '(1 3 3 1))

;; Pruebas sumarPares
;;(sumarPares '(1 1))
;;(sumarPares '(1 2 1))
;;(sumarPares '(1 3 3 1))
;;(sumarPares '(1))

;; Pruebas sumatoria
;;(sumatoria '(1) 0)
;;(sumatoria '(1) 1)
;;(sumatoria '(1) 4)
;;(sumatoria '(1 2 1) 1)