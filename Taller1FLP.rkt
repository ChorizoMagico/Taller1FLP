#lang eopl

;;1
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

;;2
(define down (lambda (L) 
                        (if
                          (null? L) '()
                          (cons (cons (car L) '()) (down (cdr L)))
                         )))

;;3
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

;;4
(define filter-in (lambda (P L)
                                (cond
                                 [(null? L) '()]
                                 [(P (car L)) (cons (car L) (filter-in P (cdr L)))]
                                 [else (filter-in P (cdr L))]
                                 )))


;;5
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

;;6
(define swapper (lambda (E1 E2 L)
                                (cond
                                 [(null? L) '()]
                                 [(eq? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]
                                 [(eq? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]
                                 [else (cons (car L) (swapper E1 E2 (cdr L)))]
                                 )))

;;7
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

;;8

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


;;9

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

;;10

(define balanced-parentheses? (lambda (L)
                                (letrec (
                                         (contadorParéntesis (lambda (counter L)
                             (cond
                               [(null? L) (if (= counter 0) #t #f) ]
                               [(< counter 0) #f]
                               [(eq? (car L) "(") (contadorParéntesis (+ counter 1) (cdr L))]
                               [(eq? (car L) ")") (contadorParéntesis (- counter 1) (cdr L))]
                               [else (contadorParéntesis counter (cdr L))]
                             )))
                                         )

                                  (contadorParéntesis 0 L)
                                  )
                                ))

;;11


(define zip (lambda (F L1 L2)
              (if (null? L1) '() (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2))
              ))))

;;12

(define filter-acum (lambda (a b F acum filter)
                      (cond
                        [(> a b) acum]
                        [(filter a) (filter-acum (+ a 1) b F (F a acum) filter)]
                        [else (filter-acum (+ a 1) b F acum filter)]
                        )
                      ))

;;13


(define operate (lambda (lrators lrands)
                  (letrec
                      ((operarConAcum (lambda (lrators lrands acum)
                                     (cond
                                       [(null? (cdr lrators)) ((car lrators) acum (car lrands) )]
                                       [else (operarConAcum (cdr lrators) (cdr lrands) ((car lrators) acum (car lrands)))]
                                       )
                                     )))
                    (operarConAcum lrators (cdr lrands) (car lrands) ))))

;;14

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

;;15

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
                   
                   


;;16
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

;;17
(define coin-change (lambda (monto monedas)
                                          (cond
                                            [(= monto 0) 1]
                                            [(< monto 0) 0]
                                            [(null? monedas) 0]
                                            [else (+ (coin-change (- monto (car monedas)) monedas) (coin-change monto (cdr monedas)))]
                                            )))

 ;;18
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
