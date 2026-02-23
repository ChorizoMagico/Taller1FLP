#lang eopl

;;1
(define aplicarPredicado (lambda (L P)
                                      (if
                                       (and (P (car L)) (P (cadr L))) (cons (cadr L) (cons (car L) '()))
                                       '()
                                       )))
                                        

(define invert (lambda (L P)
                            (cond
                               [(null? L) '()]
                               [(null? (aplicarPredicado (car L) P)) (invert (cdr L) P)]
                               [else (cons (aplicarPredicado (car L) P) (invert (cdr L) P))]
                               )
                             ))

;;2
(define down (lambda (L) 
                        (if
                          (null? L) '()
                          (cons (cons (car L) '()) (down (cdr L)))
                         )))

;;3
(define acumulador (lambda(L n x P acum)
                                        (if
                                          (eq? acum n)
                                               (if (P (car L))
                                                (cons x (cdr L))
                                                L)
                                          (cons (car L) (acumulador (cdr L) n x P (+ 1 acum)))
                                         )))

(define list-set (lambda (L n x P)
                                   (cond
                                     [(null? L) '()]
                                     [else (acumulador L n x P 0)]
                                   )))

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


;;16





