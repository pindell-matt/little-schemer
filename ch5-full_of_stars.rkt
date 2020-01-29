#lang scheme
(require rackunit) ; for checks / assertions
(include "ch4-numbers_games.rkt")

;; atom?
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; rember* - removes all atoms, regardless of nesting
(define (my_rember* a l)
  (cond
    ((null? l) '())
    ((eq? (car l) a) (my_rember* a (cdr l)))
    (else
     (cond
       ((pair? (car l))
        (cons (my_rember* a (car l))
              (my_rember* a (cdr l))))
       (else
        (cons (car l) (my_rember* a (cdr l))))))))