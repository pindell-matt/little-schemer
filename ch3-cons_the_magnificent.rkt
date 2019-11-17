#lang scheme
(require rackunit) ; for checks / assertions

;; atom? - validates an atom
;; s-expression -> #t or #f
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; rember - removes member 'a' from 'lat'
;; my attempt:
(define (my_rember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (cdr lat))
    (else (cons (car lat) (my_rember a (cdr lat))))))

(check-equal? ;; returns lat with 'mint removed
  (my_rember 'mint '(lamb chops and mint jelly))
  '(lamb chops and jelly))

(check-equal? ;; returns lat with only first 'mint removed
  (my_rember 'mint '(lamb chops and mint flavored mint jelly))
  '(lamb chops and flavored mint jelly))

(check-equal? ;; if 'a' not found - returns original lat
 (my_rember 'toast '(bacon lettuce and tomato))
 '(bacon lettuce and tomato))
 
