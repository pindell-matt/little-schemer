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
 
;; official rember
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember a
                            (cdr lat))))))))

(check-equal? ;; returns with 'bacon removed
  (rember 'bacon '(bacon lettuce and tomato))
  '(lettuce and tomato))

;; my explanation of rember:
;; rember iterates over the lat, checking if the next (car lat) matches the provided 'a' atom
;; if it matches - it returns the rest of the list: (cdr lat)
;; if it does not - it continues; passing the (cdr lat) into a new recursive call to rember: (rember (cdr lat))
;; and, as always, since we're recursively calling rember - we always check first if the lat is null: (null? lat)
