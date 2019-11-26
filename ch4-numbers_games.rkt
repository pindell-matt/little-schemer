#lang scheme
(require rackunit) ; for checks / assertions

;; atom?
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(check-true (atom? 14)) ;; all numbers are atoms

;; technically supposed to "consider only whole numbers" - but these are true:
(check-true (atom? -3))
(check-true (atom? 3.145159))

(check-equal? (add1 67) 68) ;; add1 increases number by 1
(check-equal? (sub1 5) 4)   ;; sub1 decreases number by 1

(check-equal? (sub1 0) -1) ;; although we aren't "considering" negative numbers
(check-true (zero? 0)) ;; zero? checks if provided number is 0
(check-false (zero? 1492))

(define my_+
  (lambda (augend addend) ;; formal definitions for the two terms in addition
    (cond
      ((zero? addend) augend) ;; if addend is 'spent' / whittled down to zero - return the augend
      (else
       (my_+ (add1 augend) (sub1 addend)))))) ;; else, recur - adding 1 to augend and subbing 1 from addend

(check-equal? (my_+ 2 3) 5)

(define +
  (lambda (augend addend)
    (cond
      ((zero? addend) augend)
      (else
       (add1 (+ augend (sub1 addend))))))) ;; can just pass result of '+' to add1

(check-equal? (+ 2 3) 5)
