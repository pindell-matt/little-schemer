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
(check-equal? (+ 46 12) 58)

;; add1 and cons both "add one" to an existing construct

(define my_-
  (lambda (minuend subtrahend)
    (cond
      ((zero? subtrahend) minuend)
      (else
       (sub1 (my_- minuend (sub1 subtrahend)))))))

(check-equal? (my_- 14 3) 11)
(check-equal? (my_- 17 9) 8)

(define -
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (- n (sub1 m)))))))

(check-equal? (- 14 3) 11)
(check-equal? (- 17 9) 8)

(define addtuple
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtuple (cdr tup)))))))

;; The First Commandment (first revision)
;; When recurring on a list of atoms, lat, ask two questions:
;;  (null? lat) & (else ...)
;; When recurring on a number, n, ask two questions:
;;  (zero? n) & (else ...)

;; The Fourth Commandment (first revision)
;; Always change at least one argument while recurring.
;; It must be changed to be _closer_ to termination.
;; The changing argument must be tested in the termination condition:
;;  when using cdr  - test with (null? ...)
;;  when using sub1 - test with (zero? ...)

(define my_x
  (lambda (multiplicand multiplier)
    (cond
      ((zero? multiplier) 0)
      (else (+ multiplicand (my_x multiplicand (sub1 multiplier)))))))

(check-equal? (my_x 5 3) 15)
(check-equal? (my_x 13 4) 52)
(check-equal? (my_x 4 0) 0)
(check-equal? (my_x 0 5) 0)

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (x n (sub1 m)))))))

(check-equal? (x 5 3) 15)
(check-equal? (x 13 4) 52)

;; Some comments to help aid in visualization:

;; (x 2 3)
;; (zero? 3) - #f
;; (+ 2 (x 2 (sub1 3))))
;;  (x 2 2)
;;  (zero? 2) - #f
;;  (+ 2 (x 2 (sub1 2)))
;;   (x 2 1)
;;   (zero? 1) - #f
;;   (+ 2 (x 2 (sub1 1)))
;;    (zero? 0) - #t

;; (+ 2 (+ 2 (+ 2 0)))
;; (+ 2 (+ 2 (2)))
;; (+ 2 (+ 2  2))
;; (+ 2 (4))
;; (+ 2 4)
;; 6

;; (x 2 3) = 2 + (x 2 2)
;;         = 2 + 2 + (x 2 1)
;;         = 2 + 2 + 2 + (x 2 0)
;;         = 2 + 2 + 2 + 0

;; The Fifth Commandement
;; When building a value with +
;;  always use 0 for the value of the terminating line - adding 0 does not change the value
;; When building a value with x
;;  always use 1 for the value of the terminating line - multiplying by 1 does not change the value
;; When building a value with cons
;;  always consider '() for the value of the terminiating line

(define my_tup+
  (lambda (tup1 tup2)
   (cond
     ((or (null? tup1)
          (null? tup2)) '())
     (else
      (cons (+ (car tup1) (car tup2)) (my_tup+ (cdr tup1) (cdr tup2)))))))

(check-equal?
 (my_tup+
  '(3 6 9 11 4)
  '(8 5 2 0  7))
 '(11 11 11 11 11))