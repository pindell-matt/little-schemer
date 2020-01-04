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
          (null? tup2)) '()) ;; my implementation runs on uneven tuple pairings
     (else
      (cons (+ (car tup1) (car tup2))
            (my_tup+ (cdr tup1) (cdr tup2)))))))

(check-equal?
 (my_tup+
  '(3 6 9 11 4)
  '(8 5 2 0  7))
 '(11 11 11 11 11))

(check-equal?
 (my_tup+ '(2 3) '(4 6))
 '(6 9))

(check-equal?
 (my_tup+
  '(1 2 3 4 5 6 7 8 9)
  '(4 3 2 1))
  '(5 5 5 5)) ;; my implementations adds up until the smaller of the two tuples
              ;; ignoring the rest

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2))
       (quote ()))
      (else
       (cons (+ (car tup1) (car tup2))
             (tup+
              (cdr tup1) (cdr tup2)))))))

;; greedy_tup+ is a variation of tup+ that accepts any two tups
;; glomming the rest of the longest tup onto the final output
;; as though you were adding '0'

(define greedy_tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+ (car tup1) (car tup2))
             (greedy_tup+
              (cdr tup1) (cdr tup2)))))))

(check-equal?
 (greedy_tup+
  '(1 2 3 4 5 6 7 8 9)
  '(4 3 2 1))
  '(5 5 5 5 5 6 7 8 9))

(define my_>
  (lambda (n m)
    (cond
      ((zero? n) #f) ;; if n hits zero first - it was smaller
      ((zero? m) #t) ;; if m hits zero first - n  was larger!
      (else
       (my_> (sub1 n) (sub1 m))))))

(check-true (my_> 5 4))
(check-false (my_> 1 4))

(define my_<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (my_< (sub1 n) (sub1 m))))))

(check-false (my_< 5 4))
(check-true (my_< 1 4))

;; define = using my_> and my_<
(define =
  (lambda (n m)
    (and
     (not (my_> n m))
     (not (my_< n m)))))

;; exponents
(define ^
  (lambda (base exponent)
    (cond
      ((zero? exponent) 1)
      (else
       (* base (^ base (sub1 exponent )))))))


(check-equal? (^ 1 1) 1)
(check-equal? (^ 2 3) 8)
(check-equal? (^ 5 3) 125)

;; return length of a given list
(define my_length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (+ 1 (my_length (cdr lat)))))))

(check-equal? (my_length '(hotdogs with mustard sauerktraut and pickles)) 6)
(check-equal? (my_length '(ham and cheese on rye)) 5)

(define book_length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (book_length (cdr lat)))))))

;; pull out atom at provided index
(define pick
  (lambda (index lat)
    (cond
      ((null? lat) '())
      ((eq? index 1) (car lat))
      (else
       (pick (sub1 index) (cdr lat))))))

(check-equal?
 (pick 4 '(lasagna spaghetti ravioli macaroni meatball))
 'macaroni)

(check-equal?
 (pick 0 '())
 '())

(define book_pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (book_pick (sub1 n) (cdr lat))))))

;; remove atom at index
(define rempick
  (lambda (index lat)
    (cond
      ((zero? (sub1 index)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 index) (cdr lat)))))))

(check-equal?
 (rempick 3 '(hotdogs with hot mustard))
 '(hotdogs with mustard))

;; filters a list of atoms to remove all numbers
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
       (cons (car lat) (no-nums (cdr lat)))))))

(check-equal?
 (no-nums '(5 pears 6 prunes 9 dates))
 '(pears prunes dates))

(define book-no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat))
               (no-nums (cdr lat)))
              (else (cons (car lat)
                          (book-no-nums
                           (cdr lat)))))))))

(check-equal?
 (book-no-nums '(5 pears 6 prunes 9 dates))
 '(pears prunes dates))

;; filters a list of atoms down to keep only numbers
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond ((number? (car lat))
              (cons (car lat) (all-nums (cdr lat))))
              (else
               (all-nums (cdr lat))))))))

(check-equal?
 (all-nums '(5 pears 6 prunes 9 dates))
 '(5 6 9))

;; checks equivalence for both numbers and atoms
(define equan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)
            (number? a2))
       (= a1 a2))
      ((and (atom? a1)
            (atom? a2))
       (eq? a1 a2))
      (else (#f)))))

(check-true  (equan? 2 2))
(check-false (equan? 2 3))
(check-true  (equan? 'a 'a))
(check-false (equan? 'a 'b))
(check-false (equan? 1 'b))

(define book-equan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

;; return total count of times atom 'a' occurs
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((equan? a (car lat)) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(check-equal?
 (occur 'this '(this is the number 1 test for occur I hope this works))
 2)

(define one?
  (lambda (n)
    (zero? (sub1 n))))

(check-true (one? 1))
(check-false (one? 2))
(check-false (one? 0))

(define book-one?
  (lambda (n)
    (= n 1)))


;; new rempick using one?
(define new-rempick
  (lambda (index lat)
    (cond
      ((one? index) (cdr lat))
      (else
       (cons (car lat)
             (new-rempick (sub1 index) (cdr lat)))))))

