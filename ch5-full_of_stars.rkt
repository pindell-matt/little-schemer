#lang scheme
(require rackunit) ; for checks / assertions
(include "ch4-numbers_games.rkt")

(define (atom? a)
  (and (not (null? a)) (not (pair? a))))

;; rember* - removes all atoms, regardless of nesting
(define (rember* a l)
  (cond
    ((null? l) '())
    ((eq? (car l) a) (rember* a (cdr l)))
    (else
     (cond
       ((pair? (car l))
        (cons (rember* a (car l))
              (rember* a (cdr l))))
       (else
        (cons (car l) (rember* a (cdr l))))))))

(check-equal?
 (rember*
  'cup
  '((coffee) cup ((tea) cup) (and (hick)) cup))
 '((coffee) ((tea)) (and (hick))))  

(check-equal?
 (rember*
  'sauce
  '(((tomato sauce) ((bean) sauce) (and ((flying)) sauce))))
 '(((tomato) ((bean)) (and ((flying))))))

;; rember* - book version
;; (define rember*
;;   (lambda (a l)
;;     (cond
;;       ((null? l) (quote ()))
;;       ((atom? (car l))
;;        (cond
;;          ((eq? (car l) a)
;;           (rember* a (cdr l)))
;;          (else
;;           (cons (car l) (rember* a (cdr l))))))
;;       (else (cons (rember* a (car l))
;;                   (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old (cons new (insertR* new old (cdr l)))))
         (else
          (cons (car l) (insertR* new old (cdr l))))))
      (else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))))))

(check-equal?
 (insertR*
  'roast
  'chuck
  '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
 '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))

;; (check-equal? my_insertR* book_insertR*) #t !
;; different indentation - but same structure!

;; The First Commandment
;; (final version)
;; When recurring on a list of atoms 'lat'
;;  Ask 2 questions: (null? lat) and else
;; When recurring on a number 'n'
;;  Ask 2 questions: (zero? n) and else
;; When recurring on a list of S-Expressions 'l'
;;  Ask 3 questions: (null? l), (atom? (car l)), and else

;; Q: How are insertR* and rember* similar?
;; A: Both recur on the (car l) if it is a list

;; Q: How are rember* and multirember different?
;; A: rember* removes matching atoms from sub-lists, whereas multirember only removes from the top level of the list

;; Q: How are all *-functions similar?
;; A: They all recur on both (car l) and (cdr l), digging into nested S-Expressions if (car l) is not an atom and not null.

;; The Fourth Commandment
;; (final version)
;; Always change at least one argument while recurring
;; When recurring on a list of atoms 'lat'
;;  use (cdr lat)
;; When recurring on a number 'n'
;;  use (sub1 n)
;; When recurring on a list of S-Expressions 'l'
;;  use (car l) and (cdr l) if neither (null? l) nor (atom? (car l)) are true

;; It must be changed to be closer to termination. The changing argument must be tested in the termination condition.
;; When using cdr  - test termination with null?
;; when using sub1 - test termination with zero?

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? (car l) a) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))))
      (else
        (+ (occur* a (car l))
           (occur* a (cdr l)))))))

(check-equal?
  (occur* 'banana '((banana (split ((((banana ice))) (cream (banana)) sherbert)) (banana) (bread) (banana brandy))))
  5)