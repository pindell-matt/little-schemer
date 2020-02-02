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

(check-equal?
 (my_rember*
  'cup
  '((coffee) cup ((tea) cup) (and (hick)) cup))
 '((coffee) ((tea)) (and (hick))))  

(check-equal?
 (my_rember*
  'sauce
  '(((tomato sauce) ((bean) sauce) (and ((flying)) sauce))))
 '(((tomato) ((bean)) (and ((flying))))))

;; book version
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else
          (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))


(define my_insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old (cons new (my_insertR* new old (cdr l)))))
         (else
          (cons (car l) (my_insertR* new old (cdr l))))))
      (else
       (cons (my_insertR* new old (car l))
             (my_insertR* new old (cdr l)))))))

(check-equal?
 (my_insertR*
  'roast
  'chuck
  '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
 '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))
 