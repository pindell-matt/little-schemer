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
    (else (cons (car lat)
                (my_rember a (cdr lat))))))

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

(check-exn exn:fail? ;; our current implementation fails - turns out we forgot to use cons!
           (lambda ()
             (check-equal?
              (rember 'and '(bacon lettuce and tomato))
              '(bacon lettuce tomato))))

;; official rember - now with cons!
(define rember_with_cons
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember_with_cons a (cdr lat)))))))

;; The Second Commandment: use cons to build lists

(check-equal? ;; removes only first instance of 'sauce
 (rember_with_cons 'sauce '(soy sauce and tomato sauce))
 '(soy and tomato sauce))

;; firsts - takes first element of each provided lat
;; my attempt:
(define my_firsts
  (lambda (llat)
    (cond
      ((or
        (null? llat)
        (null? (car (car llat)))) '())
      (else (cons (car (car llat))
                  (my_firsts (cdr llat)))))))

(check-equal?
 (my_firsts '((apple peach pumpkin)
              (plum pear cherry)
              (grape raisin pea)
              (bean carrot eggplant)))
 '(apple plum grape bean))

(check-equal? (my_firsts '()) '())

(check-equal?
 (my_firsts '((five plums)
              (four)
              (eleven green oranges)))
 '(five four eleven))

;; firsts takes a l ist of lats and builds a new list with the first element (car lat) of each lat

;; official firsts - better because it doesn't do needless (null? (car (car llat)) check!
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(check-equal?
 (firsts '((a b) (c d) (e f)))
 '(a c e))

;; The Third Commandment: When building a list, describe the first typical element,
;; and then cons it onto the natural recursion.

;; ex: (car (car l))    == typical element
;;     (firsts (cdr l)) == natural recursion

;; insertR - inserts 'new' atom after 'old' atom in a given lat
;; my attemt:
(define my_insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else
       (cons (car lat)
             (my_insertR new old (cdr lat)))))))

(check-equal?
 (my_insertR
  'topping
  'fudge
  '(ice cream with fudge for dessert))
 '(ice cream with fudge topping for dessert))