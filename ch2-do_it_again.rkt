#lang scheme

(require rackunit) ; for checks / assertions

;; atom? - validates an atom
;; s-expression -> #t or #f
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; lat? - my attempt
;; s-expression -> #t or #f
(define (mylat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (mylat? (cdr l)))
    (else #f)))

(check-true ; all elements are atoms
 (mylat? '(Jack Sprat could eat no chicken fat)))
(check-false ; first element is a list itself '(Jack)
 (mylat? '((Jack) Sprat could eat no chicken fat)))
(check-false ; '(Sprat could) is a list
 (mylat? '(Jack (Sprat could) eat no chicken fat)))
(check-true ; an empty list does not have any invalid elements
 (mylat? '()))

;; a lat is a list of atoms: l(ist)at(oms)

;; lat? - as defined by the book
;; s-expression -> #t or #f
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(check-true (lat? '(bacon and eggs)))

;; the conditions in lat?
;; (null? l) checks whether the list is null/empty - per the First Commandment (else is asked at the end)
;; (atom? (car l)) checks if the next first-position s-expression is an atom
;;   if so, lat? recursively calls itself again - on the remaining elements in the list: (cdr l)
;; (else #f) - to close out the First Commandment and ensure lat? defaults to #f / false

;; example:
;; (null? '(bacon and eggs)) #f - continue on to next conditional
;; (atom? (car '(bacon and eggs))) #t - 'bacon is an atom, so we evalute the next half of the conditional
;; (lat? (cdr '(bacon and eggs))) - passes the cdr of the original list into a new recursive call to lat?

;;   (null? '(and eggs)) #f - continue on to next conditional
;;   (atom? (car '(and eggs))) #t - 'and is an atom, so we evaluate the next half of the conditional
;;   (lat? (cdr '(and eggs))) - passes the result of cdr: '(eggs) into a new recursive call to lat?

;;     (null? '(eggs)) #f - continue on to next conditional
;;     (atom? (car '(eggs))) #t - 'eggs is an atom
;;     (lat? (cdr '(eggs))) # passes '() to a new recursive call to lat?

;;       (null? '()) #t - the final return value

;; We got to the end of the list, exhausting all the items and since all qualified as atoms, the list was a lat!

;; if any s-expression in the list "l" had been a list - it would have continued down to the else condition
;; "else" always evaluates as true and so the application would have returned the second half of the else conditional
;; meaning it would return #f!


(check-true (or (null? '(a b c)) (null? '()))) ; or returns #t if any of the provided s-expressions is true
(check-false (or (null? '(abc)) (null? '(atom))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(check-false (member? 'poached '(fried eggs and scrambled eggs)))
(check-true (member? 'meat '(mashed potatoes and meat gravy)))

