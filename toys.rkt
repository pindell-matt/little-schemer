#lang racket
(require rackunit) ; for checks / assertions

;; atom?
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;; atoms
(check-true (atom? 'atom))   ; 'atom is a string of characters, and strings are atoms
(check-true (atom? 'turkey)) ; 'turkey is a string of characters
(check-true (atom? 1492))    ; 1492 is a string of digits
(check-true (atom? 'u))      ; 'u is a one character string
(check-true (atom? '*abc$))  ; '*abc$ is a string of special characters [not including "(" or ")"]

;; lists
(check-true (list? '(atom)))           ; '(atom) is an atom enclosed in a list: '(...)
(check-true (list? '(atom turkey or))) ; list of 3 elements: 'atom, 'turkey, and 'or
(check-exn
  exn:fail?
  (lambda () (list? '(atom turkey) 'or))) ; 'or is a single atom, not within the list
(check-true (list? '((atom turkey) or)))  ; list of 2 elements: '(atom turkey), and 'or

;; s-expressions
(check-not-exn (lambda () 'xyz))       ; atoms are valid s-expressions
(check-not-exn (lambda () '(x y z)))   ; lists of atoms are valid s-expressions
(check-not-exn (lambda () '((x y) z))) ; lists themselves are valid s-expressions

(check-true (list? '(how are you doing so far))) ; list of 6 s-expressions
;; all atoms:
;; 'how, 'are, 'you, 'doing, 'so, 'far

(check-true (list? '(((how) are) ((you) (doing so)) far))) ; list of 3 s-expressions
;; two nested lists and an atom:
;; '((how) are), '((you) (doing so)), 'far

(check-true (list? '()))            ; an empty (null) list is still a valid s-expression
(check-true (list? '(() () () ()))) ; since empty lists are valid s-expressions, they can populate a list
(check-false (atom? '()))           ; but empty lists are not atoms

;; The Law of Car - car [contents of address register]
;; the primitive car returns first element of a list, only for non-empty lists
(check-equal? (car '(a b c)) 'a)
(check-equal? (car '((a b c) x y z)) '(a b c))
(check-exn exn:fail? (lambda () (car 'string))) ; contract violation for 'string
(check-exn exn:fail? (lambda () (car '())))     ; contract violation for empty list

;; '((hotdogs))
(check-equal?
  (car '(((hotdogs)) (and) (pickle) relish))
  '((hotdogs)))

;; '((hotdogs)) -> '(hotdogs)
(check-equal?
  (car (car '(((hotdogs)) (and))))
  '(hotdogs))

;; '(b c)
(check-equal?
   (cdr '(a b c))
   '(b c))

