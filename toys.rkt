#lang scheme
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
(check-exn exn:fail?
  (lambda () (list? '(atom turkey) 'or))) ; 'or is a single atom, not within the list
(check-true (list? '((atom turkey) or)))  ; list of 2 elements: '(atom turkey), and 'or

;; s-expressions
(check-not-exn (lambda () 'xyz))       ; atoms are valid s-expressions
(check-not-exn (lambda () '(x y z)))   ; lists of atoms are valid s-expressions
(check-not-exn (lambda () '((x y) z))) ; lists themselves are valid s-expressions

(check-true                           ; list of 6 s-expressions (atoms):
 (list? '(how are you doing so far))) ; 'how, 'are, 'you, 'doing, 'so, 'far

(check-true                                     ; list of 3 s-expressions (2 lists, 1 atom):
 (list? '(((how) are) ((you) (doing so)) far))) ; '((how) are), '((you) (doing so)), 'far

(check-true (list? '()))            ; an empty (null) list is still a valid s-expression
(check-true (list? '(() () () ()))) ; since empty lists are valid s-expressions, they can populate a list
(check-false (atom? '()))           ; but empty lists are not atoms

;; The Law of Car - car [contents of address register]
;; the primitive car returns first element of a list, only for non-empty lists
(check-equal? (car '(a b c)) 'a)
(check-equal? (car '((a b c) x y z)) '(a b c))
(check-exn exn:fail? (lambda () (car 'string))) ; contract violation for 'string
(check-exn exn:fail? (lambda () (car '())))     ; contract violation for empty list

(check-equal? ; the first element is the list containing a list that contains 'hotdog
  (car '(((hotdogs)) (and) (pickle) relish))
  '((hotdogs)))

(check-equal? ; the additional car means we get a list containing only 'hotdog
  (car (car '(((hotdogs)) (and))))
  '(hotdogs))

;; The Law of Car - car [contents of address register - first element]
;; the primitive car returns first element of a list, only for non-empty lists

(check-equal? (cdr '(a b c)) '(b c))   ; the 'rest' of the list is 'b and 'c
(check-equal? (cdr '(hamburger)) '())  ; the 'rest' of the list is empty - hence an empty list
(check-equal? (cdr '((x) t r)) '(t r)) ; it doesn't matter that the first element can be a list itself

(check-exn exn:fail?
  (lambda () (cdr 'hotdogs))) ; contract violation for 'string

(check-exn exn:fail?
  (lambda () (cdr '()))) ; contract violation for empty list

(check-equal?
  (car (cdr '((b) (x y) ((c)))))
  '(x y))

(check-equal?
  (cdr (cdr '((b) (x y) ((c)))))
  '(((c))))

(check-exn exn:fail?
  (lambda () (cdr (car '(a (b (c)) d))))) ; contract violation 'a is an atom

;; The Law of Cdr - cdr [contents of decrement register - all after first element (car)]
;; the primitive cdr returns everything after the car of a list

(check-equal? ; 'peanut is prepended to the provided list
  (cons 'peanut '(butter and jelly))
  '(peanut butter and jelly))

(check-equal? ; the list '(banana and) is prepended as the first item of the list
  (cons '(banana and) '(peanut butter and jelly))
  '((banana and) peanut butter and jelly))

(check-equal?
   (cons '((help) this)
         '(is very ((hard) to learn)))
   '(((help) this) is very ((hard) to learn)))

(check-equal? ; you can add to previously empty lists
  (cons '(a b (c))
        '())
  '((a b (c))))

(check-equal?
  (cons 'a '())
  '(a))

;; The Law of Cons - cons [constructs a list]
;; the primitive cons takes two arguments and returns a list
;; the second argument must be a list

(check-equal? ; you can constract new lists from car/cdr results
  (cons 'a (car '((b) c d)))
  '(a b))

(check-equal?
  (cons 'a (cdr '((b) c d)))
  '(a c d))

(check-true (null? '()))        ; null? checks for null/empty lists
(check-true (null? (quote ()))) ; '() and (quote ()) are equivalent

(check-equal? ; '() and (quote ()) are both ways of creating null lists
  '()
  (quote ()))

(check-false (null? '(a b c)))
(check-false (null? 'spaghetti))
(check-true (atom? 'Harry))
(check-false (atom? '(Harry had a heap of apples)))

;; atom? takes 1 argument and it's an s-expression

(check-true ; the first element is 'harry
  (atom?
    (car '(harry had a heap of apples))))

(check-false ; the cdr returns '(had a heap of apples)
  (atom?
    (cdr '(Harry had a heap of apples))))

(check-false ; the cdr returns a null list
  (atom?
    (cdr '(Harry))))

(check-true ; the result is 'low which is an atom
  (atom?
    (car (cdr '(swing low sweet cherry oat)))))

(check-false ; the result is the list: '(low sweet)
  (atom?
    (car (cdr '(swing (low sweet) cherry oat)))))

(check-equal? 'Harry 'Harry) ; matching strings are considered identical
(check-true (eq? 'Harry 'Harry))
(check-false (eq? 'margarine 'butter))

;; The Law of Eq - eq? [equivalent]
;; eq? takes two arguments: both must be non-numeric atoms
;; note - in scheme, it does accept lists

(check-true ; both equal 'Mary
  (eq?
    (car '(Mary had a little lamb chop))
    'Mary))

(check-false ; '(milk) != 'milk
  (eq?
    (cdr '(soured milk))
    'milk))

(check-true ; 'beans is the car of the original list, and the cdr-created list
  (eq?
    (car '(beans beans we need jelly beans))
    (car
      (cdr '(beans beans we need jelly beans)))))
