#lang scheme
(require rackunit) ; for checks / assertions

;; atom? - validates an atom
;; s-expression -> #t or #f
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))