#lang racket

(provide cree-liste-tableau)
(provide ajout-tableau)

(provide NOT)
(provide get-proposition-NOT)

(define cree-liste-tableau list)
(define ajout-tableau list)

(define (NOT x) (list 'NOT x))
(define get-proposition-NOT cadr)