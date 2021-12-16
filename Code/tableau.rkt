#lang racket

(provide cree-liste-tableau)
(provide ajout-tableau)

(provide mk-NOT)
(provide get-proposition-NOT)

(define cree-liste-tableau list)
(define ajout-tableau list)

(define (mk-NOT x) (list 'NOT x))
(define get-proposition-NOT cadr)



;Faire fonction cree-liste-vide -> '() et fonction (cree-liste nbr-aleatoire-tableau) -> liste des tableau en arguments
;(or (eqv? 'AND (caar liste-formule)) (and (eqv? 'NOT (caar liste-formule)) (eqv? 'IFTHEN (caadar liste-formule)))