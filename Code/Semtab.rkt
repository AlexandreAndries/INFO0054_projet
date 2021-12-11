#lang racket

(require "tableau.rkt")
;Vérifie si la liste x est un atome
;   précondition: x != null
;
;   retourne: 
;         #t si x est un atome
;         #f sinon
(define (atom? x) (and (not (null? x)) (not (pair? x))))


(define test '((NOT a) (NOT (OR a b)) (NOT (AND a b)) (NOT (IFTHEN a b)) (NOT a) (IFTHEN a b) (OR a b) (AND a b)))


(define (premier? bool operation) (if (eqv? bool (car operation)) #t #f))

;Elimine les opérations dites simples en un ou deux tableaux maximum
;   précondition: operation != null
;
;   retourne:
;         liste contenant les différents tableaux d'opération
(define (elimination-ope operation)
  (if (not (atom? (cadr operation)))
    ;consequent
    (let* ((sous-op (cadr operation)) (prem-elem (cadr sous-op)))
      (if (premier? 'NOT sous-op)
        ;consequent
        (cree-liste-tableau prem-elem)
        ;else
        (let ((sec-elem (caddr sous-op)))
          (cond
            ((premier? 'AND sous-op) (cree-liste-tableau (ajout-tableau (NOT prem-elem)) (ajout-tableau (NOT sec-elem))))
            ((premier? 'OR sous-op) (cree-liste-tableau (ajout-tableau (NOT prem-elem) (NOT sec-elem))))
            ((premier? 'IFTHEN sous-op) (cree-liste-tableau (ajout-tableau prem-elem (NOT sec-elem))))
          ))))
    ;else
    (let ((prem-elem (cadr operation)))
      (if (premier? 'NOT operation)
        ;consequent
        (cree-liste-tableau (NOT prem-elem))
        ;else
        (let ((sec-elem (caddr operation)))
          (cond 
            ((premier? 'AND operation) (cree-liste-tableau (ajout-tableau prem-elem sec-elem)))
            ((premier? 'OR operation) (cree-liste-tableau (ajout-tableau prem-elem) (ajout-tableau sec-elem)))
            ((premier? 'IFTHEN operation) (cree-liste-tableau (ajout-tableau (NOT prem-elem) sec-elem)))
          ))))))

(map elimination-ope test)