#lang racket
; ---------------------------------------------------------------------------- ;
; --------------------------------TABLEAU.RKT--------------------------------- ;
; ---------------------------------------------------------------------------- ;
; # Auteurs : ROTHEUDT Thomas - ANDRIES Alexandre - KHALIPHI Abdelilah         ;
; # Projet  : INFO0054 - Programmation Fonctionelle                            ;
; # Date    : 11/12/2021                                                       ;
; ---------------------------------------------------------------------------- ;
(provide cree-liste-tab)
(provide ajout-tab)

(provide mk-NOT)
(provide get-proposition-NOT)
; ---------------------------------------------------------------------------- ;
; -----------------------Définition des fonctions utiles--------------------- ;
; ---------------------------------------------------------------------------- ;
;Vérifie si la liste x est un atome
;   précondition: x != null
;
;   retourne:
;         #t si x est un atome
;         #f sinon
(define (atom? x) (and (not (null? x)) (not (pair? x))))
(provide atom?)
; ---------------------------------------------------------------------------- ;
;Vérifie si "x" appartient à "ls"
;   précondition: x != null
;
;   retourne:
;         #t si x appartient à ls
;         #f sinon
(define (contient? x ls) (if (member x ls) #t #f))
(provide contient?)
; ---------------------------------------------------------------------------- ;
;Supprime tous les duplicats existant dans l
;   précondition: l != null
;
;   retourne:
;         une liste traité ne possèdant plus de duplicats
(define (remove-duplicates l)
  (cond ((null? l) '())
        ((member (car l) (cdr l)) (remove-duplicates (cdr l)))
        (else (cons (car l) (remove-duplicates (cdr l)))))
)
(provide remove-duplicates)
; ---------------------------------------------------------------------------- ;
;Vérifie si le tableau entré contient une contradiction (ex: a et (NOT a))
;   précondition: tableau != null
;
;   retourne:
;         #t si tableau contient une contradiction
;         #f sinon
(define (contient-contradiction? tableau)
  (if (null? tableau)
    ;consequent
    #f
    ;else
    (let* ((head (car tableau)) (tail (cdr tableau)) (continue (contient-contradiction? tail)))
      (if (atom? head)
        (if (contient? (mk-NOT head) tail) #t continue)
        (if (contient? (get-proposition-NOT head) tail) #t continue)))
  )
)
(provide contient-contradiction?)
; ---------------------------------------------------------------------------- ;
; ---------------Définition des fonctions pour les listes--------------------- ;
; ---------------------------------------------------------------------------- ;
(define cree-liste-tab list)

; ---------------------------------------------------------------------------- ;
(define ajout-tab list)
; ---------------------------------------------------------------------------- ;
(define (mk-NOT x) (list 'NOT x))
; ---------------------------------------------------------------------------- ;
(define get-proposition-NOT cadr)
; ---------------------------------------------------------------------------- ;
; ----------------------Fin des fonctions pour les listes--------------------- ;
; ---------------------------------------------------------------------------- ;
