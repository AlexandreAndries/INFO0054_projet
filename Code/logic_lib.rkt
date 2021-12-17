#lang racket
; ---------------------------------------------------------------------------- ;
; -----------------------------------SEMTAB.RKT------------------------------- ;
; ---------------------------------------------------------------------------- ;
; # Auteurs : ROTHEUDT Thomas - ANDRIES Alexandre - KHALIPHI Abdelilah         ;
; # Projet  : INFO0054 - Programmation Fonctionelle                            ;
; # Date    : 11/12/2021                                                       ;
; ---------------------------------------------------------------------------- ;
(require "tableau.rkt")
(require "Semtab.rkt")
; ---------------------------------------------------------------------------- ;
; -----------------------Définition des tableaux de tests--------------------- ;
; ---------------------------------------------------------------------------- ;
(define test-tautology '(OR a (NOT a)))
(define test-satisfiable '(a (NOT b) (IFTHEN (NOT b) c)))
(define test-contradiction '(AND a (OR b (NOT a))))
; ---------------------------------------------------------------------------- ;
; --------------------------------FONCTION LOGIC------------------------------ ;
; ---------------------------------------------------------------------------- ;

(define (satisfiable? liste-formule) (contient? #f (map contient-contradiction? (semtab liste-formule)))
)
; ---------------------------------------------------------------------------- ;
(define (tautology? formule) (not (contient? #f (map (lambda (x) (not (contient-contradiction? x))) (semtab (cree-liste-tableau formule)))))
)
; ---------------------------------------------------------------------------- ;
(define (contradiction? formule) (contient? #t (map (lambda (x) (contient-contradiction? x)) (semtab (cree-liste-tableau formule))))
)
; ---------------------------------------------------------------------------- ;
; ---------------------------------------TEST--------------------------------- ;
; ---------------------------------------------------------------------------- ;
(tautology? test-tautology)                  ; doit donner #t 
(satisfiable? test-satisfiable)              ; doit donner #t
(contradiction? test-contradiction)          ; doit donner #t
; ---------------------------------------------------------------------------- ;
; -----------------------------------FIN TEST--------------------------------- ;
; ---------------------------------------------------------------------------- ;