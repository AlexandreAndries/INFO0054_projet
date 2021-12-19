#lang racket
; ---------------------------------------------------------------------------- ;
; ---------------------------------MODULE.RKT--------------------------------- ;
; ---------------------------------------------------------------------------- ;
; # Auteurs : ROTHEUDT Thomas - ANDRIES Alexandre - KHALIPHI Abdelilah         ;
; # Projet  : INFO0054 - Programmation Fonctionelle                            ;
; # Date    : 11/12/2021                                                       ;
; ---------------------------------------------------------------------------- ;
(require "tableau.rkt")
(require "semtab.rkt")
(require racket/trace)
; ---------------------------------------------------------------------------- ;
; -----------------------Définition des tableaux de tests--------------------- ;
; ---------------------------------------------------------------------------- ;
(define test-tautology '(OR a (NOT a)))
(define test-satisfiable '(a (NOT b) (IFTHEN (NOT b) c)))
(define test-contradiction '(AND a (OR b (NOT a))))

(define test-elements '((a (NOT b)) (a c) (a b (NOT c))))

; ---------------------------------------------------------------------------- ;
; ------------------------------fonctions locales----------------------------- ;
; ---------------------------------------------------------------------------- ;
;Vérifie si la liste x est un atome
;   précondition: x != null
;
;   retourne:
;         #t si x est un atome
;         #f sinon
(define (atom? x) (and (not (null? x)) (not (pair? x))))
; ---------------------------------------------------------------------------- ;
(define (elements liste-formule)
        (letrec
          ((elements-aux (lambda (liste-formule)
                                  (if (null? liste-formule)
                                      '()
                                      (let* ((ls (flatten liste-formule))
                                             (head (car ls))
                                             (reste (elements (cdr ls))))
                                          (match head
                                                  ('NOT reste)
                                                  ('IFTHEN reste)
                                                  ('AND reste)
                                                  ('OR reste)
                                                  (a (cons head reste))
                                          )
                                      )
                                  )
                         )
           )
          )

          (remove-duplicates (elements-aux liste-formule))
        )
)
; ---------------------------------------------------------------------------- ;

; ---------------------------------------------------------------------------- ;
; -------------------------------FONCTIONS LOGIC------------------------------ ;
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
; (define (models liste-formule)
; )
; ---------------------------------------------------------------------------- ;
;(define (counterexamples? formule) (contient? #t (map (lambda (x y) (satisfiable? (append x (not y)))) (semtab (cree-liste-tableau formule))))
;)
; ---------------------------------------------------------------------------- ;
; ---------------------------------------------------------------------------- ;
; ---------------------------------------TEST--------------------------------- ;
; ---------------------------------------------------------------------------- ;
;(tautology? test-tautology)                  ; doit donner #t
;(satisfiable? test-satisfiable)              ; doit donner #t
;(contradiction? test-contradiction)          ; doit donner #t

; (define test (semtab '((IFTHEN p (IFTHEN q r)) (NOT (IFTHEN (IFTHEN p q) r)))))

(elements (semtab '((IFTHEN p (IFTHEN q r)) (NOT (IFTHEN (IFTHEN p q) r)))))
(map elements (semtab '((IFTHEN p (IFTHEN q r)) (NOT (IFTHEN (IFTHEN p q) r)))))
; ---------------------------------------------------------------------------- ;
; -----------------------------------FIN TEST--------------------------------- ;
; ---------------------------------------------------------------------------- ;
