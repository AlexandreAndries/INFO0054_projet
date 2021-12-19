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
(define (elements-ls l)
        (if (null? l)
            '()
            (let* ((head (car l)) (tail (cdr l)) (continue (elements-ls tail)))
                  (cond ((not (atom? head)) (cons (cdr head) continue))
                        ((member head tail) continue)
                        (else (cons head continue))
                  )
            )
        )
)
; ---------------------------------------------------------------------------- ;
(define (elements liste-formule)
        (remove-duplicates
          (flatten
           (map elements-ls liste-formule)
          )
        )
)
; ---------------------------------------------------------------------------- ;
(define (models-rec ls elem)
        (if (null? elem)
            '()
            (let* ((head (car elem)) (tail (cdr elem)) (continue (models-rec tail)))
                  (if (member head ls)
                      ; construire list avec ls et head et mk-NOT head.. difficile
                  )
            )
        )
)
; ---------------------------------------------------------------------------- ;
(define (models-aux ls)
        (let ((elem (elements ls)))
              (models-rec ls elem)
        )
)
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
(define (models liste-formule) (map models-aux (semtab liste-formule))
)
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

(define test (semtab '((IFTHEN p (IFTHEN q r)) (NOT (IFTHEN (IFTHEN p q) r)))))

(display 'test= )
(models '((IFTHEN p (IFTHEN q r)) (NOT (IFTHEN (IFTHEN p q) r))))
; ---------------------------------------------------------------------------- ;
; -----------------------------------FIN TEST--------------------------------- ;
; ---------------------------------------------------------------------------- ;
