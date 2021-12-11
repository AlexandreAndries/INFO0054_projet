#lang racket
; ---------------------------------------------------------------------------- ;
; -----------------------------------SEMTAB.RKT------------------------------- ;
; ---------------------------------------------------------------------------- ;
; # Auteurs : ROTHEUDT Thomas - ANDRIES Alexandre - KHALIPHI Abdelilah         ;
; # Projet  : INFO0054 - Programmation Fonctionelle                            ;
; # Date    : 11/12/2021                                                       ;
; ---------------------------------------------------------------------------- ;
(require racket/trace)
(require "tableau.rkt")
; ---------------------------------------------------------------------------- ;
; -----------------------Définition des tableaux de tests--------------------- ;
; ---------------------------------------------------------------------------- ;
(define test '((NOT a) (NOT (OR a b)) (NOT (AND a b)) (NOT (IFTHEN a b)) (NOT a) (IFTHEN a b) (OR a b) (AND a b)))
(define test-contra-f '((b c (NOT a) d)))
(define test-contra-t '((b c (NOT a) d a)))
(define test-ouvert-t '((b (IFTHEN b c) c (NOT a) d)))
(define test-ouvert-f '((b c (NOT a) d e)))
; ---------------------------------------------------------------------------- ;
; -----------------------Définition des fonctions locales--------------------- ;
; ---------------------------------------------------------------------------- ;
;Vérifie si la liste x est un atome
;   précondition: x != null
;
;   retourne:
;         #t si x est un atome
;         #f sinon
(define (atom? x) (and (not (null? x)) (not (pair? x))))
; ---------------------------------------------------------------------------- ;
(define (contient? x ls) (if (member x ls) #t #f))
; ---------------------------------------------------------------------------- ;
(define (premier? bool operation) (if (eqv? bool (car operation)) #t #f))
; ---------------------------------------------------------------------------- ;
;Elimine les opérations dites simples en un ou deux tableaux maximum
;   précondition: operation != null
;
;   retourne:
;         liste contenant les différents tableaux d'opération (1 ou 2 tableau dans la liste en fonction des cas)
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
          )
        )
      )
    )
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
          )
        )
      )
    )
  )
)
; ---------------------------------------------------------------------------- ;
(define (contient-contradiction? tableau)
  (if (null? tableau)
    ;consequent
    #f
    ;else
    (let* ((prem-elem (car tableau)) (reste-tab (cdr tableau)) (continue (contient-contradiction? reste-tab)))
      (if (atom? prem-elem)
        (if (contient? (NOT prem-elem) reste-tab) #t continue)
        (if (contient? (get-proposition-NOT prem-elem) reste-tab) #t continue)
      ))))
; ---------------------------------------------------------------------------- ;
(define (contient-operateur? tableau)
  (if (null? tableau)
    ;consequent
    #f
    ;else
    (let* ((prem-elem (car tableau)) (reste-tab (cdr tableau)) (continue (contient-operateur? reste-tab)))
      (if (atom? prem-elem)
        (if (or (contient? 'IFTHEN reste-tab) (contient? 'AND reste-tab) (contient? 'OR reste-tab)) #t continue)
        continue ;temporaire, a modifier
        ; (let (sous-tab (cadr reste-tab))
        ;   (if (or (contient? 'IFTHEN sous-tab) (contient? 'AND sous-tab) (contient? 'OR sous-tab)) #t continue)
        ; )
      )
    )
  )
)
; ---------------------------------------------------------------------------- ;
(define (est-ouvert? tableau)
  (cond ((contient-contradiction? tableau) #f)
        ((contient-operateur? tableau) #f)
        (else #t)
  )
)
; ---------------------------------------------------------------------------- ;
; -----------------------------------SEMTAB----------------------------------- ;
; ---------------------------------------------------------------------------- ;
; définir semtab ici

; ---------------------------------------------------------------------------- ;
; -------------------------------Zone de tests-------------------------------- ;
; ---------------------------------------------------------------------------- ;
(contient-contradiction? test-contra-f)    ; doit donner #f
(contient-contradiction? test-contra-t)    ; doit donner #t
(contient-operateur? test-ouvert-t)        ; doit donner #f
(est-ouvert? test-ouvert-t)                ; doit donner #t
(contient-operateur? test-ouvert-f)        ; doit donner #t
(est-ouvert? test-ouvert-f)                ; doit donner #f
; ---------------------------------------------------------------------------- ;
; ---------------------------------FIN SEMTAB--------------------------------- ;
; ---------------------------------------------------------------------------- ;
