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
(define test-ferme '((b (IFTHEN b c) c (NOT a) d)))
(define test-ouvert '((b c (NOT a) d e)))
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
    (let* ((prem-elem (car tableau)) (reste-tab (cdr tableau)) (continue (contient-contradiction? reste-tab)))
      (if (atom? prem-elem)
        (if (contient? (NOT prem-elem) reste-tab) #t continue)
        (if (contient? (get-proposition-NOT prem-elem) reste-tab) #t continue)
      )
    )
  )
)
; ---------------------------------------------------------------------------- ;
;Vérifie si le tableau entré contient un opérateur binaire (AND/OR/IFTHEN)
;   précondition: tableau != null
;
;   retourne:
;         #t si tableau contient un opérateur binaire
;         #f sinon (ne contient que des atomes et des 'NOT)
(define (contient-operateur? tableau)
  (if (null? tableau)
    ;consequent
    #f
    ;else
    (let* ((prem-elem (car tableau)) (reste-tab (cdr tableau)) (continue (contient-operateur? reste-tab)))
      (if (atom? prem-elem)
        continue
        (if (not (equal? 'NOT (car prem-elem))) #t continue)
      )
    )
  )
)
; ---------------------------------------------------------------------------- ;
;Vérifie si le tableau entré est ouvert (ou, sinon, fermé)
;   précondition: tableau != null
;
;   retourne:
;         #t si tableau est ouvert (ne contient ni contra., ni opér. binaire)
;         #f sinon (le tableau est alors fermé)
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
(contient-contradiction? (car test-contra-f))    ; doit donner #f
(contient-contradiction? (car test-contra-t))    ; doit donner #t
(contient-operateur? (car test-ouvert))          ; doit donner #f
(est-ouvert? (car test-ouvert))                  ; doit donner #t
(contient-operateur? (car test-ferme))           ; doit donner #t
(est-ouvert? (car test-ferme))                   ; doit donner #f
; ---------------------------------------------------------------------------- ;
; ---------------------------------FIN SEMTAB--------------------------------- ;
; ---------------------------------------------------------------------------- ;
