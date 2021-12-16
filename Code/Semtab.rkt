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
(define (elim-operation operation)
  (match formule
        ((list 'AND a b) (list (list a b)))
        ((list 'OR a b) (list (list a) (list b)))
        ((list 'IFTHEN a b) (list (list (mk-NOT a)) (list b)))
        ((list 'NOT (list 'AND a b)) (list (list (mk-NOT a)) (list (mk-NOT b))))
        ((list 'NOT (list 'OR a b)) (list (list (mk-NOT a) (mk-NOT b))))
        ((list 'NOT (list 'IFTHEN a b)) (list (list a (mk-NOT b))))
        ((list 'NOT (list 'NOT a)) (list (list a)))
        ((list 'NOT a) (list (list(mk-NOT a))))
        (a (list (list a)))
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
        (if (contient? (get-proposition-NOT prem-elem) reste-tab) #t continue)))
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
        (if (not (equal? 'NOT (car prem-elem))) #t continue)))
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
  (cond ((contient-operateur? tableau) #f)
        ((contient-contradiction? tableau) #f)
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
(elim-operation '(NOT (AND a b)))                ; doit donner (((NOT a)) ((NOT b)))
; ---------------------------------------------------------------------------- ;
; ---------------------------------FIN SEMTAB--------------------------------- ;
; ---------------------------------------------------------------------------- ;
