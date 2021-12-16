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
(define test-semtab '(c (NOT b) (OR (OR a b) f)))
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
(define (unique ls) 
  (if (null? ls)
    '()
    (cons (car ls) (unique (filter (lambda (x) (not (eqv? x (car ls)))) ls)))))

(unique '((a) (a)))
; ---------------------------------------------------------------------------- ;
;Elimine les opérations dites simples en un ou deux tableaux maximum
;   précondition: operation != null
;
;   retourne:
;         liste contenant les différents tableaux d'opération (1 ou 2 tableau dans la liste en fonction des cas)
(define (elim-operation operation)
  (match operation
        ((list 'AND a b) (cree-liste-tableau (ajout-tableau a b)))
        ((list 'OR a b) (cree-liste-tableau (ajout-tableau a) (ajout-tableau b)))
        ((list 'IFTHEN a b) (cree-liste-tableau (ajout-tableau (mk-NOT a)) (ajout-tableau b)))
        ((list 'NOT (list 'AND a b)) (cree-liste-tableau (ajout-tableau (mk-NOT a)) (ajout-tableau (mk-NOT b))))
        ((list 'NOT (list 'OR a b)) (cree-liste-tableau (ajout-tableau (mk-NOT a) (mk-NOT b))))
        ((list 'NOT (list 'IFTHEN a b)) (cree-liste-tableau (ajout-tableau a (mk-NOT b))))
        ((list 'NOT (list 'NOT a)) (cree-liste-tableau (ajout-tableau a)))
        ((list 'NOT a) (cree-liste-tableau (ajout-tableau(mk-NOT a))))
        (a (cree-liste-tableau (ajout-tableau a)))
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
        (if (contient? (mk-NOT prem-elem) reste-tab) #t continue)
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
(define (cherche-elimination liste-formule)
  (letrec ((meilleur-formule? (lambda (ls)
                                (if (null? ls)
                                  #f
                                  (let ((operation (car ls)) (reste (meilleur-formule? (cdr ls))))
                                    (match operation
                                      ((list 'AND a b) operation)
                                      ((list 'NOT (list 'IFTHEN a b)) operation)
                                      ((list 'NOT (list 'OR a b)) operation)
                                      ((list 'NOT (list 'NOT a)) operation)
                                      ((list 'NOT (list 'AND a b)) reste)
                                      ((list 'OR a b) reste)
                                      ((list 'IFTHEN a b) reste)
                                      (a reste)
                                    )))))

              (formule? (lambda (ls)
                          (if (null? ls)
                            #f
                            (let ((operation (car ls)) (reste (formule? (cdr ls))))
                              (match operation
                                      ((list 'AND a b) reste)
                                      ((list 'NOT (list 'IFTHEN a b)) reste)
                                      ((list 'NOT (list 'OR a b)) reste)
                                      ((list 'NOT (list 'NOT a)) reste)
                                      ((list 'NOT (list 'AND a b)) operation)
                                      ((list 'OR a b) operation)
                                      ((list 'IFTHEN a b) operation)
                                      (a reste)
                                    ))))))

    (if (meilleur-formule? liste-formule)
      (meilleur-formule? liste-formule)
      (formule? liste-formule)
    )))
; ---------------------------------------------------------------------------- ;
; -----------------------------------SEMTAB----------------------------------- ;
; ---------------------------------------------------------------------------- ;
(define (semtab-aux F acc)
  (if (null? F)
    (map unique acc)
    (let* ((tableau (car F)) (operation (cherche-elimination tableau)) (reste (cdr F)))
      (if operation
        (semtab-aux (append (map (lambda (x) (append x (remove operation tableau))) (elim-operation operation)) reste) acc)
        (semtab-aux reste (append (list tableau) acc))
      ))))

(define (semtab liste-formule)
  (semtab-aux (cree-liste-tableau liste-formule) '())
)


; ---------------------------------------------------------------------------- ;
; -------------------------------Zone de tests-------------------------------- ;
; ---------------------------------------------------------------------------- ;
(elim-operation '(NOT (AND a b)))                ; doit donner (((NOT a)) ((NOT b)))
(cherche-elimination test-semtab)                ; doit donner (NOT (OR c b))
(display "\n")
(semtab-aux (list test-semtab) '())
; ---------------------------------------------------------------------------- ;
; ---------------------------------FIN SEMTAB--------------------------------- ;
; ---------------------------------------------------------------------------- ;
