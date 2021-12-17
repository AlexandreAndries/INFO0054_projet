#lang racket
; ---------------------------------------------------------------------------- ;
; -----------------------------------SEMTAB.RKT------------------------------- ;
; ---------------------------------------------------------------------------- ;
; # Auteurs : ROTHEUDT Thomas - ANDRIES Alexandre - KHALIPHI Abdelilah         ;
; # Projet  : INFO0054 - Programmation Fonctionelle                            ;
; # Date    : 11/12/2021                                                       ;
; ---------------------------------------------------------------------------- ;
(require "tableau.rkt")
(require racket/trace)
; ---------------------------------------------------------------------------- ;
; -----------------------Définition des tableaux de tests--------------------- ;
; ---------------------------------------------------------------------------- ;
(define test-semtab '(OR a (NOT a)))
(define test '(b (NOT b) c (NOT b)))
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
(provide contient-contradiction?)
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
(provide contient-operateur?)
; ---------------------------------------------------------------------------- ;
;Vérifie si il y a des opérations et renvoie une formule depuis liste-formule.
;La fonction renvoie en priorité les formules qui ne créent qu'un tableau. (AND, NOT OR, NOT  IFTHEN).
;   précondition: liste-formule != null && liste-formule contient des formules ou conditions
;
;   retourne:
;         operation si il y a une opération de trouvée
;         #f sinon (le tableau est alors ouvert/fermé)
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
(define (semtab liste-formule)
  (letrec ((semtab-aux (lambda (F acc)
                          (if (null? F)
                            (map remove-duplicates acc)
                            (let* ((tableau (car F)) (operation (cherche-elimination tableau)) (reste (cdr F)))
                              (if operation
                                (semtab-aux (append (map (lambda (x) (append x (remove operation tableau))) (elim-operation operation)) reste) acc)
                                (semtab-aux reste (append (list tableau) acc))))))))

    (trace semtab-aux)
    (semtab-aux (list liste-formule) '())
  )
)
(provide semtab)
; ---------------------------------------------------------------------------- ;
; ---------------------------------FIN SEMTAB--------------------------------- ;
; ---------------------------------------------------------------------------- ;
