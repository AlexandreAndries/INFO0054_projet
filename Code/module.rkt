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
;Renvoie la liste des variables propositionnelles contenue dans liste-formule.
;
;   précondition: liste-formule != null && liste-formule contient des formules
;                 ou conditions.
;
;
;   retourne:
;         la nouvelle liste contenant toutes la variables propositionnelles de
;         liste-formule.
          ; ------------------------FONCTIONNE-------------------- ;
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
;Vérifie si chaque tableau de la liste contient bien la variable propositionnelle
; elem, et sinon, deux tableau basé sur le tableau initial: un avec elem en plus,
; l'autre avec NOT elem en plus afin de créer des modèles. (ACCUMULATEUR)
;
;   précondition: liste-formule != null && liste-formule contient des formules
;                 ou conditions.
;                 elem != NULL && elem contient un élément de la Formule de base.
;
;   retourne:
;         la nouvelle liste contenant tous les modèles comprenant elem.
          ; ------------------------FONCTIONNE-------------------- ;
(define (models-aux-acc elem ls acc)
        (if (null? ls)
            acc
            (let* ((head (car ls))
                   (tail (cdr ls))
                   (continue (models-aux-acc elem tail acc)))
                  (if (or (contient? elem head) (contient? (list 'NOT elem) head))
                      (cons head continue)
                      (append (list (append head (list elem)))
                              (list (append head (list (mk-NOT elem))))
                              continue)
                  )
            )
        )
)
; ---------------------------------------------------------------------------- ;
;Applique models-aux-acc à la liste ls pour chaque variable comprise dans
; elements et accumule le résultat dans output. Le résultat étant la liste
; de tous les modèles. (ACCUMULATEUR)
;
;   précondition: liste-formule != null && liste-formule contient des formules
;                 ou conditions.
;                 elem != NULL && elem contient un élément de la Formule de base.
;
;   retourne:
;         la nouvelle liste contenant tous les modèles de la formule ls
          ; ------------------------FONCTIONNE-------------------- ;
(define (models-aux elements ls output)
        (if (null? elements)
            output
            (let* ((head (car elements))
                   (tail (cdr elements))
                   (continue (models-aux tail ls output)))
                  (append (models-aux-acc head ls output) continue)
            )
        )
)
; ---------------------------------------------------------------------------- ;
;Vérifie si deux tableaux contiennent les mêmes éléments.
;
;   précondition: set1 != null && set2!= null
;
;   retourne:
;         #t si set1 et set2 ont les mêmes éléments (peu importe l'ordre).
;         #f sinon.
          ; ------------------------FONCTIONNE-------------------- ;
(define (subset-eqv? set1 set2)
        (equal? (list->set set1)
                (list->set set2)
        )
)
; ---------------------------------------------------------------------------- ;
;Renvoie une liste de tableaux sans tableaux équivalents. La liste ne contient
; que des tableaux valides, sans répétitions.
;
;   précondition: ls != null
;
;   retourne:
;         A COMPLETER
          ; ------------------------FONCTIONNE-------------------- ;
(define (remove-eqv-subsets ls acc)
        (if (null? ls)
        acc
        (let* ((head (car ls))
               (tail (cdr ls))
               (continue (remove-eqv-subsets tail acc))
              )
              (append continue (filter (lambda (x) (subset-eqv? head x)) tail))

        )
        )
)
; ---------------------------------------------------------------------------- ;
; -------------------------------FONCTIONS LOGIC------------------------------ ;
; ---------------------------------------------------------------------------- ;
; A SPECIFIER
(define (satisfiable? liste-formule) (contient? #f (map contient-contradiction? (semtab liste-formule)))
)
; ---------------------------------------------------------------------------- ;
; A SPECIFIER
(define (tautology? formule) (not (contient? #f (map (lambda (x) (not (contient-contradiction? x))) (semtab (cree-liste-tableau formule)))))
)
; ---------------------------------------------------------------------------- ;
; A SPECIFIER
(define (contradiction? formule) (contient? #t (map (lambda (x) (contient-contradiction? x)) (semtab (cree-liste-tableau formule))))
)
; ---------------------------------------------------------------------------- ;
; A SPECIFIER
(define (models liste-formule)
        (let* ((ls (filter-not contient-contradiction? (semtab liste-formule)))
              (elem (elements ls))
              (len (length elem))
              (set (remove-duplicates (filter (lambda (x) (eqv? len (length x))) (models-aux elem ls '()))))
              )

              (remove-eqv-subsets set '())
        )
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
; ---------------------------------------------------------------------------- ;

; (define test '((IFTHEN p (IFTHEN q r)) (NOT (IFTHEN (IFTHEN p q) r))))
(define test-2 '((AND (OR a b) (NOT c)) (AND a b)))

; (semtab test)
; (semtab test-2)

; (filter-not contient-contradiction? (semtab test-2))
(models-aux (elements test-2) (filter-not contient-contradiction? (semtab test-2)) '())

; (models test)
; (models test-2)
; ---------------------------------------------------------------------------- ;
; -----------------------------------FIN TEST--------------------------------- ;
; ---------------------------------------------------------------------------- ;
