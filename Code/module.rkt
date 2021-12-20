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
(define test-1 '((IFTHEN p (IFTHEN q r)) (NOT (IFTHEN (IFTHEN p q) r))))
(define test-2 '((AND (OR a b) (NOT c)) (AND a b)))
(define test-3 '((IFTHEN a b) (AND (OR (NOT a) b) c)))
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
; elem, et sinon, crée deux tableau basé sur le tableau initial: un avec elem en plus,
; l'autre avec NOT elem en plus afin de créer des modèles. (ACCUMULATEUR)
;
;   précondition: liste-formule != null && liste-formule contient des formules
;                 ou conditions.
;                 elem != NULL && elem contient un élément de la Formule de base.
;
;   retourne:
;         la nouvelle liste contenant tous les modèles comprenant elem.
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
(define (subset-eqv? set1 set2)
        (equal? (list->set set1)
                (list->set set2)
        )
)
; ---------------------------------------------------------------------------- ;
;Vérifie si tous les tableaux d'un liste de tableaux sont équivalents (ou non)
;
;   précondition: ls != null
;
;   retourne:
;         #t si tous les tableaux sont équivalents dans la liste
;         #f sinon.
(define (are-subsets-eqv? ls)
        (andmap (lambda (x) (subset-eqv? x (car ls))
                )
                ls
        )
)
; ---------------------------------------------------------------------------- ;
;Renvoie une liste de tableaux sans tableaux équivalents. La liste ne contient
; que des tableaux valides, sans répétitions.
;
;   précondition: ls != null
;
;   retourne:
;         la nouvelle liste, exempte de tableaux doublons.
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
;Pour verifier si une liste de formule est satisfaisable ou pas
;
;   précondition: liste-formule != null
;
;   retourne:
;         soit #t soit #f

(define (satisfiable? liste-formule) (contient? #f (map contient-contradiction? (semtab liste-formule)))
)
; ---------------------------------------------------------------------------- ;
;Pour verifier si une formule est une tautologie (toujours faux)
;
;   précondition: formule != null
;
;
;   retourne:
;         soit #t soit #f

(define (tautology? formule) (not (contient? #f (map (lambda (x) (not (contient-contradiction? x))) (semtab (cree-liste-tableau formule)))))
)
; ---------------------------------------------------------------------------- ;
;Pour verifier si une formule est une contradiction (toujours faux)
;
;   précondition: formule != null
;
;
;   retourne:
;         soit #t soit #f


(define (contradiction? formule) (contient? #t (map (lambda (x) (contient-contradiction? x)) (semtab (cree-liste-tableau formule))))
)
; ---------------------------------------------------------------------------- ;
;Renvoie la liste des modèles (tableau sémantique) à partir d'une liste de formules
;
;   précondition: liste-formule != null
;
;   retourne:
;         la liste des modèles.
(define (models liste-formule)
        (let* ((ls (filter-not contient-contradiction? (semtab liste-formule)))
              (elem (elements ls))
              (len (length elem))
              (set (remove-duplicates (filter (lambda (x) (eqv? len (length x))) (models-aux elem ls '()))))
              )

              (if (eqv? (length set) 1)
                  set
                  (let ((clean-set (remove-eqv-subsets set '())))
                       (if (are-subsets-eqv? clean-set)
                           (car clean-set)
                           clean-set
                       )
                  )
              )

        )
)
; ---------------------------------------------------------------------------- ;
;Pour verifier et renvoier la liste des counterexamples
;
;   précondition: liste-formule != null
;                 formule       != null
;
;   retourne:
;         la liste des conterexamples

(define (conterexamples? formule list-formule)
    (let ((new-liste (append list-formule (list (mk-NOT formule)))))
         (if (satisfiable? new-liste)
            (filter-not contient-contradiction? (semtab new-liste))
            #f
        )
    )
)

; ---------------------------------------------------------------------------- ;
;pour verifier valid ou pas
;
;   précondition: liste-formule != null
;                 formule       != null
;
;   retourne:
;         soit #t soit #f
 (define (valid? formule list-formule)
      (or (subset-eqv? (models(list formule))(models list-formule)) (not(satisfiable? (append list-formule (list (mk-NOT formule)))))
)
)
; ---------------------------------------------------------------------------- ;
; ---------------------------------------------------------------------------- ;
; ---------------------------------------TEST--------------------------------- ;
; ---------------------------------------------------------------------------- ;
(tautology? test-tautology)                  ; doit donner #t
(satisfiable? test-satisfiable)              ; doit donner #t
(contradiction? test-contradiction)          ; doit donner #t
; ---------------------------------------------------------------------------- ;
(display 'test1==> )
(semtab test-1)
(display 'model_test1==> )
(models test-1)                              ; doit donner '(((NOT p) (NOT r) (NOT q))
                                             ;               ((NOT p) (NOT r) q))
(display 'test2==> )
(semtab test-2)
(display 'model_test2==> )
(models test-2)                              ; doit donner '((a b (NOT c)))

(display 'test3==> )
(semtab test-3)
(display 'model_test3==> )
(models test-3)                              ; doit donner '(b c (NOT a))

(conterexamples? 'a '((OR a (NOT a))))

;(valid? 'a '((OR a (NOT a))))

(semtab '((EQUIV a b)))
; ---------------------------------------------------------------------------- ;
; -----------------------------------FIN TEST--------------------------------- ;
; ---------------------------------------------------------------------------- ;
