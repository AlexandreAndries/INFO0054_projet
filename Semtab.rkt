
#lang racket

;  table de vérité de OR
;    ___________________
;   | p    q |  (OR p q)|
;   | #f   #f|     #f   |
;   | #f   #t|     #t   |
;   | #t   #f|     #t   |
;   | #t   #t|     #t   |

;  table de vérité de AND
;    ____________________
;   | p    q |  (AND p q)|
;   | #f   #f|     #f    |
;   | #f   #t|     #f    |
;   | #t   #f|     #f    |
;   | #t   #t|     #t    |

;  table de vérité de IFTHEN
;    _______________________
;   | p    q |  (IFTHEN p q)|
;   | #f   #f|     #t       |
;   | #f   #t|     #t       |
;   | #t   #f|     #f       |
;   | #t   #t|     #t       |

;  table de vérité de NOT
;    _____________
;   |  p | (NOT p)|
;   | #f |   #t   |
;   | #t |   #f   |
    
(define p #t)
(define q #f)
(define r #t)

;Vérifie si la liste x est un atome
;   précondition: x != null
;
;   retourne: 
;         #t si x est un atome
;         #f sinon
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (premier? verif ls)
  (if (equal? (car ls) verif) #t #f)
)



(define (OR_simpli a b) 
  (list (list a) (list b))
)

(define (AND_simpli a b)
  (list a b)
)

(define (IFTHEN_simpli a b)
  (list (list (NOT a) (list b)))
)

(define (test model_brut)
  body)

(define (semtab formule_propositionnel)
  (cond ((null? formule_propositionnel) (display "Aucune formule propositionnel\n"))
        
  )
)


