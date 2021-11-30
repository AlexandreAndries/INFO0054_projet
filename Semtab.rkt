
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


(define (semtab formule_propositionnel)
  (cond ((null? formule_propositionnel) (display "Aucune formule propositionnel\n"))
  )
)

(semtab '())