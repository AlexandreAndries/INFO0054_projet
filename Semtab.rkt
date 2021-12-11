
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

(define op '(IFTHEN a b))

(define (premier? bool operation) (if (eqv? bool (car operation)) #t #f))


(define (elimination-ope operation)
  (if (not (atom? (cadr operation)))
    ;consequent
    (let* ((sous-op (cadr operation)) (prem-elem (cadr sous-op)))
      (if (premier? 'NOT sous-op)
        ;consequent
        (list prem-elem)
        ;else
        (let ((sec-elem (caddr sous-op)))
          (cond
            ((premier? 'AND sous-op) (list (list 'NOT prem-elem) (list 'NOT sec-elem)))
            ((premier? 'OR sous-op) (list (list (list 'NOT prem-elem) (list 'NOT sec-elem))))
            ((premier? 'IFTHEN sous-op) (list prem-elem (list 'NOT sec-elem)))
          ))))
    ;else
    (let ((prem-elem (cadr operation)))
      (if (premier? 'NOT operation)
        ;consequent
        (list (list 'NOT prem-elem))
        ;else
        (let ((sec-elem (caddr operation)))
          (cond 
            ((premier? 'AND operation) (list (list prem-elem sec-elem)))
            ((premier? 'OR operation) (list (list prem-elem) (list sec-elem)))
            ((premier? 'IFTHEN operation) (list (list (list 'NOT prem-elem)) (list sec-elem)))
          ))))))

(elimination-ope op)