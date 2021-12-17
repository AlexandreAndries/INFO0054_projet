#lang racket
(require "Semtab.rkt")


(define test-semtab '((OR a (NOT a))))

(define (satisfiable? formule) (contient? #f (map contient-contradiction? (elim-operation formule))))

(define (tautology? formule) (not (contient? #f (map (lambda (x) (not (contient-contradiction? x))) (elim-operation formule)))))

(define (contradiction? formule) (contient? #t (map (lambda (x) (contient-contradiction? x)) (elim-operation formule))))

(contradiction? '(AND a (NOT a)))
