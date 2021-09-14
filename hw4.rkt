
#lang racket

(define-syntax while-less
  (syntax-rules ()
    [(while-less e1 do e2)
     (letrec (
       [v e1]
       [f (lambda ()
            (or (>= e2 v) (f)))])
       (f)
     )]
))
