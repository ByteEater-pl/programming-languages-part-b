; my attempts at eliminating code repetition in a pattern
; cf. https://stackoverflow.com/questions/69146394/how-to-avoid-code-repetition-in-patterns

#lang racket

(struct my-struct (field1 field2 field3) #:transparent)

(define s (my-struct '(3 4) '(6 5) '(7 8)))

(match s [(my-struct `(,x ,y)
                    (app (curryr sort <) `(,lo1 ,hi1))
                    (app (curryr sort <) `(,lo2 ,hi2)))
          (- (* x lo1 lo2) (* y hi1 hi2) (expt lo1 hi2))])

(match s [(app struct->vector
               (vector 'struct:my-struct p (app (curryr sort <) `(,lo ,hi)) ...))
; The following line is long and the computation artificial, but just for the sake of an example.
; However, imagine my-struct having more fields.
          (apply - `(,@(map (curry apply *) (map cons p `(,lo ,hi))) ,(expt (car lo) (cadr hi))))])

(match s [(app (compose vector->list struct->vector)
               (list 'struct:my-struct p (app (curryr sort <) `(,lo ,hi)) ...))
          (apply - `(,@(map (curry apply *) (map cons p `(,lo ,hi))) ,(expt (car lo) (cadr hi))))])

(require racket/struct) ; provides struct->list

(match s [(? my-struct?
             (app struct->list
                  (list p (app (curryr sort <) `(,lo ,hi)) ...)))
          (apply - `(,@(map (curry apply *) (map cons p `(,lo ,hi))) ,(expt (car lo) (cadr hi))))])
