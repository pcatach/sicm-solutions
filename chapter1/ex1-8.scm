; exercise 1.8. Implementation of delta

(define (((delta eta) f) q)
  (define (g epsilon)
    (f (+ q (* epsilon eta))))
    ((D g) 0))

(define (f q)
  (compose (literal-function 'f (-> (UP Real Real Real) Real))
           (Gamma q)))

(define eta (literal-function 'eta))
(define q (literal-function 'q))

(print-expression ((((delta eta) f) q) 't))
