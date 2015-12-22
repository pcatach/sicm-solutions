; exercise 1.8. Implementation of delta

; a

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

; b

;1.23

(define eta (literal-function 'eta))
(define q (literal-function 'q))
(define (f q) (compose (literal-function 'f) q))
(define (g q) (compose (literal-function 'g) q))

(print-expression ((( (delta eta) (* f g) ) q) 't))
(print-expression (+
                   ((*
                    (((delta eta) f) q)
                    (g q)
                    ) 't)

                    ((*
                    (((delta eta) g) q)
                    (f q)
                    ) 't)
                   )

;1.24

(print-expression ((( (delta eta) (+ f g) ) q) 't))
(print-expression (+
                   ((((delta eta) f) q) 't)
                   ((((delta eta) g) q) 't)
                   ))

;1.25

(print-expression ((( (delta eta) (* 'c g) ) q) 't))
(print-expression (*
                   'c
                   ((((delta eta) g) q) 't)
                   ))

;1.26

(define F (literal-function 'F))
(define (h q)
  (compose F (g q) ))

(print-expression ((( (delta eta) h ) q) 't))
(print-expression ((*
                   (compose (D F) (g q))
                   (((delta eta) g) q)
                   ) 't))

;1.27

(define (g q) (D (f q)))
(print-expression ((D (((delta eta) f) q) ) 't) )
(print-expression ((( (delta eta) g) q) 't))
