;; exercise 1.31, velocity transformation
;; this function takes a function f-bar that depends on the path and returns
;; a function that depends on the local tuple
;; obs: osculating-path takes a number of local components and returns a path
;; with these components
(define ((Gamma-bar f-bar) local)
  ((f-bar (osculating-path local)) (time local)))

;; we use the above function to create a F->C (coordinate transformation to tuple transformation)
(define (F->C F)
  (define (f-bar q-prime)
    (define q
      (compose F (Gamma q-prime)))
    (Gamma q))
  (Gamma-bar f-bar))

;; example
(show-expression
  ((F->C p->r)
   (->local 't (up 'r 'theta) (up 'rdot 'thetadot))))

(define (F->C F)
  (define (f-bar q-prime)
    (define q
      (compose F (Gamma q-prime)))
    (ref (Gamma q) 2))
  (Gamma-bar f-bar))

(show-expression
  ((F->C p->r)
   (->local 't (up 'r 'theta) (up 'rdot 'thetadot))))
