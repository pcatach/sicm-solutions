; exercise 1.12

; a)

(define ((Lagrange-equations Lagrangian) q)
  (+
   (D (D (compose ((partial 3) Lagrangian) (Gamma q 4))))
   (- (D (compose ((partial 2) Lagrangian) (Gamma q 4))))
   (compose ((partial 1) Lagrangian) (Gamma q 4))
   ))

; b)

(define ((L m k) local)
  (let ((x (coordinate local))
        (v (velocity local))
        (a (acceleration local)))
    (+
     (* -1 1/2 m x a)
     (* -1 1/2 k (square x)))
    ))

(define q (literal-function 'q))

(print-expression (((Lagrange-equations (L 'm 'k)) q) 't))
; mx'' = -kx
; it looks like a simple harmonic oscillator?

; c) that is an interesting exercise!
