; exercise 1.13. Central force motion in rectangular and polar coordinates

(define ((Lagrange-equations Lagrangian) q)
  (- (D (compose ((partial 2) Lagrangian) (Gamma q)))
     (compose ((partial 1) Lagrangian) (Gamma q))))

; Lagrangians in rectangular and polar coordinates

(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

(define ((L-central-polar m U) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0)) (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
           (+ (square rdot)
              (square (* r phidot))) )
         (U r)))))

; Lagrange equations in rectangular and polar coordinates

(define (L-equations-rect coordinates)
 (((Lagrange-equations
    (L-central-rectangular 'm (literal-function 'U)))
   coordinates)
  't))


(define (L-equations-polar coordinates)
 (((Lagrange-equations
    (L-central-polar 'm (literal-function 'U)))
   coordinates)
  't))

(define rect-coordinates (up (literal-function 'x) (literal-function 'y)))
(define polar-coordinates (up (literal-function 'r) (literal-function 'phi)))

(print-expression (L-equations-rect rect-coordinates))
(print-expression (L-equations-polar polar-coordinates))

;(display last-tex-string-generated)

; we will show that the lagrange equations in polar and rectangular form are equivalent.
; going from the polar to the rectangular representation.

(define (polar->rect-coordinates polar-coordinates)
  (let ((r (ref polar-coordinates 0))
        (phi (ref polar-coordinates 1)))
    (up
     (* r (cos phi))
     (* r (sin phi)))))

; what happened here?
(print-expression (L-equations-rect (polar->rect-coordinates polar-coordinates)))


(print-expression (L-equations-polar polar-coordinates))
