; exercise 1.19 two bar linkage
; the constraints are:
; |r2 - r1| = l1
; |r3 - r2| = l2

; as generalized coordinates, we use the angles phi and theta and the height h of m2
; phi is the angle between the vertical and l1
; theta is the angle between the vertical and l2

(define ((L-linkage m1 m2 m3 l1 l2 g) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (let ((h (ref q 0))
          (phi (ref q 1))
          (theta (ref q 2))
          (hdot (ref v 0))
          (phidot (ref v 1))
          (thetadot (ref v 2)))
      (-
       (+
        (* 1/2 m2 (square hdot))
        (* 1/2 m1 (+ (square hdot) (square (* phidot l1))))
        (* 1/2 m3 (+ (square hdot) (square (* thetadot l2)))))
       (+
        (* m2 g h)
        (* m1 g (- h (* l1 (cos phi))))
        (* m3 g (- h (* l2 (cos theta)))))))))

(print-expression ((L-linkage 'm1 'm2 'm3 'l1 'l2 'g)
                   (->local 't (up 'h 'phi 'theta) (up 'hdot 'phidot 'thetadot))))

(define ((Lagrange-equations Lagrangian) q)
  (- (D (compose ((partial 2) Lagrangian) (Gamma q)))
     (compose ((partial 1) Lagrangian) (Gamma q))))

(print-expression (((Lagrange-equations (L-linkage 'm1 'm2 'm3 'l1 'l2 'g))
                    (up (literal-function 'h)
                        (literal-function 'phi)
                        (literal-function 'theta))) 't))
