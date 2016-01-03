; exercise 1.20 Sliding pendulum
; Our generalized coordinates will be the position of m1 along the x axis
; and the angle theta between the vertical and the pendulum.

(define ((T-sliding m1 m2 l) local)
  (let ((qdot (velocity local)))
    (let ((xdot (ref qdot 0))
          (thetadot (ref qdot 1)))
      (+
       (* 1/2 m1 (square xdot))
       (* 1/2 m2 (square (+ xdot (* thetadot l))))))))

(define ((V-sliding m2 g l ) local)
  (let ((theta (ref (coordinate local) 1)))
    (* m2 g (* l (cos theta)))))

(define ((L-sliding m1 m2 g l) local)
  (-
   ((T-sliding m1 m2 l) local)
   ((V-sliding m2 g l) local)))

(print-expression ((L-sliding 'm1 'm2 'g 'l)
                   (->local 't (up 'x 'theta) (up 'xdot 'thetadot))))

(define ((Lagrange-equations Lagrangian) q)
  (- (D (compose ((partial 2) Lagrangian) (Gamma q)))
     (compose ((partial 1) Lagrangian) (Gamma q))))

(print-expression (((Lagrange-equations (L-sliding 'm1 'm2 'g 'l))
                    (up (literal-function 'x)
                        (literal-function 'theta))) 't))
