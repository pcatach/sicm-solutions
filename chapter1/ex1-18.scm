; exercise 1.18 Bead on a triaxial surface
; We use spherical coordinates.
; There is a constraint as the position of the
; bead satisfies the equation:
; x²/a² + y²/b² + z²/c² = 1

; parametrization in spherical coordinates:
; x = a*sin(phi)*cos(theta)
; y = b*sin(phi)*sin(theta)
; z = c*cos(phi)

; free bead
(define ((L-bead m) local)
  (let ((v (velocity local)))
    (* 1/2 m (dot-product v v))))

; transform local tuples from spherical to rectangular coordinates
; so that we can calculate the lagrangian as in the procedura L-bead
(define ((F->C F) local)
  (->local (time local)
           (F local)
           (+ (((partial 0) F) local)
              (* (((partial 1) F) local)
                 (velocity local)))))

(define (spherical->rectangular local)
  (let ((spherical-tuple (coordinate local)))
    (let ((theta (ref spherical-tuple 1))
          (phi (ref spherical-tuple 2)))
      (let ((x (* 'a (sin theta) (cos phi)))
            (y (* 'b (sin theta) (sin phi)))
            (z (* 'c (cos theta))))
        (up x y z)))))

(print-expression ((L-bead 'm)
                   ((F->C spherical->rectangular)
                    (->local 't (up 'r 'theta 'phi) (up 'rdot 'thetadot 'phidot)))))

(define ((Lagrange-equations Lagrangian) q)
  (- (D (compose ((partial 2) Lagrangian) (Gamma q)))
     (compose ((partial 1) Lagrangian) (Gamma q))))

(define q (literal-function 'q))

(print-expression (((Lagrange-equations (L-bead 'm)) q) 't))
