; exercise 1.15 3D central force motion

(define ((L-3d-rect m U) local)
  (let ((v (velocity local))
        (q (coordinate local)))
    (-
     (* 1/2 m (dot-product v v))
     (U (sqrt (dot-product q q))))))

(print-expression ((L-3d-rect 'm (literal-function 'U))
                   (->local 't (up 'x 'y 'z) (up 'vx 'vy 'vz))))

(define ((F->C F) local)
  (->local (time local)
           (F local)
           (+ (((partial 0) F) local)
              (* (((partial 1) F) local)
                 (velocity local)))))

(define (spherical->rectangular local)
  (let ((spherical-tuple (coordinate local)))
    (let ((r (ref spherical-tuple 0))
          (theta (ref spherical-tuple 1))
          (phi (ref spherical-tuple 2)))
      (let ((x (* r (sin theta) (cos phi)))
            (y (* r (sin theta) (sin phi)))
            (z (* r (cos theta))))
        (up x y z)))))

; in spherical coordinates
(print-expression ((L-3d-rect 'm (literal-function 'U))
                   ((F->C spherical->rectangular)
                    (->local 't (up 'r 'theta 'phi) (up 'rdot 'thetadot 'phidot)))))
