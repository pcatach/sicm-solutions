; exercise 1.17 Bead on a helical wire
; We use cylindrical coordinates.
; There is a constraint as the position of the
; bead along the x axis (parallel to the helix's
; axis) is determined by the number of turns
; that the bead has completed: x = n/h.
; But this number of turns is given by n = theta/2pi
; so that x = theta/2pi*h
; For the radius coordinate: r = d/2 (constant)
; theta is the angle from the horizontal plane.

; velocities: vx = thetadot/2pi*h
; vy = D(r*cos(theta)) = r*sin(theta)*thetadot
; vz = D(r*sin(theta)) = -r*cos(theta)*thetadot
; vx²+vy²+vz² = (thetadot/2pih)² + r²*thetadot²
(define ((T-bead m h d) local)
  (let ((thetadot (velocity local)))
    (* 1/2 m (+ (/ thetadot (* 2 :pi h)) (* 1/4 (square d) (square thetadot))))))

(define ((V-bead m g h d) local)
  (let ((theta (coordinate local)))
    (* m g (* d 1/2 (sin theta)))))

(define ((L-bead m g h d) local)
  (-
   ((T-bead m h d) local)
   ((V-bead m g h d) local)))

(print-expression ((L-bead 'm 'g 'h 'd)
                   (->local 't 'theta 'thetadot)))

(define ((Lagrange-equations Lagrangian) q)
  (- (D (compose ((partial 2) Lagrangian) (Gamma q)))
     (compose ((partial 1) Lagrangian) (Gamma q))))

(define q (literal-function 'q))

(print-expression (((Lagrange-equations (L-bead 'm 'g 'h 'd)) q) 't))

; check solution

(define (Lagrangian->acceleration L)
  (let ((P ((partial 2) L))
        (F ((partial 1) L)))
    (/ (- F
          (+ ((partial 0) P)
             (* ((partial 1) P) velocity)))
       ((partial 2) P))))

(define (Lagrangian->state-derivative L)
  (let ((acceleration (Lagrangian->acceleration L)))
    (lambda (state)
      (up 1
          (velocity state)
          (acceleration state)))))

(define (bead->state-derivative)
  (Lagrangian->state-derivative (L-bead 1 9.8 10 1)))

(define ((monitor-theta win) state)
  (let ((theta (coordinate state)))
    (plot-point win (time state) theta)))

(define plot-win (frame 0. 50. -4 4))
(graphics-clear plot-win)

((evolve bead->state-derivative)
 (up 0.2
     0.2
     0.2)
 (monitor-theta plot-win)
 0.005
 50.0
 1.0e-12)
