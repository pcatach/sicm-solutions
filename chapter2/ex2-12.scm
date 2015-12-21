; exercise 2-1. Nutation of the top

(define ((Lagrange-equations L) q)
  (-
   (D (compose ((partial 2) L) (Gamma q)))
   (compose ((partial 1) L) (Gamma q))
   )
  )

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

(define (euler-state->omega-body local)
  (let ((q (coordinate local)) (qdot (velocity local)))
    (let ((theta (ref q 0))
          (psi (ref q 2))
          (thetadot (ref qdot 0))
          (phidot (ref qdot 1))
          (psidot (ref qdot 2)))
      (let ((omega-a (+ (* thetadot (cos psi))
                        (* phidot (sin theta) (sin psi))))
            (omega-b (+ (* -1 thetadot (sin psi))
                        (* phidot (sin theta) (cos psi))))
            (omega-c (+ (*  phidot (cos theta)) psidot)))
        (column-matrix omega-a omega-b omega-c)))))

(define ((T-rigid-body A B C) local)
  (let ((omega-body (euler-state->omega-body local)))
    (* 1/2
       (+ (* A (square (ref omega-body 0)))
          (* B (square (ref omega-body 1)))
          (* C (square (ref omega-body 2)))))))

(define ((V-rigid-body gMR) local)
  (let ((theta (ref (coordinate local) 0)))
    (* gMR (cos theta))))

(define ((L-top gMR A B C) local)
  (- ((T-rigid-body A B C) local) ((V-rigid-body gMR) local)))

(define (top->state-derivative)
  (Lagrangian->state-derivative (L-top 0.0456 3.28e-4 3.28e-4 6.6e-5 )))

(define ((monitor-theta win) state)
  ;;(let ((theta ((principal-value :pi) (ref (coordinate state) 0))))
  (let ((theta (ref (coordinate state) 0)))
    ;;(plot-point win (ref (coordinate state) 1) (- :pi theta))))
    (plot-point win (time state) (- :pi theta))))

(define plot-win (frame 0. 2. 0. :pi))
(graphics-clear plot-win)

((evolve top->state-derivative)
 (up 0.0
     (up 0.1 0. 0. )
     (up 0. -15. 140.))
 (monitor-theta plot-win)
 0.001
 50.0
 1.0e-12)
