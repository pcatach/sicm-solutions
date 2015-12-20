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

(define ((V-dp g m1 m2 l1 l2) local)
  (let ((theta1 (ref (coordinate local) 0))
        (theta2 (ref (coordinate local) 1)))
    (let ((y1 (* l1 (cos theta1)))
          (y2 (+ (* l1 (cos theta1))
                 (* l2 (cos (+ theta1 theta2))))))
      (+ (* -1 g m1 y1)
         (* -1 g m2 y1))
      )))

(define ((K-dp m1 m2 l1 l2) local)
  (let ((theta1 (ref (coordinate local) 0))
        (theta2 (ref (coordinate local) 1))
        (alpha1 (ref (velocity local) 0))
        (alpha2 (ref (velocity local) 1)))
    (let ((v1x (* l1 alpha1 (cos theta1)))
          (v1y (* -1 l1 alpha1 (sin theta1)))
          (v2x (+
                (* l1 alpha1 (cos theta1))
                (* l2 (+ alpha1 alpha2) (cos (+ theta1 theta2)))))
          (v2y (+
                (* -1 l1 alpha1 (sin theta1))
                (* -1 l2 (+ alpha1 alpha2) (sin (+ theta1 theta2))))))
      (+ (* 1/2 m1 (+ (square v1x) (square v1y)))
         (* 1/2 m2 (+ (square v2x) (square v2y)))
         ))))

(define ((L-double-pendulum g m1 m2 l1 l2) local)
  (- ((V-dp g m1 m2 l1 l2) local) ((K-dp m1 m2 l1 l2) local)))

(define (double-pendulum->state-derivative)
  (Lagrangian->state-derivative (L-double-pendulum 9.8 1 3 1 .9)))

(define ((monitor-theta win) state)
  (let ((theta ((principal-value :pi) (ref (coordinate state) 1))))
    (plot-point win (time state) theta)))

(define plot-win (frame 0. 50. -4 4))
(graphics-clear plot-win)

((evolve double-pendulum->state-derivative)
 (up 0.0
     ;;(up :pi/2 (+ :pi 0.00001))
     (up :pi/2 :pi)
     (up 0 0))
 (monitor-theta plot-win)
 0.005
 50.0
 1.0e-12)



;;;;;;;;;;;;;;;;;

(print-expression
 ((doublepend-state-derivative 'g 'l1 'l2 'm1 'm2)
  (up 't
      (up 'theta1 'theta2)
      (up 'theta1dot 'theta2dot))))

(print-expression
 ((doublepend-state-derivative 9.8 1.0 0.9 1. 3.)
  (up 0.0
      (up :pi/2 :pi)
      (up 0 0))))

(print-expression
 ((state-advancer doublepend-state-derivative 9.8 1. 0.9 1. 3.)
  (up 0.0
      (up :pi/2 :pi)
      (up 0 0))
  10
  1.e-12))

(print-expression
 (((Lagrange-equations
    (L-doublepend 'g 'l1 'l2 'm1 'm2))
   (up (literal-function 'theta1)
       (literal-function 'theta2)))
  't))
