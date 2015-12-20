(define ((T-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local))
        (thetadot (velocity local)))
    (let ((vys (D ys)))
      (* 1/2 m
         (+ (square (* l thetadot))
            (square (vys t))
            (* 2 l (vys t) thetadot (sin theta)))))))

(define ((V-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local)))
    (* m g (- (ys t) (* l (cos theta))))))

(define L-pend (- T-pend V-pend))

(define ((periodic-drive a omega phase) t)
  (* a (cos (+ (* omega t) phase))))

(define (L-periodically-driven-pendulum m l g a omega)
  (let ((ys (periodic-drive a omega 0)))
    (L-pend m l g ys)))

(define (Legendre-transform F)
  (let ((w-of-v (D F)))
    (define (G w)
      (let ((z (dual-zero w)))
        (let ((M ((D w-of-v) z))
              (b (w-of-v z)))
          (let ((v (/ (- w b) M)))
            (- (* w v) (F v))))))
    G))

(define ((Lagrangian->Hamiltonian Lagrangian) H-state)
  (let ((t (time H-state))
        (q (coordinate H-state))
        (p (momentum H-state)))
    (define (L qdot)
      (Lagrangian (up t q qdot)))
    ((Legendre-transform L) p)))

(print-expression
 ((Lagrangian->Hamiltonian
   (L-periodically-driven-pendulum 'm 'l 'g 'a 'omega))
  (up 't 'theta 'p_theta)))

(define ((phase-state-derivative Hamiltonian) H-state)
  (up 1
      (((partial 2) Hamiltonian) H-state)
      (- (((partial 1) Hamiltonian) H-state))))

(define (H-pend-sysder m l g a omega)
  (phase-state-derivative
   (Lagrangian->Hamiltonian
    (L-periodically-driven-pendulum m l g a omega))))

(define ((monitor-p-theta win) state)
  (let ((t (time state))
        (q ((principal-value :pi) (coordinate state)))
        (p (momentum state)))
    (plot-point win q p)))

(define win (frame :-pi :pi -15.0 15.0))

(graphics-clear win)

(let ((m 1.)
      (l 9.8)
      (g 9.8)
      (A 0.1)
      (omega 1.))
  ((evolve H-pend-sysder m l g A omega)
   (up 0.0
       1.0
       0.0)
   (monitor-p-theta win)
   0.01 100.0 1.0e-12))

(define (driven-pendulum-map m l g A omega)
  (let ((advance (state-advancer H-pend-sysder m l g A omega))
        (map-period (/ :2pi omega)))
    (lambda (theta ptheta return fail)
      (let ((ns (advance
                 (up 0 theta ptheta)  ; initial state
                 map-period)))        ; integration interval
        (return ((principal-value :pi) (coordinate ns))
                (momentum ns))))))

(define win2 (frame :-pi :pi -20 20))
(graphics-clear win2)

(let ((m 1.)
      (l 9.8)
      (g 9.8)
      (A 0.006985)
      (omega 40))
  (explore-map
   win2
   (driven-pendulum-map m l g A omega)
   1000))

;; inverted equilibrium: omega = 300 a omega = 100
;; chaotic regions connected: omega ~ 40, A = 0.007
