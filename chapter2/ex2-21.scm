(define ((qp->H-state-path q p) t)
  (up t (q t) (p t)))

(define ((phase-state-derivative Hamiltonian) H-state)
  (up 1
      (((partial 2) Hamiltonian) H-state)
      (- (((partial 1) Hamiltonian) H-state))))

(define ((Hamilton-equations Hamiltonian) q p)
  (let ((H-state-path (qp->H-state-path q p)))
    (- (D H-state-path)
       (compose (phase-state-derivative Hamiltonian)
                H-state-path))))

(print-expression
 (((Hamilton-equations (H-spin-orbit 'R 'eps 'e 'n 'a 'C)) (literal-function 'q) (literal-function 'p))
  't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((T-spin-orbit C) local)
  (let ((p_theta (momentum local)))
    (* 1/2 (/ 1 C) (square p_theta))))

(define ((V-spin-orbit R eps e n a C) local)
  (let ((t (time local))
        (theta (coordinate local)))
    (* -1/4 (square n) (square eps) (cube a) C (/ 1 (cube R)) (+ (cos (* 2 (- theta (* n t)))) (* 7/2 e (cos (- (* 2 theta) (* 3 n t)))) (* -1/2 e (cos (- (* 2 theta) (* n t))))))))

;; e = sqrt(3(B-A)/C) out of roundness

(define ((H-spin-orbit R eps e n a C) local)
  ((-
   (T-spin-orbit C)
   (V-spin-orbit R eps e n a C)) local))

(define (H-spin-orbit-sysder R eps e n a C)
  (phase-state-derivative
   (H-spin-orbit R eps e n a C)))

(define ((monitor-p-theta win) state)
  (let ((t (time state))
        (q (coordinate state))
        (p (momentum state)))
    (plot-point win t (- q t))))

(define win (frame 0 50 -1 1))

(graphics-clear win)

(let ((R 1.)
      (eps 0.1)
      (e 0.5)
      (n 1.)
      (a 1.)
      (C 1.))
  ((evolve H-spin-orbit-sysder R eps e n a C)
   (up 0.0 0.0 1.01)
   (monitor-p-theta win)
   0.01 50.0 1.0e-12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
