; exercise 1-5. Solution Process

(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))

(define (make-path t0 q0 t1 q1 qs)
  (let ((n (length qs)))
    (let ((ts (linear-interpolants t0 t1 n)))
      (Lagrange-interpolation-function
       (append (list q0) qs (list q1))
       (append (list t0) ts (list t1))))))

(define win2 (frame 0. :pi/2 0. 1.2))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1) intermediate-qs)
  (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
    ;;display path
    (graphics-clear win2)
    (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
    ;;compute action
    (Lagrangian-action Lagrangian path t0 t1)))

(define (find-path Lagrangian t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
           (multidimensional-minimize
            (parametric-path-action Lagrangian t0 q0 t1 q1)
            initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))

(find-path (L-harmonic 1.0 1.0) 0. 1. :pi/2 0. 2)
