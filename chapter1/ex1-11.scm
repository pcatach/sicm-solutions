; exercise 1.11.
; Compute lagrange's equations for:
; a) particle of mass in potential V(x, y) = (x² + y²)/2 + x² y - y³/3,
; b) planar pendulum with lagrangian  L = (1/2) m l² v² + m g l cosx
; c) particle constrained to move on a sphere of radius R, L(t;x,y;α,ß) = (1/2) m R² (α² + (ß sinx )²). The angle  is the colatitude of the particle and  is the longitude; the rate of change of the colatitude is α and the rate of change of the longitude is ß.

(define ((Lagrange-equations Lagrangian) q)
  (- (D (compose ((partial 2) Lagrangian) (Gamma q)))
     (compose ((partial 1) Lagrangian) (Gamma q))))

; a)

(define ((L-particle m) local)
  (let ((q (coordinate local)) (v (velocity local)))
    (let ((x (ref q 0)) (y (ref q 1))
          (vx (ref v 0)) (vy (ref v 1)))
      (-
       (* 1/2 m (+ (square vx) (square vy)))
       (-
        (+ (/ (+ (square x) (square y)) 2) (* (square x) y))
        (/ (cube y) 3)))
      )))

(define q (literal-function 'q (-> Real (UP Real Real))))

(print-expression
 (((Lagrange-equations (L-particle 'm))
   q)
  't))

; b)

(define ((L-planar-pendulum m l g) local)
  (let ((q (coordinate local)) (v (velocity local)))
    (+
     (* 1/2 m (square l) (square v))
     (* m g l (cos q)))
      ))

(define q2 (literal-function 'theta))

(print-expression
 (((Lagrange-equations (L-planar-pendulum 'm 'l 'g))
   q2)
  't))

; c)

(define ((L-sphere m R) local)
  (let ((q (coordinate local)) (v (velocity local)))
    (let ((theta (ref q 0)) (phi (ref q 1))
          (alpha (ref v 0)) (beta (ref v 1)))
      (* 1/2 m (square R) (+ (square alpha) (square (* beta (sin theta)))))
      )))

(define q3 (literal-function 'q (-> Real (UP Real Real))))

(print-expression
 (((Lagrange-equations (L-sphere 'm 'R))
   q3)
  't))
