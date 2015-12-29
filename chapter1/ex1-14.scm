; exercise 1.14. Lagrangians and coordinate transformations

(define ((F->C F) local)
  (->local (time local)
           (F local)
           (+ (((partial 0) F) local)
              (* (((partial 1) F) local)
                 (velocity local)))))

(define (p->r local)
  (let ((polar-tuple (coordinate local)))
    (let ((r (ref polar-tuple 0))
          (phi (ref polar-tuple 1)))
      (let ((x (* r (cos phi)))
            (y (* r (sin phi))))
        (up x y)))))

(print-expression
 (velocity
  ((F->C p->r)
   (->local 't (up 'r 'phi) (up 'rdot 'phidot)))))

(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

(define (L-central-polar m U)
  (compose (L-central-rectangular m U) (F->C p->r)))

(print-expression
  ((L-central-polar 'm (literal-function 'U))
   (->local 't (up 'r 'phi) (up 'rdot 'phidot))))

;;;;;;;;; without cordinate transformation

(define ((L-central-polar m U) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0)) (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
           (+ (square rdot)
              (square (* r phidot))) )
         (U r)))))

(print-expression
  ((L-central-polar 'm (literal-function 'U))
   (->local 't (up 'r 'phi) (up 'rdot 'phidot))))
