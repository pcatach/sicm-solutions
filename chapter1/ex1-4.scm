; exercise 1.4 Lagrangian actions

(define ((Lagrangian m) local)
  (let ((v (velocity local)))
    (* 1/2 m (dot-product v v))))

(define (path t)
  (up (+ 'xa (* (- 'xb 'xa) (/ (- t 'ta) (- 'tb 'ta))))))

; As the velocity is constant, the value of L is constant
; integral of constant function from x1 to x2  = f(x1)*(x2-x1)

(define (Action L q t_1 t_2)
  (* (- t_2 t_1) ((compose L (Gamma q)) t_1)))

(show-expression (Lagrangian-action% (L-free-particle 'm) straight-line-path 'ta 'tb))

(display last-tex-string-generated)
