;;cria o frame

(define win (frame 0. :pi -2.0 2.0))

;; calcula a acao de uma lagrangiana

(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))

;; lagrangiana do pendulo

(define ((L-pend m l g) local)
  (let ((q (coordinate local))
	(v (velocity local)))
    (- (* 1/2 m (square (* l v))) (* m g l (cos q)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ex: (fourier 1 (list 1 2 3 4) 3 3)

(define ((fourier w n A) t)
  (if (= n 0)
      (* (list-ref A n) (sin (* w t)))
      (+ (* (list-ref A n) (sin (* (- (* 2 n) 1) w t))) ((fourier w (- n 1) A) t) )))

;; fazer uma trajetoria usando a serie de fourier com parametros w e A, onde A ´e uma lista

(define ((make-path w n A) t)
  ((fourier w (- n 1) A) t))

;; plota a trajetoria e retorna o valor da acao para uma dada lagrangiana

(define ((parametric-path-action Lagrangian t0 q0 t1 q1 w n) A)
  (let ((path (make-path w n A)))
    (graphics-clear win)
    (plot-function win path t0 t1 (/ (- t1 t0) 100))
    (Lagrangian-action Lagrangian path t0 t1)))

;; acha a trajetoria que minimiza a acao

(define (find-path Lagrangian t0 q0 t1 q1 n w)
  (let ((initial-A (list:generate n (lambda (i) -0.5))))
    (let ((minimizing-A
	   (multidimensional-minimize
	    (parametric-path-action Lagrangian t0 q0 t1 q1 w n) initial-A )))
      (print-expression minimizing-A))))

(find-path (L-pend 1. 1. 9.8) 0. 1.5 :pi 0.5 3 (* (/ 4 5) (sqrt (/ 9.8 1.))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



(define (pendulum-state-derivative m l g)
  (Lagrangian->state-derivative (L-pend m l g)))

(print-expression
 ((pendulum-state-derivative 'm 'l 'g)
  (up 't 'q 'v)))

(print-expression
 ((state-advancer pendulum-state-derivative 1. 1. 9.8)
  (up 0 1 3) 10 1.e-12))