(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (car v1)
                (car v2))
             (+ (cdr v1)
                (cdr v2))))

(define (sub-vect v1 v2)
  (make-vect (- (car v1)
                (car v2))
             (- (cdr v1)
                (cdr v2))))

(define (scale-vect s v)
  (make-vect (* s (car v))
             (* s (cdr v))))