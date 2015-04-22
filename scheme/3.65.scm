(define (intwo-summands n)
    (cons-stream (/ 1.0 n)
                 (lambda ()
                   (stream-map - (intwo-summands (+ n 1))))))

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (lambda ()
                 (stream-map (lambda (x)
                               (+  (stream-car stream) x))
                             (partial-sums (stream-cdr stream))))))
(define intwo-stream-1
  (partial-sums (intwo-summands 1)))

(define (euler-transform s)
  (define (square x) (* x x))
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (lambda () (euler-transform (stream-cdr s))))))

(define intwo-stream-2
  (euler-transform intwo-stream-1))

(define (make-tableau transform s)
  (cons-stream s
               (lambda ()
                 (make-tableau transform
                               (transform s)))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define intwo-stream-3
  (accelerated-sequence euler-transform intwo-stream-1))

(define a (stream-map show intwo-stream-1))
(define b (stream-map show intwo-stream-2))
(define c (stream-map show intwo-stream-3))