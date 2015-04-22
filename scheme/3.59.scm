(define (integrate-series stream)
  (define (integrate-series-index s n)
    (cons-stream (* (/ 1 n) (stream-car s))
                 (lambda () 
                   (integrate-series-index (stream-cdr s) (+ n 1)))))
  (integrate-series-index stream 1))

(define exp-series
  (cons-stream 1 
               (lambda ()
                 (integrate-series exp-series))))

(define consine-series
  (cons-stream 1 
               (lambda ()
                 (integrate-series sine-series))))

(define sine-series
  (cons-stream 0
               (lambda ()
                 (integrate-series (scale-stream consine-series -1)))))

(define x (stream-map show consine-series))
(define y (stream-map show sine-series))
(define z (stream-map show exp-series))
