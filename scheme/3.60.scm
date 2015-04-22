(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (lambda ()
                 (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                              (mul-series s1 (stream-cdr s2))))))

(define w 
  (stream-map show 
              (add-streams (mul-series consine-series
                                       consine-series)
                           (mul-series sine-series 
                                       sine-series))))