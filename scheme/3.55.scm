(define (partial-sums s)
  (cons-stream (stream-car s)
               (lambda ()
                 (add-streams (partial-sums s)
                              (stream-cdr s)))))