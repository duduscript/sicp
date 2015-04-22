(define (make-monitored function)
  (define times 0)
  (define dispatch
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          times
          (begin (set! times (+ times 1))
                 (function x)))))
  dispatch)