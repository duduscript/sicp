(define (make-accumulator number)
  (define (plus n)
    (set! number (+ number n))
    number)
  (define dispatch
    (lambda (number)
      (plus number)))
  dispatch)