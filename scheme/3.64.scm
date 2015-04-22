(define (stream-limit stream tolerance)
  (let ((bar (stream-car stream))
        (foo (stream-car (stream-cdr stream))))
    (if (< (abs (- bar foo)) tolerance)
        bar
        (stream-limit (stream-cdr stream) tolerance))))
  
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (sqrt-stream x)
  (define (average a b) (/ (+ a b) 2))
  (define guesses
    (cons-stream 1
                 (lambda ()
                   (stream-map (lambda (guess)
                                 (average guess (/ x guess)))
                               guesses))))
  guesses)