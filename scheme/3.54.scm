(define factorials
  (cons-stream 1 (lambda ()
                   (mul-streams integers factorials))))