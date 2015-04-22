
(define (my-expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (lambda ()
     (my-expand (remainder (* num radix) den) den radix))))