(define sign 0)
(define (f n)
  (cond ((and (= n 0) (= sign 1))
         (begin (set! sign 0)
                0))
        ((and (= n 0) (= sign 0))
         (begin (set! sign 1)
                0))
        ((and (= n 1) (= sign 0))
         (begin (set! sign 1)
                1))
        ((and (= n 1) (= sign 1))
         (begin (set! sign 0)
                0))))