(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (+ x1 x2)
  (lambda (f)
    (lambda (x)
      (lambda (kk)
        (lambda (ff)
          (lambda (xx)
            ((x1 ff) xx)))
        (lambda (fff)
          (lambda (xxx)
            (x2 fff) xxx))))))