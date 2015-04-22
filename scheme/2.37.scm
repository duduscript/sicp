(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (vector)
         (dot-product vector v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (vector-m)
           (map (lambda (vector-cols)
                  (dot-product vector-m vector-cols))
                cols)) 
         m)))