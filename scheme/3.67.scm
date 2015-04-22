(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (lambda ()
     (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (stream-map (lambda (x) (list x (stream-car t)))
                  (stream-cdr s))
      (pairs (stream-cdr s) (stream-cdr t))))))

(define (interleave s1 s2 s3)
  (cons-stream (stream-car s1)
               (lambda ()
                 (interleave s2 s3 (stream-cdr s1)))))

(define int-pair
  (stream-map show
              (pairs integers integers)))