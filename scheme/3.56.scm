(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((> s1car s2car)
                  (cons-stream s2car 
                               (lambda () 
                                 (merge s1 (stream-cdr s2)))))
                 ((< s1car s2car)
                  (cons-stream s1car 
                               (lambda ()
                                 (merge (stream-cdr s1) s2))))
                 (else
                  (cons-stream s1car 
                               (lambda ()
                                 (merge (stream-cdr s1) 
                                        (stream-cdr s2))))))))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define S (cons-stream 1 
                       (lambda ()
                         (merge
                          (scale-stream S 2)
                          (merge (scale-stream S 3)
                                 (scale-stream S 5))))))

(define s (stream-map show S))