(define (f n)
  (accumulate append nil 
              (map (lambda (i)
                     (map (lambda (j)
                            (map (lambda (k)
                                   (list i j k))
                                 (remove j
                                         (remove i
                                                 (enumerate-interval 1 n)))))
                          (remove i
                                  (enumerate-interval 1 n))))
                   (enumerate-interval 1 n))))

(define (remove x seq)
  (filter (lambda (y) (not (= x y)))
          seq))