(define (count-leaves x)
  (cond((null? x) 0)
       ((not (pair? x)) 1)
       (else(+ (count-leaves (car x)) 
               (count-leaves (cdr x))))))

(define (reserve x)
  (cond((null? (cdr x)) x)
       (else (append (reserve (cdr x))
                     (list (car x))))))

(define (deep-reserve x)
  (cond((not (pair? x)) x)
       (else
         (map (lambda (x)
                (deep-reserve x))
              (reserve x)))))