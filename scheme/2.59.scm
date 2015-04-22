(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (cdr exp)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (deriv (base exp) var)
                       (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (cddr s))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (cddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (** base exp)
  (if (= exp 0)
      1
      (* base (** base (- exp 1)))))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (exponentiation? x)
  (and (pair? x)
       (equal? (car x) '**)))

(define (make-exponentiation base exp)
  (cond ((number? base) (** base exp))
        ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(define (filter predicate seq)
  (cond((null? seq) '())
       ((predicate (car seq))
        (cons (car seq)
              (filter predicate (cdr seq))))
       (else (filter predicate (cdr seq)))))

(define (accumulate op initial seq)
  (cond((null? seq) initial)
       ((op (car seq)
            (accumulate op initial (cdr seq))))))

(define (make-sum plusnum)
  (let ((num (accumulate + 0
                         (filter number? plusnum)))
        (varlist (filter (lambda (x) (not (number? x))) plusnum)))
    (cond((null? varlist) num)
         ((and (=number? num 0) (null? (cdr varlist))) (car varlist))
         (else(cons '+ (cons num varlist))))))

(define (make-product multinum)
  (let ((num (accumulate * 1
                        (filter number? multinum)))
        (varlist (filter (lambda (x) (not (number? x))) multinum)))
    (cond((=number? num 0) 0)
         ((null? varlist) num)
         ((=number? num 1)
          (cond((null? (cdr varlist)) (car varlist))
               (else (cons '* varlist))))
         ((null? (cdr varlist)) num)
         (else (cons '*
                     (cons num varlist))))))