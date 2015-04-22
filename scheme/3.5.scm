(define (estuimate-integral trails)
  (* 4 (monte-carlo trails experiment)))

(define (monte-carlo trails experiment)
  (define (iter trails-remaining trails-passed)
    (cond ((= trails-remaining 0)
           (/ trails-passed trails))
          ((experiment)
           (iter (- trails-remaining 1) (+ trails-passed 1)))
          (else
           (iter (- trails-remaining 1) trails-passed))))
  (iter trails 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (experiment)
  (define (get-number)
    (let ((times 10000)
          (x1 2)
          (x2 8)
          (y1 4)
          (y2 10))
      (cons (/ (random-in-range (* x1 times) (* x2 times)) times)
            (/ (random-in-range (* y1 times) (* y2 times)) times))))
  (define (is-in-range? number)
    (define (square x) (* x x))
    (let ((x (- (car number) 5))
          (y (- (cdr number) 7)))
      (let ((square-x (square x))
            (square-y (square y)))
        (<= (+ square-x square-y) (square 3)))))
  (is-in-range? (get-number)))