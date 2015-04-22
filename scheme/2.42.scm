(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (enumerate-interval a b)
  (if (= a b)
      (list a)
      (cons a
            (enumerate-interval (+ a 1) b))))

(define (filter f seq)
  (cond((null? seq) '())
       ((f (car seq))
        (cons (car seq)
              (filter f (cdr seq))))
       (else (filter f (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (if (= k 1)
      (list new-row)
      (cons (car rest-of-queens)
            (adjoin-position new-row (- k 1) (cdr rest-of-queens)))))

(define (safe? k positions)
  (define (last n pos)
    (if (= n 1)
        (car pos)
        (last (- n 1) (cdr pos))))
  (define (equal n pos last rowplusline rowminusline)
    (if (= n 1)
        true
        (if (or (= (car pos) last) 
                (= (+ n (car pos)) rowplusline) 
                (= (- n (car pos)) rowminusline))
            false
            (equal (- n 1) (cdr pos) last rowplusline rowminusline))))
  (let((lastpos (last k positions)))
    (equal k positions lastpos (+ 1 lastpos)(- 1 lastpos))))