(define (square-tree-1 tree)
  (cond((null? tree) '())
       ((pair? tree)
        (cons
          (square-tree-1 (car tree))
          (square-tree-1 (cdr tree))))
       (else (square tree))))
(define (square x) (* x x))