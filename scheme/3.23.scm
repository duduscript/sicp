(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (null? (car deque)))

(define (front-deque deque)
  (car deque))

(define (rear-deque deque)
  (cdr deque))

(define (set-front-ptr! deque node)
  (set-car! deque node))

(define (set-rear-ptr! deque node)
  (set-cdr! deque node))

(define (prev-node node)
  (car (cdr node)))

(define (next-node node)
  (cdr (cdr node)))

(define (set-prev-node! the-node node)
  (set-car! (cdr the-node) node))

(define (set-next-node! the-node node)
  (set-cdr! (cdr the-node) node))

(define (front-insert-deque! deque value)
  (let ((new-node (cons value (cons '() '()))))
    (if (empty-deque? deque)
        (begin
          (set-front-ptr! deque new-node)
          (set-rear-ptr! deque new-node)
          deque)
        (begin
          (set-prev-node! (front-deque deque) new-node)
          (set-next-node! new-node (front-deque deque))
          (set-front-ptr! deque new-node)
          deque))))

(define (rear-insert-deque! deque value)
  (let ((new-node (cons value (cons '() '()))))
    (if (empty-deque? deque)
        (begin
          (set-front-ptr! deque new-node)
          (set-rear-ptr! deque new-node)
          deque)
        (begin
          (set-next-node! (rear-deque deque) new-node)
          (set-prev-node! new-node (rear-deque deque))
          (set-rear-ptr! deque new-node)
          deque))))

(define (front-delete-deque! deque)
  (begin
    (set-front-ptr! deque (next-node (front-deque deque)))
    (set-prev-node! (front-deque deque) '())
    deque))

(define (rear-delete-deque! deque)
  (begin
    (set-rear-ptr! deque (prev-node (rear-deque deque)))
    (set-next-node! (rear-deque deque) '())
    deque))