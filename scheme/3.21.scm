(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (car queue))

(define (rear-queue queue)
  (cdr queue))

(define (set-front-ptr! queue value)
  (set-car! queue value))

(define (set-rear-ptr! queue value)
  (set-cdr! queue value))

(define (empty? queue)
  (null? (front-queue queue)))

(define (insert-queue! queue value)
  (let ((new-node (cons value '())))
    (if (empty? queue)
        (begin
          (set-front-ptr! queue new-node)
          (set-rear-ptr! queue new-node)
          queue)
        (begin
          (set-cdr! (rear-queue queue) new-node)
          (set-rear-ptr! queue new-node)
          queue))))

(define (delete-queue! queue)
  (if (empty? queue)
      (error "Empty queue!")
      (begin
        (set-front-ptr! queue (cdr (front-queue queue)))
        queue)))

(define (print-queue queue)
  (front-queue queue))