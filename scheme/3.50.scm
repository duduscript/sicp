(define (streams-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply streams-map 
              (cons proc (map stream-cdr argstreams))))))