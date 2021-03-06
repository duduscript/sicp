(define (encode-symbol char tree)
  (define (exist-in-tree? char symbols)
    (cond ((null? symbols) false)
          ((eq? char (car symbols)) true)
          (else (exist-in-tree? char (cdr symbols)))))
  (define judge (lambda (c t)
                  (if (leaf? t)
                      '()
                      (if (exist-in-tree? char (symbols (left-branch t)))
                          (append (list 0) (judge c (left-branch t)))
                          (append (list 1) (judge c (right-branch t)))))))
  (if (exist-in-tree? char (symbols tree))
      (judge char tree)
      (error "bad message -- CHOOSE-BRANCH" char)))