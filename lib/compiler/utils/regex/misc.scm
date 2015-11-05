(define (compound? x)
  (and (pair? x) (not (null? (cdr x)))))
