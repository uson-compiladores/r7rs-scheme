(import (scheme base)
	(scheme lazy))

(define stream-null '())

(define stream-cons "not yet implemented")

(define stream? "not yet implemented")

(define stream-car "not yet implemented")

(define stream-cdr "not yet implemented")

(define-syntax stream-lambda
  (syntax-rules ()
    ((stream-lambda formals body0 body1 ...)
     (lambda formals (stream-lazy (let () body0 body1 ...))))))
