(define stream-null (delay '()))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons kar kdr)
     (delay (cons (delay kar) (delay kdr))))))

(define (stream? x)
  (and (promise? x) (or (stream-null? x) (stream-pair? x))))

(define (stream-null? x)
  (and (promise? x) (null? (force x))))

(define (stream-pair? x)
  (pair? (force x)))

(define (stream-car x)
  (cond ((not (stream? x)) (error "stream-car: non-stream" x))
	((stream-null? x)  (error "stream-car: null stream" (force x)))
	(else              (force (car (force x))))))

(define (stream-cdr x)
  (cond ((not (stream? x)) (error "stream-cdr: non-stream" x))
	((stream-null? x)  (error "stream-cdr: null-stream" (force x)))
	(else              (force (cdr (force x))))))

(define-syntax stream-lambda
  (syntax-rules ()
    ((stream-lambda formals body0 body1 ...)
     (lambda formals
       (delay-force
	(let () body0 body1 ...))))))
