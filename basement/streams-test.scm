(import (scheme base)
	(scheme load))

(load "./streams.scm")

(tester
 "SRFI-41 compliance"

 (test-define "A stream with the first three natural numbers"
	      strm123
	      (stream-cons 1 (stream-cons 2 (stream-cons 3 stream-null))))

 (test/equal ""
	     (stream-car strm123) 1)

 (test/equal ""
	     (stream-car (stream-cdr strm123) 2))

 (test/equal ""
	     (stream-pair? (stream-cdr (stream-cons (/ 1 0) stream-null))) #f)

 (test/equal ""
	     (stream? (list 1 2 3)) #f)

 (test-define ""
	      iter
	      (stream-lambda (f x)
			     (stream-cons x (iter f (f x)))))

 (test-define ""
	      nats
	      (iter (lambda (x) (+ x 1)) 0))

 (test/equal ""
	     (stream-car (stream-cdr nats)) 1)

 (test-define ""
	      stream-add
	      (stream-lambda (s1 s2)
			     (stream-cons
			      (+ (stream-car s1) (stream-car s2))
			      (stream-add (stream-cdr s1)
					  (stream-cdr s2)))))

 (test-define ""
	      evens
	      (stream-add nats nats))

 (test/equal ""
	     (stream-car evens) 0)

 (test/equal ""
	     (stream-car (stream-cdr evens)) 2)

 (test/equal ""
	     (stream-car (stream-cdr (stream-cdr evens))) 4))
