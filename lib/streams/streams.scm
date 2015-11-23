(import (scheme base)
	(scheme lazy)
					;(compiler utils tester)
	)

(define (fast-load)
  (load "./lib/streams/streams.scm"))

;;; (streams primitive)
(define stream-null (delay '()))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons kar kdr)
     (delay (cons (delay kar) (delay kdr))))))

(define (stream? x)
  (and (promise? x) (or (stream-null? x) (stream-pair? x))))

(define (stream-null? x)
  (null? (force x)))

(define (stream-pair? x)
  (pair? (force x)))

(define (stream-car x)
  (cond ((not (stream? x)) (error "non-stream" x))
	((stream-null? x)  (error "null stream" (force x)))
	(else              (force (car (force x))))))

(define (stream-cdr x)
  (cond ((not (stream? x)) (error "non-stream" x))
	((stream-null? x)  (error "null-stream" (force x)))
	(else              (force (cdr (force x))))))

(define-syntax stream-lambda
  (syntax-rules ()
    ((stream-lambda formals body0 body1 ...)
     (lambda formals
       (delay-force
	(let () body0 body1 ...))))))

;;; (streams derived)
(define-syntax define-stream
  (syntax-rules ()
    ((define-stream (name . formal) body0 body1 ...)
     (define name (stream-lambda formal body0 body1 ...)))))

(define (list->stream lst)
  (define-stream (convert lst)
    (if (null? lst)
	stream-null
	(stream-cons (car lst)
		     (convert (cdr lst)))))
  (if (not (list? lst))
      (error "non-list argument" lst)
      (convert lst)))

(define (port->stream . port)
  (define-stream (convert p)
    (let ((c (read-char p)))
      (if (eof-object? c)
	  stream-null
	  (stream-cons c (convert p)))))
  (let ((p (if (null? port) (current-input-port) (car port))))
    (if (input-port? p)
	(convert p)
	(error "non-input-port argument" p))))

(define-syntax stream
  (syntax-rules ()
    ((stream) stream-null)
    ((stream x y ...) (stream-cons x (stream y ...)))))

(define (stream->list . args)
  (let ((n (if (= 1 (length args)) #f (car args)))
	(strm (if (= 1 (length args)) (car args) (cadr args))))
    (cond ((not (stream? strm)) (error "non-stream-argument" strm))
	  ((and n (not (integer? n))) (error "non-integer count" n))
	  ((and n (negative? n)) (error "negarive count" n))
	  (else (let loop ((n (if n n -1)) (strm strm))
		  (if (or (zero? n) (stream-null? strm))
		      '()
		      (cons (stream-car strm) (loop (- n 1) (stream-cdr strm)))))))))

(define (exists f xs . others)
  (cond ((null? xs) #f)
	((apply f (car xs) (map car others)) #t)
	(else
	 (apply exists f (cdr xs) (map cdr others)))))


(define (stream-append . strms)
  (define-stream (stream-append strms)
    (cond ((null? (cdr strms)) (car strms))
	  ((stream-null? (car strms)) (stream-append (cdr strms)))
	  (else (stream-cons (stream-car (car strms))
			     (stream-append (cons (stream-cdr (car strms)) (cdr strms)))))))
  (cond ((null? strms) stream-null)
	((exists (lambda (x) (not (stream? x))) strms)
	 (error "non-stream argument"))
	(else (stream-append strms))))

(define (stream-concat strms)
  (define-stream (stream-concat strms)
    (cond ((stream-null? strms) stream-null)
	  ((not (stream? (stream-car strms)))
	   (error "non-stream object in input stream" (stream-car strms)))
	  ((stream-null? (stream-car strms))
	   (stream-concat (stream-cdr strms)))
	  (else
	   (stream-cons (stream-car (stream-car strms))
			(stream-concat
			 (stream-cons (stream-cdr (stream-car strms))
				      (stream-cdr strms)))))))
  (if (not (stream? strms))
      (error "non-stream argument" strms)
      (stream-concat strms)))

(define-stream (stream-constant . objs)
  (cond ((null? objs) stream-null)
	((null? (cdr objs))
	 (stream-cons (car objs) (stream-constant (car objs))))
	(else
	 (stream-cons (car objs)
		      (apply stream-constant (append (cdr objs) (list (car objs))))))))
(define (stream-drop n strm)
  (define-stream (stream-drop n strm)
    (if (or (zero? n) (stream-null? strm))
	strm
	(stream-drop (- n 1) (stream-cdr strm))))
  (cond ((not (integer? n)) (error "non-integer argument" n))
	((negative? n) (error "negative argument" n))
	((not (stream? strm)) (error "non-stream argument" strm))
	(else (stream-drop n strm))))

(define (stream-drop-while pred? strm)
  (define-stream (stream-drop-while strm)
    (if (and (stream-pair? strm) (pred? (stream-car strm)))
	(stream-drop-while (stream-cdr strm))
	strm))
  (cond ((not (procedure? pred?)) (error "non-procedural argument" pred?))
	((not (stream? strm)) (error "non-stream argument" strm))
	(else (stream-drop-while strm))))

(define (stream-filter pred? strm)
  (define-stream (stream-filter strm)
    (cond ((stream-null? strm) stream-null)
	  ((pred? (stream-car strm))
	   (stream-cons (stream-car strm) (stream-filter (stream-cdr strm))))
	  (else (stream-filter (stream-cdr strm)))))
  (cond ((not (procedure? pred?)) (error "non-procedural argument" pred?))
	((not (stream? strm)) (error "non-stream argument" strm))
	(else (stream-filter strm))))

(define (stream-fold proc base strm)
  (cond ((not (procedure? proc)) (error "non-procedural argument" proc))
	((not (stream? strm)) (error "non-stream argument"))
	(else (let loop ((base base)
			 (strm strm))
		(if (stream-null? strm)
		    base
		    (loop (proc base (stream-car strm)) (stream-cdr strm)))))))

(define (stream-for-each proc .strms)
  (define (stream-for-each strms)
    (if (not (exists stream-null? strms))
	(begin (apply proc (map stream-car strms))
	       (stream-for-each (map stream-cdr strms)))))
  (cond ((not (procedure? proc)) (error "non-procedural argument" proc))
	((null? strms) (error "no stream arguments" strms))
	((exists (lambda (x) (not (stream? x))) strms)
	 (error "non-stream argument"))
	(else (stream-for-each strms))))

(define (stream-from first . step)
  (define-stream (stream-from first delta)
    (stream-cons first (stream-from (+ first delta) delta)))
  (let ((delta (if (null? step) 1 (car step))))
    (cond ((not (number? first)) (error "non-numeric starting number" first))
	  ((not (number? delta)) (error "non-numeric step size" delta))
	  (else (stream-from first delta)))))

(define (stream-iterate proc base)
  (define-stream (stream-iterate base)
    (stream-cons base (stream-iterate (proc base))))
  (if (not (procedure? proc))
      (error "non-procedural argument" proc)
      (stream-iterate base)))

(define (stream-length strm)
  (if (not (stream? strm))
      (error "non-stream argument" strm)
      (let loop ((len 0) (strm strm))
	(if (stream-null? strm)
	    len
	    (loop (+ len 1) (stream-cdr strm))))))

(define-syntax stream-let
  (syntax-rules ()
    ((stream-let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (stream-lambda (name ...) body1 body2 ...))) tag) val ...))))

(define (stream-map proc . strms)
  (define-stream (stream-map strms)
    (if (exists stream-null? strms)
	stream-null
	(stream-cons (apply proc (map stream-car strms))
		     (stream-map (map stream-cdr strms)))))
  (cond ((not (procedure? proc)) (error "non-procedural argument" proc))
	((null? strms) (error "non stream arguments" strms))
	((exists (lambda (x) (not (stream? x))) strms)
	 (error "non-stream argument"))
	(else (stream-map strms))))

(define-syntax stream-of
  (syntax-rules ()
    ((_ expr rest ...)
     (stream-of-aux expr stream-null rest ...))))

(define-syntax stream-of-aux
  (syntax-rules (in is)
    ((stream-of-aux expr base)
     (stream-cons expr base))
    ((stream-of-aux expr base (var in stream) rest ...)
     (stream-let loop ((strm stream))
		 (if (stream-null? strm)
		     base
		     (let ((var (stream-car strm)))
		       (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
    ((stream-of-aux expr base (var is exp) rest ...)
     (let ((var exp)) (stream-of-aux expr base rest ...)))
    ((stream-of-aux expr base pred? rest ...)
     (if pred? (stream-of-aux expr base rest ...) base))))

(define (stream-range first past . step)
  (define-stream (stream-range first past delta lt?)
    (if (lt? first past)
	(stream-cons first (stream-range (+ first delta) past delta lt?))
	stream-null))
  (cond ((not (number? first)) (error "non-numeric starting number" first))
	((not (number? past)) (error "non-numeric ending number" past))
	(else
	 (let ((delta (cond ((pair? step) (car step))
			    ((< first past) 1)
			    (else -1))))
	   (if (not (number? delta))
	       (error "non-numeric step size" delta)
	       (let ((lt? (if (< 0 delta) < >)))
		 (stream-range first past delta lt?)))))))

(define (stream-ref strm n)
  (cond ((not (stream? strm)) (error "non-stream argument" strm))
	((not (integer? n)) (error "non-integer argument" n))
	((negative? n) (error "negative argument" n))
	(else (let loop ((strm strm) (n n))
		(cond ((stream-null? strm) (error "beyond end of stream"))
		      ((zero? n) (stream-car strm))
		      (else (loop (stream-cdr strm (- n 1)))))))))

(define (stream-reverse strm)
  (define-stream (stream-reverse strm rev)
    (if (stream-null? strm)
	rev
	(stream-reverse (stream-cdr strm) (stream-cons (stream-car strm) rev))))
  (if (not (stream? strm))
      (error "non-stream argument" strm)
      (stream-reverse strm stream-null)))

(define (stream-scan proc base strm)
  (define-stream (stream-scan base strm)
    (if (stream-null? strm)
	(stream base)
	(stream-cons base (stream-scan (proc base (stream-car strm)) (stream-cdr strm)))))
  (cond ((not (procedure? proc)) (error "non-procedural argument" proc))
	((not (stream? strm)) (error "non-stream argument" strm))
	(else (stream-scan base strm))))

(define (stream-take n strm)
  (define-stream (stream-take n strm)
    (if (or (stream-null? strm) (zero? n))
	stream-null
	(stream-cons (stream-car strm) (stream-take (- n 1) (stream-cdr strm)))))
  (cond ((not (stream? strm)) (error "non-stream argument" strm))
	((not (integer? n)) (error "non-integer argument" n))
	((negative? n) (error "negative argument" n))
	(else (stream-take n strm))))

(define (stream-take-while pred? strm)
  (define-stream (stream-take-while strm)
    (cond ((stream-null? strm) stream-null)
	  ((pred? (stream-car strm))
	   (stream-cons (stream-car strm) (stream-take-while (stream-cdr strm))))
	  (else stream-null)))
  (cond ((not (stream? strm)) (error "non-stream argument" strm))
	((not (procedure? pred?)) (error "non-procedural argument" pred?))
	(else (stream-take-while strm))))

(define (stream-unfold mapper pred? generator base)
  (define-stream (stream-unfold base)
    (if (pred? base)
	(stream-cons (mapper base) (stream-unfold (generator base)))
	stream-null))
  (cond ((not (procedure? mapper)) (error "non-procedural mapper" mapper))
	((not (procedure? pred?)) (error "non-procedural pred?" pred?))
	((not (procedure? generator)) (error "non-procedural generator" generator))
	(else (stream-unfold base))))

(define (stream-unfolds gen seed)
  (define (len-values gen seed)
    (call-with-values
	(lambda () (gen seed))
      (lambda vs (- (length vs) 1))))
  (define-stream (unfold-result-stream gen seed)
    (call-with-values
	(lambda () (gen seed))
      (lambda (next . results)
	(stream-cons results (unfold-result-stream gen next)))))
  (define-stream (result-stream->output-stream result-stream i)
    (let ((result (list-ref (stream-car result-stream) (- i 1))))
      (cond ((pair? result) (stream-cons (car result)
					 (result-stream->output-stream (stream-cdr result-stream) i)))
	    ((not result) (result-stream->output-stream (stream-cdr result-stream) i))
	    ((null? result) stream-null)
	    (else (error "can't happen")))))
  (define (result-stream->output-streams result-stream)
    (let loop ((i (len-values gen seed)) (outputs '()))
      (if (zero? i)
	  (apply values outputs)
	  (loop (- i 1) (cons (result-stream->output-stream result-stream i) outputs)))))
  (if (not (procedure? gen))
      (error "non-procedural argument" gen)
      (result-stream->output-streams (unfold-result-stream gen seed))))

(define (stream-zip . strms)
  (define-stream (stream-zip strms)
    (if (exists stream-null? strms)
	stream-null
	(stream-cons (map stream-car strms) (stream-zip (map stream-cdr strms)))))
  (cond ((null? strms) (error "no stream arguments" strms))
	((exists (lambda (x) (not (stream? x))) strms)
	 (error "non-stream argument"))
	(else (stream-zip strms))))


(tester
 "(stream primitives) implementation SRFI-41 compliance"
 (test-define "stream construction"
	      strm123
	      (stream-cons 1 (stream-cons 2 (stream-cons 3 stream-null))))
 (test/equal "stream-car of strm123"
	     (stream-car strm123) 1)
 (test/equal "stream-car of strm123 stream-cdr"
	     (stream-car (stream-cdr strm123)) 2)
 (test/equal "lazyness of a stream and pair null distinctimon"
	     (stream-pair? (stream-cdr (stream-cons (/ 1 0) stream-null))) #f)
 (test/equal "stream recognizer correctness"
	     (stream? (list 1 2 3)) #f)
 (test-define "stream iterator"
	      iter
	      (stream-lambda (f x) (stream-cons x (iter f (f x)))))
 (test-define "all the natural numbers in a stream"
	      nats
	      (iter (lambda (x) (+ x 1)) 0))
 (test/equal "validate accessing natural numbers"
	     (stream-car (stream-cdr nats)) 1)
 (test-define "recursive stream add procedure"
	      stream-add
	      (stream-lambda (s1 s2) (stream-cons
				      (+ (stream-car s1) (stream-car s2))
				      (stream-add (stream-cdr s1)
						  (stream-cdr s2)))))
 (test-define "even numbers as a stream"
	      evens
	      (stream-add nats nats))
 (test/equal "first even number"
	     (stream-car evens) 0)
 (test/equal "second even number"
	     (stream-car (stream-cdr evens)) 2)
 (test/equal "third even number"
	     (stream-car (stream-cdr (stream-cdr evens))) 4))
