(define-stream (file->scanner filename)
  (let ((p (open-input-file filename)))
    (stream-let loop ((c (read-char p)))
      (if (eof-object? c)
          (begin (close-input-port p)
                 stream-null)
          (stream-cons c
		       (loop (read-char p)))))))

(define-stream (string->scanner str)
  (let ((p (open-input-string str)))
    (stream-let loop ((c (read-char p)))
      (if (eof-object? c)
	  (begin (close-input-port p)
		 stream-null)
	  (stream-cons c
		       (loop (read-char p)))))))

(define (scanner-read strm)
  (stream-car strm))

(define (scanner-next strm)
  (stream-cdr strm))
