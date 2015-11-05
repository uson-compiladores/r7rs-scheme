(define (scanner-make port)
  (define (scan c)
    (if (eof-object? c)
	(delay (cons c '()))
	(delay (cons c (scan (read-char port))))))
  (scan (read-char port)))

(define (scanner-read scanner)
  (car (force scanner)))

(define (scanner-next scanner)
  (cdr (force scanner)))

(define (scanner->list scanner)
  (define c (scanner-read scanner))
  (if (eof-object? c)
      '()
      (cons c (scanner->list (scanner-next scanner)))))
