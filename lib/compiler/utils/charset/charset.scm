;;; RECORD DEFINITION ;
(define-record-type <charset>
  (%%charset-make v c l r)
  charset?
  (v charset-value)
  (c charset-count)
  (l charset-left)
  (r charset-right))

(define (charset-size cs)
  (if (charset? cs) (charset-count cs) 0))

(define (charset-components cs)
  (values (charset-value cs)
	  (charset-count cs)
	  (charset-left  cs)
	  (charset-right cs)))
;;; RECORD DEFINITION ;


;;; AUXILIARY ;
(define null '())

(define-syntax charset-unbox
  (syntax-rules ()
    ((charset-unbox ((x (a b c d)) ...)
		    y ...)
     (let-values (((a b c d) (charset-components x))
		  ...)
       y ...))))

(define-syntax charset-unbox*
  (syntax-rules ()
    ((charset-unbox* ((x (a b c d)) ...)
		     y ...)
     (let*-values (((a b c d) (charset-components x))
		   ...)
       y ...))))

(define (%charset-make v l r)
  (%%charset-make v
		  (+ 1 (charset-size l) (charset-size r))
		  l
		  r))
;;; AUXILIARY ;


;;; BALANCING CONSTRUCTOR ;
(define (charset-make v l r)
  (define ls (charset-size l))
  (define rs (charset-size r))
  (cond ((< (+ ls rs) 2)
	 (%charset-make v l r))
	((> rs (* 4 ls))
	 (charset-unbox ((r (rv rc rl rr)))
	  (if (< (charset-size rl)
		 (charset-size rr))
	      (slr v l r)
	      (dlr v l r))))
	((> ls (* 4 rs))
	 (charset-unbox ((l (lv lc ll lr)))
	  (if (< (charset-size lr)
		 (charset-size ll))
	      (srr v l r)
	      (drr v l r))))
	(else
	 (%charset-make v l r))))

(define (slr v l r)
  (charset-unbox ((r (rv _ rl rr)))
   (%charset-make rv (%charset-make v l rl) rr)))

(define (dlr v l r)
  (charset-unbox* ((r  (rv  rc  rl  rr))
                   (rl (rlv rlc rll rlr)))
   (%charset-make rlv
		  (%charset-make v   l   rll)
		  (%charset-make rv  rlr rr))))

(define (srr v l r)
  (charset-unbox ((l (lv _ ll lr)))
   (%charset-make lv ll (%charset-make v lr r))))

(define (drr v l r)
  (charset-unbox* ((l  (lv  lc  ll  lr))
                   (lr (lrv lrc lrl lrr)))
   (%charset-make lrv
		  (%charset-make lv  ll  lrl)
		  (%charset-make v   lrr r))))
;;; BALANCING CONSTRUCTOR ;


;;; SORTED CONTAINER OPERATIONS ;
(define (charset-contains? cs x)
  (if (charset? cs)
      (charset-unbox ((cs (v _ l r)))
       (cond ((char<? x v) (charset-contains? l x))
	     ((char<? v x) (charset-contains? r x))
	     (else         #true)))
      #false))

(define (charset=? cs1 cs2)
  (let ((lst1 (charset->list cs1))
	(lst2 (charset->list cs2)))
    (equal? lst1 lst2)))

(define (charset-min cs)
  (charset-unbox ((cs (v _ l r)))
   (if (charset? l)
       (charset-min l)
       v)))

(define (charset-add cs x)
  (if (charset? cs)
      (charset-unbox ((cs (v _ l r)))
       (cond ((char<? x v)
	      (charset-make v (charset-add l x) r))
	     ((char<? v x)
	      (charset-make v l (charset-add r x)))
	     (else
	      (%charset-make x l r))))
      (%charset-make x null null)))

(define (charset-delete cs x)
  (if (charset? cs)
      (charset-unbox ((cs (v _ l r)))
       (cond ((char<? x v)
	      (charset-make v (charset-delete l x) r))
	     ((char<? v x)
	      (charset-make v l (charset-delete r x)))
	     ((null? l) r)
	     ((null? r) l)
	     (else
	      (charset-make (charset-min r)
			    l
			    (delmin r)))))
      cs))

(define (delmin cs)
  (charset-unbox ((cs (v _ l r)))
   (if (charset? l)
       (charset-make v (delmin l) r)
       r)))
;;; SORTED CONTAINER OPERATIONS ;


;;; CONTAINER PROCESSING OPERATIONS ;
(define (charset-fold proc neut cs)
  (if (charset? cs)
      (charset-unbox ((cs (v _ l r)))
       (charset-fold proc
		     (proc v (charset-fold proc neut r))
		     l))
      neut))

(define (filter pred lst)
  (cond ((null? lst) '())
	((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
	(else (filter pred (cdr lst)))))

(define (charset-filter pred cs)
  (apply charset (filter pred (charset->list cs))))
;;; CONTAINER PROCESSING OPERATIONS ;


;;; CONTAINER TRANSFORMATIONS ;
(define (charset->list cs)
  (charset-fold cons '() cs))

(define (list->charset lst)
  (define (fold proc neut lst)
    (if (null? lst)
	neut
	(proc (car lst)
	      (fold proc neut (cdr lst)))))
  (fold (lambda (c cs)
	  (charset-add cs c))
	null
	lst))

(define (charset->string cs)
  (apply string (charset->list cs)))

(define (string->charset str)
  (define cs null)
  (string-for-each (lambda (c) (charset-add cs c))
		   str))
;;; CONTAINER TRANSFORMATIONS ;


;;; SET OPERATIONS ;
(define (charset-union cs1 cs2)
  (charset-fold (lambda (c cs)
		  (charset-add cs c))
		cs1
		cs2))

(define (charset-difference cs1 cs2)
  (charset-fold (lambda (c cs)
		  (charset-delete cs c))
		cs1
		cs2))

(define (charset-intersection cs1 cs2)
  (charset-difference cs2
		      (charset-difference cs2 cs1)))
;;; SET OPERATIONS ;


;;; CONVENIENT CONSTRUCTORS ;
(define (charset . args)
  (define (charstring x)
    (cond ((string? x) (string->list x))
	  ((char? x)   (list x))
	  (else        (list))))
  (let loop ((set '())
	     (chars (apply append (map charstring args))))
    (if (null? chars)
	set
	(loop (charset-add set (car chars))
	      (cdr chars)))))

(define (charset-int-range from to)
  (let loop ((cs null)
	     (i  from))
    (if (> i to)
	cs
	(loop (charset-add cs (integer->char i))
	      (+ i 1)))))

(define (charset-range from to)
  (charset-int-range (char->integer from)
		     (char->integer to)))
;;; CONVENIENT CONSTRUCTORS ;
