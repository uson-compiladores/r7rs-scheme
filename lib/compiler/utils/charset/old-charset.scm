(define-record-type <charset>
  (%%charset-make v c l r)
  charset-node?
  (v charset-value)
  (c charset-count)
  (l charset-left)
  (r charset-right))

(define (charset-components cs)
  (values (charset-value cs)
	  (charset-left  cs)
	  (charset-right cs)))

(define charset-null '())

(define (charset-null? x)
  (null? x))

(define (charset-size cs)
  (if (charset-node? cs)
      (charset-count cs)
      0))

(define (%charset-make v l r)
  (%%charset-make v (+ 1
		       (charset-size l)
		       (charset-size r))
		  l r))

(define (charset-member? cs x)
  (if (charset-node? cs)
      (let-values (((v l r) (charset-components cs)))
	(cond ((char<? x v) (charset-member? l x))
	      ((char<? v x) (charset-member? r x))
	      (else         #true)))
      #false))

(define (charset-min cs)
  (define-values (v l r) (charset-components cs))
  (if (charset-node? l) (charset-min l) v))

(define (slr a x r)
  (define-values (b y z) (charset-components r))
  (%charset-make b (%charset-make a x y) z))

(define (dlr a x r)
  (define-values (c l z)   (charset-components r))
  (define-values (b y1 y2) (charset-components l))
  (%charset-make b
		 (%charset-make a x y1)
		 (%charset-make c y2 z)))

(define (srr b l z)
  (define-values (a x y) (charset-components l))
  (%charset-make a x (%charset-make b y z)))

(define (drr c l z)
  (define-values (a x r)   (charset-components l))
  (define-values (b y1 y2) (charset-components r))
  (%charset-make b
		 (%charset-make a x y1)
		 (%charset-make c y2 z)))

(define (charset-make v l r)
  (define ls (charset-size l))
  (define rs (charset-size r))
  (cond ((< (+ ls rs) 2)
	 (%charset-make v l r))
	((> rs (* 4 ls))
	 (let-values (((rv rl rr) (charset-components r)))
	   (if (< (charset-size rl)
		  (charset-size rr))
	       (slr v l r)
	       (dlr v l r))))
	((> ls (* 4 rs))
	 (let-values (((lv ll lr) (charset-components l)))
	   (if (< (charset-size lr)
		  (charset-size ll))
	       (srr v l r)
	       (drr v l r))))
	(else
	 (%charset-make v l r))))

(define (charset-add cs x)
  (if (charset-node? cs)
      (let-values (((v l r) (charset-components cs)))
	(cond ((char<? x v)
	       (charset-make v (charset-add l x) r))
	      ((char<? v x)
	       (charset-make v l (charset-add r x)))
	      (else
	       (%charset-make x l r))))
      (%charset-make x charset-null charset-null)))

(define (charset-delete cs x)
  (if (charset-node? cs)
      (let-values (((v l r) (charset-components cs)))
	(cond ((char<? x v)
	       (charset-make v (charset-delete l x) r))
	      ((char<? v x)
	       (charset-make v l (charset-delete r x)))
	      ((charset-null? l) r)
	      ((charset-null? r) l)
	      (else
	       (charset-make (charset-min r)
			     l
			     (charset-delete-min r)))))
      cs))

(define (charset-delete-min cs)
  (define-values (v l r) (charset-components cs))
  (if (charset-node? l)
      (charset-make v (charset-delete-min l) r)
      r))

(define (charset-fold f b cs)
  (if (charset-node? cs)
      (let-values (((v l r) (charset-components cs)))
	(charset-fold f (f v (charset-fold f b r)) l))
      b))

(define (charset->list cs)
  (charset-fold cons '() cs))

(define (list->charset lst)
  (define (fold f b lst)
    (if (null? lst) b (f (car lst) (fold f b (cdr lst)))))
  (fold (lambda (c cs) (charset-add cs c)) charset-null lst))

(define (charset->string cs)
  (apply string (charset->list cs)))

(define (string->charset str)
  (define cs charset-null)
  (string-for-each (lambda (c) (charset-add cs c)) str))

(define (charset-difference cs1 cs2)
  (if (or (charset-null? cs1) (charset-null? cs2))
      cs1
      (let-values (((v l r) (charset-components cs2)))
	(charset-concat (charset-difference (charset-split-< cs1 v)
					    l)
			(charset-difference (charset-split-> cs1 v)
					    r)))))

(define (charset-concat cs1 cs2)
  (cond ((charset-null? cs1) cs2)
	((charset-null? cs2) cs1)
	(else
	 (let-values (((v1 l1 r1) (charset-components cs1))
		      ((v2 l2 r2) (charset-components cs2)))
	   (let ((n1 (charset-size cs1))
		 (n2 (charset-size cs2)))
	     (cond ((< (* 4 n1) n2)
		    (charset-make v2 (charset-concat cs1 l2) r2))
		   ((< (* 4 n2) n1)
		    (charset-make v1 l1 (charset-concat r1 cs2)))
		   (else
		    (charset-make (charset-min cs2)
				  cs1
				  (charset-delete-min cs2)))))))))

(define (charset-intersection cs1 cs2)
  (if (or (charset-null? cs1) (charset-null? cs2))
      charset-null
      (let-values (((v l r) (charset-components cs2)))
	(let ((l* (charset-split-< cs1 v))
	      (r* (charset-split-> cs1 v)))
	  (if (charset-member? cs1 v)
	      (charset-concat3 v
			       (charset-intersection l* l)
			       (charset-intersection r* r))
	      (charset-concat (charset-intersection l* l)
			      (charset-intersection r* r)))))))

(define (charset-union cs1 cs2)
  (cond ((charset-null? cs2) cs1)
	((charset-null? cs1) cs2)
	(else
	 (let-values (((v1 l1 r1) (charset-components cs1))
		      ((v2 l2 r2) (charset-components cs2)))
	   (charset-concat3 v1
			    (charset-uni-hi l1 (charset-trim-hi v1 cs2) v1)
			    (charset-uni-lo r1 (charset-trim-lo v1 cs2) v1))))))

(define (charset-trim-hi x cs)
  (if (charset-null? cs)
      cs
      (let-values (((v l r) (charset-components cs)))
	(if (char<? v x)
	    cs
	    (charset-trim-hi x l)))))

(define (charset-trim-lo x cs)
  (if (charset-null? cs)
      cs
      (let-values (((v l r) (charset-components cs)))
	(if (char<? x v)
	    cs
	    (charset-trim-lo x r)))))

(define (charset-uni-hi cs1 cs2 x)
  (cond ((charset-null? cs2)
	 cs1)
	((charset-null? cs1)
	 (let-values (((v l r) (charset-components cs2)))
	   (charset-concat3 v l (charset-split-< r x))))
	(else
	 (let-values (((v1 l1 r1) (charset-components cs1))
		      ((v2 l2 r2) (charset-components cs2)))
	   (charset-concat3 v1
			    (charset-uni-hi l1 (charset-trim-hi v1 cs2) v1)
			    (charset-uni-bd r1 (charset-trim v1 x cs2) v1 x))))))

(define (charset-uni-lo cs1 cs2 x)
  (cond ((charset-null? cs2)
	 cs1)
	((charset-null? cs1)
	 (let-values (((v l r) (charset-components cs2)))
	   (charset-concat3 v (charset-split-> l x) r)))
	(else
	 (let-values (((v1 l1 r1) (charset-components cs1))
		      ((v2 l2 r2) (charset-components cs2)))
	   (charset-concat3 v1
			    (charset-uni-bd l1 (charset-trim x v1 cs2) x v1)
			    (charset-uni-lo r1 (charset-trim-lo v1 cs2) v1))))))

(define (charset-trim lo hi cs)
  (if (charset-null? cs)
      cs
      (let-values (((v l r) (charset-components cs)))
	(if (char<? lo v)
	    (if (char<? v hi)
		cs
		(charset-trim lo hi l))
	    (charset-trim lo hi r)))))

(define (charset-uni-bd cs1 cs2 lo hi)
  (cond ((charset-null? cs2)
	 cs1)
	((charset-null? cs1)
	 (let-values (((v l r) (charset-components cs2)))
	   (charset-concat3 v (charset-split-> l lo) (charset-split-< r hi))))
	(else
	 (let-values (((v1 l1 r1) (charset-components cs1))
		      ((v2 l2 r2) (charset-components cs2)))
	   (charset-concat3 v1
			    (charset-uni-bd l1 (charset-trim lo v1 cs2) lo v1)
			    (charset-uni-bd r1 (charset-trim v1 hi cs2) v1 hi))))))
(define (charset-split-< cs x)
  (if (charset-null? cs)
      cs
      (let-values (((v l r) (charset-components cs)))
	(cond ((char<? x v)
	       (charset-split-< l x))
	      ((char<? v x)
	       (charset-concat3 v l (charset-split-< r x)))
	      (else
	       l)))))

(define (charset-split-> cs x)
  (if (charset-null? cs)
      cs
      (let-values (((v l r) (charset-components cs)))
	(cond ((char>? x v)
	       (charset-split-> r x))
	      ((char>? v x)
	       (charset-concat3 v r (charset-split-> l x)))
	      (else
	       r)))))

(define (charset-concat3 v l r)
  (cond ((charset-null? l)
	 (charset-add r v))
	((charset-null? r)
	 (charset-add l v))
	(else
	 (let-values (((v1 l1 r1) (charset-components l))
		      ((v2 l2 r2) (charset-components r)))
	   (let ((n1 (charset-size l))
		 (n2 (charset-size r)))
	     (cond ((< (* 4 n1) n2)
		    (charset-make v2 (charset-concat3 v l l2) r2))
		   ((< (* 4 n2) n1)
		    (charset-make v1 l1 (charset-concat3 v r1 r)))
		   (else
		    (%charset-make v l r))))))))

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
  (let loop ((cs charset-null)
	     (i from))
    (if (> i to)
	cs
	(loop (charset-add cs (integer->char i))
	      (+ i 1)))))

(define (charset-range from to)
  (charset-int-range (char->integer from)
		     (char->integer to)))

(define set charset-null)
(define set (charset-add set #\a))
(define set (charset-add set #\b))
(define set (charset-add set #\c))
(define set (charset-add set #\d))
(define set (charset-add set #\e))
(define set (charset-add set #\f))

(define set2 (charset-split-< set #\d))
(define set3 (charset-split-> set #\c))


