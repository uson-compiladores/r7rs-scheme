;;; ABSTRACT SYNTAX OF REGULAR EXPRESIONS ;
;;;
;;; cs       ; charset
;;; epsilon  ; the empty string
;;; (.. r s) ; concatenation
;;; (* r)    ; kleene closure
;;; (: r s)  ; logical or (alternation)
;;; (& r s)  ; logical and
;;; (~ r)    ; complement

;;; LANGUAGE OF REGULAR EXPRESSIONS ;
;;;
;;; L[cs]       ; cs
;;; L[epsilon]  ; { epsilon }
;;; L[(.. r s)] ; { uv | u in L[r] and v in L[s] }
;;; L[(* r)]    ; { epsilon } union { (.. r (* r)) }
;;; L[(: r s)]  ; L[r] union L[s]
;;; L[(& r s)]  ; L[r] intersection L[s]
;;; L[(~ r)]    ; universe \ L[r]

(define regex-alphabet (charset-int-range 0 127))

(define (regex-charset . args)
  (apply charset args))

(define (regex-charset? x)
  (or (charset? x) (regex-null? x)))

(define (regex-null? x)
  (null? x))

(define (regex-epsilon)
  'epsil)

(define (regex-epsilon? x)
  (eq? x 'epsil))

(define (regex-concat r s)
  (define (wrap a b)
    (list '.. a b))
  (cond ((or (regex-null? r) (regex-null? s))
	 (regex-charset))
	((regex-epsilon? r) s)
	((regex-epsilon? s) r)
	((regex-concat? r)
	 (regex-concat (regex-concat-first r)
		       (regex-concat (regex-concat-second r) s)))
	(else
	 (wrap r s))))

(define (regex-concat? x)
  (and (compound? x) (eq? (car x) '..)))

(define (regex-concat-first r)
  (cadr r))

(define (regex-concat-second r)
  (caddr r))

(define (regex-string str)
  (define (recur char chars)
    (if (null? chars)
	(regex-charset char)
	(regex-concat (regex-charset char)
		      (recur (car chars) (cdr chars)))))
  (if (zero? (string-length str))
      (regex-epsilon)
      (let ((lst (string->list str)))
	(recur (car lst) (cdr lst)))))

(define (regex-kleene r)
  (define (wrap a)
    (list '* a))
  (cond ((regex-kleene? r)  r)
	((regex-epsilon? r) (regex-epsilon))
	((regex-null? r)    (regex-epsilon))
	(else
	 (wrap r))))

(define (regex-kleene? x)
  (and (compound? x) (eq? (car x) '*)))

(define (regex-kleene-arg r)
  (cadr r))

(define (regex-altern r s)
  (define (wrap a b)
    (list ': a b))
  (cond ((regex-similar? r s) r)
	((and (regex-charset? r)
	      (regex-charset? s))
	 (charset-union r s))		;MOD
	((regex-altern? r)
	 (regex-altern (regex-altern-first r)
		       (regex-altern (regex-altern-second r) s)))
	((regex-null? r) s)
	((regex-null? s) r)
	((and (regex-neg? r) (regex-null? (regex-neg-arg r)))
	 r)
	((and (regex-neg? s) (regex-null? (regex-neg-arg s)))
	 s)
	(else
	 (wrap r s))))

(define (regex-altern? x)
  (and (compound? x) (eq? (car x) ':)))

(define (regex-altern-first r)
  (cadr r))

(define (regex-altern-second r)
  (caddr r))

(define (regex-and r s)
  (define (wrap a b)
    (list '& a b))
  (cond ((regex-similar? r s) r)
	((regex-and? r)
	 (regex-and (regex-and-first r)
		    (regex-and (regex-and-second r) s)))
	((or (regex-null? r) (regex-null? s))
	 (regex-charset))
	((and (regex-neg? r) (regex-null? (regex-neg-arg r)))
	 s)
	((and (regex-neg? s) (regex-null? (regex-neg-arg s)))
	 r)
	(else
	 (wrap r s))))

(define (regex-and? x)
  (and (compound? x) (eq? (car x) '&)))

(define (regex-and-first r)
  (cadr r))

(define (regex-and-second r)
  (caddr r))

(define (regex-neg r)
  (define (wrap a)
    (list '~ a))
  (cond ((regex-neg? r) r)
	((regex-charset? r)
	 (charset-difference regex-alphabet r)) ;MOD
	(else
	 (wrap r))))

(define (regex-neg? x)
  (and (compound? x) (eq? (car x) '~)))

(define (regex-neg-arg r)
  (cadr r))

(define (regex-similar? r s)
  (cond ((and (regex-charset? r)
	      (regex-charset? s))
	 (charset=? r s))
	(else
	 (equal? r s))))

;;; NULLABILITY ;
;;;
;;; (nullable r) ; epsilon if r is nullable otherwise '()
;;;
;;; (nullable epsilon)  ==> epsilon
;;; (nullable cs)       ==> '()
;;; (nullable (.. r s)) ==> (& (nullable r) (nullable s))
;;; (nullable (: r s))  ==> (: (nullable r) (nullable s))
;;; (nullable (* r))    ==> epsilon
;;; (nullable (& r s))  ==> (& (nullable r) (nullable s))
;;; (nullable (~ r))    ==> '() if r is nullable otherwise epsilon

(define (nullable r)
  (cond ((regex-epsilon? r)
	 (regex-epsilon))
	((regex-charset? r)
	 (regex-charset))
	((regex-concat? r)
	 (regex-and (nullable (regex-concat-first r))
		    (nullable (regex-concat-second r))))
	((regex-altern? r)
	 (regex-altern (nullable (regex-altern-first r))
		       (nullable (regex-altern-second r))))
	((regex-kleene? r)
	 (regex-epsilon))
	((regex-and? r)
	 (regex-and (nullable (regex-and-first r))
		    (nullable (regex-and-second r))))
	((regex-neg? r)
	 (if (regex-epsilon? (nullable (regex-neg-arg r)))
	     (regex-charset)
	     (regex-epsilon)))
	(else
	 (error "nullable: not a regular expression" r))))

(define (nullable? r)
  (eq? (regex-epsilon) (nullable r)))

;;; DERIVATIVES ;
;;;
;;; (deriv epsilon c) ==> '()
;;; (deriv cs c) ==> epsilon if c in cs otherwise '()
;;; (deriv (.. r s) c) ==> (: (.. (deriv r c) s) (.. (nullable r) (deriv s c))
;;; (deriv (* r) c) ==> (.. (deriv r c) (* r))
;;; (deriv (: r s) c) ==> (: (deriv r c) (deriv s c))
;;; (deriv (& r s) c) ==> (& (deriv r c) (deriv s c))
;;; (deriv (~ r) c) ==> (~ (deriv r c))
;;;
;;; consecutive derivatives / derivate with respect to a string
;;;
;;; (deriv r epsilon) ==> r
;;; (deriv r str) ==> (deriv (deriv r u) a) where str is composed of chars u and a

(define (regex->list r)
  (cond ((regex-epsilon? r) 'epsil)
	((regex-charset? r) (cons 'set (charset->list r)))
	((regex-concat? r) (list '..
				 (regex->list (regex-concat-first r))
				 (regex->list (regex-concat-second r))))
	((regex-kleene? r) (list '* (regex->list (regex-kleene-arg r))))
	((regex-altern? r) (list ':
				 (regex->list (regex-altern-first r))
				 (regex->list (regex-altern-second r))))
	((regex-and? r) (list '&
			      (regex->list (regex-and-first r))
			      (regex->list (regex-and-second r))))
	((regex-neg? r) (list '~ (regex->list (regex-neg-arg r))))))

(define (deriv r c)
  (cond ((regex-epsilon? c)
	 r)
	((regex-epsilon? r)
	 (regex-charset))
	((regex-charset? r)
	 (if (charset-contains? r c)
	     (regex-epsilon)
	     (regex-charset)))
	((regex-concat? r)
	 (let ((first  (regex-concat-first r))
	       (second (regex-concat-second r)))
	   (regex-altern (regex-concat (deriv first c) second)
			 (regex-concat (nullable first) (deriv second c)))))
	((regex-kleene? r)
	 (regex-concat (deriv (regex-kleene-arg r) c)
		       (regex-kleene (regex-kleene-arg r))))
	((regex-altern? r)
	 (regex-altern (deriv (regex-altern-first r) c)
		       (deriv (regex-altern-second r) c)))
	((regex-and? r)
	 (regex-and (deriv (regex-and-first r) c)
		    (deriv (regex-and-second r) c)))
	((regex-neg? r)
	 (regex-neg (deriv (regex-neg-arg r) c)))
	(else
	 (error "deriv: not a regular expression" r))))

(define (deriv* r str)
  (let loop ((lst (string->list str))
	     (dd  (deriv r (regex-epsilon))))
    (if (null? lst)
	dd
	(loop (cdr lst) (deriv dd (car lst))))))


;;; SYNTAX ;

(define-syntax rx
  (syntax-rules (: .. * ~ & e c s)
    ((rx e)            (regex-epsilon))
    ((rx (c x ...))    (regex-charset x ...))
    ((rx (s x))        (regex-string x))
    ((rx (.. x))       (rx x))
    ((rx (.. x y ...)) (regex-concat (rx x) (rx (.. y ...))))
    ((rx (* x))        (regex-kleene (rx x)))
    ((rx (: x))        (rx x))
    ((rx (: x y ...))  (regex-altern (rx x) (rx (: y ...))))
    ((rx (& x))        (rx x))
    ((rx (& x y ...))  (regex-and (rx x) (rx (& y ...))))
    ((rx (~ x))        (regex-neg (rx x)))
    ((rx x)            x)))
