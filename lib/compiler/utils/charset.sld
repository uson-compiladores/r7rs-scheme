(define-library (compiler utils charset)
  (import (scheme base)
	  (scheme char))
  (export
   ;; charset
   charset?
   charset-size
   charset
   charset-int-range
   charset-range

   ;; sorted container operations
   charset-contains?
   charset=?
   charset-min
   charset-add
   charset-delete

   ;; processing charsets
   charset-fold
   charset-filter

   ;; charset transformations
   charset->list
   list->charset
   charset->string
   string->charset

   ;; set operations
   charset-union
   charset-difference
   charset-intersection)
  (include "./charset/charset.scm"))
