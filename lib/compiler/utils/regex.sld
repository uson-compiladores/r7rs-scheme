(define-library (compiler utils regex)
  (import (scheme base)
	  (scheme cxr)
	  (compiler utils charset))
  (export
   ;; regex types
   regex-alphabet
   regex-charset regex-charset? regex-null?
   regex-epsilon regex-epsilon?
   regex-concat  regex-concat?  regex-concat-first regex-concat-second
   regex-kleene  regex-kleene?  regex-kleene-arg
   regex-altern  regex-altern?  regex-altern-first regex-altern-second
   regex-and     regex-and?     regex-and-first    regex-and-second
   regex-neg     regex-neg?     regex-neg-arg

   regex-similar?
   
   ;; nullability
   nullable
   nullable?

   ;; derivatives
   deriv
   deriv*

   ;; syntax
   rx

   ;; debug
   regex->list)
  (include "./regex/misc.scm")
  (include "./regex/regex.scm"))

