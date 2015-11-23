(define-library (streams derived)
  (import (scheme base)
	  (scheme lazy)
	  (streams primitive))
  (export define-stream
	  list->stream
	  port->stream
	  stream
	  stream->list
	  stream-append
	  stream-concat
	  stream-constant
	  stream-drop
	  stream-drop-while
	  stream-filter
	  stream-fold
	  stream-for-each
	  stream-from
	  stream-iterate
	  stream-length
	  stream-let
	  stream-map
	  stream-of
	  stream-range
	  stream-ref
	  stream-reverse
	  stream-scan
	  stream-take
	  stream-take-while
	  stream-unfold
	  stream-unfolds
	  stream-zip)
  (include "./derived/derived.scm"))
