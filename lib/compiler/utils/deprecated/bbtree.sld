(define-library (utils bbtree)
  (import (scheme base)
	  (scheme write))
  (export bbtree-node?
	  bbtree-value
	  bbtree-left
	  bbtree-right
	  bbtree-null
	  bbtree-null?
	  bbtree-size

	  bbtree-member?
	  bbtree-min

	  bbtree-make
	  bbtree-add
	  bbtree-delete
	  bbtree-delete-min

	  bbtree-inorder-fold
	  bbtree-preorder-fold
	  bbtree-postorder-fold
	  members
	  bbtree-fold
	  bbtree-reverse-add
	  fold
	  list->bbtree
	  treesort

	  bbtree-union

	  tree
	  )
  (include "./bbtree/bbtree.scm"))
