;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ("CONDITIONS" :USE "LISP" :SHADOW ("BREAK" "ERROR" "CERROR" "WARN" "CHECK-TYPE" "ASSERT" "ETYPECASE" "CTYPECASE" "ECASE" "CCASE")); Base: 10 -*-
; From arisia.xerox.com:/cl/conditions/cond18.lisp
;;;
;;; CONDITIONS
;;;
;;; This is a sample implementation. It is not in any way intended as the definition
;;; of any aspect of the condition system. It is simply an existence proof that the
;;; condition system can be implemented.
;;;
;;; While this written to be "portable", this is not a portable condition system
;;; in that loading this file will not redefine your condition system. Loading this
;;; file will define a bunch of functions which work like a condition system. Redefining
;;; existing condition systems is beyond the goal of this implementation attempt.

(make-package :conditions :use '(:lisp))
(in-package :conditions :use '(:lisp))


(import '(si::*handler-clusters* si::unique-id si::condition-class-p si::make-condition))

(export '(handler-case handler-bind ignore-errors define-condition make-condition
	  condition warning serious-condition simple-condition-format-control simple-condition-format-arguments
	  storage-condition stack-overflow storage-exhausted type-error
	  type-error-datum type-error-expected-type simple-type-error
	  program-error control-error stream-error stream-error-stream
	  end-of-file file-error file-error-pathname cell-error
	  unbound-variable undefined-function arithmetic-error
	  arithmetic-error-operation arithmetic-error-operands
	  package-error package-error-package
	  division-by-zero floating-point-overflow floating-point-underflow))

(defvar *this-package* (find-package :conditions))


