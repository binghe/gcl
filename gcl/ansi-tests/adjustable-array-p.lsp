;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 20 21:25:22 2003
;;;; Contains: Tests for ADJUSTABLE-ARRAY-P

(in-package :cl-test)

(deftest adjustable-array-p.1
  (notnot (adjustable-array-p (make-array '(5) :adjustable t)))
  t)

(deftest adjustable-array-p.2
  (notnot (adjustable-array-p (make-array nil :adjustable t)))
  t)

(deftest adjustable-array-p.3
  (notnot (adjustable-array-p (make-array '(2 3) :adjustable t)))
  t)

(deftest adjustable-array-p.4
  (notnot (adjustable-array-p (make-array '(2 2 2) :adjustable t)))
  t)

(deftest adjustable-array-p.5
  (notnot (adjustable-array-p (make-array '(2 2 2 2) :adjustable t)))
  t)

;;; Error tests

(deftest adjustable-array-p.error.1
  (classify-error (adjustable-array-p))
  program-error)

(deftest adjustable-array-p.error.2
  (classify-error (adjustable-array-p "aaa" nil))
  program-error)

(deftest adjustable-array-p.error.3
  (classify-error (adjustable-array-p 10))
  type-error)

(deftest adjustable-array-p.error.4
  (let (why)
    (loop for e in *mini-universe*
	  unless (or (typep e 'array)
		     (eq 'type-error 
			 (setq why (classify-error**
				    `(adjustable-array-p ',e)))))
	  collect (list e why)))
  nil)
