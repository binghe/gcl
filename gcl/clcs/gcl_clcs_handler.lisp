;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: "CONDITIONS"; Base: 10 -*-

(in-package :conditions)

(defmacro handler-bind (bindings &body forms)
  (declare (optimize (safety 2)))
  `(let ((*handler-clusters* (cons (list ,@(mapcar (lambda (x) `(cons ',(car x) ,(cadr x))) bindings))
				   *handler-clusters*)))
     ,@forms))


(defmacro handler-case (form &rest cases)
  (declare (optimize (safety 2)))
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (gensym)) (error-return  (gensym)))
	  `(block ,error-return
	     (multiple-value-call (lambda ,@(cdr no-error-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let ((block (gensym))(var (gensym))(tcases (mapcar (lambda (x) (cons (gensym) x)) cases)))
	  `(block ,block
	     (let (,var)
	       (declare (ignorable ,var))
	       (tagbody
		 (handler-bind ,(mapcar (lambda (x &aux (tag (pop x))(type (pop x))(ll (car x)))
					  (list type `(lambda (x) ,(if ll `(setq ,var x) `(declare (ignore x))) (go ,tag))))
					tcases)
			       (return-from ,block ,form))
		 ,@(mapcan (lambda (x &aux (tag (pop x))(type (pop x))(ll (pop x))(body x))
			     (list tag `(return-from ,block (let ,(when ll `((,(car ll) ,var))) ,@body))))
			   tcases))))))))

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

