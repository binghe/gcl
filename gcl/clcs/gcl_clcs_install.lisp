;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: "CONDITIONS"; Base: 10 -*-

(in-package "CONDITIONS")

(defvar *shadowed-symbols* 
  '(BREAK ERROR CERROR WARN CHECK-TYPE ASSERT ETYPECASE CTYPECASE ECASE CCASE))

(defun install-symbol (real clcs)
  (unless (get real 'definition-before-clcs)
    (setf (get real 'definition-before-clcs)
	  (symbol-function real)))
  (unless (eq (symbol-function real)
	      (symbol-function clcs))	       
    (setf (symbol-function real)
	  (symbol-function clcs))))

(defun revert-symbol (real)
  (when (and (get real 'definition-before-clcs)
	     (not (eq (symbol-function real)
		      (get real 'definition-before-clcs))))
    (setf (symbol-function real)
	  (get real 'definition-before-clcs))))

;FIXME
(defvar *clcs-redefinitions*
  (nconc (mapcar #'(lambda (symbol)
		     (list (intern (symbol-name symbol) "LISP") symbol))
		 *shadowed-symbols*)
	 '(;(compile-file clcs-compile-file)
;	   (compile clcs-compile)
;           (load clcs-load)
;           (open clcs-open)
;	   #+kcl (si::break-level si::clcs-break-level)
	   #+kcl (si::terminal-interrupt si::clcs-terminal-interrupt)
	   #+kcl (si::break-quit si::clcs-break-quit)
	   #+kcl (si::error-set clcs-error-set)
	   #+kcl (si::universal-error-handler clcs-universal-error-handler))))

(defun install-clcs-symbols ()
  (dolist (r *clcs-redefinitions*)
    (install-symbol (first r) (second r)))
  nil)

(defun revert-clcs-symbols ()
  (dolist (r (reverse *clcs-redefinitions*))
    (revert-symbol (first r)))
  nil)

(defun compile-file (file &rest args)
  (let (warnings failures)
    (handler-bind
     ((warning (lambda (c) 
		 (setq warnings t) 
		 (unless (typep c 'style-warning)
		   (setq failures t))
		 (when (not compiler::*compile-verbose*) 
		   (invoke-restart (find-restart 'muffle-warning c)))))
      (error (lambda (c)
	       (declare (ignore c))
	       (setq failures t))))
     (loop 
      (with-simple-restart 
       (retry "Retry compiling file ~S." file)
       (let ((res (apply #.(si::function-src 'compile-file) file args)))
	 (when compiler::*error-p* (error "Compilation of ~s failed." file))
	 (return (values res warnings failures))))))))

;(defun clcs-compile-file (file &rest args)
;  (loop (with-simple-restart (retry "Retry compiling file ~S." file)
;	  (let ((values (multiple-value-list 
;			    (apply (or (get 'compile-file 'definition-before-clcs)
;				       #'compile-file)
;				   file args))))
;	    (unless #+kcl compiler::*error-p* #-kcl nil
;	      (return-from clcs-compile-file
;		(values-list values)))
;	    (error "~S failed." 'compile-file)))))

(defun compile (name &optional def)
  (let (warnings failures)
    (handler-bind
     ((warning (lambda (c) 
		 (setq warnings t) 
		 (unless (typep c 'style-warning)
		   (setq failures t))
		 (when (not compiler::*compile-verbose*) 
		   (invoke-restart (find-restart 'muffle-warning c)))))
      (error (lambda (c)
	       (declare (ignore c))
	       (setq failures t))))
     (loop 
      (with-simple-restart 
       (retry "Retry compiling ~S." (list name def))
       (let ((res (funcall #.(si::function-src 'compile) name def)))
	 (when compiler::*error-p* (error "Compilation of ~s failed." (list name def)))
	 (return (values res warnings failures))))))))

;(defun clcs-compile (&rest args)
;  (loop (with-simple-restart (retry "Retry compiling ~S." (car args))
;	  (let ((values (multiple-value-list 
;			    (apply (or (get 'compile 'definition-before-clcs)
;				       #'compile-file)
;				   args))))
;	    (unless #+kcl compiler::*error-p* #-kcl nil
;	      (return-from clcs-compile
;		(values-list values)))
;	    (error "~S failed." 'compile)))))

(defun load (pn &rest args)
  (loop (with-simple-restart 
	 (retry "Retry loading file ~S." (car args))
	 (return (apply #.(si::function-src 'load) pn args)))))

(defun open (pn &rest args)
  (loop (with-simple-restart 
	 (retry "Retry opening file ~S." (car args))
	 (return (apply #.(si::function-src 'open) pn args)))))

;(defun clcs-load (&rest args)
;  (loop (with-simple-restart (retry "Retry loading file ~S." (car args))
;          (return-from clcs-load 
;                       (apply (or (get 'load 'definition-before-clcs) #'load) args)))))

;(defun clcs-open (&rest args)
;  (loop (with-simple-restart (retry "Retry opening file ~S." (car args))
;			       (return-from clcs-open
;                       (apply (or (get 'open 'definition-before-clcs) #'open) args)))))

#+(or kcl lucid cmu)
(install-clcs-symbols)

#+dsys
(defun dsys::retry-operation (function retry-string)
  (loop (with-simple-restart (retry retry-string)
	  (return-from dsys::retry-operation
	    (funcall function)))))

#+dsys
(defun dsys::operate-on-module (module initial-state system-operation)
  (if (null dsys::*retry-operation-list*)
      (dsys::operate-on-module1 module initial-state system-operation)
      (let ((retry-operation (car (last dsys::*retry-operation-list*)))
	    (dsys::*retry-operation-list* (butlast dsys::*retry-operation-list*)))
	(restart-bind ((retry 
			#'(lambda (&rest ignore)
			    (declare (ignore ignore))
			    (funcall (car retry-operation)))
			:report-function
			#'(lambda (stream)
			    (write-string (cdr retry-operation) stream))))
	   (dsys::operate-on-module module initial-state system-operation)))))
