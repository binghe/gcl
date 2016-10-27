;; -*-Lisp-*-
(in-package :si)

(macrolet 
 ((make-conditionp (condition &aux (n (intern (string-concatenate (string condition) "P"))))
		   `(defun ,n (x &aux (z (si-find-class ',condition)))
		      (when z
			(funcall (setf (symbol-function ',n) (lambda (x) (typep x z))) x))))
  (make-condition-classp (class &aux (n (intern (string-concatenate (string class) "-CLASS-P"))))
			 `(defun ,n (x &aux (s (si-find-class 'standard-class)) (z (si-find-class ',class)))
			    (when (and s z)
			      (funcall (setf (symbol-function ',n)
					     (lambda (x &aux (x (if (symbolp x) (si-find-class x) x)))
					       (when (typep x s)
						 (member z (si-class-precedence-list x))))) x)))))
 (make-conditionp condition)
 (make-conditionp warning)
 (make-condition-classp condition)
 (make-condition-classp simple-condition))
 
(proclaim '(ftype (function (t *) t) make-condition))

(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((conditionp datum)
	 (if arguments
	     (cerror "ignore the additional arguments."
		     'simple-type-error
		     :datum arguments
		     :expected-type 'null
		     :format-control "you may not supply additional arguments ~
				     when giving ~s to ~s."
		     :format-arguments (list datum function-name)))
	 datum)
        ((condition-class-p datum)
	 (apply #'make-condition datum arguments))
        ((when (condition-class-p default-type) (or (stringp datum) (functionp datum)))
	 (make-condition default-type :format-control datum :format-arguments arguments))
	((coerce-to-string datum arguments))))

(defvar *handler-clusters* nil)
(defvar *break-on-signals* nil)

(defun signal (datum &rest arguments)
  (declare (optimize (safety 1)))
  (let ((*handler-clusters* *handler-clusters*)
	(condition (coerce-to-condition datum arguments 'simple-condition 'signal)))
    (if (typep condition *break-on-signals*)
	(break "~a~%break entered because of *break-on-signals*." condition))
    (do nil ((not *handler-clusters*))
	(dolist (handler (pop *handler-clusters*))
	  (when (typep condition (car handler))
	    (funcall (cdr handler) condition))))
    nil))

(defvar *debugger-hook* nil)
(defvar *debug-level* 1)
(defvar *debug-restarts* nil)
(defvar *debug-abort* nil)
(defvar *debug-continue* nil)
(defvar *abort-restarts* nil)

(defun break-level-invoke-restart (n)
  (cond ((when (plusp n) (< n (+ (length *debug-restarts*) 1)))
	 (invoke-restart-interactively (nth (1- n) *debug-restarts*)))
	((format t "~&no such restart."))))

(defun find-ihs (s i &optional (j i))
  (cond ((eq (ihs-fname i) s) i)
	((and (> i 0) (find-ihs s (1- i) j)))
	(j)))

(defmacro without-interrupts (&rest forms)
  `(let (*quit-tag* *quit-tags* *restarts*)
     ,@forms))

(defun process-args (args &aux (control (member :format-control args)))
  (labels ((r (x &aux (z (member-if (lambda (x) (member x '(:format-control :format-arguments))) x)))
	      (if z (nconc (ldiff x z) (r (cddr z))) x)))
    (if control
	(nconc (r args) (list (apply 'format nil (cadr control) (cadr (member :format-arguments args)))))
      args)))    

(defun coerce-to-string (datum args) 
  (cond ((stringp datum)
	 (if args 
	     (let ((*print-pretty* nil)
		   (*print-level* *debug-print-level*)
		   (*print-length* *debug-print-level*)
		   (*print-case* :upcase))
	       (apply 'format nil datum args))
	   datum))
	((symbolp datum)
	 (let ((args (process-args args)))
	   (substitute 
	    #\^ #\~ 
	    (coerce-to-string
	     (if args
		 (apply 'string-concatenate (cons datum (make-list (length args) :initial-element " ~s")))
	       (string datum))
	     args))))
	("unknown error")))

(defvar *break-on-warnings* nil)

(defun warn (datum &rest arguments)
  (declare (optimize (safety 2)))
  (let ((c (process-error datum arguments 'simple-warning)))
    (check-type c (or string (satisfies warningp)) "a warning condition")
    (when *break-on-warnings*
      (break "~A~%break entered because of *break-on-warnings*." c))
    (restart-case
     (signal c)
     (muffle-warning nil :report "Skip warning."  (return-from warn nil)))
    (format *error-output* "~&Warning: ~a~%" c)
    nil))

(dolist (l '(break cerror error universal-error-handler ihs-top get-sig-fn-name next-stack-frame check-type-symbol))
  (setf (get l 'dbl-invisible) t))

(defvar *sig-fn-name* nil)

(defun get-sig-fn-name (&aux (p (ihs-top))(p (next-stack-frame p)))
  (when p (ihs-fname p)))

(defun process-error (datum args &optional (default-type 'simple-error))
  (let ((internal (cond ((simple-condition-class-p datum)
			 (find-symbol (string-concatenate "INTERNAL-" (string datum)) :conditions))
			((condition-class-p datum)
			 (find-symbol (string-concatenate "INTERNAL-SIMPLE-" (string datum)) :conditions)))))
    (coerce-to-condition (or internal datum) (if internal (list* :function-name *sig-fn-name* args) args) default-type 'process-error)))

(defun universal-error-handler (n cp fn cs es &rest args &aux (*sig-fn-name* fn))
  (declare (ignore es))
  (if cp (apply #'cerror cs n args) (apply #'error n args)))

(defun cerror (continue-string datum &rest args &aux (*sig-fn-name* (or *sig-fn-name* (get-sig-fn-name))))
  (values 
   (with-simple-restart 
    (continue continue-string args)
    (apply #'error datum args))))
(putprop 'cerror t 'compiler::cmp-notinline)


(defun error (datum &rest args &aux (*sig-fn-name* (or *sig-fn-name* (get-sig-fn-name))))
  (let ((c (process-error datum args))(q (or *quit-tag* +top-level-quit-tag+)))
    (signal c)
    (invoke-debugger c)
    (throw q q)))
(putprop 'error t 'compiler::cmp-notinline)
  

(defun invoke-debugger (condition)

  (when *debugger-hook*
	(let ((hook *debugger-hook*) *debugger-hook*)
	  (funcall hook condition hook)))

  (maybe-clear-input)
  
  (let ((correctable (find-restart 'continue))
	*print-pretty*
	(*print-level* *debug-print-level*)
	(*print-length* *debug-print-level*)
	(*print-case* :upcase))
    (terpri *error-output*)
    (format *error-output* (if (and correctable *break-enable*) "~&Correctable error: " "~&Error: "))
    (let ((*indent-formatted-output* t))
      (when (stringp condition) (format *error-output* condition)))
    (terpri *error-output*)
    (if (> (length *link-array*) 0)
	(format *error-output* "Fast links are on: do (si::use-fast-links nil) for debugging~%"))
    (format *error-output* "Signalled by ~:@(~S~).~%" (or *sig-fn-name* "an anonymous function"))
    (when (and correctable *break-enable*)
      (format *error-output* "~&If continued: ")
      (funcall (restart-report-function correctable) *error-output*))
    (force-output *error-output*)
    (when *break-enable* (break-level condition))))


(defun dbl-eval (- &aux (break-command t))
  (let ((val-list (multiple-value-list
		   (cond 
		    ((keywordp -) (break-call - nil 'break-command))
		    ((and (consp -) (keywordp (car -))) (break-call (car -) (cdr -) 'break-command))
		    ((integerp -) (break-level-invoke-restart -))     
		    (t (setq break-command nil) (evalhook - nil nil *break-env*))))))
    (cons break-command val-list)))

(defun dbl-rpl-loop (p-e-p)

  (setq +++ ++ ++ + + -)

  (if *no-prompt*
      (setq *no-prompt* nil)
    (format *debug-io* "~&~a~a>~{~*>~}"
	    (if p-e-p "" "dbl:")
	    (if (eq *package* (find-package 'user)) "" (package-name *package*))
	    *break-level*))
  (force-output *error-output*)

  (setq - (dbl-read *debug-io* nil *top-eof*))
  (when (eq - *top-eof*) (bye -1))
  (let* ((ev (dbl-eval -))
	 (break-command (car ev))
	 (values (cdr ev)))
    (unless (and break-command (eq (car values) :resume))
      (setq /// // // / / values *** ** ** * * (car /))
      (fresh-line *debug-io*)
      (dolist (val /)
	(prin1 val *debug-io*)
	(terpri *debug-io*))
      (dbl-rpl-loop p-e-p))))

(defun do-break-level (at env p-e-p debug-level); break-level

  (unless
      (with-simple-restart
       (abort "Return to debug level ~D." debug-level)

       (catch-fatal 1)
       (setq *interrupt-enable* t)
       (cond (p-e-p
	      (format *debug-io* "~&~A~2%" at)
	      (set-current)
	      (setq *no-prompt* nil)
	      (show-restarts))
	     ((set-back at env)))

       (not (catch 'step-continue (dbl-rpl-loop p-e-p))))

    (terpri *debug-io*)
    (break-current)
    (do-break-level at env p-e-p debug-level)))


(defun break-level (at &optional env)
  (let* ((p-e-p (unless (listp at) t))
         (+ +) (++ ++) (+++ +++)
         (- -)
         (* *) (** **) (*** ***)
         (/ /) (// //) (/// ///)
	 (debug-level *debug-level*)
	 (*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
	 *quit-tag*
	 (*break-level* (if p-e-p (cons t *break-level*) *break-level*))
	 (*ihs-base* (1+ *ihs-top*))
	 (*ihs-top* (ihs-top))
	 (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
	 (*frs-top*  (frs-top))
	 (*current-ihs* *ihs-top*)
	 (*debug-level* (1+ *debug-level*))
	 (*debug-restarts* (compute-restarts))
	 (*debug-abort* (find-restart 'abort))
	 (*debug-continue* (find-restart 'continue))
	 (*abort-restarts* (remove-if-not (lambda (x) (eq 'abort (restart-name x))) *debug-restarts*))
	 (*readtable* (or *break-readtable* *readtable*))
	 *break-env* *read-suppress*)
    
      (do-break-level at env p-e-p debug-level)))

(putprop 'break-level t 'compiler::cmp-notinline)

(defun break (&optional format-string &rest args &aux message (*sig-fn-name* (or *sig-fn-name* (get-sig-fn-name))))

  (let ((*print-pretty* nil)
	(*print-level* 4)
	(*print-length* 4)
	(*print-case* :upcase))
    (terpri *error-output*)
    (cond (format-string
	   (format *error-output* "~&Break: ")
	   (let ((*indent-formatted-output* t))
	     (apply 'format *error-output* format-string args))
	   (terpri *error-output*)
	   (setq message (apply 'format nil format-string args)))
	  (t (format *error-output* "~&Break.~%")
	     (setq message ""))))
  (with-simple-restart 
   (continue "Return from break.")
   (break-level message))
  nil)
(putprop 'break t 'compiler::cmp-notinline)
