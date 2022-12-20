;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;	evalmacros.lsp


(in-package :si)


;(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))
;(eval-when (eval compile) (defun si:clear-compiler-properties (symbol)))
(eval-when (eval compile)
  (setq si:*inhibit-macro-special* nil)
  (defmacro ?cons (f x &aux (s (sgen "?CONS"))) `(let ((,s ,x)) (if (cdr ,s) (cons ,f ,s) (car ,s))))
  (defmacro ?list (x &aux (s (sgen "?LIST"))) `(let ((,s ,x)) (when ,s (list ,s))))
  (defmacro collect (v r rp np &aux (s (sgen "COLLECT")))
    `(let ((,s ,v)) (setf rp (if rp (rplacd rp (list ,s)) (setq r ,s)) rp np)))
  (defmacro ?let (k kf r) `(let ((r ,r)) (if (eq ,k ,kf) r `(let ((,,k ,,kf)) (declare (ignorable ,,k)) ,r))))
  (defmacro ?key (x &aux (s (sgen "?KEY"))) `(if (or (constantp ,x) (symbolp ,x)) ,x ',s)))

(defmacro sgen (&optional (pref "G")) `(load-time-value (gensym ,pref)))

(defmacro defvar (var &optional (form nil form-sp) doc-string)
  (declare (optimize (safety 1)))
  `(progn (*make-special ',var)
	  ,@(when doc-string `((putprop ',var ,doc-string 'variable-documentation)))
	  ,@(when form-sp `((unless (boundp ',var) (setq ,var ,form))))
	  ',var))

(defmacro defparameter (var form &optional doc-string)
  (declare (optimize (safety 1)))
  `(progn (*make-special ',var)
	  ,@(when doc-string `((putprop ',var ,doc-string 'variable-documentation)))
	  (setq ,var ,form)
	  ',var))

(defmacro defconstant (var form &optional doc-string)
  (declare (optimize (safety 1)))
  `(progn (*make-constant ',var ,form)
	  ,@(when doc-string `((putprop ',var ,doc-string 'variable-documentation)))
	  ',var))


;;; Each of the following macros is also defined as a special form.
;;; Thus their names need not be exported.

(defmacro and (&rest forms &aux r rp np)
  (declare (optimize (safety 1)))
  (do ((y forms))((endp y) (if forms r t))
    (let ((x (pop y)))
      (if (constantp x) (unless (if (eval x) y) (collect x r rp np) (setq y nil))
	(if y (collect `(if ,@(setq np (list x))) r rp np)
	  (collect x r rp np))))))

(defmacro or (&rest forms &aux r rp np (s (sgen "OR")))
  (declare (optimize (safety 1)))
  (do ((y forms))((endp y) r)
    (let ((x (pop y)))
      (if (constantp x) (when (eval x) (collect x r rp np) (setq y nil))
	(if (symbolp x) (collect `(if ,x ,@(setq np (list x))) r rp np)
	  (if y (collect `(let ((,s ,x)) (if ,s ,@(setq np (list s)))) r rp np)
	    (collect x r rp np)))))))

(defun parse-body-header (x)
  (let* ((doc x)(x (or (when (stringp (car x)) (cdr x)) x))
	 (dec x)(x (member-if-not (lambda (x) (when (consp x) (eq (car x) 'declare))) x))
	 (ctp x)(x (member-if-not (lambda (x) (when (consp x) (eq (car x) 'check-type))) x)))
    (values (car (ldiff doc dec)) (ldiff dec ctp) (ldiff ctp x) x)))

(defmacro locally (&rest body)
  (multiple-value-bind
   (doc dec)
   (parse-body-header body)
   (declare (ignore doc))
   `(let (,@(mapcan (lambda (x &aux (z (pop x))(z (if (eq z 'type) (pop x) z)))
		      (case z
			    ((ftype inline notinline optimize) nil)
			    (otherwise (mapcar (lambda (x) (list x x)) x))))
		   (apply 'append (mapcar 'cdr dec))))
      ,@body)))

(defmacro loop (&rest body &aux (tag (sgen "LOOP")))
  `(block nil (tagbody ,tag ,(?cons 'progn body) (go ,tag))))

(defmacro while (test &rest forms)
 `(loop (unless ,test (return)) ,@forms))

(defmacro defmacro (name vl &rest body)
  `(si:define-macro ',name (si:defmacro* ',name ',vl ',body)))

(defmacro defun (name lambda-list &rest body)
  (multiple-value-bind
      (doc dec ctp body)
      (parse-body-header body)
    `(progn ,@(when doc `((setf (get ',name 'function-documentation) ,doc)))
	    (setf (symbol-function ',name) (lambda ,lambda-list  ,@dec ,@ctp (block ,name ,@body)))
	    ',name)))

; assignment

(defmacro psetq (&rest args)
  (declare (optimize (safety 1)))
  (assert (evenp (length args)))
  (let ((x (let ((i 0)) (mapcon (lambda (x) (when (oddp (incf i)) `((,(cadr x) ,(car x) ,(gensym))))) args))))
    (when x
      `(let* ,(mapcar (lambda (x) `(,(caddr x) ,(car x))) x)
	 (setq ,@(mapcan 'cdr x))
	 nil))))

; conditionals
(defmacro cond (&rest clauses &aux r rp np (s (sgen "COND")))
  (declare (optimize (safety 1)))
  (do ((y clauses))((endp y) r)
    (let* ((x (pop y))(z (pop x)))
      (if (constantp z) (when (eval z) (collect (if x (?cons 'progn x) z) r rp np) (setq y nil))
	(if x (collect `(if ,z ,@(setq np (list (?cons 'progn x)))) r rp np)
	  (if (symbolp z) (collect `(if ,z ,@(setq np (list z))) r rp np)
	    (if y (collect `(let ((,s ,z)) (if ,s ,@(setq np (list s)))) r rp np)
	      (collect `(values ,z) r rp np))))))))

(defmacro when (pred &rest body &aux (x (?cons 'progn body)))
  (declare (optimize (safety 1)))
  (if (constantp pred) (if (eval pred) x) `(if ,pred ,x)))

(defmacro unless (pred &rest body &aux (x (?cons 'progn body)))
  (declare (optimize (safety 1)))
  (if (constantp pred) (if (not (eval pred)) x) `(if (not ,pred) ,x)))

; program feature

(defun prog?* (let?* vl body)
  (multiple-value-bind
      (doc dec ctp body)
      (parse-body-header body)
    (declare (ignore doc))
    `(block nil (,let?* ,vl ,@dec (tagbody ,@(append ctp body))))))

(defmacro prog (vl &rest body)
  (prog?* 'let vl body))

(defmacro prog* (vl &rest body)
  (prog?* 'let* vl body))

; sequencing

(defmacro prog1 (first &rest body &aux (sym (sgen "PROG1")))
  `(let ((,sym ,first)) ,@body ,sym))

(defmacro prog2 (first second &rest body &aux (sym (sgen "PROG2")))
  `(progn ,first (let ((,sym ,second)) ,@body ,sym)))

; multiple values

(defmacro multiple-value-list (form)
  `(multiple-value-call 'list ,form))

(defmacro multiple-value-setq (vars form)
  (declare (optimize (safety 1)))
  (let ((syms (mapcar (lambda (x) (declare (ignore x)) (gensym)) (or vars (list nil)))))
    `(multiple-value-bind ,syms ,form ,@(?list (?cons 'setq (mapcan 'list vars syms))) ,(car syms))))

(defmacro multiple-value-bind (vars form &rest body &aux (sym (sgen "MULTIPLE-VALUE-BIND")))
  (declare (optimize (safety 1)))
  `(let* ((,sym (multiple-value-list ,form))
	  ,@(mapcon (lambda (x) `((,(car x) (car ,sym)) ,@(when (cdr x) `((,sym (cdr ,sym)))))) vars))
     (declare (ignorable ,sym))
     ,@body))

(defun do?* (?* control test result body &aux (label (sgen "DO")))
  (multiple-value-bind
      (doc dec ctp body)
      (parse-body-header body)
    (declare (ignore doc))
    (labels ((?let (vl dec body) (if (or vl dec) `(,(if ?* 'let* 'let) ,vl ,@dec ,body) body))
	     (?tagbody (l x y &aux (x (macroexpand x))) (if x `(tagbody ,l ,x ,@(?list (when (eq (car x) 'if) y))) y)))
      `(block nil
	 ,(?let
	   (mapcar (lambda (x) (if (listp x) (ldiff x (cddr x)) x)) control)
	   dec
	   (?tagbody
	    label
	    `(unless ,test
	       ,@(?list (?cons 'tagbody (append ctp body)))
	       ,@(?list (?cons (if ?* 'setq 'psetq) (mapcan (lambda (x) (when (and (listp x) (cddr x)) (list (car x) (caddr x)))) control)))
	       (go ,label))
	    `(return ,(?cons 'progn result))))))))

(defmacro do (control (test . result) &rest body)
  (do?* nil control test result body))

(defmacro do* (control (test . result) &rest body)
  (do?* t control test result body))

(defmacro case (keyform &rest clauses &aux r rp np (key (?key keyform)))
  (declare (optimize (safety 1)))
  (labels ((sw (x) `(eql ,key ,(if (constantp x) x `',x))))
    (do ((y clauses))((endp y) (?let key keyform r))
      (let* ((x (pop y))(z (pop x)))
	(if (member z '(t otherwise))
	    (if y (error "default case must be last") (collect (?cons 'progn x) r rp np))
	  (when z
	    (if (constantp key)
		(let ((key (eval key))) (when (if (listp z) (member key z) (eql key z)) (collect (?cons 'progn x) r rp np) (setq y nil)))
	      (collect `(if ,(if (listp z) (?cons 'or (mapcar #'sw z)) (sw z))
			    ,@(setq np (list (?cons 'progn x)))) r rp np))))))))

(defmacro ecase (keyform &rest clauses &aux (key (?key keyform)))
  (declare (optimize (safety 1)))
  (?let key keyform
	`(case ,key
	   ,@(mapcar (lambda (x) (if (member (car x) '(t otherwise)) (cons (list (car x)) (cdr x)) x)) clauses)
	   (otherwise
	    (error 'type-error :datum ,key
		   :expected-type '(member ,@(apply 'append (mapcar (lambda (x &aux (x (car x))) (if (listp x) x (list x))) clauses))))))))

(defmacro ccase (keyform &rest clauses &aux (key (?key keyform)))
  (declare (optimize (safety 1)))
  (?let key keyform
	`(do nil (nil)
	   (case ,key
	     ,@(mapcar (lambda (x &aux (k (pop x)))
			 `(,(if (member k '(t otherwise)) (list k) k) (return ,(?cons 'progn x)))) clauses)
	     (otherwise
	      (check-type ,key (member ,@(apply 'append (mapcar (lambda (x &aux (x (car x))) (if (listp x) x (list x))) clauses)))))))))


(defmacro return (&optional val) `(return-from nil ,val))

(defmacro dolist ((var form &optional (val nil)) &rest body &aux (temp (sgen "DOLIST")))
  `(do* ((,temp ,form (cdr ,temp))(,var (car ,temp) (car ,temp)))
	((endp ,temp) ,val)
	,@body))

;; In principle, a more complete job could be done here by trying to
;; capture fixnum type declarations from the surrounding context or
;; environment, or from within the compiler's internal structures at
;; compile time.  See gcl-devel archives for examples.  This
;; implementation relies on the fact that the gcc optimizer will
;; eliminate the bignum branch if the supplied form is a symbol
;; declared to be fixnum, as the comparison of a long integer variable
;; with most-positive-fixnum is then vacuous.  Care must be taken in
;; making comparisons with most-negative-fixnum, as the C environment
;; appears to treat this as positive or negative depending on the sign
;; of the other argument in the comparison, apparently to symmetrize
;; the long integer range.  20040403 CM.
(defmacro dotimes ((var form &optional val) &rest body &aux (s (sgen "DOTIMES"))(m (sgen "DOTIMES")))
  `(let* ((,s (block nil ,form))(,m (min ,s most-positive-fixnum)))
     (declare (fixnum ,m))
     (do ((,var 0 (1+ ,var)))
	 ((>= ,var ,m) (if (eql ,s ,m) ,val (do ((,var ,m (1+ ,var)))((>= ,var ,s) ,val) ,@body)))
	   (declare (fixnum ,var))
	   ,@body)))

(defmacro declaim (&rest l)
  `(eval-when (compile eval load)
     ,@(mapcar (lambda (x) `(proclaim ',x)) l)))

(defmacro lambda (&rest l) `(function (lambda ,@l)))

(defun compiler-macro-function (name)
  (get name 'compiler-macro))
