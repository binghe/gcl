;; -*-Lisp-*-
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

(export '(*debug* *compiler-check-args* *safe-compile* *compiler-new-safety*
		  *compiler-push-events* *space* *speed*
 		  *alien-declarations*
 		  lit sgen cmp-inline cmp-notinline cmp-type))

(eval-when (eval compile) (setq si:*inhibit-macro-special* nil))

(defun lit (&rest r)
  (error "lit called with args ~s~%" r))

(defmacro sgen (&optional (pref "G"))
  `(load-time-value (gensym ,pref)))

(defmacro defvar (var &optional (form nil form-sp) doc-string)
  (declare (optimize (safety 2)))
  `(progn (si:*make-special ',var)
	  ,(if doc-string
	       `(si:putprop ',var ,doc-string 'variable-documentation))
	  ,(if form-sp
	       `(or (boundp ',var)
		    (setq ,var ,form)))
	  ',var)
	  )

(defmacro defparameter (var form &optional doc-string)
  (declare (optimize (safety 2)))
  `(progn (si:*make-special ',var)
	  ,@(when doc-string `((si:putprop ',var ,doc-string 'variable-documentation)))
	  (setq ,var ,form)
	  ',var))

(defmacro defconstant (var form &optional doc-string)
  (declare (optimize (safety 2)))
  `(progn (si:*make-constant ',var ,form)
	  ,@(when doc-string `((si:putprop ',var ,doc-string 'variable-documentation)))
	  ',var))


;;; Each of the following macros is also defined as a special form.
;;; Thus their names need not be exported.

(defmacro and (&rest forms)
  (declare (optimize (safety 2)))
  (if (endp forms)
      t
      (let ((x (reverse forms)))
           (do ((forms (cdr x) (cdr forms))
                (form (car x) `(if ,(car forms) ,form)))
               ((endp forms) form)))))

(defmacro or (&rest forms)
  (declare (optimize (safety 2)))
  (unless (endp forms)
    (let ((x (reverse forms)))
      (do ((forms (cdr x) (cdr forms)) (gs (sgen "OR"))
	   (form (car x)
		 (if (or (constantp (car forms)) (symbolp (car forms)))
		     (let ((temp (car forms)))
		       `(if ,temp ,temp ,form))
		   `(let ((,gs ,(car forms)))
		      (declare (ignorable ,gs))
		      (if ,gs ,gs ,form)))))
               ((endp forms) form)))))

(defmacro locally (&rest body)
  (multiple-value-bind
   (doc decls ctps body)
   (parse-body-header body)
   `(let (,@(mapcan (lambda (x &aux (z (pop x))(z (if (eq z 'type) (pop x) z)))
		      (case z
			    ((ftype inline notinline optimize special) nil)
			    (otherwise (mapcar (lambda (x) (list x x)) x))))
		   (apply 'append (mapcar 'cdr decls))))
      ,@(when doc (list doc))
      ,@decls
      ,@ctps
      ,@body)))

(defmacro loop (&rest body &aux (tag (sgen "LOOP")))
  (declare (optimize (safety 2)))
  `(block nil (tagbody ,tag (progn ,@body) (go ,tag))))

(defun import (s &optional (p *package*))
  (import-internal s p)
  t)

(defun delete-package (p)
  (the boolean (values (delete-package-internal p))))

;(import 'while #+ansi-cl 'cl-user #-ansi-cl 'user)
(defmacro while (test &rest forms)
  (declare (optimize (safety 2)))
 `(loop (unless ,test (return)) ,@forms))

(defun funid-sym-p (funid &optional err)
  (cond ((symbolp funid) funid)
	((when (consp funid);FIXME Too early for typecase
	   (when (eq (car funid) 'setf)
	     (when (consp (cdr funid))
	       (when (symbolp (cadr funid))
		 (null (cddr funid))))))
	 (setf-sym (cadr funid)))
	(t (when err (error 'type-error :datum funid :expected-type 'function-name)))))

(defun funid-sym (funid)
  (funid-sym-p funid t))

(defun funid-p (funid &optional err)
  (cond ((symbolp funid) funid)
	((when (consp funid)
	   (eq (car funid) 'lambda))
	 funid)
	((when (consp funid);FIXME Too early for typecase
	   (when (eq (car funid) 'setf)
	     (when (consp (cdr funid))
	       (when (symbolp (cadr funid))
		 (null (cddr funid))))))
	 (setf-sym (cadr funid)))
	(t (when err (error 'type-error :datum funid :expected-type 'function-name)))))

(defun funid (funid)
  (funid-p funid t))

(defun funid-to-sym (funid) (funid-sym funid))


(defun setf-sym (funid)
  (values       
   (intern (si::string-concatenate
	    (let ((x (symbol-package funid))) (if x (package-name x) ""))
	    "::"
	    (symbol-name funid))
	   (load-time-value (or (find-package 'setf) (make-package 'setf))))))

(defmacro defmacro (name vl &rest body)
  (declare (optimize (safety 2)))
  `(progn
     (setf (macro-function ',name) ,(defmacro-lambda name vl body))
     ',name))

(defmacro define-symbol-macro (sym exp) 
  (declare (optimize (safety 2)) (ignore sym exp)) nil);FIXME placeholder

(defmacro defun (name lambda-list &rest body)
  (declare (optimize (safety 2)))
  (let* ((doc (parse-body-header body))
	 (rs (funid-sym name))
	 (bn (if (eq rs name) name (cadr name))))
    `(progn ,@(when doc `((setf (get ',rs 'function-documentation) ,doc)))
	    (setf (symbol-function ',rs) ,(block-lambda lambda-list bn body))
	    ',name)))
  
; assignment

(defmacro psetq (&rest args)
  (declare (optimize (safety 2)))
   (do ((l args (cddr l))
        (forms nil)
        (bindings nil))
       ((endp l) (list* 'let* (nreverse bindings) (nreverse (cons nil forms))))
       (let ((sym (gensym)))
            (push (list sym (cadr l)) bindings)
            (push (list 'setq (car l) sym) forms))))

; conditionals

(defmacro cond (&rest clauses &aux (form nil))
  (declare (optimize (safety 2)))
  (let ((x (reverse clauses)))
    (dolist (l x form)
      (cond ((endp (cdr l))
	     (setq form (cond ((eq l (car x)) `(values ,(car l)))
			      (`(or ,(car l) ,form)))))
	    ((let ((c (car l)))
	       (when (constantp c) (cond ((symbolp c) (symbol-value c)) ;fixme constant-eval
					 ((when (consp c) (eq (car c) 'quote)) (cadr c)) (t))))
	     (setq form (if (endp (cddr l)) (cadr l) `(progn ,@(cdr l)))))
	    ((setq form `(if ,(car l) ,(if (endp (cddr l)) (cadr l) `(progn ,@(cdr l))) ,form)))))))

(defmacro when (pred &rest body)
  (declare (optimize (safety 2)))
  `(if ,pred (progn ,@body)))

(defmacro unless (pred &rest body)
  (declare (optimize (safety 2)))
  `(if (not ,pred) (progn ,@body)))

; program feature

(defmacro prog (vl &rest body &aux (decl nil))
  (declare (optimize (safety 2)))
  (do ()
      ((or (endp body)
           (not (consp (car body)))
           (not (eq (caar body) 'declare)))
       `(block nil (let ,vl ,@decl (tagbody ,@body)))
       )
      (push (car body) decl)
      (pop body))
  )

(defmacro prog* (vl &rest body &aux (decl nil))
  (declare (optimize (safety 2)))
  (do ()
      ((or (endp body)
           (not (consp (car body)))
           (not (eq (caar body) 'declare)))
       `(block nil (let* ,vl ,@decl (tagbody ,@body)))
       )
      (push (car body) decl)
      (pop body))
  )

; sequencing

(defmacro prog1 (first &rest body &aux (sym (sgen "PROG1")))
  (declare (optimize (safety 2)))
  `(let ((,sym ,first)) (declare (ignorable ,sym)) ,@body ,sym))

(defmacro prog2 (first second &rest body &aux (sym (sgen "PROG2")))
  (declare (optimize (safety 2)))
  `(progn ,first (let ((,sym ,second)) (declare (ignorable ,sym)) ,@body ,sym)))

; multiple values

(defmacro multiple-value-list (form)
  (declare (optimize (safety 2)))
  `(multiple-value-call 'list ,form))

(defmacro multiple-value-setq (vars form)
  (declare (optimize (safety 2)))
  (let ((syms (mapcar (lambda (x) (declare (ignore x)) (gensym)) (or vars (list nil)))))
    `(multiple-value-bind ,syms ,form (setq ,@(mapcan 'list vars syms)) ,(car syms))))
;;   (do ((vl vars (cdr vl))
;;        (sym (sgen))
;;        (forms nil))
;;       ((endp vl) `(let ((,sym (multiple-value-list ,form))) (prog1 ,@(nreverse forms))))
;;       (push `(setq ,@(when forms `(,sym (cdr ,sym))) ,(car vl) (car ,sym)) forms)))

(defmacro multiple-value-bind (vars form &rest body)
  (declare (optimize (safety 2)))
  (do ((vl vars (cdr vl))
       (sym (sgen "MULTIPLE-VALUE-BIND"))
       (bind nil))
      ((endp vl) `(let* ((,sym (multiple-value-list ,form)) ,@(nreverse bind))
		    (declare (ignorable ,sym))
		    ,@body))
      (push `(,(car vl) (car ,sym)) bind)
      (unless (endp (cdr vl)) (push `(,sym (cdr ,sym)) bind))))

(defmacro do (control (test . result) &rest body
              &aux (decl nil) (label (sgen "DO")) (vl nil) (step nil))
  (declare (optimize (safety 2)))
  (do ()
      ((or (endp body)
           (not (consp (car body)))
           (not (eq (caar body) 'declare))))
      (push (car body) decl)
      (pop body))
  (dolist (c control)
    (declare (object c))
    (if (symbolp  c) (setq c (list c)))
    (push (list (car c) (cadr c)) vl)
    (unless (endp (cddr c))
      (push (car c) step)
      (push (caddr c) step)))
  `(block nil
          (let ,(reverse vl)
	    ,@decl
	    (tagbody
	     ,label (if (not ,test)
			(progn 
			  (tagbody ,@body)
			  (psetq ,@(reverse step))
			  (go ,label))
		      (return (progn ,@result)))))))

(defmacro do* (control (test . result) &rest body
               &aux (decl nil) (label (sgen "DO*")) (vl nil) (step nil))
  (declare (optimize (safety 2)))
  (do ()
      ((or (endp body)
           (not (consp (car body)))
           (not (eq (caar body) 'declare))))
      (push (car body) decl)
      (pop body))
  (dolist (c control)
          (declare (object c))
    (if(symbolp  c) (setq c (list c)))
        (push (list (car c) (cadr c)) vl)
    (unless (endp (cddr c))
            (push (car c) step)
            (push (caddr c) step)))
  `(block nil
          (let* ,(reverse vl)
                ,@decl
                (tagbody
                 ,label (if (not ,test)
			    (progn 
			      (tagbody ,@body)
			      (setq ,@(reverse step))
			      (go ,label))
			  (return (progn ,@result)))))))


(defmacro case (keyform &rest clauses &aux (key (sgen "CASE")) (c (reverse clauses)))
  (declare (optimize (safety 2)))
  (labels ((sw (x) `(eql ,key ',x))(dfp (x) (or (eq x t) (eq x 'otherwise)))
	   (v (x) (if (when (listp x) (not (cdr x))) (car x) x))
	   (m (x c &aux (v (v x))) (if (eq v x) (cons c v) v)))
	  `(let ((,key ,keyform))
	     (declare (ignorable ,key))
	     ,(let ((df (when (dfp (caar c)) (m (cdr (pop c)) 'progn))))
		(lreduce (lambda (y c &aux (a (pop c))(v (v a)))
			  (when (dfp a) (error 'program-error "default case must be last"))
			  `(if ,(if (when (eq a v) (listp v)) (m (mapcar #'sw v) 'or) (sw v)) ,(m c 'progn) ,y))
			c :initial-value df)))))



;; (defmacro case (keyform &rest clauses &aux (key (sgen "CASE")) f)
;;   (declare (optimize (safety 2)))
;;   (labels ((sw (x) `(eql ,key ',x))
;; 	   (df (aa ff) (when (member aa '(t otherwise)) (when ff (error 'program-error "default case must be last")) t)))
;; 	  `(let ((,key ,keyform))
;; 	     (declare (ignorable ,key))
;; 	     ,(reduce (lambda (c y &aux (ff f)) (setq f t)
;; 			(let* ((aa (pop c))
;; 			       (ka (or (atom aa) (cdr aa)))
;; 			       (da (if (and (listp c) (cdr c)) (cons 'progn c) (car c)))
;; 			       (v (if ka aa (car aa))))
;; 			  (if (df aa ff) da
;; 			    `(if ,(if (when ka (listp aa)) `(or ,@(mapcar #'sw v)) (sw v)) ,da ,y))))
;; 		      clauses :initial-value nil :from-end t))))

(defmacro ecase (keyform &rest clauses &aux (key (sgen "ECASE")))
  (declare (optimize (safety 2)))
  `(let ((,key ,keyform))
     (declare (ignorable ,key))
     (case ,key
	   ,@(mapcar (lambda (x) (if (member (car x) '(t otherwise)) (cons (list (car x)) (cdr x)) x)) clauses)
	   (otherwise
	    (error 'type-error :datum ,key
		   :expected-type '(member ,@(apply 'append (mapcar (lambda (x &aux (x (car x))) (if (listp x) x (list x))) clauses))))))))


(defmacro ccase (keyform &rest clauses &aux (key (sgen "CCASE")))
  (declare (optimize (safety 2)))
  `(let ((,key ,keyform))
     (declare (ignorable ,key))
     (loop
      (case ,key
	    ,@(mapcar (lambda (x &aux (k (pop x)))
			`(,(if (member k '(t otherwise)) (list k) k) (return ,(if (cdr x) (cons 'progn x) (car x))))) clauses)
	    (otherwise 
	     (check-type ,key (member ,@(apply 'append (mapcar (lambda (x &aux (x (car x))) (if (listp x) x (list x))) clauses)))))))))

;; (defmacro case (keyform &rest clauses &aux (form nil) (key (sgen "CASE")))
;;   (declare (optimize (safety 2)))
;;   (dolist (clause (reverse clauses) `(let ((,key ,keyform)) (declare (ignorable ,key)) ,form))
;;     (cond ((or (eq (car clause) 't) (eq (car clause) 'otherwise))
;;            (setq form `(progn ,@(cdr clause))))
;;           ((consp (car clause))
;;            (setq form `(if (or ,@(mapcar (lambda (x) `(eql ,key ',x)) (car clause)));(member ,key ',(car clause))
;;                            (progn ,@(cdr clause))
;;                            ,form)))
;;           ((car clause)
;;            (setq form `(if (eql ,key ',(car clause))
;;                            (progn ,@(cdr clause))
;;                            ,form))))))


(defmacro return (&optional (val nil))   (declare (optimize (safety 2))) `(return-from nil ,val))

(defmacro dolist ((var form &optional (val nil)) &rest body
                                                 &aux (temp (sgen "DOLIST")))
  (declare (optimize (safety 2)))
  `(do* ((,temp ,form (cdr ,temp))
	 (,var (car ,temp) (car ,temp)))
	((endp ,temp) ,val)
	(declare (ignorable ,temp))
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
(defmacro dotimes ((var form &optional (val nil)) &rest body)
  (declare (optimize (safety 2)))
  `(block 
    nil
    ,(cond
       ((symbolp form)
	(let ((temp (sgen "DOTIMES")))
	  `(cond ((< ,form 0)
		  (let ((,var 0))
		    (declare (fixnum ,var) (ignorable ,var))
		    ,val))
		 ((<= ,form most-positive-fixnum)
		  (let ((,temp ,form))
		    (declare (ignorable ,temp) (fixnum ,temp))
		    (do* ((,var 0 (1+ ,var))) ((>= ,var ,temp) ,val)
			 (declare (fixnum ,var))
			 ,@body)))
		 ((let ((,temp ,form))
		    (declare (ignorable ,temp))
		    (do* ((,var 0 (1+ ,var))) ((>= ,var ,temp) ,val)
			 ,@body))))))
       ((constantp form)
	(cond ((< form 0)
	       `(let ((,var 0))
		  (declare (fixnum ,var) (ignorable ,var))
		  ,val))
	      ((<= form most-positive-fixnum)
	       `(do* ((,var 0 (1+ ,var))) ((>= ,var ,form) ,val)
		     (declare (fixnum ,var))
		     ,@body))
	      (`(do* ((,var 0 (1+ ,var))) ((>= ,var ,form) ,val)
		     ,@body))))
       ((let ((temp (sgen "DOTIMES")))
	  `(let ((,temp ,form))
	     (declare (ignorable ,temp))
	     (cond ((< ,temp 0)
		    (let ((,var 0))
		      (declare (fixnum ,var) (ignorable ,var))
		      ,val))
		   ((<= ,temp most-positive-fixnum)
		    (let ((,temp ,temp))
		      (declare (ignorable ,temp) (fixnum ,temp))
		      (do* ((,var 0 (1+ ,var))) ((>= ,var ,temp) ,val)
			   (declare (fixnum ,var))
			   ,@body)))
		   ((do* ((,var 0 (1+ ,var))) ((>= ,var ,temp) ,val)
			 ,@body)))))))))


(defmacro declaim (&rest l)
  (declare (optimize (safety 2)))
  `(eval-when (compile eval load)
     ,@(mapcar (lambda (x) `(proclaim ',x)) l)))

(defmacro lambda (&whole l &rest args)
  (declare (optimize (safety 2)) (ignore args))
  `(function ,l))

(defmacro memq (a b) `(member ,a ,b :test 'eq))

(defmacro background (form) 
  (let ((x (sgen "BACKGROUND"))) 
    `(let ((,x (si::fork))) 
       (if (eql 0 (car ,x)) 
	   (progn (si::write-pointer-object ,form ,x)(bye)) 
	 ,x))))

(defmacro with-read-values ((i r b) (forms timeout) &body body)
  (let* ((m (sgen "WITH-READ-VALUES"))
	 (j (sgen "WITH-READ-VALUES"))
	 (k (sgen "WITH-READ-VALUES"))
	 (p (sgen "WITH-READ-VALUES"))
	 (pbl (length forms))
	 (pbm (1- (ash 1 pbl))))
  `(let* ((,m ,pbm)
	  (,b (list ,@(mapcar (lambda (x) `(background ,x)) forms))))
     (declare ((integer 0 ,pbm) ,m))
     (unwind-protect
	 (do nil ((= ,m 0))
	     (let ((,p (si::select-read ,b ,timeout)));;FAILURE code here on 0 return
	       (declare ((integer 0 ,pbm) ,p))
	       (do ((,i 0 (1+ ,i))(,j 1 (ash ,j 1)) (,k ,b (cdr ,k))) 
		   ((= ,i ,pbl) (setq ,m (logandc2 ,m ,p)))
		   (declare ((integer 0 ,pbl) ,i) ((integer 1 ,(1+ pbm)) ,j))
		   (when (/= 0 (logand ,j ,p))
		     (let ((,r (si::read-pointer-object (car ,k))))
		       ,@body)))))
       (dolist (,b ,b (cdr ,b)) (si::kill ,b 0))))))
  
(defmacro p-let (bindings &body body) 
  (let* ((i (sgen "PLET")) (r (sgen "PLET")) (c (sgen "PLET"))
	 (pb (remove-if 'atom bindings)))
  `(let* (,@(mapcar 'car pb) ,@(remove-if 'consp bindings))
     (with-read-values 
      (,i ,r ,c) (,(mapcar 'cadr pb) -1)
      (case ,i
	    ,@(let ((g -1)) 
		(mapcar (lambda (x) `(,(incf g) (setq ,(car x) ,r))) pb))))
     ,@body)))

(defmacro p-and (&rest forms) 
  (let* ((i (sgen "P-AND")) (r (sgen "P-AND")) (c (sgen "P-AND")) (top (sgen "P-AND")))
    `(block ,top
       (with-read-values 
	(,i ,r ,c) (,forms -1)
	(unless ,r
	  (dolist (,c ,c) (si::kill ,c 0))
	  (return-from ,top nil)))
       t)))

(defmacro p-or (&rest forms) 
  (let* ((i (sgen "P-OR")) (r (sgen "P-OR")) (c (sgen "P-OR")) (top (sgen "P-OR")))
    `(block ,top
       (with-read-values 
	(,i ,r ,c) (,forms -1)
	(when ,r
	  (dolist (,c ,c) (si::kill ,c 0))
	  (return-from ,top t)))
       nil)))


(defmacro define-compiler-macro (name vl &rest body)
  (declare (optimize (safety 2)))
  (let ((n (funid-sym name)))
    `(progn (putprop ',n
		     ,(defmacro-lambda (if (eq n name) name (cadr name)) vl body)
		     'compiler-macro-prop)
	    ',name)))

(defun compiler-macro-function (name)
  (let ((name (funid-sym name)))
    (get name 'compiler-macro-prop)))

(defun undef-compiler-macro (name)
  (let ((name (funid-sym name)))
    (remprop name 'compiler-macro-prop)))


(defvar *safe-compile* nil)
(defvar *compiler-check-args* nil)
(defvar *compiler-new-safety* nil)
(defvar *compiler-push-events* nil)
(defvar *speed* 3)
(defvar *space* 0)
(defvar *debug* 0)

(defvar *alien-declarations* nil)

(defvar *uniq-list* (make-hash-table :test 'equal))

(defun uniq-list (list) (or (gethash list *uniq-list*) (setf (gethash list *uniq-list*) list)))

(defun normalize-function-plist (plist)
  (setf (car plist) (uniq-list (car plist))
	(cadr plist) (mapcar (lambda (x)
			       (uniq-list (cons (car x) (uniq-list (cdr x)))))
			     (cadr plist)))
  plist)

(defvar *function-plists* nil);rely on defvar not resetting to nil on loading this file compiled

(defun make-function-plist (&rest args)
  (cond ((and (fboundp 'cmp-norm-tp) (fboundp 'typep))
	 (mapc 'normalize-function-plist *function-plists*)
	 (unintern '*function-plists*)
	 (defun make-function-plist (&rest args) (normalize-function-plist args))
	 (normalize-function-plist args))
	((car (push args *function-plists*)))))


(defun proclaim (decl &aux (a (car decl))(d (cdr decl)))
 (declare (optimize (safety 1)))
 (check-type decl list)
 (check-type (cdr decl) list)
 (case a
   (special (mapc (lambda (x) (check-type x symbol) (*make-special x)) d))
   (optimize
    (mapc (lambda (y &aux (x (if (symbolp y) (list y 3) y)))
	    (check-type x (cons t (cons (integer 0 3) null)))
	    (let ((a (pop x))(ad (car x)))
	      (ecase a
		(debug (setq *debug* ad))
		(safety (setq *compiler-check-args* (>= ad 1))
			(setq *safe-compile* (>= ad 2))
			(setq *compiler-new-safety* (>= ad 3))
			(setq *compiler-push-events* (>= ad 4)))
		(space (setq *space* ad))
		(speed (setq *speed* ad))
		(compilation-speed (setq *speed* (- 3 ad)))))) d))
   (type  (let ((q (car d))) (check-type q  type-spec) (proclaim d)))
   (ftype (let ((q (car d))) (check-type q ftype-spec) (proclaim d)))
   ((inline notinline)
    (mapc (lambda (x &aux (y (funid-sym x)))
	    (check-type x function-name)
	    (putprop y t (if (eq a 'inline) 'cmp-inline    'cmp-notinline))
	    (remprop y   (if (eq a 'inline) 'cmp-notinline 'cmp-inline))) d))
   ((ignore ignorable) (mapc (lambda (x) (check-type x function-name)) d))
   (declaration (mapc (lambda (x) (check-type x symbol) (pushnew x *alien-declarations*)) d))
   (otherwise
    (cond ((typep a 'type-spec) (proclaim-var a d))
	  ((typep a 'ftype-spec)); (add-function-proclamation (pop d) (cdr a) d))
	  ((unless (member a *alien-declarations*) (warn "The declaration specifier ~s is unknown." a)))
	  ((symbolp a) (let ((y (get a :proclaim))) (when y (mapc (lambda (x) (funcall y x)) d)))))))
 nil)


(defun proclaim-var (tp l &aux (tp (cmp-norm-tp tp)))
  (unless (or (eq tp '*) (eq tp t))
    (mapc (lambda (x)
	    (check-type x symbol)
	    (assert (setq tp (type-and tp (get x 'cmp-type t))))
	    (putprop x tp 'cmp-type)) l)));sch-global, improper-list
