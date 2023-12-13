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
		  *compiler-push-events* *space* *speed* proclaimed-signature
 		  *alien-declarations* write-sys-proclaims make-function-plist
 		  lit sgen cmp-inline cmp-notinline cmp-type))

;(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))
;(eval-when (eval compile) (defun si:clear-compiler-properties (symbol)))
(eval-when (eval compile)
  (setq si:*inhibit-macro-special* nil)
  (defmacro ?cons (f x &aux (s (sgen "?CONS"))) `(let ((,s ,x)) (if (cdr ,s) (cons ,f ,s) (car ,s))))
  (defmacro ?list (x &aux (s (sgen "?LIST"))) `(let ((,s ,x)) (when ,s (list ,s))))
  (defmacro zcollect (v r rp np &aux (s (sgen "ZCOLLECT")))
    `(let ((,s ,v)) (setf rp (if rp (rplacd rp (list ,s)) (setq r ,s)) rp np)))
  (defmacro ?let (k kf r) `(let ((r ,r)) (if (eq ,k ,kf) r `(let ((,,k ,,kf)) (declare (ignorable ,,k)) ,r))))
  (defmacro ?key (x &aux (s (sgen "?KEY"))) `(if (or (constantp ,x) (symbolp ,x)) ,x ',s)))

(defun lit (&rest r)
  (error "lit called with args ~s~%" r))

(defmacro sgen (&optional (pref "G"))
  `(load-time-value (gensym ,pref)))

(defmacro defvar (var &optional (form nil form-sp) doc-string)
  (declare (optimize (safety 2)))
  `(progn (*make-special ',var)
	  ,@(when doc-string `((set-documentation ',var 'variable ,doc-string)))
	  ,@(when form-sp `((unless (boundp ',var) (setq ,var ,form))))
	  ',var))

(defmacro defparameter (var form &optional doc-string)
  (declare (optimize (safety 2)))
  `(progn (*make-special ',var)
	  ,@(when doc-string `((set-documentation ',var 'variable ,doc-string)))
	  (setq ,var ,form)
	  ',var))

(defmacro defconstant (var form &optional doc-string)
  (declare (optimize (safety 2)))
  `(progn (*make-constant ',var ,form)
	  ,@(when doc-string `((set-documentation ',var 'variable ,doc-string)))
	  ',var))


;;; Each of the following macros is also defined as a special form.
;;; Thus their names need not be exported.

(defmacro and (&rest forms &aux r rp np);FIXME simplify with recursive labels
  (declare (optimize (safety 2)))
  (do ((y forms))((endp y) (if forms r t))
    (let ((x (pop y)))
      (if (constantp x) (unless (if (eval x) y) (zcollect x r rp np) (setq y nil))
	(if y (zcollect `(if ,@(setq np (list x))) r rp np)
	  (zcollect x r rp np))))))

(defmacro or (&rest forms &aux r rp np (s (sgen "OR")))
  (declare (optimize (safety 2)))
  (do ((y forms))((endp y) r)
    (let ((x (pop y)))
      (if (constantp x) (when (eval x) (zcollect x r rp np) (setq y nil))
	(if (symbolp x) (zcollect `(if ,x ,@(setq np (list x))) r rp np)
	  (if y (zcollect `(let ((,s ,x)) (if ,s ,@(setq np (list s)))) r rp np)
	    (zcollect x r rp np)))))))

;; ,@(mapcan (lambda (x &aux (z (pop x))(z (if (eq z 'type) (pop x) z)))
;; 		      (case z
;; 			    ((ftype inline notinline optimize special dynamic-extent) nil)
;; 			    (otherwise (mapcar (lambda (x) (list x x)) x))))
;; 		   (apply 'append (mapcar 'cdr dec)))
(defmacro locally (&rest body)
  (declare (optimize (safety 2)))
  `(let () ,@body))

(defmacro loop (&rest body &aux (tag (sgen "LOOP")))
  `(block nil (tagbody ,tag ,(?cons 'progn body) (go ,tag))))

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
	   (load-time-value (or (find-package 'setf) (make-package-int 'setf nil nil))))))

(defmacro defmacro (name vl &rest body)
  (declare (optimize (safety 2)))
  `(let ((.fn. ,(defmacro-lambda name vl body)))
     (setf (macro-function ',name) .fn.)
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
  (assert (evenp (length args)))
  (let ((x (let ((i 0)) (mapcon (lambda (x) (when (oddp (incf i)) `((,(cadr x) ,(car x) ,(gensym))))) args))))
    (when x
      `(let* ,(mapcar (lambda (x) `(,(caddr x) ,(car x))) x)
	 (setq ,@(mapcan 'cdr x))
	 nil))))

; conditionals
(defmacro cond (&rest clauses &aux r rp np (s (sgen "COND")))
  (declare (optimize (safety 2)))
  (do ((y clauses))((endp y) r)
    (let* ((x (pop y))(z (pop x)))
      (if (constantp z) (when (eval z) (zcollect (if x (?cons 'progn x) z) r rp np) (setq y nil))
	(if x (zcollect `(if ,z ,@(setq np (list (?cons 'progn x)))) r rp np)
	  (if (symbolp z) (zcollect `(if ,z ,@(setq np (list z))) r rp np)
	    (if y (zcollect `(let ((,s ,z)) (if ,s ,@(setq np (list s)))) r rp np)
	      (zcollect `(values ,z) r rp np))))))))

(defmacro when (pred &rest body &aux (x (?cons 'progn body)))
  (declare (optimize (safety 2)))
  (if (constantp pred) (if (eval pred) x) `(if ,pred ,x)))

(defmacro unless (pred &rest body &aux (x (?cons 'progn body)))
  (declare (optimize (safety 2)))
  (if (constantp pred) (if (not (eval pred)) x) `(if (not ,pred) ,x)))

; program feature

(defun prog?* (let?* vl body)
  (multiple-value-bind
      (doc dec ctp body)
      (parse-body-header body)
    (declare (ignore doc))
    `(block nil (,let?* ,vl ,@dec (tagbody ,@(append ctp body))))))

(defmacro prog (vl &rest body)
  (declare (optimize (safety 2)))
  (prog?* 'let vl body))

(defmacro prog* (vl &rest body)
  (declare (optimize (safety 2)))
  (prog?* 'let* vl body))

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
    `(multiple-value-bind ,syms ,form ,@(?list (?cons 'setq (mapcan 'list vars syms))) ,(car syms))))

(defmacro multiple-value-bind (vars form &rest body &aux (sym (sgen "MULTIPLE-VALUE-BIND")))
  (declare (optimize (safety 2)))
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
	   (mapcar (lambda (x) (if (listp x) (ldiff-nf x (cddr x)) x)) control)
	   dec
	   (?tagbody
	    label
	    `(unless ,test
	       ,@(?list (?cons 'tagbody (append ctp body)))
	       ,@(?list (?cons (if ?* 'setq 'psetq) (mapcan (lambda (x) (when (and (listp x) (cddr x)) (list (car x) (caddr x)))) control)))
	       (go ,label))
	    `(return ,(?cons 'progn result))))))))

(defmacro do (control (test . result) &rest body)
  (declare (optimize (safety 2)))
  (do?* nil control test result body))

(defmacro do* (control (test . result) &rest body)
  (declare (optimize (safety 2)))
  (do?* t control test result body))

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

(defmacro ecase (keyform &rest clauses &aux (key (?key keyform)))
  (declare (optimize (safety 2)))
  (?let key keyform
	`(case ,key
	   ,@(mapcar (lambda (x) (if (member (car x) '(t otherwise)) (cons (list (car x)) (cdr x)) x)) clauses)
	   (otherwise
	    (error 'type-error :datum ,key
		   :expected-type '(member ,@(apply 'append (mapcar (lambda (x &aux (x (car x))) (if (listp x) x (list x))) clauses))))))))

(defmacro ccase (keyform &rest clauses &aux (key (?key keyform)))
  (declare (optimize (safety 2)))
  (?let key keyform
	`(do nil (nil);FIXME block
	   (case ,key
	     ,@(mapcar (lambda (x &aux (k (pop x)))
			 `(,(if (member k '(t otherwise)) (list k) k) (return ,(?cons 'progn x))))
		       clauses)
	     (otherwise
	      (check-type ,key (member ,@(apply 'append (mapcar (lambda (x &aux (x (car x))) (if (listp x) x (list x))) clauses)))))))))


(defmacro return (&optional (val nil))   (declare (optimize (safety 2))) `(return-from nil ,val))

(defmacro dolist ((var form &optional (val nil)) &rest body
                                                 &aux (temp (sgen "DOLIST")))
  (declare (optimize (safety 2)))
  `(do* ((,temp ,form (cdr ,temp))
	 (,var (car ,temp) (car ,temp)))
	((endp ,temp) ,val)
	(declare (ignorable ,temp))
	,@body))

;FIXME try labels
(defmacro dotimes ((var form &optional val) &rest body
		   &aux (s (sgen "DOTIMES"))(m (sgen "DOTIMES")))
  (declare (optimize (safety 1)))
  `(let* ((,s (block nil ,form))(,m (min (max 0 ,s) most-positive-fixnum)))
     (check-type ,s integer)
     (do ((,var 0 (1+ ,var)))
	 ((>= ,var ,m)
	  (when (> ,s most-positive-fixnum)
	    (do ((,var most-positive-fixnum (1+ ,var)))((>= ,var ,s)) ,@body))
	  ,val);FIXME bbumps to non-negative-integer
       ,@body)))


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


(defmacro define-compiler-macro (name vl &rest body &aux (n (funid-sym name)) (q (gensym (string n))))
  (declare (optimize (safety 2)))
  `(progn (defun ,q ,@(cdr (defmacro-lambda (if (eq n name) name (cadr name)) vl body)))
	  (putprop ',n ;FIXME setf not available at pre_gcl stage
		   (symbol-function ',q)
		   'compiler-macro-prop)
	  ',name))

(defun undef-compiler-macro (name)
  (remprop (funid-sym name) 'compiler-macro-prop))

(defun compiler-macro-function (n &optional env &aux (n (funid-sym n)))
  (declare (ignorable env))
  (get n 'compiler-macro-prop))

(defun (setf compiler-macro-function) (fun n &optional env &aux (n (funid-sym n)))
  (declare (ignorable env))
  (setf (get n 'compiler-macro-prop) fun))


(defvar *safe-compile* nil)
(defvar *compiler-check-args* nil)
(defvar *compiler-new-safety* nil)
(defvar *compiler-push-events* nil)
(defvar *speed* 3)
(defvar *space* 0)
(defvar *debug* 0)

(defvar *alien-declarations* nil)

(defvar *uniq-list* (gcl-make-hash-table 'equal))

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
   (type  (let ((q (pop d))) (check-type q  type-spec) (proclaim-var q d)))
   (ftype (let ((q (pop d))) (check-type q ftype-spec) (proclaim-ftype q d)))
   ((inline notinline)
    (mapc (lambda (x &aux (y (funid-sym x)))
	    (check-type x function-name)
	    (putprop y t (if (eq a 'inline) 'cmp-inline    'cmp-notinline))
	    (remprop y   (if (eq a 'inline) 'cmp-notinline 'cmp-inline)))
	  d))
   ((ignore ignorable) (mapc (lambda (x) (check-type x function-name)) d))
   (declaration (mapc (lambda (x) (check-type x symbol) (pushnew x *alien-declarations*)) d))
   (otherwise
    (cond ((when (symbolp a) (cmp-norm-tp a)) (proclaim-var a d))
	  ((unless (member a *alien-declarations*) (warn "The declaration specifier ~s is unknown." a)))
	  ((symbolp a) (let ((y (get a :proclaim))) (when y (mapc (lambda (x) (funcall y x)) d)))))))
 nil)


(defun proclaim-var (tp l &aux (tp (cmp-norm-tp tp)))
  (declare (optimize (safety 2)))
  (unless (or (eq tp '*) (eq tp t))
    (mapc (lambda (x)
	    (check-type x symbol)
	    (assert (setq tp (type-and tp (get x 'cmp-type t))))
	    (putprop x tp 'cmp-type)) l)));sch-global, improper-list

(defun readable-sig (sig)
  (list (mapcar 'cmp-unnorm-tp (car sig)) (cmp-unnorm-tp (cadr sig))))

(defun type= (t1 t2)
  (when (type>= t1 t2)
    (type>= t2 t1)))

(defun sig= (s1 s2)
  (when (eql (length (car s1)) (length (car s2)))
    (unless (notevery 'type= (car s1) (car s2))
      (type= (cadr s1) (cadr s2)))))

;FIXME, implement these in place of returns-exactly, etc.
(defun ftype-to-sig (ftype &aux (a (pop ftype))(d (car ftype)))
  (let* ((x (member-if (lambda (x) (member x '(&optional &rest &key))) a))
	 (a (nconc (ldiff a x) (when x '(*))))
	 (x (when (and (listp d) (eq (car d) 'values)) d))
	 (y (member '&optional x))
	 (z (member-if (lambda (x) (member x '(&rest &allow-other-keys))) x))
	 (d (cond (z '*)(y (remove '&optional d))(x `(returns-exactly ,@(cdr d)))(d))))
    (list a d)))

(defun norm-possibly-unkown-type (type &aux (tp (cmp-norm-tp type)))
  (flet ((fix (tp) (or tp t)))
    (cond ((cmpt tp) `(,(pop tp) ,@(mapcar #'fix tp)))
	  ((fix tp)))))

(defun proclaim-ftype (ftype var-list
		       &aux  (sig (ftype-to-sig (cdr ftype)))
			 (sig (uniq-list (list (mapcar 'norm-possibly-unkown-type (car sig))
					       (norm-possibly-unkown-type (cadr sig))))))
  (declare (optimize (safety 2)))
  (mapc (lambda (x &aux (c (car (call x))))
	  (cond (c (unless (sig= c sig)
		     (warn "Ignoring proclaimed signature ~s on ~s, currently fbound to ~s~%"
			   (readable-sig sig) x (readable-sig c))))
		((setf (get x 'proclaimed-signature) sig))))
	var-list))

(defun write-sys-proclaims (fn &rest package-list &aux
				  (pl (mapcar 'find-package (or package-list (list-all-packages))))
				  (h (make-hash-table :test 'eq))
				  (*print-readably* t))
  (with-open-file
   (q fn :direction :output)
   (do-all-symbols
    (s)
    (when (member (symbol-package s) pl)
      (let ((x (sig s)))
	(when x
	  (setf (gethash x h)
		(adjoin s (gethash x h)))))))
   (maphash (lambda (x y)
	      (print `(proclaim '(ftype (function ,(mapcar 'cmp-unnorm-tp (car x)) ,(cmp-unnorm-tp (cadr x))) ,@y))
		     q))
	    h)))
