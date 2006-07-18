;; -*-Lisp-*-
(in-package 'si)

(*make-special '*pahl*)
(*make-special '*boot*)
(*make-special '*tmp-dir*)
(eval-when (load eval)
	   (setq *pahl* nil)
	   (setq *boot* nil))

(export '(function-lambda-expression))

(defun add-hash (fn sig callees src)
  (cond ((not (eq *boot* t))
	 (setq *pahl* (cons `(add-hash ',fn ',sig ',callees ,src) *pahl*))
	 (unless (or (not (fboundp 'make-s-data)) (not (let ((s (find-symbol "FIND"))) (and s (fboundp s)))) *boot*)
	   (setq *boot* 'add-hash) 
	   (let ((*package* (find-package "SI")))
	     (defstruct (call (:copier copy-call)
			      (:predicate call-p)
			      (:constructor make-call))
			      sig callees callers src)
	     (defvar *call-hash-table* (make-hash-table :test 'eq))
	     (defvar *needs-recompile* (make-array 10 :fill-pointer 0 :adjustable t))
	     (defvar *ach* (make-hash-table :test 'eq))
	     (defvar *acr* (make-hash-table :test 'eq))
	     (setq *tmp-dir* (get-temp-dir))
	     (setq *boot* t)
	     (mapc 'eval (nreverse *pahl*))
	     (setq *pahl* nil))))
	((let ((h (or (gethash fn *call-hash-table*)
		      (setf (gethash fn *call-hash-table*) (make-call :sig sig)))))
	   (when sig 
	     (unless (eq (cadr sig) '*) (putprop fn t 'proclaimed-function))
	     (putprop fn (car sig) 'proclaimed-arg-types)
	     (putprop fn (cadr sig) 'proclaimed-return-type))
;	     (proclaim `(ftype (function ,@sig) ,fn)))
	   (when (and sig (not (equal sig (call-sig h))))
	     (dolist (l (call-callers h))
	       (unless (eq l fn)
		 (add-recompile l fn (call-sig h) sig)))
	     (setf (call-sig h) sig))
	   (when src (setf (call-src h) src))
	   (let (ar)
	     (dolist (l callees (unless ar (when sig (remove-recompile fn))))
	       (pushnew (car l) (call-callees h))
	       (let ((h (or (gethash (car l) *call-hash-table*)
			    (setf (gethash (car l) *call-hash-table*) (make-call :sig (cdr l) :callers (list fn))))))
		 (pushnew fn (call-callers h))
		 (unless (or (eq fn (car l)) (equal (cdr l) (call-sig h)))
		   (add-recompile fn (car l) (cdr l) (call-sig h))
		   (setq ar t)))))))))

(defun clear-compiler-properties (sym code)
  (cond ((not (eq *boot* t))
	 (push `(clear-compiler-properties ',sym nil) *pahl*))
	((let ((h (or (gethash sym *call-hash-table*) 
		      (setf (gethash sym *call-hash-table*) (make-call)))))
	   (dolist (l (call-callees h))
	     (let ((l (gethash l *call-hash-table*)))
	       (setf (call-callers l) (delete sym (call-callers l)))))
	   (remhash sym *ach*)
	   (remhash sym *acr*)
	   (unless *disable-recompile*
	     (let ((x (get sym 'state-function)))
	       (when x
		 (let ((o (old-src x)))
		   (mapc (lambda (x) (remprop x 'state-function)) (car o))
		   (mapc (lambda (x y) 
			   (unless (eq sym x)
			     (eval `(defun ,x ,@(cdr y))))) (car o) (cadr o))
		   (unintern x)))))
	   (let (new)
	     (maphash (lambda (x y) 
			(when (and (fboundp x) (eq (symbol-function x) code) (call-src y))
			  (setq new x))) *call-hash-table*)
	     (cond (new
		    (let ((nr (find new *needs-recompile* :key 'car)))
		      (when nr (add-recompile sym (cadr nr) (caddr nr) (cadddr nr))))
		    (setq new (gethash new *call-hash-table*))
		    (let ((ns (call-sig new)))
		      (unless (equal ns (call-sig h))
			(dolist (l (call-callers h))
			  (add-recompile l sym (call-sig h) ns)))
		      (setf (call-sig h) ns)
		      (proclaim `(ftype (function ,@ns) ,sym)))
		    (dolist (l (call-callees new)) 
		      (pushnew sym (call-callers (gethash l *call-hash-table*))))
		    (setf (call-callees h) (call-callees new) (call-src h) (call-src new)))
		   ((progn
		      (remove-recompile sym)
		      (setf (call-callees h) nil (call-src h) nil)))))))))

(defun add-recompile (fn why assumed-sig actual-sig)
  (unless (find fn *needs-recompile* :key 'car)
;    (format t "add-recompile ~s ~s ~s ~s~%" fn why assumed-sig actual-sig)
    (vector-push-extend (list fn why assumed-sig actual-sig) *needs-recompile*)
    nil))

(defun remove-recompile (fn)
  (let ((p (position fn *needs-recompile* :key 'car)))
    (when p
;      (format t "removing recompile of ~s~%" fn)
      (decf (fill-pointer *needs-recompile*))
      (do ((i p (1+ i))) ((= i (length *needs-recompile*)))
	(setf (aref *needs-recompile* i) (aref *needs-recompile* (1+ i)))))))

(defun clr-call nil 
  (clrhash *call-hash-table*)
  (setf (fill-pointer *needs-recompile*) 0))


(defun all-callees (x y)
  (let ((z (gethash x *ach*)))
    (if z (union z y)
      (let ((z (call-callees (gethash x *call-hash-table*))))
	(do ((l (set-difference z y) (cdr l))
	     (r (union z y) (all-callees (car l) r)))
	    ((endp l) 
	     (unless (intersection z y) (setf (gethash x *ach*) (set-difference r y)))
	     r))))))

(defun all-callers (x y)
  (let ((z (gethash x *acr*)))
    (if z (union z y)
      (let ((z (call-callers (gethash x *call-hash-table*))))
	(do ((l (set-difference z y) (cdr l))
	     (r (union z y) (all-callers (car l) r)))
	    ((endp l) 
	     (unless (intersection z y) (setf (gethash x *acr*) (set-difference r y)))
	     r))))))

(defun block-lambda (ll block body)
  (let* ((z body)
	 (doc (when (and z (stringp (car z))) (list (pop z))))
	 (decls (let (d) (do nil ((or (not z) (not (consp (car z))) (not (eq (caar z) 'declare))) (nreverse d))
			      (push (pop z) d))))
	 (rest z))
  `(lambda ,ll ,@doc ,@decls (block ,block ,@rest))))
       

(defun function-lambda-expression (x) 
  (if (typep x 'interpreted-function) 
      (let* ((x (si::interpreted-function-lambda x)))
	(case (car x)
	      (lambda (values x nil nil))
	      (lambda-block (values (block-lambda (caddr x) (cadr x) (cdddr x)) nil (cadr x)))
	      (lambda-closure (values (cons 'lambda (cddr (cddr x)))  (not (not (cadr x)))  nil))
	      (lambda-block-closure (values (block-lambda (caddr (cdddr x)) (cadr (cdddr x)) (cddr (cddr (cddr x)))) 
				     (not (not (cadr x))) (fifth x)))
	      (otherwise (values nil t nil))))
    (values nil t nil)))

(defun function-src (sym)
  (or
   (let* ((h (gethash sym *call-hash-table*))
	  (fas (when h (call-src h))))
     (when fas
       (let* ((ss (open-fasd (make-string-input-stream fas) :input 'eof nil))
	      (out (read-fasd-top ss)))
	 (close-fasd ss)
	 out)))
   (and (fboundp sym) (typep (symbol-function sym) 'interpreted-function) (function-lambda-expression (symbol-function sym)))))

(defun nsyms (n &optional syms)
  (declare (seqind n))
  (cond ((= n 0) (nreverse syms))
	((nsyms (1- n) (cons (gensym) syms)))))

(defun max-types (sigs &optional res)
  (cond ((not res) (max-types (cdr sigs) (ldiff (caar sigs) (member '* (caar sigs)))))
	((not sigs) res)
	((max-types (cdr sigs) 
		    (let ((z (ldiff (caar sigs) (member '* (caar sigs)))))
		      (append
		       (mapcar (lambda (x y) (or (not (equal x y)) x)) z res)
		     (early-nthcdr (length z) res)))))))

(defun early-nthcdr (i x)
  (declare (seqind i))
  (cond ((= 0 i) x)
	((early-nthcdr (1- i) (cdr x)))))

(defun old-src (stfn &optional src syms sts srcs)
  (cond (stfn (old-src nil (function-src stfn) syms sts srcs))
	((atom src) nil)
	((eq (car src) 'macrolet)
	 (list (mapcar 'car (cadr src)) 
	       (mapcar (lambda (x) (if (eq (caadr x) 'funcall) (cadadr x) (caadr x))) (cddr (caddr src)))))
	((or (old-src stfn (car src) syms sts srcs) (old-src stfn (cdr src) syms sts srcs)))))

(defun lambda-vars (ll)
  (remove '&optional (mapcar (lambda (x) (if (consp x) (car x) x)) ll)))

(defun inlinef (n syms sts fns)
    (unless (some (lambda (x) (intersection '(&rest &key &aux &allow-other-keys) (cadr x))) fns)
      (let* ((lsst (1- (length sts)))
	     (tps (max-types (mapcar 'sig syms)))
	     (min (reduce 'min (mapcar (lambda (x) 
					 (let* ((ll (cadr x))) 
					   (- (length ll) (length (member '&optional ll))))) fns)))
	     (max (reduce 'max (mapcar (lambda (x) (length (lambda-vars (cadr x)))) fns)))
	     (reqs (nsyms min))
	     (opts (nsyms (- max min)))
	     (ll (append reqs (when (> max min) (cons '&optional opts))))
	     (all (reverse (append reqs opts))))
	`(defun ,n ,(cons 'state ll)
	   (declare (fixnum state) ,@(mapcar 'list tps reqs))
	   ,@(let (d (z (cddr (car fns)))) 
	       (when (stringp (car z)) (pop z))
	       (do nil ((or (not z) (not (consp (car z))) (not (eq (caar z) 'declare))) (nreverse d)) 
		   (let ((q (pop z)))
		     (when (and (consp (cadr q)) (eq 'optimize (caadr q))) 
		       (push q d)))))
	   (macrolet ,(mapcan (lambda (x y z) `((,x ,(cadr y) `(,',n ,,z ,,@(lambda-vars (cadr y)))))) syms fns sts)
		     (case state
			   ,@(mapcar (lambda (x y)
				       `(,(if (= x lsst) 'otherwise x) 
					 (funcall ,y ,@(reverse 
							(early-nthcdr 
							 (- max (length (lambda-vars (cadr y))))
							 all))))) sts fns)))))))

(defun sig (x) (let ((h (gethash x *call-hash-table*))) (when h (call-sig h))))
(defun callees (x) (let ((h (gethash x *call-hash-table*))) (when h (call-callees h))))
(defun callers (x) (let ((h (gethash x *call-hash-table*))) (when h (call-callers h))))
(defun *s (x) 
  (let ((p (find-package x)))
    (remove-if-not
     (lambda (y) (eq (symbol-package y) p)) 
     (let (r) 
       (maphash (lambda (x y) (when (eq '* (cadr (call-sig y))) (push x r))) *call-hash-table*)
       r))))

(defun mutual-recursion-peers (sym)
  (let ((y (sig sym)))
    (when (eq '* (cadr y)) 
      (let* ((e (all-callees sym nil))
	     (r (all-callers sym nil))
	     (i (intersection e r)))
	(remove-if (lambda (x) (or (get x 'mutual-recursion-group)
				   (get x 'state-function)))
		     (remove-if-not (lambda (x) (eq '* (cadr (sig x)))) i))))))

(defun convert-to-state (sym)
  (let ((syms (mutual-recursion-peers sym)))
    (when (and (remove sym syms) (member sym syms))
      (let* ((fns (mapcar 'function-src syms))
	     (n (intern (symbol-name (gensym (symbol-name sym))) (symbol-package sym)))
	     (sts (let (sts) (dotimes (i (length syms) (nreverse sts)) (push i sts))))
	     (ns (inlinef n syms sts fns)))
	(when ns
	  (mapc (lambda (x y z) (let ((z (cadr z))) (eval `(defun ,x ,z (,n ,y ,@(remove '&optional z)))))) syms sts fns)
	  (mapc (lambda (x) (putprop x n 'state-function)) syms)
	  (eval ns)
	  (dolist (l syms) (add-hash l nil (list (list n)) nil))
	  (putprop n syms 'mutual-recursion-group)
	  (add-recompile n 'mutual-recursion nil nil)
	  n)))))
    
(defun temp-prefix nil
  (concatenate 'string *tmp-dir* "gazonk_" (write-to-string (abs (si::getpid))) "_"))

(defun recompile (fn)
  (with-temp-file 
      (s tpn) ((temp-prefix) "lsp")
      (let ((*print-radix* nil)
	    (*print-base* 10)
	    (*print-circle* t)
	    (*print-pretty* nil)
	    (*print-level* nil)
	    (*print-length* nil)
	    (*print-case* :downcase)
	    (*print-gensym* t)
	    (*print-array* t)
	    (si::*print-package* t)
	    (si::*print-structure* t))
	(let* ((src (function-src fn)))
	  (if src (prin1 `(defun ,fn ,@(cdr src)) s)
	    (remove-recompile fn))
	  (let ((o (compile-file tpn)))
	    (load o)
	    (delete-file o))))))

(defun get-temp-dir ()
  (dolist (x `(,@(mapcar 'si::getenv '("TMPDIR" "TMP" "TEMP")) "/tmp" ""))
    (when x
      (let* ((x (pathname x))
	     (x (if (pathname-name x) x 
		  (merge-pathnames
		   (make-pathname :directory (butlast (pathname-directory x)) 
				  :name (car (last (pathname-directory x))))
		   x))))
	(when (directory x) 
	  (return-from 
	   get-temp-dir 
	   (namestring 
	    (make-pathname 
	     :device (pathname-device x)
	     :directory (when (or (pathname-directory x) (pathname-name x))
			  (append (pathname-directory x) (list (pathname-name x))))))))))))

(defun do-recompile (&optional pn)
  (unless (or *disable-recompile* (= 0 (length *needs-recompile*)))
    (let ((*disable-recompile* t))
      (clrhash *ach*)
      (clrhash *acr*)
      (maphash (lambda (x y) (declare (ignore y)) (convert-to-state x)) *call-hash-table*)
      (sort *needs-recompile*
	    (lambda (x y) 
	      (member (car x) (all-callees (car y) nil))))
      (map nil (lambda (fn)
		 (format t "Callee ~s sigchange ~s to ~s, recompiling ~s~%" 
			 (cadr fn) (caddr fn) (cadddr fn) (car fn))) *needs-recompile*)
      (let ((f (when pn (open1 pn :direction :output :if-exists :append :if-does-not-exist :create))))
	(with-temp-file 
	    (s tpn) ((temp-prefix) "lsp")
	    (let ((*print-radix* nil)
		  (*print-base* 10)
		  (*print-circle* t)
		  (*print-pretty* nil)
		  (*print-level* nil)
		  (*print-length* nil)
		  (*print-case* :downcase)
		  (*print-gensym* t)
		  (*print-array* t)
		  (si::*print-package* t)
		  (si::*print-structure* t))
	      (dotimes (i (length *needs-recompile*))
		(let* ((fn (car (aref *needs-recompile* i)))
		       (src (function-src fn)))
		  (cond (src 
			 (prin1 `(defun ,fn ,@(cdr src)) s)
			 (when f 
			   (prin1 `(defun ,fn ,@(cdr src)) f)
			   (let ((x (get fn 'state-function)))
			     (when x (prin1 `(putprop ',fn ',x 'state-function) f)))
			   (let ((x (get fn 'mutual-recursion-group)))
			     (when x (prin1 `(putprop ',fn ',x 'mutual-recursion-group) f)))))
			((remove-recompile fn))))))
	    (when f (close f))
	    (let ((o (compile-file tpn)))
	      (load o)
	      (delete-file o)))))
    (cond ((> (length *needs-recompile*) 0) (do-recompile pn))
	  (pn (compile-file pn :system-p t :c-file t :h-file t :data-file t)))))

;FIXME!!!
(defun is-eq-test-item-list (x y z w)
  (format t "Should never be called ~s ~s ~s ~s~%" x y z w))

(defun cmp-vec-length (x)
  (declare (vector x))
  (if (array-has-fill-pointer-p x) (fill-pointer x) (array-dimension x 0)))

