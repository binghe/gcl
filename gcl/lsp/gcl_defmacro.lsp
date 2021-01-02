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


;;;;    defmacro.lsp
;;;;
;;;;         defines SI:DEFMACRO*, the defmacro preprocessor


;; (in-package :lisp)
;; (export '(lambda defvar import &whole &environment &body))


(in-package :system)


;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
;;; (DEFMACRO <name> <symbol> ... ) is equivalent to
;;; (DEFMACRO <name> (&REST <symbol>) ...).
;;; Defamcro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

;; defvar is not yet available.
(mapc '*make-special '(*dl* *key-check* *arg-check*))


(defun get-&environment(vl &aux env)
  (let ((env-m
	 (and (listp vl)
	      (do ((tail vl (cdr tail)))
		  ((not (consp tail)) nil)
		(when (eq '&environment (car tail))
		  (return tail))))))
    (cond (env-m
 	   (setq env (cadr env-m))
 	   (setq vl (append (ldiff vl env-m) (cddr env-m)))))
    (values vl env)))



(defun gensym (&optional (x nil xp))
  (cond ((not xp) (gensym0))
	((stringp x) (gensym1s x))
	((gensym1ig x))))
			 
(let* ((gsyms (mapl #'(lambda (x) (setf (car x) (gensym))) (make-list 50)))(syms gsyms))
  (defun tsym (&optional r)
    (cond (r (setq syms gsyms) nil)
	  ((or (pop syms) (error 'program-error :format-control "Out of symbols when binding lambda list" :format-arguments nil))))))

(defun unbnd (k l &aux (lc (when (or (eq k '&optional) (eq k '&key)) (consp l)))
	      (ln (if lc (pop l) l)) (ld (when lc (pop l))) (lp (when lc (car l)))
	      (lc (when (eq k '&key) (consp ln))) (lnn (if lc (pop ln) ln)) (lb (if lc (car ln) ln)))
	   (values lnn lb ld lp))

(*make-special '*rv*)
(*make-constant '+kev+ (gensym "KE"))
(setq *rv* nil)

(let ((vp (gensym "VP"))(val (gensym "VAL")))
  (defun rpop (rv lv np negp &aux )
    `(do (,vp ,val)
	 ((>= 0 ,np) ,lv)
	 (declare (proper-list ,val) ,@(when (cdr rv) `((:dynamic-extent ,val))))
	 (setq ,val ,(vp)
	       ,val (if (and ,negp (= ,np 0)) ,val (cons ,val nil))
	       ,vp (cond (,vp (rplacd ,vp ,val) ,val) ((setq ,lv ,val)))))))

(defun bind (targ &optional (src nil srcp) defp &aux (sp (when (listp targ) srcp)) (v (if sp (tsym) targ)))
  (push (if srcp (list v (if defp (na src) src)) v) *rv*)
  (when sp
    (let ((x (tsym))(lv (lvp)))
      (bind x lv)
      (setq *rv* (nreconc (cadr (blla targ nil v nil nil nil nil t)) *rv*))
      (bind lv x)))
  v)

(defun badll (x) (error 'program-error :format-control "Bad lambda list ~s" :format-arguments (list x)))
(defun insuf (x) `(error 'program-error :format-control "Insufficient arguments when binding ~s" :format-arguments (list ',x)))
(defun extra (x) `(error 'program-error :format-control "Extra argument ~s" :format-arguments (list ,x)))
(defun nokv  (x) `(error 'program-error :format-control "Key ~s missing value" :format-arguments (list ,x)))
(defun badk  (x v) `(error 'program-error :format-control "Key ~s ~s not permitted" :format-arguments (list ,x ,v)))

(defun wcr (x) (when (cdr x) x))

(let (n nr f np lv a last
	(+np+ (gensym "NP"))
	(+negp+ (gensym "NEGP"))
	(+ff+ (gensym "FF"))
	(+lvpv+ (gensym "LVP")))

  (defun state-init (nn nnr nf na nlast);FIXME, illogical wrt. recursion
    (setq n nn nr nnr f nf a na last nlast np nil lv nil))

  (defun vp (&aux (v `(va-pop)))
    `(progn
       (setq ,+np+ (number-minus ,+np+ 1))
       ,(if f `(cond (,+ff+ (setq ,+ff+ nil) ,f)(,v)) v)))

  (defun la (def &optional k p &aux (v (lvp))(vp (vp)))
    (wcr `(cond ,@(when np `(((and ,+negp+ (= ,np 1) (setq ,v ,vp) ,@(unless p `(nil))))))
		,@(when np `(((> ,np 0) ,@(unless p `(,vp)))))
		,@(when v `((,v ,@(unless p `((pop ,v))))))
		,@(when def `((,def)))
		,@(when k `((,(nokv k)))))))
  (defun na (&optional def) (if a (pop a) (la def)))
  (defun nap nil (if a t (la nil nil t)))

  (defun bind-a nil (setq a (mapcar #'(lambda (x) (bind (tsym) x)) a)))

  (defun lvp (&optional rv)
    (cond
     (rv (lvp) (if np (bind lv (rpop rv lv np +negp+)) lv))
     (lv)
     (last (bind (setq lv +lvpv+) last))
     (n
      (when f (bind +ff+ t))
      (bind (setq np +np+) n)
      (bind +negp+ `(< ,np 0))
      (bind np `(if ,+negp+ (number-minus 0 ,np) ,np))
      (bind np `(number-minus ,np ,nr))
      (bind (setq lv +lvpv+)))))

  (defun post (post nkys &aux lv)
    (setq nkys (nreverse nkys))
    (do ((ex a)) ((not ex));FIXME  this is fragile as the binding must be visible to mvars/inls
	(bind 'k (pop ex))
	(bind 'v (if ex (pop ex) (la nil 'k)))
	(bind +kev+ `(case k ,@nkys)))
    (cond
     (n (bind +kev+ `(do (k v) ((not ,(nap)))
			  (setq k ,(la nil) v ,(la nil 'k));(if ,(la nil t) ,(la nil 'done) ,(nokv 'k))
			  (case k ,@nkys))))
     ((setq lv (lvp)) (bind +kev+ `(labels ((kb (k v) (case k ,@nkys))
					     (kbb (x) (when x (kb (car x) (if (cdr x) (cadr x) ,(nokv '(car x))))(kbb (cddr x)))))
					    (kbb ,lv)))))
    (mapc #'(lambda (x) (apply 'bind x)) (nreverse post)))
  )

(defun sdde (rv decls)
  (mapc #'(lambda (decl)
	  (mapc #'(lambda (clause)
		  (case
		   (pop clause)
		   ((dynamic-extent :dynamic-extent)
		    (when (member rv clause)
		      (return-from sdde t)))))
		(cdr decl)))
	decls)
  nil)

(defun blla (l a last body &optional n nr f (rcr (tsym t))
	       &aux rvd kk *rv* k tmp nkys post wv rv aok kev
	       (l (subst '&rest '&body (let ((s (last l))) (if (cdr s) (append (butlast l) (list (car s) '&rest (cdr s))) l))))
	       (lo l)(llk '(&whole &optional &rest &key &allow-other-keys &aux)))
  (declare (optimize (safety 0)))
;  (assert (not (and last n)))

  (state-init n nr f a last)

  (multiple-value-bind
   (doc decls ctps body) (parse-body-header body) (declare (ignore doc))

   (do ((l l)(lk llk))((not l))
      (cond ((setq tmp (member (car l) lk)) (setq lk (cdr tmp) k (pop l) kk (if (eq k '&aux) kk k)))
	    ((member (car l) llk) (badll lo))
	    ((case k
		   (&whole (when (or wv (not (eq (cdr lo) l))) (badll lo)) (setq k nil) (bind (setq wv (pop l)) (append a last)))
		   ((nil &optional)
		    (multiple-value-bind
		     (ln lb ld lp) (unbnd k (pop l)) (declare (ignore lb))
		     (when lp (bind lp t))
		     (let ((ld (if k ld (insuf ln))))
		       (bind ln (if lp `(progn (setq ,lp nil) ,ld) ld) t))))
		   (&rest
		    (when rv (badll lo))
		    (bind (setq rv (pop l)) `(list* ,@(bind-a) ,(lvp (cons rv (setq rvd (sdde rv decls)))))))
		   (&key
		    (multiple-value-bind
		     (ln lb ld lp) (unbnd k (pop l))
		     (let* ((lpt (tsym))(lbt (tsym))(ln (intern (string ln) 'keyword)))
		       (when (eq ln :allow-other-keys) (setq aok lbt))
		       (bind lpt)(bind lbt)
		       (push `(,ln (unless ,lpt (setq ,lbt v ,lpt t))) nkys)
		       (push `(,lb (if ,lpt ,lbt ,ld)) post)
		       (when lp (push `(,lp ,lpt) post)))))
		   (&aux (bind (pop l)))))))

   (let ((nap (nap)))
     (when nap
       (case kk
	     ((nil &optional) (unless n (bind +kev+ (let ((q (extra (na)))) (if (eq nap t) q `(when ,nap ,q))))))
	     (&key
	      (unless aok
		(let ((aop (tsym)))
		  (bind aop)(bind (setq aok (tsym)))
		  (push `(:allow-other-keys (unless ,aop (setq ,aok v ,aop t))) nkys)))
	      (let ((lpt (tsym))(lbt (tsym))(bk (tsym)))
		(bind lpt)(bind lbt)(bind bk)
		(push `(otherwise (unless ,lpt (setq ,lbt v ,lpt t ,bk k))) nkys)
		(push `(,+kev+ (unless ,aok (when ,lpt ,(badk bk lbt)))) post))))))

   (when post (post post nkys))
   (setq kev (member +kev+ *rv* :key #'(lambda (x) (when (listp x) (car x)))))

   `(let* ,(nreverse *rv*)
      ,@(when rvd (when (lvp) `((declare (:dynamic-extent ,(lvp))))))
      ,@(when kev `((declare (ignore ,+kev+))))
      ,@decls
      ,@ctps
      ,@body)))

(export '(blocked-body-name parse-body-header blla va-pop))

(defun parse-body-header (x &optional doc decl ctps &aux (a (car x)))
  (cond 
   ((unless (or doc ctps) (and (stringp a) (cdr x))) (parse-body-header (cdr x) a decl ctps))
   ((unless ctps (when (consp a) (eq (car a) 'declare)))  (parse-body-header (cdr x) doc (cons a decl) ctps))
   ((when (consp a) (member (car a) '(check-type assert))) (parse-body-header (cdr x) doc decl (cons a ctps)))
   (t (values doc (nreverse decl) (nreverse ctps) x))))

(defun make-blocked-lambda (ll decls ctps body block)
  (let ((body (if (eq block (blocked-body-name body)) body `((block ,block ,@body)))))
    `(lambda ,ll ,@decls ,@ctps ,@body)))

(defun blocked-body-name (body)
  (when (and (not (cdr body))
	     (consp (car body))
	     (eq (caar body) 'block))
    (cadar body)))


(defun block-body (name body ignore)
  (multiple-value-bind
   (doc decls ctps body) (parse-body-header body)
   `(,@(when doc `(,doc))
     ,@decls
     ,@(when ignore `((declare (ignore ,ignore))))
     ,@ctps
     ,@(if (eq name (blocked-body-name body)) body `((block ,name ,@body))))))

(defun defmacro-lambda (name vl body &aux n e (w (eq '&whole (car vl))))
  (multiple-value-bind
   (vl env) (get-&environment vl)
   `(lambda (l ,(or env (setq e (gensym))))
      ,@(unless env `((declare (ignore ,e))))
      ,(blla
	 (if w (list (list* (pop vl) (pop vl) (setq n (gensym)) vl)) vl)
	 nil
	 (if w `(list l) `(cdr l))
	 (block-body name body n)))))

(defun find-declarations (body)
  (if (endp body)
      (values nil nil)
      (let ((d (macroexpand (car body))))
        (cond ((stringp d)
               (if (endp (cdr body))
                   (values nil (list d))
                   (multiple-value-bind (ds b)
                       (find-declarations (cdr body))
                     (values (cons d ds) b))))
              ((and (consp d) (eq (car d) 'declare))
               (multiple-value-bind (ds b)
                   (find-declarations (cdr body))
                 (values (cons d ds) b)))
              (t
               (values nil (cons d (cdr body))))))))

(defmacro symbol-to-function (sym)
  (let* ((n (gensym))
	 (gf (find-symbol "C-SYMBOL-GFDEF" (find-package :s))))
    `(when (symbolp ,sym)
       ,(if (fboundp gf) `(let ((,n (address (,gf ,sym))))
			    (unless (= +objnull+ ,n) (nani ,n)))
	  `(let* ((,n (when (fboundp ,sym) (symbol-function ,sym)))
		  (,n (if (and (consp ,n) (eq (car ,n) 'macro)) (cdr ,n) ,n)))
	     (unless (consp ,n) ,n))))))

(defmacro call (sym &optional f &rest keys) ;FIXME macro
  (let* ((fnf (gensym))(n (gensym)))
    `(let* ((,fnf (if (functionp ,sym) ,sym (symbol-to-function ,sym))));(coerce ,sym 'function)
       (or (when ,fnf (cfun-call ,fnf))
	   (when ,f
	     (let ((,n (make-call ,@keys)))
	       (when ,fnf (set-cfun-call ,n ,fnf))
	       ,n))))))
