;;; CMPLAM  Lambda expression.
;;;
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


(in-package :compiler)

;;; During Pass1, a lambda-list
;;;
;;; (	{ var }*
;;; 	[ &optional { var | ( var [ initform [ svar ] ] ) }* ]
;;; 	[ &rest var ]
;;; 	[ &key { var | ( { var | ( kwd var ) } [initform [ svar ]])}*
;;; 		[&allow-other-keys]]
;;; 	[ &aux {var | (var [initform])}*]
;;; )
;;;
;;; is transformed into
;;;
;;; (	( { var }* )				; required
;;; 	( { (var initform svar) }* )		; optional
;;; 	{ var | nil }				; rest
;;; 	key-flag
;;; 	( { ( kwd-vv-index var initform svar) }* )	; key
;;; 	allow-other-keys-flag
;;; )
;;;
;;; where
;;; 	svar:	  nil		; means svar is not supplied
;;;	        | var
;;;
;;; &aux parameters will be embedded into LET*.
;;;
;;; c1lambda-expr receives
;;;	( lambda-list { doc | decl }* . body )
;;; and returns
;;;	( lambda info-object lambda-list' doc body' )
;;;
;;; Doc is NIL if no doc string is supplied.
;;; Body' is body possibly surrounded by a LET* (if &aux parameters are
;;; supplied) and an implicit block.

(defmacro ck-spec (condition)
  `(unless ,condition
           (cmperr "The parameter specification ~s is illegal." spec)))

(defmacro ck-vl (condition)
  `(unless ,condition
           (cmperr "The lambda list ~s is illegal." vl)))


(defun wfs-error ()
  (error "This error is not supposed to occur: Contact Schelter ~
    ~%wfs@math.utexas.edu"))

(defun decls-from-procls (ll procls body)
  (cond ((or (null procls) (eq (car procls) '*)
	     (null ll) (member (car ll) '(&whole &optional &rest &key &environment))) nil)
	((eq (car procls) t)
	 (decls-from-procls (cdr ll) (cdr procls) body))
	(t
	 (cons (list (car procls) (or (if (atom (car ll)) (car ll) (caar ll))))
	       (decls-from-procls (cdr ll) (cdr procls) body)))))
	 
(defun c1lambda-expr (args &aux (regs (pop args)) requireds tv
			   doc body ss is ts other-decls (ovars *vars*)
			   (*vars* *vars*) narg (info (make-info)) ctps)


  (multiple-value-setq (body ss ts is other-decls doc ctps) (c1body args t));FIXME parse-body-header
  
  (mapc (lambda (x &aux (y (c1make-var x ss is ts))) 
	  (setf (var-mt y) nil)
	  (push-var y nil) (push y requireds)) regs)
  (when (member +nargs+ ts :key 'car)
    (setq narg (list (c1make-var +nargs+ ss is ts))))
  (setq tv (append narg requireds))

  (c1add-globals ss)
  (check-vdecl (mapcar 'var-name tv) ts is)
  
  (setq body (c1decl-body other-decls body))
  (ref-vars body requireds)
  (dolist (var requireds) (check-vref var))
  
  (dolist (v requireds)
    (when (var-p v)
      (unless (type>= (var-type v) (var-mt v))
	(setf (var-type v) (var-mt v)))));FIXME?
  (let ((*vars* ovars)) (add-info info (cadr body)))
  (cond (*compiler-new-safety*
	 (mapc (lambda (x) (setf (var-type x) #tt)) requireds)
	 (let ((i (cadr body)))
	   (setf (info-type i) (if (single-type-p (info-type i)) #tt #t*))))
	((mapc (lambda (l) (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps)))))) tv)));FIXME?
  
  `(lambda ,info ,(list (nreverse requireds) narg) ,doc ,body))

;; (defun c1lambda-expr (args &aux (regs (pop args)) requireds tv
;; 			   doc body ss is ts other-decls (ovars *vars*)
;; 			   (*vars* *vars*) narg (info (make-info)) ctps)


;;   (multiple-value-setq (body ss ts is other-decls doc ctps) (c1body args t));FIXME parse-body-header
  
;;   (mapc (lambda (x &aux (y (c1make-var x ss is ts))) (push-var y nil) (push y requireds)) regs)
;;   (when (member +nargs+ ts :key 'car)
;;     (setq narg (list (c1make-var +nargs+ ss is ts))))
;;   (setq tv (append narg requireds))

;;   (c1add-globals ss)
;;   (check-vdecl (mapcar 'var-name tv) ts is)
  
;;   (setq body (c1decl-body other-decls body))
;;   (ref-vars body requireds)
;;   (dolist (var requireds) (check-vref var))
  
;;   (dolist (v requireds)
;;     (when (var-p v)
;;       (setf (var-type v) (var-mt v))));FIXME?
;;   (let ((*vars* ovars)) (add-info info (cadr body)))
;;   (cond (*compiler-new-safety*
;; 	 (mapc (lambda (x) (setf (var-type x) #tt)) requireds)
;; 	 (let ((i (cadr body)))
;; 	   (setf (info-type i) (if (single-type-p (info-type i)) #tt #t*))))
;; 	((mapc (lambda (l) (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps)))))) tv)));FIXME?
  
;;   `(lambda ,info ,(list (nreverse requireds) narg) ,doc ,body))

;; (defun c1lambda-expr (args &aux (regs (pop args)) requireds tv
;; 			   doc body ss is ts other-decls (ovars *vars*)
;; 			   (*vars* *vars*) narg (info (make-info)) ctps)


;;   (multiple-value-setq (body ss ts is other-decls doc ctps) (c1body args t));FIXME parse-body-header
  
;;   (mapc (lambda (x &aux (y (c1make-var x ss is ts))) (push-var y nil) (push y requireds)) regs)
;;   (when (member +nargs+ ts :key 'car)
;;     (setq narg (list (c1make-var +nargs+ ss is ts))))
;;   (setq tv (append narg requireds))

;;   (c1add-globals ss)
;;   (check-vdecl (mapcar 'var-name tv) ts is)
  
;;   (setq body (c1decl-body other-decls body))
;;   (ref-vars body requireds)
;;   (dolist (var requireds) (check-vref var))
  
;;   (dolist (v requireds)
;;     (when (var-p v)
;;       (setf (var-type v) (var-mt v))));FIXME?
;;   (let ((*vars* ovars)) (add-info info (cadr body)))
;;   (unless *compiler-new-safety*
;;     (dolist (l tv) 
;;       (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps)))))));FIXME?
  
;;   `(lambda ,info ,(list (nreverse requireds) narg) ,doc ,body))

;; (defun c1lambda-expr (args &aux (regs (pop args)) requireds tv
;; 			   doc body ss is ts other-decls (ovars *vars*)
;; 			   (*vars* *vars*) narg (info (make-info)) ctps)


;;   (multiple-value-setq (body ss ts is other-decls doc ctps) (c1body args t));FIXME parse-body-header
  
;;   (mapc (lambda (x &aux (y (c1make-var x ss is ts))) (push-var y nil) (push y requireds)) regs)
;;   (when (member +nargs+ ts :key 'car)
;;     (setq narg (list (c1make-var +nargs+ ss is ts))))
;;   (setq tv (append narg requireds))

;;   (c1add-globals ss)
;;   (check-vdecl (mapcar 'var-name tv) ts is)
  
;;   (setq body (c1decl-body other-decls body))
;;   (ref-vars body requireds)
;;   (dolist (var requireds) (check-vref var))
  
;;   (dolist (v requireds)
;;     (when (var-p v)
;;       (setf (var-type v) (var-mt v))));FIXME?
;;   (let ((*vars* ovars)) (add-info info (cadr body)))
;;   (dolist (l tv) 
;;     (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps))))));FIXME?
  
;;   `(lambda ,info ,(list (nreverse requireds) narg) ,doc ,body))

;; (defun c1lambda-expr (args &aux (regs (pop args)) requireds tv
;; 			   doc body ss is ts other-decls (ovars *vars*)
;; 			   (*vars* *vars*) narg (info (make-info)) ctps)


;;   (multiple-value-setq (body ss ts is other-decls doc ctps) (c1body args t));FIXME parse-body-header
  
;;   (mapc (lambda (x &aux (y (c1make-var x ss is ts))) (push y *vars*) (push y requireds)) regs)
;;   (when (member +nargs+ ts :key 'car)
;;     (setq narg (list (c1make-var +nargs+ ss is ts))))
;;   (setq tv (append narg requireds))

;;   (c1add-globals ss)
;;   (check-vdecl (mapcar 'var-name tv) ts is)
  
;;   (setq body (c1decl-body other-decls body))
;;   (ref-vars body requireds)
;;   (dolist (var requireds) (check-vref var))
  
;;   (dolist (v requireds)
;;     (when (var-p v)
;;       (setf (var-type v) (var-mt v))));FIXME?
;;   (let ((*vars* ovars)) (add-info info (cadr body)))
;;   (dolist (l tv) 
;;     (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps))))));FIXME?
  
;;   `(lambda ,info ,(list (nreverse requireds) narg) ,doc ,body))

;; (defun c1lambda-expr (args &aux (regs (pop args)) requireds tv
;; 			   doc body ss is ts other-decls
;; 			   (*vars* *vars*) narg (info (make-info)) ctps)


;;   (multiple-value-setq (body ss ts is other-decls doc ctps) (c1body args t));FIXME parse-body-header
  
;;   (mapc (lambda (x &aux (y (c1make-var x ss is ts))) (push y *vars*) (push y requireds)) regs)
;;   (when (member +nargs+ ts :key 'car)
;;     (setq narg (list (c1make-var +nargs+ ss is ts))))
;;   (setq tv (append narg requireds))

;;   (c1add-globals ss)
;;   (check-vdecl (mapcar 'var-name tv) ts is)
  
;;   (setq body (c1decl-body other-decls body))
;;   (ref-vars body requireds)
;;   (dolist (var requireds) (check-vref var))
  
;;   (dolist (v requireds)
;;     (when (var-p v)
;;       (setf (var-type v) (var-mt v))));FIXME?
;;   (add-info info (cadr body))
;;   (dolist (l tv) 
;;     (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps))))));FIXME?
  
;;   `(lambda ,info ,(list (nreverse requireds) narg) ,doc ,body))

;; (defun c1lambda-expr (lambda-expr
;;                       &optional (block-name nil block-it)
;;                       &aux (requireds nil) (optionals nil) (rest nil)
;;                            (keywords nil) (key-flag nil)
;; 			   lambda-list
;;                            (allow-other-keys nil) (aux-vars nil)
;;                            (aux-inits nil) doc vl spec body ss is ts
;;                            other-decls vnames
;;                            (*vars* *vars*)
;;                            (info (make-info))
;;                            (aux-info nil)
;; 			   (setjmps *setjmps*) ctps)

;;   (cmpck (endp lambda-expr)
;;          "The lambda expression ~s is illegal." (cons 'lambda lambda-expr))


;;   ;;FIXME -- this is backwards, as the proclamations should be
;;   ;;generated from the declarations.  What we need here and in the let
;;   ;;code is reverse type propagation.  CM 20050106
;; ;;   (let ((decls (decls-from-procls
;; ;; 		(car lambda-expr)
;; ;; 		(and block-it (get-arg-types block-name))
;; ;; 		(cdr lambda-expr))))
;; ;;     (when decls
;; ;;       (cmpnote "~S function args declared: ~S~%" block-name decls)
;; ;;       (setq lambda-expr (cons (car lambda-expr) (cons (cons 'declare decls) (cdr lambda-expr))))))
    
;;   (multiple-value-setq (body ss ts is other-decls doc ctps)
;;                        (c1body (cdr lambda-expr) t))
  
;;   (when block-it (setq body (list (cons 'block (cons block-name body)))))

;;   (c1add-globals ss)

;;   (setq vl (car lambda-expr))
;;   (block parse
;;    (tagbody
;;     Lreq
;;       (when (null vl) (return-from parse))
;;       (ck-vl (consp vl))
;;       (case (setq spec (pop vl))
;;             (&optional (go Lopt))
;;             (&rest (go Lrest))
;;             (&key (go Lkey))
;;             (&aux (go Laux)))
;;       (let ((v (c1make-var spec ss is ts)))
;;            (push spec vnames)
;;            (push v *vars*)
;;            (push v requireds))
;;       (go Lreq)

;;     Lopt
;;       (when (null vl) (return-from parse))
;;       (ck-vl (consp vl))
;;       (case (setq spec (pop vl))
;;             (&rest (go Lrest))
;;             (&key (go Lkey))
;;             (&aux (go Laux)))
;;       (cond ((not (consp spec))
;;              (let ((v (c1make-var spec ss is ts)))
;;                   (push spec vnames)
;;                   (push (list v (default-init (var-type v)) nil) optionals)
;;                   (push v *vars*)))
;;             ((not (consp (cdr spec)))
;;              (ck-spec (null (cdr spec)))
;;              (let ((v (c1make-var (car spec) ss is ts)))
;;                   (push (car spec) vnames)
;;                   (push (list v (default-init (var-type v)) nil) optionals)
;;                   (push v *vars*)))
;;             ((not (consp (cddr spec)))
;;              (ck-spec (null (cddr spec)))
;;              (let ((init (c1expr* (cadr spec) info))
;;                    (v (c1make-var (car spec) ss is ts)))
;;                   (push (car spec) vnames)
;;                   (push
;;                    (list v (and-form-type (var-type v) init (cadr spec)) nil)
;;                    optionals)
;;                   (push v *vars*)))
;;             (t
;;              (ck-spec (null (cdddr spec)))
;;              (let ((init (c1expr* (cadr spec) info))
;;                    (v (c1make-var (car spec) ss is ts))
;;                    (sv (c1make-var (caddr spec) ss is ts))
;;                    )
;;                   (push (car spec) vnames)
;;                   (push (caddr spec) vnames)
;;                   (push
;;                    (list v (and-form-type (var-type v) init (cadr spec)) sv)
;;                    optionals)
;;                   (push v *vars*)
;;                   (push sv *vars*))))
;;       (go Lopt)

;;     Lrest
;;       (ck-vl (consp vl))
;;       (push (car vl) vnames)
;;       (setq rest (c1make-var (pop vl) ss is ts))
;;       (push rest *vars*)
;;       (when (null vl) (return-from parse))
;;       (ck-vl (consp vl))
;;       (case (setq spec (pop vl))
;;             (&key (go Lkey))
;;             (&aux (go Laux)))
;;       (cmperr "Either &key or &aux is missing before ~s." spec)

;;     Lkey
;;       (setq key-flag t)
;;       (when (null vl) (return-from parse))
;;       (ck-vl (consp vl))
;;       (case (setq spec (pop vl))
;;             (&aux (go Laux))
;;             (&allow-other-keys (setq allow-other-keys t)
;;                                (when (null vl) (return-from parse))
;;                                (ck-vl (consp vl))
;;                                (case (setq spec (pop vl))
;;                                      (&aux (go Laux)))
;;                                (cmperr "&aux is missing before ~s." spec)))
;;       (when (not (consp spec)) (setq spec (list spec)))
;;       (cond ((consp (car spec))
;;              (ck-spec (and (keywordp (caar spec))
;;                            (consp (cdar spec))
;;                            (null (cddar spec))))
;;              (setq spec (cons (caar spec) (cons (cadar spec) (cdr spec)))))
;;             (t
;;              (ck-spec (symbolp (car spec)))
;;              (setq spec (cons (intern (string (car spec)) 'keyword)
;;                               (cons (car spec) (cdr spec))))))
;;       (cond ((not (consp (cddr spec)))
;;              (ck-spec (null (cddr spec)))
;;              (let ((v (c1make-var (cadr spec) ss is ts)))
;;                   (push (cadr spec) vnames)
;;                   (push
;;                    (list (car spec) v (default-init (var-type v))
;;                          (make-var :kind 'DUMMY))
;;                    keywords)
;;                   (push v *vars*)))
;;             ((not (consp (cdddr spec)))
;;              (ck-spec (null (cdddr spec)))
;;              (let ((init (c1expr* (caddr spec) info))
;;                    (v (c1make-var (cadr spec) ss is ts)))
;;                   (push (cadr spec) vnames)
;;                   (push (list (car spec) v
;;                               (and-form-type (var-type v) init (caddr spec))
;;                               (make-var :kind 'DUMMY))
;;                         keywords)
;;                   (push v *vars*)))
;;             (t
;;              (ck-spec (null (cddddr spec)))
;;              (let ((init (c1expr* (caddr spec) info))
;;                    (v (c1make-var (cadr spec) ss is ts))
;;                    (sv (c1make-var (cadddr spec) ss is ts)))
;;                   (push (cadr spec) vnames)
;;                   (push (cadddr spec) vnames)
;;                   (push (list (car spec) v
;;                               (and-form-type (var-type v) init (caddr spec))
;;                               sv)
;;                         keywords)
;;                   (push v *vars*)
;;                   (push sv *vars*))))
;;       (go Lkey)

;;     Laux
;;       (setq aux-info (make-info))
;;     Laux1
;;       (when (null vl) (add-info info aux-info) (return-from parse))
;;       (ck-vl (consp vl))
;;       (setq spec (pop vl))
;;       (cond ((consp spec)
;;              (cond ((not (consp (cdr spec)))
;;                     (ck-spec (null (cdr spec)))
;;                     (let ((v (c1make-var (car spec) ss is ts)))
;;                          (push (car spec) vnames)
;;                          (push (default-init (var-type v)) aux-inits)
;;                          (push v aux-vars)
;;                          (push v *vars*)))
;;                    (t
;;                     (ck-spec (null (cddr spec)))
;;                     (let ((init (c1expr* (cadr spec) aux-info))
;;                           (v (c1make-var (car spec) ss is ts)))
;;                          (push (car spec) vnames)
;;                          (push (and-form-type (var-type v) init (cadr spec))
;;                                aux-inits)
;;                          (push v aux-vars)
;;                          (push v *vars*)))))
;;             (t
;;              (let ((v (c1make-var spec ss is ts)))
;;                   (push spec vnames)
;;                   (push (default-init (var-type v)) aux-inits)
;;                   (push v aux-vars)
;;                   (push v *vars*))))
;;       (set-var-init-type (car aux-vars) (info-type (second (car aux-inits))))
;;       (go Laux1)
;;       )
;;    )
;;   (setq requireds (reverse requireds)
;;         optionals (reverse optionals)
;;         keywords (reverse keywords)
;;         aux-vars (reverse aux-vars)
;;         aux-inits (reverse aux-inits))

;;   (check-vdecl vnames ts is)

;;   (setq body (c1decl-body other-decls body))

;;   (ref-vars body (append requireds optionals keywords aux-vars));FIXME aux?

;;   (dolist (l (list requireds optionals keywords aux-vars))
;;     (dolist (v l)
;;       (when (var-p v)
;; 	(setf (var-type v) (var-mt v)))))

;;   (add-info info (cadr body))

;;   (dolist** (var requireds) (check-vref var))
;;   (dolist** (opt optionals)
;;             (check-vref (car opt))
;;             (when (caddr opt) (check-vref (caddr opt))))
;;   (when rest (check-vref rest))
;;   (dolist** (kwd keywords)
;;             (check-vref (cadr kwd))
;;             (when (cadddr kwd) (check-vref (cadddr kwd))))
;;   (dolist** (var aux-vars) (check-vref var))
  
;;   (when aux-vars
;;         (add-info aux-info (cadr body))
;; 	(setf (info-type aux-info) (info-type (cadr body)))
;;         (setq body (list 'let* aux-info aux-vars aux-inits body))
;; 	(or (eql setjmps *setjmps*) (setf (info-volatile aux-info) 1)))

;;   ;;FIXME -- is above for aux needed too?
;;   (when (or optionals keywords)
;;     (or (eql setjmps *setjmps*) (setf (info-volatile info) 1)))
;;   (setq optionals (list (is-narg-le lambda-expr)));FIXME
;;   (setq lambda-list
;; 	(list requireds optionals rest key-flag keywords allow-other-keys))
;;   (dolist (l requireds) 
;;     (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps))))));(unboxed-type (cdr (assoc (var-name l) ctps)))
;;   (and *record-call-info* (record-arg-info lambda-list))
;;   `(lambda ,info ,lambda-list ,doc ,body))

;; (defun c1lambda-expr (args
;;                       &optional (block-name nil block-it)
;;                       &aux (regs (pop args)) requireds
;; 		      lambda-list doc vl spec body ss is ts
;; 		      other-decls vnames
;; 		      (*vars* *vars*)
;; 		      (info (make-info)) ctps)


;;   (multiple-value-setq (body ss ts is other-decls doc ctps) (c1body args t))
  
;;   (mapc (lambda (x &aux (y (c1make-var x ss is ts))) (push y *vars*) (push y requireds)) regs)

;;   (c1add-globals ss)
  
;;   (check-vdecl requireds ts is)
  
;;   (setq body (c1decl-body other-decls body))
  
;;   (ref-vars body requireds)
  
;;   (dolist (v requireds)
;;     (when (var-p v)
;;       (setf (var-type v) (var-mt v))))
  
;;   (add-info info (cadr body))
  
;;   (dolist (var requireds) (check-vref var))
  
;;   (let* ((narg (is-narg-le args)))
;;     (when narg (print (list 'narg block-name)))
;;     (setq lambda-list (list (nreverse requireds) (when narg (list +nargs+)))))
;;   (dolist (l requireds) 
;;     (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps))))));(unboxed-type (cdr (assoc (var-name l) ctps)))
  
;;   `(lambda ,info ,lambda-list ,doc ,body))

;; (defun c1lambda-expr (lambda-expr
;;                       &optional (block-name nil block-it)
;;                       &aux (requireds nil) (optionals nil) (rest nil)
;;                            (keywords nil) (key-flag nil)
;; 			   lambda-list
;;                            (allow-other-keys nil) (aux-vars nil)
;;                            (aux-inits nil) doc vl spec body ss is ts
;;                            other-decls vnames
;;                            (*vars* *vars*)
;;                            (info (make-info))
;;                            (aux-info nil)
;; 			   (setjmps *setjmps*) ctps
;;                       )

;;   (cmpck (endp lambda-expr)
;;          "The lambda expression ~s is illegal." (cons 'lambda lambda-expr))


;;   ;;FIXME -- this is backwards, as the proclamations should be
;;   ;;generated from the declarations.  What we need here and in the let
;;   ;;code is reverse type propagation.  CM 20050106
;; ;;   (let ((decls (decls-from-procls
;; ;; 		(car lambda-expr)
;; ;; 		(and block-it (get-arg-types block-name))
;; ;; 		(cdr lambda-expr))))
;; ;;     (when decls
;; ;;       (cmpnote "~S function args declared: ~S~%" block-name decls)
;; ;;       (setq lambda-expr (cons (car lambda-expr) (cons (cons 'declare decls) (cdr lambda-expr))))))
    
;;   (multiple-value-setq (body ss ts is other-decls doc ctps)
;;                        (c1body (cdr lambda-expr) t))
  
;;   (when block-it (setq body (list (cons 'block (cons block-name body)))))

;;   (c1add-globals ss)

;;   (setq vl (car lambda-expr))
;;   (block parse
;;    (tagbody
;;     Lreq
;;       (when (null vl) (return-from parse))
;;       (ck-vl (consp vl))
;;       (case (setq spec (pop vl))
;;             (&optional (go Lopt))
;;             (&rest (go Lrest))
;;             (&key (go Lkey))
;;             (&aux (go Laux)))
;;       (let ((v (c1make-var spec ss is ts)))
;;            (push spec vnames)
;;            (push v *vars*)
;;            (push v requireds))
;;       (go Lreq)

;;     Lopt
;;       (when (null vl) (return-from parse))
;;       (ck-vl (consp vl))
;;       (case (setq spec (pop vl))
;;             (&rest (go Lrest))
;;             (&key (go Lkey))
;;             (&aux (go Laux)))
;;       (cond ((not (consp spec))
;;              (let ((v (c1make-var spec ss is ts)))
;;                   (push spec vnames)
;;                   (push (list v (default-init (var-type v)) nil) optionals)
;;                   (push v *vars*)))
;;             ((not (consp (cdr spec)))
;;              (ck-spec (null (cdr spec)))
;;              (let ((v (c1make-var (car spec) ss is ts)))
;;                   (push (car spec) vnames)
;;                   (push (list v (default-init (var-type v)) nil) optionals)
;;                   (push v *vars*)))
;;             ((not (consp (cddr spec)))
;;              (ck-spec (null (cddr spec)))
;;              (let ((init (c1expr* (cadr spec) info))
;;                    (v (c1make-var (car spec) ss is ts)))
;;                   (push (car spec) vnames)
;;                   (push
;;                    (list v (and-form-type (var-type v) init (cadr spec)) nil)
;;                    optionals)
;;                   (push v *vars*)))
;;             (t
;;              (ck-spec (null (cdddr spec)))
;;              (let ((init (c1expr* (cadr spec) info))
;;                    (v (c1make-var (car spec) ss is ts))
;;                    (sv (c1make-var (caddr spec) ss is ts))
;;                    )
;;                   (push (car spec) vnames)
;;                   (push (caddr spec) vnames)
;;                   (push
;;                    (list v (and-form-type (var-type v) init (cadr spec)) sv)
;;                    optionals)
;;                   (push v *vars*)
;;                   (push sv *vars*))))
;;       (go Lopt)

;;     Lrest
;;       (ck-vl (consp vl))
;;       (push (car vl) vnames)
;;       (setq rest (c1make-var (pop vl) ss is ts))
;;       (push rest *vars*)
;;       (when (null vl) (return-from parse))
;;       (ck-vl (consp vl))
;;       (case (setq spec (pop vl))
;;             (&key (go Lkey))
;;             (&aux (go Laux)))
;;       (cmperr "Either &key or &aux is missing before ~s." spec)

;;     Lkey
;;       (setq key-flag t)
;;       (when (null vl) (return-from parse))
;;       (ck-vl (consp vl))
;;       (case (setq spec (pop vl))
;;             (&aux (go Laux))
;;             (&allow-other-keys (setq allow-other-keys t)
;;                                (when (null vl) (return-from parse))
;;                                (ck-vl (consp vl))
;;                                (case (setq spec (pop vl))
;;                                      (&aux (go Laux)))
;;                                (cmperr "&aux is missing before ~s." spec)))
;;       (when (not (consp spec)) (setq spec (list spec)))
;;       (cond ((consp (car spec))
;;              (ck-spec (and (keywordp (caar spec))
;;                            (consp (cdar spec))
;;                            (null (cddar spec))))
;;              (setq spec (cons (caar spec) (cons (cadar spec) (cdr spec)))))
;;             (t
;;              (ck-spec (symbolp (car spec)))
;;              (setq spec (cons (intern (string (car spec)) 'keyword)
;;                               (cons (car spec) (cdr spec))))))
;;       (cond ((not (consp (cddr spec)))
;;              (ck-spec (null (cddr spec)))
;;              (let ((v (c1make-var (cadr spec) ss is ts)))
;;                   (push (cadr spec) vnames)
;;                   (push
;;                    (list (car spec) v (default-init (var-type v))
;;                          (make-var :kind 'DUMMY))
;;                    keywords)
;;                   (push v *vars*)))
;;             ((not (consp (cdddr spec)))
;;              (ck-spec (null (cdddr spec)))
;;              (let ((init (c1expr* (caddr spec) info))
;;                    (v (c1make-var (cadr spec) ss is ts)))
;;                   (push (cadr spec) vnames)
;;                   (push (list (car spec) v
;;                               (and-form-type (var-type v) init (caddr spec))
;;                               (make-var :kind 'DUMMY))
;;                         keywords)
;;                   (push v *vars*)))
;;             (t
;;              (ck-spec (null (cddddr spec)))
;;              (let ((init (c1expr* (caddr spec) info))
;;                    (v (c1make-var (cadr spec) ss is ts))
;;                    (sv (c1make-var (cadddr spec) ss is ts)))
;;                   (push (cadr spec) vnames)
;;                   (push (cadddr spec) vnames)
;;                   (push (list (car spec) v
;;                               (and-form-type (var-type v) init (caddr spec))
;;                               sv)
;;                         keywords)
;;                   (push v *vars*)
;;                   (push sv *vars*))))
;;       (go Lkey)

;;     Laux
;;       (setq aux-info (make-info))
;;     Laux1
;;       (when (null vl) (add-info info aux-info) (return-from parse))
;;       (ck-vl (consp vl))
;;       (setq spec (pop vl))
;;       (cond ((consp spec)
;;              (cond ((not (consp (cdr spec)))
;;                     (ck-spec (null (cdr spec)))
;;                     (let ((v (c1make-var (car spec) ss is ts)))
;;                          (push (car spec) vnames)
;;                          (push (default-init (var-type v)) aux-inits)
;;                          (push v aux-vars)
;;                          (push v *vars*)))
;;                    (t
;;                     (ck-spec (null (cddr spec)))
;;                     (let ((init (c1expr* (cadr spec) aux-info))
;;                           (v (c1make-var (car spec) ss is ts)))
;;                          (push (car spec) vnames)
;;                          (push (and-form-type (var-type v) init (cadr spec))
;;                                aux-inits)
;;                          (push v aux-vars)
;;                          (push v *vars*)))))
;;             (t
;;              (let ((v (c1make-var spec ss is ts)))
;;                   (push spec vnames)
;;                   (push (default-init (var-type v)) aux-inits)
;;                   (push v aux-vars)
;;                   (push v *vars*))))
;;       (set-var-init-type (car aux-vars) (info-type (second (car aux-inits))))
;;       (go Laux1)
;;       )
;;    )
;;   (setq requireds (reverse requireds)
;;         optionals (reverse optionals)
;;         keywords (reverse keywords)
;;         aux-vars (reverse aux-vars)
;;         aux-inits (reverse aux-inits))

;;   (check-vdecl vnames ts is)

;;   (setq body (c1decl-body other-decls body))

;;   (dolist (l (list requireds optionals keywords aux-vars))
;;     (dolist (v l)
;;       (when (var-p v)
;; 	(setf (var-type v) (var-mt v)))))

;;   (add-info info (cadr body))

;;   (dolist** (var requireds) (check-vref var))
;;   (dolist** (opt optionals)
;;             (check-vref (car opt))
;;             (when (caddr opt) (check-vref (caddr opt))))
;;   (when rest (check-vref rest))
;;   (dolist** (kwd keywords)
;;             (check-vref (cadr kwd))
;;             (when (cadddr kwd) (check-vref (cadddr kwd))))
;;   (dolist** (var aux-vars) (check-vref var))
  
;;   (when aux-vars
;;         (add-info aux-info (cadr body))
;; 	(setf (info-type aux-info) (info-type (cadr body)))
;;         (setq body (list 'let* aux-info aux-vars aux-inits body))
;; 	(or (eql setjmps *setjmps*) (setf (info-volatile aux-info) 1)))

;;   ;;FIXME -- is above for aux needed too?
;;   (when (or optionals keywords)
;;     (or (eql setjmps *setjmps*) (setf (info-volatile info) 1)))

;;   (setq lambda-list
;; 	(list requireds optionals rest key-flag keywords allow-other-keys))
;;   (dolist (l requireds) 
;;     (setf (var-type l) (type-and (var-type l) (nil-to-t (cdr (assoc (var-name l) ctps))))));(unboxed-type (cdr (assoc (var-name l) ctps)))
;;   (and *record-call-info* (record-arg-info lambda-list))
;;   `(lambda ,info ,lambda-list ,doc ,body))


(defun the-parameter (name)
  (cmpck (not (symbolp name)) "The parameter ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being bound." name)
  name)

(defvar *rest-on-stack* nil)  ;; non nil means put rest arg on C stack.

(defun need-to-set-vs-pointers (lambda-list)
				;;; On entry to in-line lambda expression,
				;;; vs_base and vs_top must be set iff,
   (or *safe-compile*
       *compiler-check-args*
       (nth 1 lambda-list)	;;; optional,
       (nth 2 lambda-list)	;;; rest, or
       (nth 3 lambda-list)	;;; key-flag.
       ))
