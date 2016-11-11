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


;;;;    listlib.lsp
;;;;
;;;;                        list manipulating routines

; Rewritten 11 Feb 1993 by William Schelter and Gordon Novak to use iteration
; rather than recursion, as needed for large data sets.


(in-package :si)

(eval-when (compile)
  (proclaim '(optimize (safety 0) (space 3)))
  )


(macrolet
    ((defl2fn (n &rest body) `(defun ,n (list1 list2 &key key test test-not &aux r rp
					       (key (when key (coerce key 'function)))
					       (test (when test (coerce test 'function)))
					       (test-not (when test-not (coerce test-not 'function))))
				(macrolet
				    ((check-list (list) `(do ((l ,list (cdr l)))
							     ((not (consp l))
							      (when l (error 'type-error :datum l :expected-type 'list)))))
				     (apply-to-stack (form list) `(let (r rp)
								    (dolist (l ,list r)
								      (let ((tmp (cons ,(if form `(,@form l) `l) nil)))
									(declare (dynamic-extent tmp))
									(setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp)))))))
				     (collect (x) `(let ((temp ,x))
						     (setq rp (if rp (cdr (rplacd rp temp)) (setq r temp)))))
				     (do-test (x z) `(cond (test (funcall test ,x ,z))
							   (test-not (not (funcall test-not ,x ,z)))
							   ((eql ,x ,z))))
				     (memb (item list &optional rev)  `(do ((item ,item)(l ,list (cdr l))) ((not l))
									 (let ((cl (car l)))
									   (when (do-test ,@(if rev `(cl item) `(item cl)))
									     (return l))))))
				  (check-list list1)(check-list list2)
				  (let ((klist2 (if key (apply-to-stack (funcall key) list2) list2)))
				    ,@body)))))
     
  (defl2fn intersection
    (dolist (l1 list1 r)
      (when (memb (if key (funcall key l1) l1) klist2)
	(collect (cons l1 nil)))))
  
  (defl2fn union
    (dolist (l1 list1)
      (unless (memb (if key (funcall key l1) l1) klist2)
	(collect (cons l1 nil))))
    (when rp (rplacd rp list2))
    (or r list2))
  
  (defl2fn set-difference
    (dolist (l1 list1 r)
      (unless (memb (if key (funcall key l1) l1) klist2)
	(collect (cons l1 nil)))))
  
  (defl2fn set-exclusive-or
    (let ((klist1 (if key (apply-to-stack (funcall key) list1) list1)))
      (do ((kl1 klist1 (cdr kl1))(l1 list1 (cdr l1))) ((not kl1))
	(unless (memb (car kl1) klist2)
	  (collect (cons (car l1) nil))))
      (do ((kl2 klist2 (cdr kl2))(l2 list2 (cdr l2))) ((not kl2) r)
	(unless (memb (car kl2) klist1 t)
	  (collect (cons (car l2) nil))))))
  
  (defl2fn nintersection
    (do ((l1 list1 (cdr l1)))((not l1) (when rp (rplacd rp nil)) r)
      (let ((cl1 (car l1)))
	(when (memb (if key (funcall key cl1) cl1) klist2)
	  (collect l1)))))
  
  (defl2fn nunion
    (do ((l1 list1 (cdr l1)))((not l1) (when rp (rplacd rp list2)) (or r list2))
      (let ((cl1 (car l1)))
	(unless (memb (if key (funcall key cl1) cl1) klist2)
	  (collect l1)))))
  
  (defl2fn nset-difference
    (do ((l1 list1 (cdr l1)))((not l1) (when rp (rplacd rp nil)) r)
      (let ((cl1 (car l1)))
	(unless (memb (if key (funcall key cl1) cl1) klist2)
	  (collect l1)))))
  
  (defl2fn nset-exclusive-or
    (let ((klist1 (if key (apply-to-stack (funcall key) list1) (apply-to-stack nil list1))))
      (do ((kl1 klist1 (cdr kl1))(l1 list1 (cdr l1))) ((not kl1))
	(unless (memb (car kl1) klist2)
	  (collect l1)))
      (do ((kl2 klist2 (cdr kl2))(l2 list2 (cdr l2))) ((not kl2) (when rp (rplacd rp nil)) r)
	(unless (memb (car kl2) klist1 t)
	  (collect l2)))))
  
  (defl2fn subsetp r rp
    (dolist (l1 list1 t)
      (unless (memb (if key (funcall key l1) l1) klist2)
	(return nil)))))


(defmacro tp-error (x y)
  `(error 'type-error :datum ,x :expected-type ',y))

(defun smallnthcdr (n x)
  (declare (fixnum n))
  (cond ((= n 0) x)
	((atom x) (when x (tp-error x proper-list)))
	((smallnthcdr (1- n) (cdr x)))))

(defun bignthcdr (n i s f) 
  (declare (fixnum i))
  (cond ((atom f) (when f (tp-error f proper-list)))
	((atom (cdr f)) (when (cdr f) (tp-error (cdr f) proper-list)))
	((eq s f) (smallnthcdr (mod n i) s))
	((bignthcdr n (1+ i) (cdr s) (cddr f)))))

(defun nthcdr (n x)
  (declare (optimize (safety 1)))
  (cond ((or (not (integerp n)) (minusp n)) (tp-error n (integer 0)))
	((< n array-dimension-limit) (smallnthcdr n x))
	((atom x) (when x (tp-error x proper-list)))
	((atom (cdr x)) (when (cdr x) (tp-error (cdr x) proper-list)))
	((bignthcdr n 1 (cdr x) (cddr x)))))

(defun nth (n x)
  (declare (optimize (safety 2)))
  (car (nthcdr n x)))
(defun first (x) 
  (declare (optimize (safety 2)))
  (car x))
(defun second (x) 
  (declare (optimize (safety 2)))
  (cadr x))
(defun third (x) 
  (declare (optimize (safety 2)))
  (caddr x))
(defun fourth (x) 
  (declare (optimize (safety 2)))
  (cadddr x))
(defun fifth (x) 
  (declare (optimize (safety 2)))
  (car (cddddr x)))
(defun sixth (x) 
  (declare (optimize (safety 2)))
  (cadr (cddddr x)))
(defun seventh (x) 
  (declare (optimize (safety 2)))
  (caddr (cddddr x)))
(defun eighth (x) 
  (declare (optimize (safety 2)))
  (cadddr (cddddr x)))
(defun ninth (x) 
  (declare (optimize (safety 2)))
  (car (cddddr (cddddr x))))
(defun tenth (x) 
  (declare (optimize (safety 2)))
  (cadr (cddddr (cddddr x))))

; Courtesy Paul Dietz
(defmacro nth-value (n expr)
  (declare (optimize (safety 1)))
  `(nth ,n (multiple-value-list ,expr)))

(eval-when (compile eval)

  (defmacro repl-if (tc) `(labels ((l (tr &aux (k (if kf (funcall kf tr) tr)))
				       (cond (,tc n)
					     ((atom tr) tr)
					     ((let* ((ca (car tr))(a (l ca))(cd (cdr tr))(d (l cd)))
						(if (and (eq a ca) (eq d cd)) tr (cons a d)))))))
			     (declare (ftype (function (t) t) l))
			     (l tr))))

(defun subst (n o tr &key key test test-not
		&aux (kf (when key (coerce key 'function)))
		(tf (when test (coerce test 'function)))
		(ntf (when test-not (coerce test-not 'function))))
  (declare (optimize (safety 1)))
  (check-type key (or null function))
  (check-type test (or null function))
  (check-type test-not (or null function))
  (repl-if (cond (tf (funcall tf o k))(ntf (not (funcall ntf o k)))((eql o k)))))

(defun subst-if (n p tr &key key &aux (kf (when key (coerce key 'function))))
  (declare (optimize (safety 1)))
  (check-type p function)
  (check-type key (or null function))
  (repl-if (funcall p k)))
(defun subst-if-not (n p tr &key key &aux (kf (when key (coerce key 'function))))
  (declare (optimize (safety 1)))
  (check-type p function)
  (check-type key (or null function))
  (repl-if (not (funcall p k)))))
