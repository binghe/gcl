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


;;;;   seq.lsp
;;;;
;;;;                           sequence routines


(in-package :si)

#.`(defun make-sequence-element-type (xx &aux (x (cmp-norm-tp xx)))
     (or
      #+pre-gcl(when (eq xx 'string) 'character);accelerator
      (cdr (assoc x
		 ',(mapcar (lambda (x) (cons (cmp-norm-tp (car x)) (cdr x)))
			   `((null . null) (cons . cons) (list . list)
			     ,@(mapcar (lambda (x) `((vector ,x) . ,x)) +array-types+)))
		 :test 'type<=))
      (equal #tvector (if (listp x) (car x) x))))
(setf (get 'make-sequence-element-type 'type-propagator) 'compiler::expand-type-propagator)

(defun ntp-cons-lengths (x)
  (labels ((g (x) (if (integerp x) (1+ x) x))
	   (f (x) (mapcan (lambda (x)
			    (cond ((eq x t) (list '*))
				  ((cadr x) (mapcar #'g (ntp-cons-lengths (cadr x))))))
			  x)))
    (let ((y (nconc (f (cdr (assoc 'proper-cons (car x)))) (f (cdr (assoc 'improper-cons (car x)))))))
      (if (assoc-if-not (lambda (x) (or (eq x 'proper-cons) (eq x 'improper-cons))) (car x))
	  (cons 0 y) y))))

(defun cons-tp-lengths (tp &aux (tp (type-and #tcons tp)))
  (when (consp tp)
    (let ((x (lremove-duplicates (ntp-cons-lengths (caddr tp)))))
      (unless (member '* x)
	x))))

(defun ntp-vector-lengths (x)
  (labels ((fx (x) (mapcan (lambda (x)
			    (cond ((eq x t) (list '*))
				  ((and (consp x) (not (eq 'rank (car x)))) (list (car x)))))
			   x)))
    (lreduce (lambda (y x)
	       (when (rassoc (car x) *all-array-types*)
		 (nunion (fx (cdr x)) y)))
	     (car x) :initial-value nil)))

(defun vector-tp-lengths (tp &aux (tp (type-and #tvector tp)))
  (when (consp tp)
    (let ((x (lremove-duplicates (ntp-vector-lengths (caddr tp)))))
      (unless (member '* x) x))))

(defun sequence-tp-lengths (type &aux (tp (cmp-norm-tp type)))
  #+pre-gcl(when (eq type 'string) (return-from sequence-tp-lengths nil))
  (if (type<= tp #tlist)
      (cons-tp-lengths tp)
      (vector-tp-lengths tp)))
(setf (get 'sequence-tp-lengths 'type-propagator) 'compiler::expand-type-propagator)
					;type-lengths


(defun sequence-tp-nonsimple-p (type)
  #-pre-gcl(when (eq type 'string) (return-from sequence-tp-nonsimple-p nil))
  (type<= (cmp-norm-tp type) #tnon-simple-array))
(setf (get 'sequence-tp-nonsimple-p 'type-propagator) 'compiler::expand-type-propagator)

#.`(defun make-sequence (type size &key initial-element)
     (declare (optimize (safety 1)))
     (check-type type type-spec)
     (check-type size seqbnd)
     (let* ((st (make-sequence-element-type type));FIXME cmp-norm-tp once
	    (lns (sequence-tp-lengths type)))
       (check-type st (not null))
       (when lns
	 (assert (member size lns) (size) 'type-error :datum size :expected-type (cons 'member lns)))
       (case st
	 (null (check-type size (integer 0 0)) nil)
	 ((cons list)
	  (when (eq st 'cons) (check-type size (integer 1)))
	  (make-list size :initial-element initial-element))
	 (otherwise (make-vector st size (sequence-tp-nonsimple-p type) nil nil 0 nil initial-element)))))


(defun concatenate (rt &rest seqs)
  (declare (optimize (safety 1)) (dynamic-extent seqs))
  (macrolet
   ((++ (x &optional (n 1)) `(prog1 ,x (incf ,x ,n))));FIXME immnum
   (let* ((rs (make-sequence rt (reduce '+ seqs :key 'length :initial-value 0)))
	  (rt (unless (listp rs) (array-element-type rs)))(rh rs)(i 0))
     (dolist (seq seqs rs)
       (let* ((st (unless (listp seq) (array-element-type seq)))(sh seq)(j 0)
	      (ls (if st (length seq) array-dimension-limit)))
	 (if (when rt (eq rt st))
	     (set-array-n rs (++ i ls) seq (++ j ls) ls)
	     (do nil ((or (>= j ls) (unless st (endp sh))))
	       (let ((tmp (if st (aref seq (++ j)) (pop sh))))
		 (if rt (setf (aref rs (++ i)) tmp)
		     (setf (car rh) tmp rh (cdr rh)))))))))))

(eval-when
 (compile eval)

 (defmacro locsym (f s) `(si::sgen (concatenate 'string (string ,f) ,s)))
 
 (defmacro dyncpl (x &aux (l (locsym 'dyncpl "-LOOP")));FIXME this can't cons in a labels as it might be a separate fn.  Get do to unroll too.
   `(labels ((,l (x y) (when x (setf (car x) (car y)) (,l (cdr x) (cdr y)))))
	    (declare (notinline make-list))
	    (let ((tmp (make-list (length ,x))))
	      (declare (dynamic-extent tmp))
	      (,l tmp ,x);Can't be mapl, used by
	     tmp)))

 ;; (defmacro seqend (seq seqs &aux (l (locsym 'seqend "-LOOP"))(ll (locsym 'seqend "-LOOP")))
 ;;   `(labels ((,ll (x) (if (listp x) array-dimension-limit (length x)))
 ;; 	     (,l (s z) (if s (,l (cdr s) (min (,ll (car s)) z)) z)))
 ;; 	    (,l ,seqs (length ,seq))))
 
 (defmacro seqval (seq place i)
   `(if (listp ,seq) (pop ,place) (aref ,seq ,i)))

 (defmacro seqvals (vals ns i)
   `(mapl (lambda (x y &aux (yc (car y))) (setf (car x) (seqval yc (car y) ,i))) ,vals ,ns)))

(defun map (rt f seq &rest sqs &aux (f (coerce f 'function)) (l (listp seq));FIXME test array-dimension-limit instead of length for lists
	       (sl (reduce (lambda (y x) (min y (length x))) sqs :initial-value (length seq)))
	       (x (when rt (make-sequence rt sl)))(lx (listp x)))
  (declare (optimize (safety 1))(dynamic-extent sqs))
  (check-type rt type-spec)
  (check-type f function-designator)
  (check-type seq sequence)
  (labels ((ml (i xp seq ns vals) 
	       (unless (>= i sl)
		 (let ((tmp (apply f (if l (car seq) (aref seq i)) (seqvals vals ns i))))
		   (when rt (if lx (setf (car xp) tmp) (setf (aref x i) tmp))))
		 (ml (1+ i) (cdr xp) (if l (cdr seq) seq) ns vals))))
	  (ml 0 (when (consp x) x) seq (dyncpl sqs) (dyncpl sqs)) x))

;; (defun map (rt f seq &rest sqs &aux (f (tofn f)) (l (listp seq)) (sl (seqend seq sqs))(x (when rt (make-sequence rt sl))))
;;   (declare (optimize (safety 2)) (dynamic-extent sqs))
;;   (check-type rt type-spec)
;;   (check-type f fn)
;;   (check-type seq sequence)
;;   (labels ((ml (i xp seq ns vals) 
;; 	       (unless (or (>= i sl) (when l (endp seq)) (member-if (lambda (x) (when (listp x) (endp x))) ns))
;; ;	       (when (and (< i sl) (if l (not (endp seq)) t) (not (member-if (lambda (x) (when (listp x) (endp x))) ns)))
;; 		 (let ((tmp (apply f (if l (car seq) (aref seq i)) (seqvals vals ns i))))
;; 		   (cond (xp (setf (car xp) tmp)) (rt (setf (aref x i) tmp))))
;; 		 (ml (1+ i) (cdr xp) (if l (cdr seq) seq) ns vals))))
;; 	  (ml 0 (when (consp x) x) seq (dyncpl sqs) (dyncpl sqs)) x))

;; (defun map (rt f seq &rest sqs &aux (seqs (cons seq sqs)) (l (length seqs)) (vals (make-list l)))
;;   (declare (optimize (safety 2)))
;;   (declare (:dynamic-extent sqs seqs vals))
;;   (let ((sl (reduce (lambda (y x) (min y (length x))) seqs :initial-value array-dimension-limit)))
;;     (do* ((x (when rt (make-sequence rt sl)))(xc (consp x))(xp x)(i 0 (1+ i)))
;; 	 ((or (when xc (endp xp)) (>= i sl)) x)
;; 	 (mapl (lambda (x y &aux (z (car x)))
;; 		 (setf (car y) (if (listp z) (pop (car x)) (aref z i)))) seqs vals)
;; 	 (let ((tmp (apply f vals)))
;; 	   (cond (xc (setf (car xp) tmp xp (cdr xp))) (rt (setf (aref x i) tmp)))))))

(defun map-into (rs g &rest seqs &aux 
		    (h rs) (lp (unless (listp rs) (array-total-size rs))) 
		    (fp (when lp (array-has-fill-pointer-p rs)))(j 0))
  (declare (optimize (safety 1))(dynamic-extent seqs))
  (check-type rs proper-sequence)
  (unless (member rs seqs) (when fp (setf (fill-pointer rs) lp)))
  (block exit
	 (apply 'map nil
		(lambda (x &rest r) 
		  (when (if lp (= j lp) (endp h)) (return-from exit))
		  (let ((tmp (apply g r))) 
		    (if lp (setf (aref rs j) tmp j (1+ j)) (setf (car h) tmp h (cdr h))))) rs seqs))
  (when fp (setf (fill-pointer rs) j))
  rs)
