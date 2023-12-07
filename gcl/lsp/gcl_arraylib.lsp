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


;;;;    arraylib.lsp
;;;;
;;;;                            array routines


;; (in-package :lisp)

(in-package :system)

;; (use-package :s)

;; (export 'strcat)

;; (defun strcat (&rest r)
;;   (declare (:dynamic-extent r))
;;   (apply 'string-concatenate (mapcar 'string-downcase r)))

(eval-when 
 (compile eval)

 (defun proto-array (tp) (make-vector tp 1 t nil nil 0 nil nil))

 ;(car (assoc x s::+ks+ :test (lambda (x y) (subtypep x (get y 'compiler::lisp-type)))));FIXME vs bug in interpreter
 ;; (defun af (x &aux (x (caar (member x s::+ks+ :test (lambda (x y) (subtypep x (get (car y) 'compiler::lisp-type))))))) 
 ;;   (intern (string-concatenate "*" (string (or x :object))) :s))

 (defun af (x) (cdr (assoc x '((character . *char) (bit . *char) (non-negative-char . *char);fixme
			       (unsigned-char . *uchar) (signed-char . *char)
			       #+64bit (non-negative-int . *int) #+64bit (unsigned-int . *uint) #+64bit (signed-int . *int)
			       (non-negative-short . *short) (unsigned-short . *ushort)
			       (signed-short . *short) (short-float . *float) (long-float . *double)
			       (t . *object) (non-negative-fixnum . *fixnum) (fixnum . *fixnum)))))
 
 (defvar *array-type-info* (mapcar (lambda (x &aux (y (proto-array x))) 
				     (list x (c-array-elttype y) (array-eltsize y) (array-mode y) (af x)))
				   +array-types+))

 (defun maybe-cons (car cdr)
   (if (cdr cdr) (cons car cdr) (car cdr))))

(defun c-array-eltsize (x) (array-eltsize x));FIXME

#.`(defun set-array (r i s j &optional sw);assumes arrays of same type and indices in bounds
     (declare (optimize (safety 1))(seqind i j))
     (check-type r array)
     (check-type s array)
     (flet ((sp (r i s j gf sf &aux (x (when sw (funcall gf r i)))) 
		(funcall sf (funcall gf s j) r i)
		(when sw (funcall sf x s j))))
       (case 
	(c-array-eltsize r);fixme, done?
	,@(mapcar (lambda (x &aux (z (pop x))(y (pop x))(w (car x)))
		    `(,z (infer-tp
			  r ,y (infer-tp
				s ,y
				,(if (zerop z)
				     `(sp r i s j #'0-byte-array-self #'set-0-byte-array-self)
				   `(let* ((rs (c-array-self r))(ss (if (eq r s) rs (c-array-self s))))
				      (sp rs i ss j
					  (lambda (rs i) (,w rs i nil nil))
					  (lambda (v rs i) (,w rs i t v)))))))))
		  (lreduce (lambda (y x &aux (sz (caddr x))(fn (fifth x))(z (assoc sz y))(tp (cmp-norm-tp `(array ,(car x)))))
			     (cond (z (setf (cadr z) (type-or1 (cadr z) tp) (caddr z) fn) y)
				   ((cons (list sz tp fn) y))))
			   si::*array-type-info* :initial-value nil)))))
(declaim (inline set-array))

(defun set-array-n (r i s j n);assumes arrays of same type and indices in bounds
  (declare (optimize (safety 1))(seqind i j n));FIXME
  (check-type r array)
  (check-type s array)
  (let ((z (c-array-eltsize r)))
    (if (zerop z)
	(copy-bit-vector r i s j n)
	(let* ((rs (c-array-self r))(ss (if (eq r s) rs (c-array-self s))))
	  (memmove (c+ rs (<< i (1- z))) (c+ ss (<< j (1- z))) (<< n (1- z))))))
  r)
(declaim (inline set-array-n))

#.`(defun array-element-type (x)
     (declare (optimize (safety 1)))
     (check-type x array)
     (case
	 (c-array-elttype x)
       ,@(mapcar (lambda (x &aux (tp (pop x))) `(,(car x) ',tp)) *array-type-info*)))

#.`(defun row-major-aref-int (a i)
     (ecase
	 (c-array-elttype a)
       ,@(mapcar (lambda (y &aux (x (pop y)))
		   `(,(pop y) 
		     ,(case x
			(character `(code-char (*uchar (c-array-self a) i nil nil)))
			(bit `(0-byte-array-self a i))
			(otherwise `(,(caddr y) (c-array-self a) i nil nil)))))
		 *array-type-info*)))
(declaim (inline row-major-aref-int))

(defun row-major-aref (a i)
     (declare (optimize (safety 1)))
     (check-type a array)
     (check-type i seqind)
     (assert (< i (array-total-size a)) (i) 'type-error :datum i :expected-type `(integer 0 (,(array-total-size a))))
     (row-major-aref-int a i))

#.`(defun row-major-aset (v a i)
     (declare (optimize (safety 1)))
     (check-type a array)
     (check-type i seqind)
     (assert (< i (array-total-size a)) (i) 'type-error :datum i :expected-type `(integer 0 (,(array-total-size a))))
     (ecase
	 (c-array-elttype a)
       ,@(mapcar (lambda (y &aux (x (pop y)))
		   `(,(pop y)
		     (check-type v ,x)
		     ,(case x
			(character `(progn (*uchar (c-array-self a) i t (char-code v)) v))
			(bit `(set-0-byte-array-self v a i))
			(otherwise `(,(caddr y) (c-array-self a) i t v)))))
		 *array-type-info*)))
(setf (get 'row-major-aset 'compiler::consider-inline) t)


(defun 0-byte-array-self (array index)
  (declare (optimize (safety 1)))
  (check-type array (array bit))
  (check-type index seqind)
  (let* ((off (+ index (array-offset array)))
	 (ind (>> off #.(1- (integer-length fixnum-length))))
	 (word (*fixnum (c-array-self array) ind nil nil))
	 (shft (& off #.(1- fixnum-length))))
    (& (>> word shft) 1)))
(declaim (inline 0-byte-array-self))

(defun set-0-byte-array-self (bit array index)
  (declare (optimize (safety 1)))
  (check-type array (array bit))
  (check-type index seqind)
  (check-type bit bit)
  (let* ((off (+ index (array-offset array)))
	 (ind (>> off #.(1- (integer-length fixnum-length))))
	 (word (*fixnum (c-array-self array) ind nil nil))
	 (shft (& off #.(1- fixnum-length)))
	 (val (<< 1 shft)))
    (*fixnum (c-array-self array) ind t (if (zerop bit) (& word (~ val)) (\| word val)))
    bit))
(declaim (inline set-0-byte-array-self))

(defun array-row-major-index (array &rest indices)
  (declare (dynamic-extent indices)(optimize (safety 2)))
  (check-type array array)
  (assert (apply 'array-in-bounds-p array indices));FIXME type-error??
  (labels ((cpt (i j k l) (the seqind (if (zerop k) i (c+ i (c* j l)))));FIXME
	   (armi-loop (s &optional (j 0) (k 0)) 
		      (declare (rnkind k));FIXME
		      (if s (armi-loop (cdr s) (cpt (car s) j k (array-dimension array k)) (1+ k)) j)))
	  (armi-loop indices)))

;; (defun array-row-major-index (array &rest indices)
;;   (declare (:dynamic-extent indices))
;;   (labels ((cpt (i j k)	(check-type i seqind) (if (zerop j) i (+ i (the seqind (* j k)))));FIXME
;; 	   (armi-loop (s &optional (j 0) (k 0)) (if s (armi-loop (cdr s) (cpt (car s) j (array-dimension array k)) (1+ k)) j)))
;; 	  (armi-loop indices)))

(defun aref (a &rest q)
  (declare (optimize (safety 1)) (dynamic-extent q))
  (check-type a array)
  (row-major-aref a (apply 'array-row-major-index a q)))

#-(and pre-gcl raw-image)
(defun si::aset (v a &rest q)
  (declare (optimize (safety 1)) (dynamic-extent q))
  (check-type a array)
  (row-major-aset v a (apply 'array-row-major-index a q)))
(declaim (inline si::aset))

(setf (symbol-function 'array-rank) (symbol-function 'c-array-rank)
      (symbol-function 'array-total-size) (symbol-function 'c-array-dim))

(defun array-in-bounds-p (a &rest i &aux (j 0))
  (declare (optimize (safety 1)) (dynamic-extent i))
  (check-type a array)
  (unless (member-if-not (lambda (x) (< -1 x (array-dimension a (prog1 j (incf j))))) i)
       (= j (c-array-rank a))))

;; (defun array-in-bounds-p (a &rest i)
;;   (declare (optimize (safety 1)) (:dynamic-extent i))
;;   (check-type a array)
;;   (let ((r (array-rank a)))
;;     (labels ((aibp-loop (i &optional (j 0))
;; 			(cond ((>= j r))
;; 			      ((not i) (error "bad indices"))
;; 			      ((< -1 (car i) (array-dimension a j)) (aibp-loop (cdr i) (1+ j))))))
;; 	    (aibp-loop i))))

(defun array-dimension (x i)
  (declare (optimize (safety 2)))
  (check-type x array)
  (check-type i rnkind)
  (let ((r (c-array-rank x)));FIXME
    (assert (< i r) (i) 'type-error :datum i :expected-type `(integer 0 (,r)))
    (if (= 1 r) (c-array-dim x) (array-dims x i))));(the seqind (*fixnum (c-array-dims x) i nil nil))

(defun array-displacement (x)
  (declare (optimize (safety 1)))
  (check-type x array)
  (let ((x (typecase x (adjustable-array (car (c-adjarray-displaced x))))))
    (values (car x) (or (cdr x) 0))))


;; (defun array-dimension (x i)
;;   (declare (optimize (safety 2)))
;;   (check-type x array)
;;   (check-type i rnkind)
;;   (let ((r (c-array-rank x)));FIXME
;;     (let ((*dim* r)(i i))(check-type i (satisfies array-dimension-index-less-than-rank)))
;;     (if (= 1 r) (c-array-dim x) (the seqind (*fixnum (c-array-dims x) i nil nil)))))

;; (defun array-dimension (x i)
;;   (declare (optimize (safety 1)))
;;   (check-type x array)
;;   (check-type i rnkind)
;;   (let ((r (array-rank x)))
;;     (if (= 1 r) (c-array-dim x) (the seqind (*fixnum (c-array-dims x) i nil nil)))))
					;FIXME

(defun array-dimensions (x &aux (j 0))
  (declare (optimize (safety 1)))
  (check-type x array)
  (mapl (lambda (y) (setf (car y) (array-dimension x (prog1 j (incf j))))) (make-list (c-array-rank x))));FIXME c-array-rank propagator


(defun array-has-fill-pointer-p (x)
  (declare (optimize (safety 1)))
  (check-type x array)
  (typecase
   x
   (adjustable-vector
    (not (zerop (c-array-hasfillp x))))))

;; (defun upgraded-array-element-type (type &optional environment)
;;   (declare (ignore environment) (optimize (safety 1)))
;;   (cond ((not type))
;; 	((eq type '*) '*)
;; 	((car (member type +array-types+)))
;; 	((car (member type +array-types+ :test 'subtypep1)))
;; 	((subtypep1 type 'float) 'long-float)
;; 	(t)))

(defun fill-pointer (x)
  (declare (optimize (safety 1)))
  (check-type x adjustable-vector)
  (assert (array-has-fill-pointer-p x) (x) 'type-error :datum x :expected-type '(satisfies array-has-fill-pointer-p))
  (c-adjvector-fillp x))
;  (fill-pointer-internal x)

(defun make-array (dimensions
		   &key (element-type t)
		   initial-element
		   (initial-contents nil icsp)
		   adjustable fill-pointer
		   displaced-to (displaced-index-offset 0)
		   static
		   &aux
		   (dimensions (if (and (listp dimensions) (not (cdr dimensions))) (car dimensions) dimensions))
		   (element-type (upgraded-array-element-type element-type)))
  (declare (optimize (safety 1)))
  (check-type fill-pointer (or boolean integer))
  (check-type displaced-to (or null array))
  (check-type displaced-index-offset integer)
  (etypecase 
      dimensions
    (list
     (assert (not fill-pointer))
     (dolist (d dimensions) (check-type d integer))
     (let ((x (make-array1 element-type static initial-element displaced-to displaced-index-offset dimensions adjustable)))
       (when (unless (member 0 dimensions) icsp)
	 (let ((i -1))
	   (labels ((set (d c) (cond (d (assert (eql (car d) (length c))) (map nil (lambda (z) (set (cdr d) z)) c))
				     ((row-major-aset c x (incf i))))))
	     (set dimensions initial-contents))))
       x))
    (integer
     (let ((x (make-vector element-type dimensions adjustable (when fill-pointer dimensions)
			   displaced-to displaced-index-offset static initial-element)))
       (when icsp (replace x initial-contents))
       (when (and fill-pointer (not (eq t fill-pointer))) (setf (fill-pointer x) fill-pointer))
       x))))

(defun vector (&rest objects)
  (declare (dynamic-extent objects))
  (make-array (length objects) :element-type t :initial-contents objects))

(deftype bit-array nil `(array bit))
(deftype simple-bit-array nil `(simple-array bit))

(defun bit (bit-array &rest indices)
  (declare (dynamic-extent indices)(optimize (safety 1)))
  (check-type bit-array bit-array)
  (apply 'aref bit-array indices))

#-(and pre-gcl raw-image)
(defun sbit (bit-array &rest indices)
  (declare (dynamic-extent indices)(optimize (safety 1)))
  (check-type bit-array simple-bit-array)
  (apply 'aref bit-array indices))


(defun vector-push (new-element vector)
  (declare (optimize (safety 1)))
  (check-type vector adjustable-vector)
  (assert (array-has-fill-pointer-p vector) (vector) 'type-error :datum vector :expected-type '(satisfies array-has-fill-pointer-p));FIXME
  (let ((fp (fill-pointer vector)))
    (cond ((< fp (array-dimension vector 0))
	   (setf (aref vector fp) new-element (fill-pointer vector) (1+ fp))
	   fp))))


(defun vector-push-extend (new-element vector &optional extension)
  (declare (optimize (safety 1)))
  (check-type vector adjustable-vector)
  (assert (array-has-fill-pointer-p vector) (vector) 'type-error :datum vector :expected-type '(satisfies array-has-fill-pointer-p))
  (let* ((fp (fill-pointer vector))
	 (dim (array-dimension vector 0))
	 (vector (if (< fp dim) vector
		   (adjust-array vector (the seqind (+ dim (or extension (max 5 dim))))
				 :element-type (array-element-type vector)
				 :fill-pointer fp))))
    (setf (aref vector fp) new-element (fill-pointer vector) (1+ fp))
    fp))



(defun vector-pop (vector)
  (declare (optimize (safety 1)))
  (check-type vector adjustable-vector)
  (assert (array-has-fill-pointer-p vector) (vector) 'type-error :datum vector :expected-type '(satisfies array-has-fill-pointer-p))
  (let ((fp (fill-pointer vector)))
    (check-type fp (integer 1))
    (setf (fill-pointer vector) (1- fp))
    (aref vector (1- fp))))


(defun adjustable-array-p (array)
  (declare (optimize (safety 1)))
  (check-type array array)
  (typep array 'adjustable-array))

(defun adjust-array (array new-dimensions
                     &rest r
		     &key element-type
			  initial-element
			  (initial-contents nil initial-contents-supplied-p)
			  (fill-pointer nil fill-pointer-supplied-p)
			  (displaced-to nil)
			  (displaced-index-offset 0)
			  (static nil static-supplied-p))

  (declare (ignore initial-element initial-contents static displaced-index-offset) ;FIXME
	   (dynamic-extent r)
	   (optimize (safety 2)))

  (check-type array array)
  (check-type new-dimensions (or seqind proper-list))

  (when (and (listp new-dimensions) (not (cdr new-dimensions))) (setq new-dimensions (car new-dimensions)))
  
  (setq element-type (array-element-type array))
  (unless (eq element-type t)
    (setq r (cons element-type r) r (cons :element-type r)))

  (unless static-supplied-p
    (setq r (cons (staticp array) r) r (cons :static r)))

  (cond (fill-pointer-supplied-p
	 (let ((fill-pointer (or fill-pointer (when (array-has-fill-pointer-p array) (fill-pointer array)))))
	   (setf (cadr (member :fill-pointer r)) fill-pointer)))
	((array-has-fill-pointer-p array) (setq r (cons (fill-pointer array) r) r (cons :fill-pointer r))))
      
  (let ((x (apply 'make-array new-dimensions :adjustable t r))) ;FIXME avoid when possible

    (unless (or displaced-to initial-contents-supplied-p)

      (cond ((or (seqindp new-dimensions)
		 (and (equal (cdr new-dimensions) (cdr (array-dimensions array)))
		      (or (not (eq element-type 'bit))
			  (when new-dimensions (= 0 (mod (the seqind (car (last new-dimensions))) char-length))))))
	     (copy-array-portion array x 0 0 (min (array-total-size x) (array-total-size array))))
	    ((let ((i -1))
	       (labels ((set (dim &optional (cur (make-list (length new-dimensions) :initial-element 0)) (ind cur))
			     (declare (dynamic-extent cur))
			     (cond (dim (dotimes (i (pop dim)) (setf (car cur) i) (set dim (cdr cur) ind)))
				   ((incf i)
				    (when (apply 'array-in-bounds-p array ind) 
				      (row-major-aset (apply 'aref array ind) x i))))))
		 (set new-dimensions))))))

    (if (typep array 'unadjustable-array)
	(setq array x)
      (replace-array array x))

    (when (eq fill-pointer t)
      (setq fill-pointer (array-total-size array)))
    (when fill-pointer
      (setf (fill-pointer array) fill-pointer))
   
    array))

(defun array-total-size (a)
  (declare (optimize (safety 1)))
  (check-type a array)
  (c-array-dim a))

(defun array-rank (a)
  (declare (optimize (safety 1)))
  (check-type a array)
  (c-array-rank a))

(defun array-eltsize-propagator (f x)
  (declare (ignore f))
  (when (type>= #tarray x)
    (cmp-norm-tp
     (cons 'member
	   (lreduce (lambda (y z)
		      (if (tp<= x (car z)) y (cons (cdr z) y)))
		    '#.(mapcar (lambda (x) (cons (cmp-norm-tp `(not (array ,(pop x)))) (cadr x)))
			       *array-type-info*)
		    :initial-value nil)))))
(setf (get 'c-array-eltsize 'type-propagator) 'array-eltsize-propagator)

(defun array-elttype-propagator (f x)
  (declare (ignore f))
  (when (type>= #tarray x)
    (cmp-norm-tp
     (cons 'member
	   (lreduce (lambda (y z)
		      (if (tp<= x (car z)) y (cons (cdr z) y)))
		    '#.(mapcar (lambda (x) (cons (cmp-norm-tp `(not (array ,(pop x)))) (car x)))
			       *array-type-info*)
		    :initial-value nil)))))
(setf (get 'c-array-elttype 'type-propagator) 'array-elttype-propagator)

(defun array-rank-propagator (f x)
  (declare (ignore f))
  (cond
   ((type>= #tvector x) #t(member 1))
   ((let ((d (atomic-tp-array-dimensions x)));FIXME integer rnk
      (when d (object-tp (length (car d))))))
   ((type>= #tarray x) #trnkind)))
(setf (get 'c-array-rank 'type-propagator) 'array-rank-propagator)

(defun array-dim-propagator (f t1 &aux (d (atomic-tp-array-dimensions t1)));c-array-dim?
  (declare (ignore f))
  (when d
    (object-tp (reduce '* (car d)))))
(setf (get 'c-array-dim 'type-propagator) 'array-dim-propagator)

(defun svref (x i) 
  (declare (optimize (safety 1)))
  (check-type x simple-vector)
  (check-type i seqind)
  (aref x i))

(defun svset (x i v)
  (declare (optimize (safety 1)))
  (check-type x simple-vector)
  (check-type i seqind)
  (aset v x i))
(setf (get 'svset 'cmp-inline) t)

#.`(defun array-eql-is-eq (x)
     (case (c-array-elttype x)
       (,(mapcan (lambda (x)
		   (when (subtypep x 'eql-is-eq-tp)
		     (list (c-array-elttype (make-array 1 :element-type x)))))
		 +array-types+)
	t)))
(declaim (inline array-eql-is-eq))
