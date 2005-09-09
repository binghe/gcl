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


(in-package 'lisp)


(export '(make-array array-displacement vector
          array-element-type array-rank array-dimension
          array-dimensions
          array-in-bounds-p array-row-major-index
          adjustable-array-p
          bit sbit 
          bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor
          bit-andc1 bit-andc2 bit-orc1 bit-orc2 bit-not
          array-has-fill-pointer-p fill-pointer
          vector-push vector-push-extend vector-pop
          adjust-array upgraded-array-element-type))

(in-package 'system)


(proclaim '(optimize (safety 2) (space 3)))

(defun best-array-element-type (type)
  (cond ((or (eql t type) (null type))
	 t)
	((memq type '(bit unsigned-char signed-char
				    unsigned-short
				    signed-short fixnum))
	       type)
	((subtypep type 'fixnum)
	 (dolist (v '(bit unsigned-char signed-char
				    unsigned-short
				    signed-short)
		    'fixnum)
		 (cond ((subtypep type v)
			(return v)))))
	((eql type 'character) 'string-char)
	(t (or (dolist (v '(string-char bit short-float
				    long-float))
		   (cond ((subtypep type v)
			  (return v))))
	       t))))
	 
(defun upgraded-array-element-type (type &optional environment)
  (declare (ignore environment))
  (best-array-element-type type))

;(defun array-displacement (array)
;  (let ((x (si:array-displacement1 array)))
;  (values (car x) (cdr x)))
;  )

(defun make-array (dimensions
		   &key (element-type t)
			(initial-element nil)
			(initial-contents nil initial-contents-supplied-p)
			adjustable fill-pointer
			displaced-to (displaced-index-offset 0)
			static)
  (when (integerp dimensions) (setq dimensions (list dimensions)))
  (setq element-type (best-array-element-type element-type))
  (cond ((= (length dimensions) 1)
	 (let ((x (si:make-vector element-type (car dimensions)
	                          adjustable fill-pointer
	                          displaced-to displaced-index-offset
	                          static initial-element)))
	   (when initial-contents-supplied-p
		 (do ((n (car dimensions))
		      (i 0 (1+ i)))
		     ((>= i n))
		   (declare (fixnum n i))
		   (si:aset x i (elt initial-contents i))))
	   x))
        (t
	 (let ((x
		(make-array1
		       (the fixnum(get-aelttype element-type))
			static initial-element 
		       displaced-to (the fixnum displaced-index-offset)
		       dimensions)))
	   (if fill-pointer (error "fill pointer for 1 dimensional arrays only"))
           (unless (member 0 dimensions)
	   (when initial-contents-supplied-p
		 (do ((cursor
		       (make-list (length dimensions)
		                  :initial-element 0)))
		     (nil)
		     (declare (:dynamic-extent cursor))
		   (aset-by-cursor x
			           (sequence-cursor initial-contents
			                            cursor)
				   cursor)
		   (when (increment-cursor cursor dimensions)
                          (return nil)))))
            x))))


(defun increment-cursor (cursor dimensions)
  (if (null cursor)
      t
      (let ((carry (increment-cursor (cdr cursor) (cdr dimensions))))
	(if carry
	    (cond ((>= (the fixnum (1+ (the fixnum (car cursor))))
	               (the fixnum (car dimensions)))
		   (rplaca cursor 0)
		   t)
		  (t
		   (rplaca cursor
		           (the fixnum (1+ (the fixnum (car cursor)))))
		   nil))
	    nil))))


(defun sequence-cursor (sequence cursor)
  (if (null cursor)
      sequence
      (sequence-cursor (elt sequence (the fixnum (car cursor)))
                       (cdr cursor))))


(defun vector (&rest objects &aux (l (list (length objects))))
  (declare (:dynamic-extent objects l))
  (make-array l
	      :element-type t
	      :initial-contents objects))


(defun array-dimensions (array)
  (do ((i (array-rank array))
       (d nil))
      ((= i 0) d)
    (setq i (1- i))
    (setq d (cons (array-dimension array i) d))))


(defun array-in-bounds-p (array &rest indices &aux (r (array-rank array)))
  (declare (:dynamic-extent indices))
  (when (/= r (length indices))
        (error "The rank of the array is ~R,~%~
               ~7@Tbut ~R ~:*~[indices are~;index is~:;indices are~] ~
               supplied."
               r (length indices)))
  (do ((i 0 (1+ i))
       (s indices (cdr s)))
      ((>= i r) t)
    (when (or (< (car s) 0)
              (>= (car s) (array-dimension array i)))
          (return nil))))


(defun array-row-major-index (array &rest indices)
  (declare (:dynamic-extent indices))
  (do ((i 0 (1+ i))
       (j 0 (+ (* j (array-dimension array i)) (car s)))
       (s indices (cdr s)))
      ((null s) j)))


(defun bit (bit-array &rest indices)
  (declare (:dynamic-extent indices))
  (apply #'aref bit-array indices))


(defun sbit (bit-array &rest indices)
  (declare (:dynamic-extent indices))
  (apply #'aref bit-array indices))


(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-and bit-array1 bit-array2 result-bit-array))


(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-ior bit-array1 bit-array2 result-bit-array))


(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-xor bit-array1 bit-array2 result-bit-array))


(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-eqv bit-array1 bit-array2 result-bit-array))

    
(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-nand bit-array1 bit-array2 result-bit-array))

    
(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-nor bit-array1 bit-array2 result-bit-array))

    
(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-andc1 bit-array1 bit-array2 result-bit-array))

    
(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-andc2 bit-array1 bit-array2 result-bit-array))

    
(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-orc1 bit-array1 bit-array2 result-bit-array))

    
(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-orc2 bit-array1 bit-array2 result-bit-array))

    
(defun bit-not (bit-array &optional result-bit-array)
  (bit-array-op boole-c1 bit-array bit-array result-bit-array))


(defun vector-push (new-element vector)
  (let ((fp (fill-pointer vector)))
    (declare (fixnum fp))
    (cond ((< fp (the fixnum (array-dimension vector 0)))
           (si:aset vector fp new-element)
           (si:fill-pointer-set vector (the fixnum (1+ fp)))
	   fp)
	  (t nil))))


(defun vector-push-extend (new-element vector &optional extension)
  (let ((fp (fill-pointer vector)))
    (declare (fixnum fp))
    (cond ((< fp (the fixnum (array-dimension vector 0)))
	   (si:aset vector fp new-element)
	   (si:fill-pointer-set vector (the fixnum (1+ fp)))
	   fp)
	  (t
	   (adjust-array vector
	                 (list (+ (array-dimension vector 0)
				  (or extension
				      (if (> (array-dimension vector 0)  0)
					  (array-dimension vector 0)
					5))))
	                 :element-type (array-element-type vector)
			 :fill-pointer fp)
	   (si:aset vector fp new-element)
	   (si:fill-pointer-set vector (the fixnum (1+ fp)))
	   fp))))


(defun vector-pop (vector)
  (let ((fp (fill-pointer vector)))
    (declare (fixnum fp))
    (when (= fp 0)
          (error "The fill pointer of the vector ~S zero." vector))
    (si:fill-pointer-set vector (the fixnum (1- fp)))
    (aref vector (the fixnum (1- fp)))))


(defun adjust-array (array new-dimensions
                     &rest r
		     &key element-type
			  initial-element
			  initial-contents
			  fill-pointer
			  displaced-to
			  displaced-index-offset
			  static
                     &aux fill-pointer-spec
                      )
  (declare (ignore 
                   initial-element
                   initial-contents
                   fill-pointer
                   displaced-to
                   displaced-index-offset
                   static))
  (declare (:dynamic-extent r new-dimensions))
  (when (integerp new-dimensions)
        (setq new-dimensions (list new-dimensions)))
  (if (setq fill-pointer-spec (member :fill-pointer r))
      (unless (array-has-fill-pointer-p array)
	      (error ":fill-pointer specified for array with no fill pointer"))
    (when (array-has-fill-pointer-p array)
      (push (fill-pointer array) r) (push :fill-pointer r)))

  (setq element-type (array-element-type array))
  (unless (eq element-type t) (push element-type r)
	  (push :element-type r))
  (unless (member :static r)
        (push (staticp array) r) (push :static r))
  (let ((x (apply #'make-array new-dimensions :adjustable t r)))	
    (cond ((or (null (cdr new-dimensions))
	       (and (equal (cdr new-dimensions)
			   (cdr (array-dimensions array)))
		    (or (not (eq element-type 'bit))
			(eql 0 (the fixnum
				    (mod
				      (the fixnum (car (last new-dimensions)))
				      char-size))))))
	   (copy-array-portion array   x
			       0 0
			       (min (array-total-size x)
				    (array-total-size array))))
	  (t
	    (do ((cursor (make-list (length new-dimensions)
				    :initial-element 0)))
		(nil)
		(declare (:dynamic-extent cursor))
		(when (apply #'array-in-bounds-p array cursor)
		      (aset-by-cursor x
				      (apply #'aref array cursor)
				      cursor))
		(when (increment-cursor cursor new-dimensions)
		      (return nil)))))
    
    (si:replace-array array x)
    (setf fill-pointer-spec (cadr fill-pointer-spec))
    (when fill-pointer-spec
        (cond ((eql t fill-pointer-spec)
	       (setf (fill-pointer array) (array-total-size array)))
	      ((typep fill-pointer-spec 'fixnum)
	       (setf (fill-pointer array) fill-pointer-spec))
	      (t (error "bad :fill-pointer arg: ~a" fill-pointer-spec))))
    array
    ))




