;;-*-Lisp-*-
;;; CMPTYPE  Type information.
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


(defun get-sym (args)
  (intern (apply 'concatenate 'string (mapcar 'string args))))


(defvar *c-types* (mapcar (lambda (x &aux (y (pop x)))
			    (list* y (cmp-norm-tp y) x))
`((nil                   nil                nil                nil                ""          ""                "object ")
  (null                  nil                nil                inline-cond        ""          ""                "object ")
  (true                  nil                nil                inline-cond        ""          ""                "object ")
  (boolean               nil                nil                inline-cond        ""          ""                "object ")
  (character             wt-character-loc   nil                inline-character   "char_code" "code_char"       "int8_t ")
  (bit                   wt-char-loc        return-char        inline-char        "fix"       "make_fixnum"     "int8_t ")
  (non-negative-char     wt-char-loc        return-char        inline-char        "fix"       "make_fixnum"     "int8_t ")
  (unsigned-char         wt-char-loc        return-char        inline-char        "fix"       "make_fixnum"     "uint8_t ")
  (signed-char           wt-char-loc        return-char        inline-char        "fix"       "make_fixnum"     "int8_t ")
  (char                  wt-char-loc        return-char        inline-char        "fix"       "make_fixnum"     "int8_t ")
  (non-negative-short    wt-fixnum-loc      return-fixnum      inline-fixnum      "fix"       "make_fixnum"     "int16_t ")
  (unsigned-short        wt-fixnum-loc      return-fixnum      inline-fixnum      "fix"       "make_fixnum"     "int16_t ")
  (signed-short          wt-fixnum-loc      return-fixnum      inline-fixnum      "fix"       "make_fixnum"     "uint16_t ")
  (non-negative-int      wt-fixnum-loc      return-fixnum      inline-fixnum      "fix"       "make_fixnum"     "int32_t ")
  (unsigned-int          wt-fixnum-loc      return-fixnum      inline-fixnum      "fix"       "make_fixnum"     "int32_t ")
  (signed-int            wt-fixnum-loc      return-fixnum      inline-fixnum      "fix"       "make_fixnum"     "uint32_t ")
  (non-negative-fixnum   wt-fixnum-loc      return-fixnum      inline-fixnum      "fix"       "make_fixnum"     "fixnum ")
  (fixnum                wt-fixnum-loc      return-fixnum      inline-fixnum      "fix"       "make_fixnum"     "fixnum ")
  (short-float           wt-short-float-loc return-short-float inline-short-float "sf"        "make_shortfloat" "float ")
  (long-float            wt-long-float-loc  return-long-float  inline-long-float  "lf"        "make_longfloat"  "double ") 
  (creal                 nil                nil                nil                ""          ""                "")
  (fcomplex              wt-fcomplex-loc    return-fcomplex    inline-fcomplex    "sfc"       "make_fcomplex"   "fcomplex ")
  (dcomplex              wt-dcomplex-loc    return-dcomplex    inline-dcomplex    "lfc"       "make_dcomplex"   "dcomplex ")
  (cnum                  nil                nil                nil                ""          ""                "")
  (t                     wt-loc             return-object      inline             ""          ""                "object "))))


(defconstant +c-global-arg-types-syms+   `(fixnum)) ;FIXME (long-float short-float) later
(defconstant +c-local-arg-types-syms+    (union +c-global-arg-types-syms+ '(char fixnum long-float short-float fcomplex dcomplex)))
(defconstant +c-local-var-types-syms+    (union +c-local-arg-types-syms+ '(char fixnum long-float short-float fcomplex dcomplex)))


(defvar +value-types+
  (mapcar (lambda (x)
	    (cons (cadr (assoc x *c-types*)) (get-sym `(,x "-VALUE"))))
	  (cons 'character +c-local-var-types-syms+)))


(defconstant +return-alist+
  (mapcar (lambda (x)
	    (cons (if (eq x 'object) x (cadr (assoc x *c-types*)))
		  (get-sym `("RETURN-" ,x))))
	  (cons 'object +c-local-arg-types-syms+)))

(defconstant +wt-loc-alist+
  `((object . wt-loc)
    ,@(mapcar (lambda (x)
		(cons (cadr (assoc x *c-types*))
		      (get-sym `("WT-" ,x "-LOC"))))
	      +c-local-var-types-syms+)))

(defconstant +inline-types-alist+
  `(,@(mapcar (lambda (x)
		(cons (cadr (assoc x *c-types*))
		      (case x
			    ((t) 'inline)
			    (boolean 'inline-cond)
			    (otherwise (get-sym `("INLINE-" ,x))))))
	      (list* 'boolean t +c-local-var-types-syms+))))

(defconstant +c-global-arg-types+
  (mapcar (lambda (x) (cadr (assoc x *c-types*))) +c-global-arg-types-syms+))

(defconstant +c-local-arg-types+
  (mapcar (lambda (x) (cadr (assoc x *c-types*))) +c-local-arg-types-syms+))

(defconstant +c-local-var-types+
  (mapcar (lambda (x) (cadr (assoc x *c-types*))) +c-local-var-types-syms+))

(defconstant +wt-c-var-alist+
  (nconc
   (mapcar (lambda (x &aux (z (assoc x *c-types*)))
	     (cons (cadr z) (seventh z)))
	   '(char fixnum character short-float long-float fcomplex dcomplex))
   `((object . ""))))

(defconstant +to-c-var-alist+
  (nconc
   (mapcar (lambda (x &aux (z (assoc x *c-types*)))
	     (cons (cadr z) (sixth z)))
	   '(char fixnum character short-float long-float fcomplex dcomplex))
   `((object . ""))))

(defconstant +c-type-string-alist+
  (mapcar (lambda (x &aux (z (assoc x *c-types*)))
	    (cons (cadr z) (eighth z)))
	  `(t bit character signed-char non-negative-char unsigned-char
	      signed-short non-negative-short unsigned-short fixnum
	      non-negative-fixnum signed-int non-negative-int unsigned-int
	      long-float short-float fcomplex dcomplex)))

(defconstant +cmp-array-types+
  (mapcar (lambda (x) (cadr (assoc x *c-types*))) +array-types+))

(defconstant +wt-c-rep-alist+
  (nconc
   (mapcar (lambda (x &aux (z (assoc x *c-types*)))
	     (cons (cadr z) (eighth z)))
	   `(nil char fixnum long-float short-float fcomplex dcomplex))
   `((object . "object "))))



(defconstant +cmp-type-alist+
  (mapcar (lambda (x) (cons (cmp-norm-tp (car x)) (cdr x))) +type-alist+))


;FIXME?
(defconstant +promoted-c-types+
  (nconc
   (mapcar (lambda (x) (cadr (assoc x *c-types*)))
	   '(nil null boolean))
   +c-local-var-types+))


(defconstant +clzl0+ (let ((x (1- fixnum-length))) (cmp-norm-tp `(integer ,x ,x))))

(defconstant +coersion-alist+
  (mapcar (lambda (x)
	    (cons (cadr (assoc x *c-types*)) (get-sym `(,x "-LOC"))))
	  +c-local-var-types-syms+))

(defconstant +number-inlines+
  (mapcar 'cdr
	  (remove-if-not (lambda (x)
			   (type>= #tnumber (car x)))
			 +inline-types-alist+)))


(defstruct opaque)


(defmacro nil-to-t (x) `(or ,x t))

(defun name-to-sd (x &aux (sd (when (symbolp x) (get x 's-data))))
  (unless sd
    (error "The structure ~a is undefined." x))
  sd)

(defvar *tmpsyms* nil)
(defun tmpsym nil
  (let ((x (or (pop *tmpsyms*) (gensym))))
    (setf (symbol-plist x) '(tmp t))
    x))
(defconstant +tmpsyms+ (let ((*gensym-counter* 0)) (mapl (lambda (x) (rplaca x (tmpsym))) (make-list 1000))))

(defconstant +opaque+ (gensym))

(defvar *car-limit* -1);1)
(defvar *cdr-limit* -1);5)
(defun cons-tp-limit (x i j)
  (declare (seqind i j))
  (cond ((> i *car-limit*) nil)
	((> j *cdr-limit*) nil)
	((atom x) t)
	((and (cons-tp-limit (car x) (1+ i) 0) (cons-tp-limit (cdr x) i (1+ j))))))

(defun cons-tp-limit-tp (x i j)
  (declare (seqind i j))
  (cond ((> i *car-limit*) nil)
	((> j *cdr-limit*) nil)
	((atom x))
	((not (eq (car x) 'cons)))
	((and (cons-tp-limit-tp (cadr x) (1+ i) 0) (cons-tp-limit-tp (caddr x) i (1+ j))))))

(defun object-type (thing) (object-tp thing))

(defconstant +real-contagion-list+ si::+range-types+)

(defmacro sfl (&rest l)
  `'(,@(append l)))
;  `'(,@(append l (mapcar 'symbol-function l))))

(defmacro bound (a)
  (let ((s (sgen "BOUND")))
    `(let ((,s ,a)) (if (consp ,s) (car ,s) ,s))))

(defmacro boundptr (a)
  (let ((s (tmpsym)))
    `(let ((,s ,a)) (if (consp (car ,s)) (car ,s) ,s))))


(defun maybe-coerce-val (place base)
  (unless (typep (car place) base)
    (setf (car place) (coerce (car place) base))))

(defun ensure-atomic-real-type (type)
  (when (and (consp type) 
	     (member (car type) +real-contagion-list+)
	     (not (cdddr type)))
    (when (cdr type)
      (maybe-coerce-val (boundptr (cdr type)) (car type)))
    (when (cddr type)
      (maybe-coerce-val (boundptr (cddr type)) (car type)))
    type))

(defun conv-bnd (z tp bnd def &aux (def (if (eq tp 'short-float) (coerce def tp) def)))
  (cond ((or (not bnd) (eq bnd '*)) def)
	((and z (realp bnd) (= 0 bnd)) (conv-bnd z tp '(0) def))
	((or (atom bnd) (not (realp (car bnd))) (not (= 0 (car bnd)))) bnd)
	((= def +inf) '-e)
	('+e)))

(defun qbound (tp s)
  (cond ((consp s) (car s)) 
	((eq s '+e) (contagion-irep 1 tp)) 
	((eq s '-e) (contagion-irep -1 tp)) 
	(s)))

(defun pbound (tp s)
  (cond ((consp s) (car s)) 
	((eq s '+e) (contagion-irep 0 tp)) 
	((eq s '-e) (- (contagion-irep 0 tp)))
	(s)))

(defun apply-fn (f bndf tp r)
  (let* ((r (mapcar (lambda (x) (funcall bndf tp x)) r))
	 (x (apply f r)))
    (if (numberp x) x nan)))

(defun mfc (f tp &rest r)
  (let* ((e (list-split '(+e -e) r))
	 (z (and (last r) (= 0 (pbound tp (car (last r)))) (and (symbolp f) (get f 'zero-pole))))
	 (q (apply-fn f 'qbound tp r))
	 (v (if z (cond ((< 0 q) +inf) ((> 0 q) -inf) (0)) (apply-fn f 'pbound tp r))))
    (cond 
     ((and e (= v 0)) (cond ((< q 0) '-e) ((> q 0) '+e) (0)))
     ((and r (not (member-if-not (lambda (x) (or (consp x) (member x '(+e -e)))) r))) (list v))
     (v))))


(defun m< (x y)
  (let ((x (bound x))
	(y (bound y)))
    (cond ((eql x y) nil)
	  ((eq x '-e) (or (eql y 0) (m< 0 y)))
	  ((eq x '+e) (m< 0 y))
	  ((eq y '+e) (or (eql x 0) (m< x 0)))
	  ((eq y '-e) (m< x 0))
	  ((< x y)))))

(defun mmin (x &optional (y +inf))
  (cond ((atom x) y)
	((isnan (bound (car x))) (mmin (cdr x) y))
	((m< (car x) y) (mmin (cdr x) (car x)))
	((mmin (cdr x) y))))

(defun mmax (x &optional (y -inf))
  (cond ((atom x) y)
	((isnan (bound (car x))) (mmax (cdr x) y))
	((m< (car x) y) (mmax (cdr x) y))
	((mmax (cdr x) (car x)))))


(defun unconv-bnd (x)
  (cond ((member x '(-e +e)) '(0))
	((or (= +inf (bound x)) (= -inf (bound x))) '*)
	(x)))

(defun promote-ratio (x)
  (case x ((immfix si::bfix bignum) 'integer) (ratio 'rational)(otherwise x)))
	
(defconstant +small-rat+ (rational least-positive-long-float))

(defun contagion-irep (x tp)
  (case tp
	(ratio (if (or (= 0 x) (= 1 x)) x (+ x (/ 1 x))))
	(integer x)
	(otherwise (coerce x tp))))

(defun mk-tp (&rest tp)
  (let ((v (car (member (car tp) +real-contagion-list+ :test 'typep))))
    (when v
      (list (promote-ratio v) (unconv-bnd (cadr tp)) (unconv-bnd (caddr tp))))))

(defun complex-atomic-type-fixup (tp &aux (a (car (atomic-tp tp))))
  (if (typep a 'complex)
    (let* ((r (realpart a))(i (imagpart a)))
      (cmp-norm-tp `(complex (,(typecase r
					 (integer 'integer)
					 (ratio 'ratio)
					 (short-float 'short-float)
					 (long-float 'long-float))
			      ,(min r i) ,(max r i)))))
    tp))

(defconstant +cmp-range-types+ (let ((z '(integer ratio short-float long-float)))
				 (nconc (mapcar (lambda (x) (cons x (cmp-norm-tp x))) z)
					(mapcar (lambda (x) (cons x (cmp-norm-tp `(complex ,x)))) z))))

;FIXME zero-pole, roll dsrs together, separate complex contagion

(defun dsr2 (f t1 t2 &aux (p1 (pop t1))(p2 (pop t2)))
  (let* ((z (and (symbolp f) (get f 'zero-pole)))
	 (b1 (real-bnds t1))(b2 (real-bnds t2)))
    (unless (and (car b1) (car b2))
      (return-from
       dsr2
       (cmp-norm-tp
	(car (member
	      (mfc f p2
		   (complex (contagion-irep 2 p1) (contagion-irep 3 p1))
		   (complex (contagion-irep 4 p2) (contagion-irep 5 p2)))
	      `((complex integer) (complex ratio) (complex short-float)
		(complex long-float) complex) :test 'typep)))))
    (let* ((n1 (conv-bnd nil p1 (car b1)  -inf))
	   (x1 (conv-bnd nil p1 (cadr b1) +inf))
	   (n2 (conv-bnd z p2 (car b2)  -inf))
	   (x2 (conv-bnd z p2 (cadr b2) +inf))
	   (a (mfc f p2 n1 n2))
	   (b (mfc f p2 n1 x2))
	   (c (mfc f p2 x1 n2))
	   (d (mfc f p2 x1 x2))
	   (e (mfc f p2 (contagion-irep 2 p1) (contagion-irep 3 p2)))
	   (v (list a b c d))
	   (v (if (and z 
		       (type-and t2 #t(real 0 0))
		       (not (type>= #tnon-negative-real t2))
		       (not (type>= #tnon-positive-real t2)))
		  (cons +inf (cons -inf v)) v)))
      (unless (member-if (lambda (x) (complexp (bound x))) v)
	(cmp-norm-tp (mk-tp e (mmin v) (mmax v)))))))


(defun dsr1 (f t1 &aux (p1 (pop t1)))
  (let* ((z (and (symbolp f) (get f 'zero-pole)))
	 (b1 (real-bnds t1)))
    (unless (car b1)
      (return-from
       dsr1
       (cmp-norm-tp
	(car (member
	      (mfc f p1 (complex (contagion-irep 2 p1) (contagion-irep 3 p1)))
	      `((complex integer) (complex ratio) (complex short-float)
		(complex long-float) complex) :test 'typep)))))
    (let* ((n1 (conv-bnd nil p1 (car b1)  -inf))
	   (x1 (conv-bnd nil p1 (cadr b1) +inf))
	   (a (mfc f p1 n1))
	   (b (mfc f p1 x1))
	   (e (mfc f p1 (contagion-irep 2 p1)))
	   (v (list a b))
	   (v (if (and z
		       (type-and t1 #t(real 0 0))
		       (not (type>= #tnon-negative-real t1))
		       (not (type>= #tnon-positive-real t1)))
		  (cons +inf (cons -inf v)) v)))
      (unless (member-if (lambda (x) (complexp (bound x))) v)
	(cmp-norm-tp (mk-tp e (mmin v) (mmax v)))))))

(defun dsr0 (f)
  (let* ((z (and (symbolp f) (get f 'zero-pole))))
    (let* ((a (mfc f nil))
	   (e a)
	   (v (list a)))
      (unless (member-if (lambda (x) (complexp (bound x))) v)
	(cmp-norm-tp (mk-tp e (mmin v) (mmax v)))))))

(defun range-decomp (tp)
  (mapcan (lambda (x &aux (f (pop x))(z (type-and tp x))) (when z (list (cons f z)))) +cmp-range-types+))

(defun super-range (f &optional (t1 nil t1p) (t2 nil t2p)
		      &aux (t1 (complex-atomic-type-fixup t1));FIXME
		      (t2 (complex-atomic-type-fixup t2))(r1 (range-decomp t1))(r2 (range-decomp t2)))

  (cond (t2p
	 (reduce (lambda (xx x)
		   (reduce (lambda (yy y)
			     (type-or1 yy (dsr2 f x y)))
			   r2 :initial-value xx))
		 r1 :initial-value nil))
	(t1p (reduce (lambda (xx x) (type-or1 xx (dsr1 f x))) r1 :initial-value nil))
	((dsr0 f))))



(dolist (l '(/ floor ceiling truncate round ffloor fceiling ftruncate fround))
  (si::putprop l t 'zero-pole))
(dolist (l '(si::number-plus si::number-minus si::number-times + - * exp tanh sinh asinh))
  (si::putprop l 'super-range 'type-propagator))

(defun atan-propagator (f t1 &optional (t2 nil t2p))
  (if t2p
      (type-or1
       (super-range f (type-and #tnon-negative-real t1) (type-and #tnon-negative-real t2))
       (type-or1
	(super-range f (type-and #tnon-negative-real t1) (type-and #tnegative-real t2))
	(type-or1
	 (super-range f (type-and #tnegative-real t1) (type-and #tnon-negative-real t2))
	 (super-range f (type-and #tnegative-real t1) (type-and #tnegative-real t2)))))
    (super-range f t1)))
(si::putprop 'atan 'atan-propagator 'type-propagator)

(defun float-propagator (f t1 &optional (t2 #tnull t2p))
  (if (equal t2 #tnull)
      (super-range f (type-and #treal t1))
    (super-range f (type-and #treal t1) (type-and #tfloat t2))))
(setf (get 'float 'type-propagator) 'float-propagator)

(defun bit-type (tp)
  (cond ((not tp) tp)
	((atomic-tp tp) tp)
	((type>= #tinteger tp)
	 (let* ((tp (list* 'integer (real-bnds tp)))
		(l (cadr tp))
		(l (if (consp l) (car l) l))
		(h (caddr tp))
		(h (if (consp h) (car h) h))
		(h (if (eq h '*) h (if (>= h 0) (1- (ash 1 (integer-length h))) -1)))
		(l (if (eq l '*) l (if (< l 0) (- (1- (ash 1 (integer-length l)))) 0))))
	   (cmp-norm-tp `(integer ,l ,h))))))


(defun logand2-propagator (f t1 t2)
  (when (and (type>= #tfixnum t2) (type>= #tfixnum t1));FIXME
    (let ((t1 (bit-type t1))(t2 (bit-type t2)))
      (type-or1
       (super-range f
		    (type-and #tnon-negative-integer t1)
		    (type-and #tnon-negative-integer t2))
       (type-or1
	(super-range f
		     (type-and #tnegative-integer t1)
		     (type-and #tnon-negative-integer t2))
	(type-or1
	 (super-range f
		      (type-and #tnon-negative-integer t1)
		      (type-and #tnegative-integer t2))
	 (super-range f
		      (type-and #tnegative-integer t1)
		      (type-and #tnegative-integer t2))))))))

(dolist (l '(& ^ \|))
  (si::putprop l 'logand2-propagator 'type-propagator))

(defun logand1-propagator (f t1)
  (when (type>= #tfixnum t1);FIXME
    (super-range f t1)))

(si::putprop '~ 'logand1-propagator 'type-propagator)

(defun logand-propagator (f &optional (t1 nil t1p) (t2 nil t2p) &rest r)
  (cond (r (apply 'logand-propagator f (logand-propagator f t1 t2) (car r) (cdr r)))
	(t2p (logand2-propagator f t1 t2))
	(t1p (logand1-propagator f t1))
	((not t1p) (super-range f))))

(dolist (l '(logand logior logxor logeqv logandc1 logandc2 logorc1 logorc2 lognand lognor lognot))
  (si::putprop l 'logand-propagator 'type-propagator))

(defun min-max-propagator (f &optional (t1 nil t1p) (t2 nil t2p))
  (cond (t2p (super-range f (type-and #treal t1) (type-and #treal t2)))
	(t1p (super-range f (type-and #treal t1)))))
(si::putprop 'max 'min-max-propagator 'type-propagator)
(si::putprop 'min 'min-max-propagator 'type-propagator)

(defun /-propagator (f t1 &optional t2)
  (cond (t2 (super-range f t1 (type-and t2 #t(not (real 0 0)))))
	((super-range f (type-and t1 #t(not (real 0 0)))))))
(si::putprop '/ '/-propagator 'type-propagator)

(defun log-wrap (x y)
  (if (= 0 x) (symbol-value '-inf) (log x y)))

(defun real-imag-tp (x)
  (if (eq (car x) 'complex) (cadr x)
    (real-imag-tp (cadr x))));FIXME

(defun complex-real-imag-type-propagator (f t1)
  (when (type>= #tcomplex t1)
    (reduce (lambda (&rest r) (when r (apply 'type-or1 r)))
	    (mapcar (lambda (x) (cmp-norm-tp (real-imag-tp (cmp-unnorm-tp (cdr x)))))
		    (range-decomp t1)))))
(si::putprop 'si::complex-real 'complex-real-imag-type-propagator 'type-propagator)
(si::putprop 'si::complex-imag 'complex-real-imag-type-propagator 'type-propagator)

(defun complex-propagator (f t1 &optional (t2 t1))
  (declare (ignore f))
  (let ((t1 (type-and #treal t1))
	(t2 (type-and #treal t2)))
    (to-complex-tp (super-range '+ t1 t2))))
(si::putprop 'complex 'complex-propagator 'type-propagator)

(defun c-type-propagator (f t1)
  (cmp-norm-tp
   (cons 'member
	 (reduce (lambda (y x)
		    (when (type-and t1 (car x))
		      (pushnew (c-type (eval (cdr x))) y))
		    y) si::+rn+ :initial-value nil))))
(si::putprop 'c-type 'c-type-propagator 'type-propagator)


(defconstant +e+ 2.7182818284590451)

(defun log-propagator (f t1 &optional (t2 #t(short-float #.(float +e+ 1.0s0) #.(float +e+ 1.0s0))))
  (declare (ignore f))
  (type-or1 (super-range 'log-wrap (type-and t1 #t(not real)) t2)
	    (type-or1 (super-range 'log-wrap (type-and t1 #tnon-negative-real) t2)
		      (super-range 'log-wrap (type-and t1 #tnegative-real) t2))))
(si::putprop 'log 'log-propagator 'type-propagator)

(defun last-cons-type (tp &optional l)
  (cond ((and l (atom tp)) tp)
	((and (consp tp) (eq (car tp) 'cons) 
	      (cddr tp) (not (cdddr tp))) (last-cons-type (caddr tp) t))))

(defun cdr-propagator (f t1 &aux (t1 (type-and #tlist t1)))
  (cond ((type>= #tnull t1) t1) ;FIXME clb ccb do-setq-tp
	((let ((a1 (atomic-tp t1)))
	   (when a1 (let ((tp (cdar a1))) (unless (or (spicep tp) (eq tp +opaque+)) (object-type tp))))))
	((and (consp t1) (eq (car t1) 'cons)) (caddr t1))
	((type>= #tproper-list t1) #tproper-list)))
(si::putprop 'cdr 'cdr-propagator 'type-propagator)

(defun make-list-propagator (f t1 &rest r &aux (a (atomic-tp t1)))
  (declare (ignore f r))
  (cond ((and (type>= #t(integer 0 5) t1) a)
;	 (object-type (make-list (cadr t1))))
	 (cmp-norm-tp (reduce (lambda (y x) (declare (ignore x)) `(cons t ,y)) (make-list (car a)) :initial-value 'null)))
	(#tproper-list)))
(si::putprop 'make-list 'make-list-propagator 'type-propagator)


(defun nth-cons-tp (n tp)
  (cond ((= n 0) tp)
	((and (consp tp) (eq 'cons (car tp)) (cddr tp) (not (cdddr tp)))
	 (nth-cons-tp (1- n) (caddr tp)))))

(defun nthcdr-propagator (f t1 t2)
  (declare (ignore f))
  (let ((t1 (type-and #tinteger t1))
	(t2 (type-and #tlist t2)))
    (cond ((type>= #tnull t2) t2) ;FIXME clb ccb do-setq-tp
	  ((type>= #t(integer 0 0) t1) t2)
	  ((and (consp t2) (eq (car t2) 'cons) (atomic-tp t1) (typep (cadr t1) 'seqind)) 
	   (nth-cons-tp (cadr t1) t2))
	  ((type>= #tproper-list t2) #tproper-list))))
(si::putprop 'nthcdr 'nthcdr-propagator 'type-propagator)

(defun bump-pcons (v p)
  (let ((tp (if p #tproper-cons #tcons)))
    (unless (type>= (var-type v) tp)
      (when (type>= #tproper-cons (var-type v))
	(do-setq-tp v nil tp)
	(mapc (lambda (x) (bump-pcons x p)) (var-aliases v))))))

(defun bump-pconsa (v ctp)
  (let ((tp (cons-propagator 'cons ctp (cdr-propagator 'cdr (var-type v)))))
    (unless (type>= (var-type v) tp)
      (do-setq-tp v nil tp)
      (mapc (lambda (x) (bump-pconsa x ctp)) (var-aliases v)))))

(defun c1rplacd (args)
  (let* ((info (make-info :flags (iflags side-effects)))
	 (nargs (c1args args info))
	 (p (type>= #tproper-list (info-type (cadadr nargs))))
	 (atp (car (atomic-tp (info-type (cadar nargs)))))
	 (atp1 (car (atomic-tp (info-type (cadadr nargs))))))
    (c1side-effects nil)
    (when (consp atp) 
      (when (eq atp atp1) (setq atp1 (copy-list atp1)))
      (setf (cdr atp) (or atp1 +opaque+)))
    (when (eq (caar nargs) 'var)
      (bump-pcons (caaddr (car nargs)) p))
    (setf (info-type info) (if p #tproper-cons #tcons))
    (list 'call-global info 'rplacd nargs)))

;; (defun c1rplacd (args)
;;   (let* ((info (make-info :flags (iflags side-effects)))
;; 	 (nargs (c1args args info))
;; 	 (p (type>= #tproper-list (info-type (cadadr nargs))))
;; 	 (atp (car (atomic-tp (info-type (cadar nargs)))))
;; 	 (atp1 (car (atomic-tp (info-type (cadadr nargs))))))
;;     (when (consp atp) 
;;       (when (eq atp atp1) (setq atp1 (copy-list atp1)))
;;       (setf (cdr atp) (or atp1 +opaque+)))
;;     (when (eq (caar nargs) 'var)
;;       (bump-pcons (caaddr (car nargs)) p))
;;     (setf (info-type info) (if p #tproper-cons #tcons))
;;     (list 'call-global info 'rplacd nargs)))

;; (defun c1rplacd (args)
;;   (let* ((info (make-info :flags (iflags side-effects)))
;; 	 (nargs (c1args args info))
;; 	 (p (type>= #tproper-list (info-type (cadadr nargs))))
;; 	 (atp (car (atomic-tp (info-type (cadar nargs)))))
;; 	 (atp1 (car (atomic-tp (info-type (cadadr nargs))))))
;;     (when (consp atp) 
;;       (when (eq atp atp1) (setq atp1 (copy-list atp1)))
;;       (setf (cdr atp) (or atp1 :opaque)))
;;     (when (eq (caar nargs) 'var)
;;       (bump-pcons (caaddr (car nargs)) p))
;;     (setf (info-type info) (if p #tproper-cons #tcons))
;;     (list 'call-global info 'rplacd nargs)))

;; (defun c1rplacd (args)
;;   (let* ((info (make-info :flags (iflags side-effects)))
;; 	 (nargs (c1args args info))
;; 	 (p (type>= #tproper-list (info-type (cadadr nargs))))
;; 	 (atp (car (atomic-tp (info-type (cadar nargs)))))
;; 	 (atp1 (car (atomic-tp (info-type (cadadr nargs))))))
;;     (when (consp atp) 
;;       (setf (cdr atp) (or atp1 :opaque)))
;;     (when (eq (caar nargs) 'var)
;;       (bump-pcons (caaddr (car nargs)) p))
;;     (setf (info-type info) (if p #tproper-cons #tcons))
;;     (list 'call-global info 'rplacd nargs)))
(si::putprop 'rplacd 'c1rplacd 'c1)

(defun c1rplaca (args)
  (let* ((info (make-info :flags (iflags side-effects)))
	 (nargs (c1args args info))
	 (atp (car (atomic-tp (info-type (cadar nargs)))))
;	 (atp1 (car (atomic-tp (narg-list-type (cdr nargs))))))
	 (atp1 (car (atomic-tp (info-type (cadadr nargs))))))
    (c1side-effects nil)
    (when (consp atp) 
      (when (eq atp atp1) (setq atp1 (copy-list atp1)))
      (setf (car atp) (if atp1 (car atp1) +opaque+)))
    (when (eq (caar nargs) 'var)
      (bump-pconsa (caaddr (car nargs)) (info-type (cadadr nargs))))
    (setf (info-type info) (cons-propagator 'cons (info-type (cadadr nargs))
					    (cdr-propagator 'cdr (info-type (cadar nargs)))))
    (list 'call-global info 'rplaca nargs)))

;; (defun c1rplaca (args)
;;   (let* ((info (make-info :flags (iflags side-effects)))
;; 	 (nargs (c1args args info))
;; 	 (atp (car (atomic-tp (info-type (cadar nargs)))))
;; 	 (atp1 (car (atomic-tp (info-type (cadadr nargs))))))
;;     (when (consp atp) 
;;       (when (eq atp atp1) (setq atp1 (copy-list atp1)))
;;       (setf (car atp) (or atp1 +opaque+)))
;;     (when (eq (caar nargs) 'var)
;;       (bump-pconsa (caaddr (car nargs)) (info-type (cadadr nargs))))
;;     (setf (info-type info) (cons-propagator 'cons (info-type (cadadr nargs))
;; 					    (cdr-propagator 'cdr (info-type (cadar nargs)))))
;;     (list 'call-global info 'rplaca nargs)))

;; (defun c1rplaca (args)
;;   (let* ((info (make-info :flags (iflags side-effects)))
;; 	 (nargs (c1args args info))
;; 	 (atp (car (atomic-tp (info-type (cadar nargs)))))
;; 	 (atp1 (car (atomic-tp (info-type (cadadr nargs))))))
;;     (when (consp atp) 
;;       (when (eq atp atp1) (setq atp1 (copy-list atp1)))
;;       (setf (car atp) (or atp1 :opaque)))
;;     (when (eq (caar nargs) 'var)
;;       (bump-pconsa (caaddr (car nargs)) (info-type (cadadr nargs))))
;;     (setf (info-type info) (cons-propagator 'cons (info-type (cadadr nargs))
;; 					    (cdr-propagator 'cdr (info-type (cadar nargs)))))
;;     (list 'call-global info 'rplaca nargs)))

;; (defun c1rplaca (args)
;;   (let* ((info (make-info :flags (iflags side-effects)))
;; 	 (nargs (c1args args info))
;; 	 (atp (car (atomic-tp (info-type (cadar nargs)))))
;; 	 (atp1 (car (atomic-tp (info-type (cadadr nargs))))))
;;     (when (consp atp) 
;;       (setf (car atp) (or atp1 :opaque)))
;;     (when (eq (caar nargs) 'var)
;;       (bump-pconsa (caaddr (car nargs)) (info-type (cadadr nargs))))
;;     (setf (info-type info) (cons-propagator 'cons (info-type (cadadr nargs))
;; 					    (cdr-propagator 'cdr (info-type (cadar nargs)))))
;;     (list 'call-global info 'rplaca nargs)))
(si::putprop 'rplaca 'c1rplaca 'c1)

(defun cons-propagator (f t1 t2 &aux tmp)
  (declare (ignore f))
  (cond ((let ((a1 (atomic-tp t1))
	       (a2 (atomic-tp t2)))
	   (and a1 a2 (object-type (cons (car a1) (car a2))))))
	((cons-tp-limit (setq tmp `(cons ,t1 ,t2)) 0 0) (cmp-norm-tp tmp))
	((type>= #tproper-list t2) #tproper-cons)
	(#tcons)))
(si::putprop 'cons 'cons-propagator 'type-propagator)

(defvar *in-co1carcdr* nil);FIXME

(defun co1carcdr (f x)
  (unless *in-co1carcdr*
    (let ((*in-co1carcdr* t))
      (let* ((tp (car (atomic-tp (info-type (cadr (with-restore-vars (c1arg (car x))))))))
	     (tp (when (consp tp) (funcall f tp)))
	     (tp (unless (eq tp +opaque+) (get-var tp))))
	(when tp (c1var tp))))))

(setf (get 'car 'co1) 'co1carcdr)
(setf (get 'cdr 'co1) 'co1carcdr)

(defun car-propagator (f t1 &aux (t1 (type-and #tlist t1)))
  (declare (ignore f))
  (cond ((type>= #tnull t1) t1) ;FIXME clb ccb do-setq-tp
	((let ((a1 (atomic-tp t1)))
	   (when a1 (let ((tp (caar a1))) (unless (or (spicep tp) (eq tp +opaque+)) (object-type tp))))))
	((and (consp t1) (eq (car t1) 'cons)) (cadr t1))))
(si::putprop 'car 'car-propagator 'type-propagator)

(defun contagion (t1 t2)
  (car (member (type-or1 t1 t2) `(,#tlong-float ,#tshort-float #tratio #tinteger)
	       :test 'type-and)))

(defun mod-propagator (f t1 t2 &aux (r1 (range-decomp t1))(r2 (range-decomp t2)))
  (cond
   ((cdr r1) (reduce 'type-or1 (mapcar (lambda (x) (mod-propagator f (cdr x) t2)) r1)))
   ((cdr r2) (reduce 'type-or1 (mapcar (lambda (x) (mod-propagator f t1 (cdr x))) r2)))
   ((let ((a (atomic-tp t1))(b (atomic-tp t2)))
      (when (and a b)
	(object-tp (mod (car a) (car b))))))
   ((and (type>= #treal t1) (type>= #treal t2))
	 (let* ((tp (super-range '* #t(integer 0 1) t2))
		(r (real-bnds tp))
		(r (when (numberp (cadr r)) (list (car r) (list (cadr r))))))
	   (type-and (contagion t1 t2) (cmp-norm-tp (cons 'real r)))))))
(si::putprop 'mod 'mod-propagator 'type-propagator)

(defun random-propagator (f t1 &optional t2)
  (declare (ignore t2))
  (mod-propagator f (super-range '* #t(integer 0 1) t1) t1))
(si::putprop 'random 'random-propagator 'type-propagator)

(defun gcd-propagator (f &optional (t1 nil t1p) (t2 nil t2p) &aux (t1 (type-and #tinteger t1))(t2 (type-and #tinteger t2)))
  (cond (t2p (super-range '* #t(integer 0 1) (super-range 'min t1 t2)))
	(t1p (mod-propagator f t1 t1))
	((super-range f))))
(si::putprop 'gcd 'gcd-propagator 'type-propagator)
(defun lcm-propagator (f &optional (t1 nil t1p) (t2 nil t2p) &aux (t1 (type-and #tinteger t1))(t2 (type-and #tinteger t2)))
  (cond (t2p (super-range '* #t(integer 1) (super-range 'max t1 t2)))
	(t1p (mod-propagator f t1 t1))
	((super-range f))))
(si::putprop 'lcm 'lcm-propagator 'type-propagator)

(defun rem-propagator (f t1 t2)
  (let ((t2 (mod-propagator f t1 t2)))
    (when t2
      (cond ((type>= #tnon-negative-real t1)
	     (type-or1 (type-and #tnon-negative-real t2)
		       (super-range '- (type-and #tnon-positive-real t2))))
	    ((type>= #tnon-positive-real t1)
	     (type-or1 (type-and #tnon-positive-real t2)
		       (super-range '- (type-and #tnon-negative-real t2))))
	    ((type-or1 t2 (super-range '- t2)))))))
(si::putprop 'rem 'rem-propagator 'type-propagator)

(defun rcnum-types (t1)
  (reduce (lambda (y x)
	    (type-or1 (when (type-and t1 x) x) y))
	  `(,#tinteger ,#tratio ,#tshort-float ,#tlong-float)
	  :initial-value nil))

(defun floor-propagator (f t1 &optional (t2 (type-and (rcnum-types t1) #t(real 1 1))))
  (let ((t1 (type-and t1 #treal))(t2 (type-and t2 #treal)));FIXME
    (let ((sr (type-or1 (super-range f t1 (type-and t2 #tpositive-real));FIXME real-bnds in s-r
			(super-range f t1 (type-and t2 #tnegative-real)))))
      (when sr
	`(values ,sr
		 ,(cond ((member f (sfl floor ffloor))       (mod-propagator f t1 t2))
			((member f (sfl ceiling fceiling))   (super-range '- (mod-propagator f t1 t2)))
			((member f (sfl truncate ftruncate round fround)) (rem-propagator f t1 t2))))))))
					;multiplying by 0.5 for round would convert integer ranges to float
(dolist (l '(floor ceiling truncate round ffloor fceiling ftruncate fround))
  (si::putprop l 'floor-propagator 'type-propagator)
  (si::putprop l t 'c1no-side-effects))

(defun ash-propagator (f t1 t2)
  (and
   (type>= #tfixnum t1)
   (type>= #t(integer #.most-negative-fixnum #.(integer-length most-positive-fixnum)) t2)
   (super-range f t1 t2)))
(si::putprop 'ash 'ash-propagator 'type-propagator)
(si::putprop 'si::mpz_mul_2exp 'ash-propagator 'type-propagator)
(si::putprop 'si::mpz_fdiv_q_2exp 'ash-propagator 'type-propagator)

(defun <<-propagator (f t1 t2)
  (when (type>= #tfixnum t1)
    (super-range
     (lambda (x y)
       (if (when (typep y 'fixnum)
	     (> (- #.(1+ (integer-length most-positive-fixnum)) (integer-length x)) y))
	   (funcall f x y) (return-from <<-propagator nil)))
     t1 t2)))
(si::putprop 'si::<< '<<-propagator 'type-propagator)

(defun >>-propagator (f t1 t2)
  (when (and (type>= #tfixnum t1) (type>= #t(integer 0 #.(integer-length most-positive-fixnum)) t2))
   (super-range f t1 t2)))
(si::putprop 'si::>> '>>-propagator 'type-propagator)

(defun pexpt (x y)
  (cond ((or (typep x 'float)
	     (= (abs x) 1) 
	     (= (abs x) 0) 
	     (typep y '(or float ratio (integer 0 #.(integer-length most-positive-fixnum)))))
	 (expt x y))
	((> y 0)
	 (cond ((> x 1) +inf)
	       ((< x -1) -inf)
	       (0.0)))
	((< y 0)
	 (cond ((> (abs x) 1) 0.0)
	       ((> x 0) +inf)
	       (-inf)))))
		
	     
(defun expt-propagator (f t1 t2)
  (declare (ignore f))
  (cond ((or (type-and #tcomplex t1) (type-and #tcomplex t2)) nil)
	((type-and #t(or ratio negative-real) t1) nil)
	((and (type-and #tinteger t1) (type-and #tnegative-real t2)) nil)
	((type-or1 (super-range 'pexpt (type-and #tpositive-real t1) t2)
		   (super-range 'pexpt (type-and #t(real 0 0) t1) t2)))))
(si::putprop 'expt 'expt-propagator 'type-propagator)

;; (defun exp-propagator (f t1)
;;   (declare (ignore f))
;;   (expt-propagator 'expt (if (type>= #tshort-float t1) (object-type (float +e+ 0.0s0)) (object-type +e+)) t1))
;; (si::putprop 'exp 'exp-propagator 'type-propagator)

(defun integer-length-propagator (f t1)
  (when (type>= #tfixnum t1)
    (type-or1 (super-range f (type-and #tnon-negative-real t1))
	      (super-range f (type-and #tnon-positive-real t1)))))
(si::putprop 'integer-length 'integer-length-propagator 'type-propagator)
;(defconstant +clzl0+ (let ((x (1+ (si::clzl 1)))) (cmp-norm-tp `(integer ,x ,x))))
;(defconstant +clzl0+ (let ((x (1- si::fixnum-length))) (cmp-norm-tp `(integer ,x ,x))))
(defun clzl-propagator (f t1)
  (when (type>= #tfixnum t1)
    (type-or1 (when (type-and #t(real 0 0) t1) +clzl0+)
	      (type-or1 (super-range f (type-and #tpositive-real t1))
			(super-range f (type-and #tnegative-real t1))))))
(si::putprop 'si::clzl 'clzl-propagator 'type-propagator)
(si::putprop 'si::clzl t 'cmp-inline);FIXME no declaim

(defun ctzl-propagator (f t1 &aux r)
  (when (type>= #tfixnum t1)
    (dotimes (i si::fixnum-length r)
      (let ((j (ash 1 i)))
	(setq r (type-or1 r (when (type-and t1 (object-type j)) (object-type (funcall f j)))))))))
(si::putprop 'si::ctzl 'ctzl-propagator 'compiler::type-propagator)
(si::putprop 'si::ctzl t 'compiler::cmp-inline);FIXME no declaim

(defun abs-propagator (f t1)
  (when t1
    (type-and #tnon-negative-real
	      (type-or1
	       (abs-propagator 
		f
		(super-range 
		 'float 
		 (complex-real-imag-type-propagator 'complex-real (type-and #tcomplex t1))))
	       (let ((t1 (type-and #treal t1)))
		 (type-or1 t1 (super-range '- t1)))))))
(si::putprop 'abs 'abs-propagator 'type-propagator)

(defun cosh-propagator (f t1)
  (type-or1 (super-range f (type-and t1 #t(not real)))
	    (type-or1 (super-range f (type-and t1 #tnon-negative-real))
		      (super-range f (type-and t1 #tnegative-real)))))
(si::putprop 'cosh 'cosh-propagator 'type-propagator)

(defun shift-range-fmod (t1 m &optional (mod t)
			    &aux (zz (rassoc t1 +cmp-range-types+ :test 'type<=))
			    (r (when zz (real-bnds t1))))
  (cond ((not t1) nil)
	((not zz) (reduce (lambda (y x) (type-or1 y (shift-range-fmod (type-and t1 (cdr x)) m mod)))
			 +cmp-range-types+ :initial-value nil))
	((type>= #trational t1) (shift-range-fmod (super-range 'float t1) m mod))
	((not r) nil)
	((and (eq (car r) '*) (eq (cadr r) '*)) (if mod (cmp-norm-tp (list (car zz) (- m) m)) t1))
	((eq (car r) '*)
	 (super-range '* #t(integer -1 -1) (shift-range-fmod (super-range '- t1) m mod)))
	((let* ((z (car r))
		(z (if (atom z) z (car z)))
		(z (if mod (* m (fceiling z m)) m))
		(z (cmp-norm-tp (list (car zz) z z)))
		(z (super-range '- t1 z)))
	   (if mod 
	       (type-and (cmp-norm-tp (list (car zz) (- m) m)) z)
	     z)))))

(defconstant +pi+ (atan 0 -1))
(defconstant +pid2+ (* 0.5 (atan 0 -1)))

(defun to-complex-tp (tp);FIXME
  (reduce 'type-or1 (mapcar (lambda (x) (let ((z (type-and tp x))) (when z (cmp-norm-tp `(complex ,(cmp-unnorm-tp z)))))) `(#tinteger #tratio #tshort-float #tlong-float))
	  :initial-value nil))

(defun float-proxy-propagator (f t1) 
  (declare (ignore f)) (super-range 'float t1))
(si::putprop 'si::big-to-double 'float-proxy-propagator 'type-propagator)
(si::putprop 'si::ratio-to-double 'float-proxy-propagator 'type-propagator)

(defun sqrt-propagator (f t1)
  (type-or1 (super-range f (type-and t1 #tcomplex))
	    (type-or1 (super-range f (type-and t1 #tnon-negative-real))
		      (super-range 'sqrt (to-complex-tp (type-and t1 #tnegative-real))))));FIXME
(si::putprop 'sqrt 'sqrt-propagator 'type-propagator)

(defun cos-propagator (f t1)
  (type-or1 (super-range f (type-and t1 #tcomplex))
	    (let ((z (shift-range-fmod (type-and t1 #treal) +pi+)))
	      (type-or1 (super-range f (type-and z #tnon-negative-real))
			(super-range f (type-and z #tnegative-real))))))
(si::putprop 'cos 'cos-propagator 'type-propagator)

(defun sin-propagator (f t1)
  (type-or1 (super-range f (type-and t1 #tcomplex))
	    (cos-propagator (lambda (x) (funcall f (if (floatp x) (+ x (float +pid2+ x)) (+ (float x +pid2+) +pid2+))))
			    (shift-range-fmod (type-and t1 #treal) +pid2+ nil))))
(si::putprop 'sin 'sin-propagator 'type-propagator)

(defun tan-propagator (f t1)
  (type-or1 (super-range f (type-and t1 #tcomplex))
	    (let ((z (shift-range-fmod (shift-range-fmod (type-and t1 #treal) +pid2+ nil) +pi+)))
	      (type-or1 (super-range (lambda (x) (funcall f (if (floatp x) (+ x (float +pid2+ x)) (+ (float x +pid2+) +pid2+))))
				     (type-and z #tnon-negative-real))
			(super-range (lambda (x) (funcall f (if (floatp x) (+ x (float +pid2+ x)) (+ (float x +pid2+) +pid2+))))
				     (type-and z #tnegative-real))))))
(si::putprop 'tan 'tan-propagator 'type-propagator)

(defun asin-propagator (f t1)
  (type-or1 (super-range f (type-and t1 #tcomplex))
	    (type-or1 (super-range (lambda (x) (funcall f (/ x 6))) 
				   (super-range '* #t(integer 6 6) (type-and t1 #t(real -1 1))))
		      (super-range f (to-complex-tp (type-and t1 #t(not (real -1 1))))))))
(si::putprop 'asin 'asin-propagator 'type-propagator)
(si::putprop 'acos 'asin-propagator 'type-propagator)
(si::putprop 'atanh 'asin-propagator 'type-propagator)

(defun acosh-propagator (f t1)
  (type-or1 (super-range f (type-and t1 #tcomplex))
	    (type-or1 (super-range f (type-and t1 #t(real 1)))
		      (super-range f (to-complex-tp (type-and t1 #t(real * (1))))))))
(si::putprop 'acosh 'acosh-propagator 'type-propagator)

(defun make-vector-propagator (f et st &rest r)
  (declare (ignore f))
  (cmp-norm-tp `(,(if (and (type>= #tnull (pop r))
			   (type>= #tnull (pop r))
			   (type>= #tnull (car r)))
		      'simple-array 'array)
		 ,(or (car (atomic-tp et)) '*)
		 (,(or (car (atomic-tp st)) '*)))))
(si::putprop 'si::make-vector 'make-vector-propagator 'type-propagator)
(defun make-array1-propagator (f &rest r)
  (declare (ignore f))
  (cmp-norm-tp `(array ,(or (car (atomic-tp (car r))) '*) ,(or (let* ((x (car (atomic-tp (sixth r))));FIXME centralize
								      (x (if (integerp x) (make-list x :initial-element '*) x)))
								 (mapcar (lambda (x) (if (integerp x) x '*)) x)) '*))))
(si::putprop 'si::make-array1 'make-array1-propagator 'type-propagator)

(defun promoted-c-type (type)
  (let ((type (coerce-to-one-value type)))
    (cond ((eq type 'object) type);FIXME
	  ((setq r (member type +promoted-c-types+ :test 'type<=))
	   (car r))
	  (#tt))))

(defun single-type-p (type)
  (if (listp type)
      (case (car type)
	    (returns-exactly (when (cdr type) (unless (cddr type) (cadr type))))
	    (values nil)
	    (otherwise type))
    (unless (eq type '*) type)))

(defun coerce-to-one-value (type) (type-and type t))

(defun unprintable-individualsp (u)
  (case (when (listp u) (car u))
	((or returns-exactly values) (member-if 'unprintable-individualsp (cdr u)))
	(member (member-if (lambda (x)
			     (or (si::si-classp x) (typep x '(or function cons spice array))))
			   (cdr u)))
	(otherwise (si::si-classp u))))

(defun export-type (type)
  (if (unprintable-individualsp (cmp-unnorm-tp type))
      (bump-tp type)
    type))

(defvar *unique-sigs* (make-hash-table :test 'equal))
(defun unique-sigs (sig)
  (or (gethash sig *unique-sigs*) (setf (gethash sig *unique-sigs*) sig)))
		  
(defun bump-tp (tp)
  (cond ((eq tp '*) tp)
	((and (consp tp) (member (car tp) '(values returns-exactly)))
	 `(,(car tp) ,@(mapcar 'bump-tp (cdr tp))))
	((type>= tp #tnull) (type-or1 #tnull (bump-tp (type-and #t(not null) tp))))
	((cdr (rassoc tp +useful-types-alist+ :test 'type<=)))))

(defun and-form-type (type form original-form &aux type1)
  (setq type1 (type-and type (info-type (cadr form))))
  (when (and (null type1) type (info-type (cadr form)))
        (cmpwarn "The type of the form ~s is not ~s, but ~s." original-form type (info-type (cadr form))))
  (if (or *compiler-check-args* (eq type1 (info-type (cadr form))))
      form
      (let ((info (copy-info (cadr form))))
           (setf (info-type info) type1)
           (list* (car form) info (cddr form)))))

(defun check-form-type (type form original-form)
  (when (and (null (type-and type (info-type (cadr form)))) type (info-type (cadr form)))
        (cmpwarn "The type of the form ~s is not ~s, but ~s." original-form type (info-type (cadr form)))))


(defun c-structure-def-propagator (f t1)
  (declare (ignore f))
  (when (symbolp t1)
    (let ((tem (get t1 's-data)))
      (when tem (object-type tem)))))
(setf (get 'c-structure-def 'type-propagator) 'c-structure-def-propagator)

(defun structure-name-propagator (f t1)
  (declare (ignore f))
  (when (symbolp t1)
    (when (get t1 's-data)
      (object-type t1))))
(setf (get 'si::structure-name 'type-propagator) 'structure-name-propagator)


(defun expand-type-propagator (f t1 &aux (a (atomic-tp t1))(b (car a)));FIXME organization
  (when a
    (when (constant-type-p b)
      (object-type (funcall f b)))))
(dolist (l 'si::(expand-array-element-type
		 expand-deftype sdata-includes
		 lookup-simple-typep-fn lookup-typep-fn))
  (setf (get l 'compiler::c1no-side-effects) t)
  (setf (get l 'compiler::type-propagator) 'compiler::expand-type-propagator))

(defun improper-consp-type-propagator (f t1 &optional t2)
  (declare (ignore t2))
  (cond ((not (type-and #tsi::improper-cons t1)) #tnull)
	((type>= #tsi::improper-cons t1) #ttrue)))
(dolist (l 'si::(improper-consp))
  (setf (get l 'compiler::c1no-side-effects) t)
  (setf (get l 'compiler::type-propagator) 'compiler::improper-consp-type-propagator))

(defun symbol-gfdef-propagator (f t1 &aux (a (atomic-tp t1)))
  (declare (ignore f))
  (if a (object-type (funid-to-fn (car a))) #tfunction));FIXME 0
(setf (get 'c-symbol-gfdef 'type-propagator) 'symbol-gfdef-propagator)
