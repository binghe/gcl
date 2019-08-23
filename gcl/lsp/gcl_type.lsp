(in-package :si)

(export '(cmp-norm-tp
	  cmp-unnorm-tp
	  type-and type-or1 type>= type<= tp-not tp-and tp-or
	  atomic-tp tp-bnds object-tp
	  cmpt t-to-nil returs-exactly funcallable-symbol-function
	  infer-tp cnum creal long
	  sharp-t-reader +useful-types-alist+ +useful-type-list+))

(defun sharp-t-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((tp (cmp-norm-tp (read stream))))
    (if (constantp tp) tp `',tp)))
(set-dispatch-macro-character #\# #\t 'sharp-t-reader)


(defmacro cmpt (tp)  `(and (consp ,tp) (member (car ,tp) '(returns-exactly values))))

(defun t-to-nil (x) (unless (eq x t) x))
(setf (get 't-to-nil 'cmp-inline) t)

(defconstant +vtps+ (mapcar (lambda (x) (list x (intern (string-concatenate "VECTOR-"  (string x))))) +array-types+))
(defconstant +atps+ (mapcar (lambda (x) (list x (intern (string-concatenate "ARRAY-"   (string x))))) +array-types+))
(defconstant +vtpsn+ `((nil vector-nil) ,@+vtps+))
(defconstant +atpsn+ `((nil array-nil) ,@+atps+))


(defun real-rep (x)
  (case x (integer 1) (ratio 1/2) (short-float 1.0s0) (long-float 1.0)))

(defun complex-rep (x)
  (let* ((s (symbolp x))
	 (r (real-rep (if s x (car x))))
	 (i (real-rep (if s x (cadr x)))))
    (complex r i)))

(defconstant +r+ `((immfix 1) 
		   (bfix  most-positive-fixnum)
		   (bignum (1+ most-positive-fixnum))
		   (ratio 1/2)
		   (short-float 1.0s0) 
		   (long-float 1.0)
		   ,@(mapcar (lambda (x &aux (v (complex-rep (car x))))
			       `(,(cadr x) ,v)) +ctps+)
		   (standard-char #\a)
		   (non-standard-base-char #\Return)
		   (structure (make-dummy-structure)) 
		   (std-instance (set-d-tt 1 (make-dummy-structure))) 
		   (non-logical-pathname (init-pathname nil nil nil nil nil nil ""))
		   (logical-pathname (set-d-tt 1 (init-pathname nil nil nil nil nil nil "")))
		   (hash-table-eq (make-hash-table :test 'eq))
		   (hash-table-eql (make-hash-table :test 'eql))
		   (hash-table-equal (make-hash-table :test 'equal))
		   (hash-table-equalp (make-hash-table :test 'equalp))
		   (package *package*)
		   (file-input-stream (open-int "/dev/null" :input 'character nil nil nil nil :default))
		   (file-output-stream (open-int "/dev/null" :output 'character nil nil nil nil :default))
		   (file-io-stream (open-int "/dev/null" :io 'character nil nil nil nil :default))
		   (file-probe-stream (open-int "/dev/null" :probe 'character nil nil nil nil :default))
		   (file-synonym-stream (make-synonym-stream '*standard-output*))
		   (non-file-synonym-stream *standard-input*)
		   (broadcast-stream (make-broadcast-stream))
		   (concatenated-stream (make-concatenated-stream))
		   (two-way-stream *terminal-io*)
		   (echo-stream (make-echo-stream *standard-output* *standard-output*))
		   (string-input-stream (make-string-input-stream ""))
		   (string-output-stream (make-string-output-stream));FIXME user defined, socket
		   (random-state (make-random-state)) 
		   (readtable (standard-readtable)) 
		   (non-standard-generic-compiled-function (function eq))
		   (non-standard-generic-interpreted-function (set-d-tt 2 (lambda nil nil)))
		   (standard-generic-compiled-function (set-d-tt 1 (lambda nil nil)))
		   (standard-generic-interpreted-function (set-d-tt 3 (lambda nil nil)))
		   ,@(mapcar (lambda (x)
			       `((simple-array ,(car x) 1)
				 (make-vector ',(car x) 1 nil nil nil 0 nil nil))) +vtps+)
		   ,@(mapcar (lambda (x)
			       `((matrix ,(car x))
				 (make-array1 ',(car x) nil nil nil 0 '(1 1) t))) +atps+)
		   ((non-simple-array character)
		    (make-vector 'character 1 t nil nil 0 nil nil))
		   ((non-simple-array bit)
		    (make-vector 'bit 1 t nil nil 0 nil nil))
		   ((non-simple-array t)
		    (make-vector 't 1 t nil nil 0 nil nil))
                   (spice (alloc-spice))
		   (cons '(1))
		   (keyword :a)
		   (null nil)
		   (true t)
		   (gsym 'a)))

(let ((f (car (resolve-type `(or (array nil) ,@(mapcar 'car +r+))))))
  (unless (eq t f)
    (print (list "Representative types ill-defined" f))))

(progn
  . #.(let (y)
	(flet ((orthogonalize (x &aux (z y))
			      (setq y (car (resolve-type (list 'or x y))))
			      (car (resolve-type `(and ,x (not ,z))))))

	      (let* ((q1 (lremove
			  nil
			  (mapcar
			   #'orthogonalize
			   `((unsigned-byte 0)
			     ,@(butlast
				(mapcan (lambda (n &aux (m (1- n)))
					  (list `(unsigned-byte ,m) `(signed-byte ,n) `(unsigned-byte ,n)))
					'(2 4 8 16 28 32 62 64)))
			     (and bignum (integer * -1))
			     (and bignum (integer 0))
			     ,@(mapcan (lambda (x)
					 (mapcar (lambda (y) (cons x y))
						 '((* (-1))(-1 -1) ((-1) (0)) (0 0) ((0) (1)) (1 1) ((1) *))))
				       '(ratio short-float long-float))
			     proper-cons improper-cons (vector nil) (array nil);FIXME
			     ,@(lremove 'gsym (mapcar 'car +r+))))))
		     (q2 (mapcar
			  #'orthogonalize
			  (multiple-value-bind
			   (x y) (ceiling (1+ (length q1)) fixnum-length)
			   (let ((r '(gsym)))
			     (dotimes (i (- y) r) (push `(member ,(gensym)) r)))))))

		(unless (eq y t)
		  (print (list "Types ill-defined" y)))

		`((defconstant +btp-types1+ ',q1)
		  (defconstant +btp-types+  ',(append q1 q2)))))));; pad to fixnum-length with gensym


(defconstant +btp-length+ (length +btp-types+))

(defun make-btp (&optional (i 0)) (make-vector 'bit +btp-length+ nil nil nil 0 nil i))

(deftype btp nil '(simple-array bit (#.+btp-length+)))

(defun btp-and (x y z)
  (declare (btp x y z));check-type?
  (bit-and x y z))
(defun btp-ior (x y z)
  (declare (btp x y z))
  (bit-ior x y z))
(defun btp-xor (x y z)
  (declare (btp x y z))
  (bit-xor x y z))
(defun btp-andc2 (x y z)
  (declare (btp x y z))
  (bit-andc2 x y z))
(defun btp-orc2 (x y z)
  (declare (btp x y z))
  (bit-orc2 x y z))
(defun btp-not (x y)
  (declare (btp x y))
  (bit-not x y))

(defvar *btps* (let ((i -1))
		 (mapcar (lambda (x &aux (z (make-btp)))
			   (setf (sbit z (incf i)) 1)
			   (list x (nprocess-type (normalize-type x)) z))
			 +btp-types+)))

(defvar *btpa* (let ((i -1)(z (make-vector t +btp-length+ nil nil nil 0 nil nil)))
		 (mapc (lambda (x) (setf (aref z (incf i)) x)) *btps*)
		 z))

(defvar *k-bv* (let ((i -1))
		   (lreduce (lambda (xx x &aux (z (assoc (caaar (cadr x)) xx)))
			      (unless z
				(push (setq z (cons (caaar (cadr x)) (make-btp))) xx))
			      (setf (sbit (cdr z) (incf i)) 1)
			      xx) *btps* :initial-value nil)))


(defvar *nil-tp* (make-btp))
(defvar *t-tp* (make-btp 1))

(defconstant +bit-words+ (ceiling +btp-length+ fixnum-length))


(defun copy-btp (tp &aux (n (make-btp))(ns (c-array-self n))(ts (c-array-self tp)))
  (dotimes (i +bit-words+ n)
    (*fixnum ns i t (*fixnum ts i nil nil))))

(defun copy-tp (x m tp d)
  (cond (tp (list* (copy-btp x) (copy-btp m) tp (let ((a (atomic-ntp tp))) (when a (list a)))))
	((unless (eql d 1)  (equal x *nil-tp*)) nil)
	((unless (eql d -1) (equal m *t-tp*))     t)
	((copy-btp x))))

(defun new-tp4 (k x m d z &aux (nz (unless (eql d -1) (ntp-not z))))
  (dotimes (i +btp-length+ (unless (equal x m) z))
    (unless (zerop (sbit k i))
      (let ((a (aref *btpa* i)))
	(cond ((unless (eql d  1) (eq +tp-nil+ (ntp-and (cadr a)  z)))
	       (setf (sbit x i) 0))
	      ((unless (eql d -1) (eq +tp-nil+ (ntp-and (cadr a) nz)))
	       (setf (sbit m i) 1)))))))

(let ((p1 (make-btp))(p2 (make-btp)))
  (defun tp-mask (m1 x1 &optional (m2 nil m2p)(x2 nil x2p))
    (btp-xor m1 x1 p1)
    (if x2p
	(btp-and p1 (btp-xor m2 x2 p2) p1)
      p1)))


(defun atomic-type (tp)
  (when (consp tp)
    (case (car tp)
	  (#.+range-types+
	   (let* ((d (cdr tp))(dd (cadr d))(da (car d)))
	     (and (numberp da) (numberp dd) (eql da dd) d)))
	  ((member eql) (let ((d (cdr tp))) (unless (cdr d) d))))))

(defun singleton-listp (x) (unless (cdr x) (unless (eq t (car x)) x)))

(defun singleton-rangep (x) (when (singleton-listp x) (when (eql (caar x) (cdar x)) (car x))))

(defun singleton-kingdomp (x);sync with member-ld
  (case (car x)
	((proper-cons improper-cons)
	 (let ((x (cddar (singleton-listp (cdr x)))))
	   (when (car x) x)))
	(#.+range-types+ (singleton-rangep (cdr x)))
	(null '(nil));impossible if in +btp-types+
	(true (cdr x));impossible if in +btp-types+
	((structure std-instance)
	 (when (singleton-listp (cdr x)) (unless (s-class-p (cadr x)) (cdr x))))
	(#.(mapcar 'cdr *all-array-types*) (when (singleton-listp (cdr x)) (when (arrayp (cadr x)) (cdr x))))
	(otherwise (singleton-listp (cdr x)))));FIXME others, array....

(defun atomic-ntp-array-dimensions (ntp)
  (unless (or (cadr ntp) (caddr ntp))
    (when (singleton-listp (car ntp))
      (let ((x (caar ntp)))
	(case (car x)
	      (#.(mapcar 'cdr *all-array-types*)
		 (when (singleton-listp (cdr x))
		   (when (consp (cadr x))
		     (unless (improper-consp (cadr x))
		       (unless (member-if 'symbolp (cadr x))
			 (cdr x)))))))))))

(defun atomic-tp-array-dimensions (tp)
  (when (consp tp)
    (atomic-ntp-array-dimensions (caddr tp))))

(defun atomic-ntp (ntp)
  (unless (cadr ntp)
    (when (singleton-listp (car ntp))
      (singleton-kingdomp (caar ntp)))))

(defun one-bit-btp (x &aux n)
  (dotimes (i +bit-words+ n)
    (let* ((y (*fixnum (c-array-self x) i nil nil))
	   . #.(let* ((m (mod +btp-length+ fixnum-length))(z (~ (<< -1 m))))
		 (unless (zerop m)
		   `((y (if (< i ,(1- +bit-words+)) y (& y ,z)))))))
      (unless (zerop y)
	(let* ((l (1- (integer-length y)))(l (if (minusp y) (1+ l) l)))
	  (if (unless n (eql y (<< 1 l)))
	      (setq n (+ (* i fixnum-length) l))
	    (return nil)))))))

(defun atomic-tp (tp)
  (unless (or (eq tp '*) (when (listp tp) (member (car tp) '(returns-exactly values))));FIXME
    (unless (eq tp t)
      (if (listp tp)
	  (fourth tp)
	(let ((i (one-bit-btp (xtp tp))))
	  (when i
	    (cadr (assoc i *atomic-btp-alist*))))))))

(defun object-index (x)
  (etypecase
   x
   (gsym #.(1- (length +btp-types+)))
   . #.(let ((i -1)) (mapcar (lambda (x) `(,x ,(incf i))) +btp-types1+))))


(defvar *cmp-verbose* nil)

(defvar *atomic-btp-alist* (let ((i -1))
			     (mapcan (lambda (x &aux (z (incf i)))
				       (when (atomic-type x)
					 (list (list z (cons (cadr x) (caddr x))))))
				     +btp-types+)))

(defun object-tp1 (x)
  (when *cmp-verbose* (print (list 'object-type x)))
  (if (isnan x)
      (cmp-norm-tp (car (member x '(long-float short-float) :test 'typep)));FIXME
    (let* ((i (object-index x))(z (caddr (svref *btpa* i))))
      (if (assoc i *atomic-btp-alist*) z
	(copy-tp z *nil-tp* (nprocess-type (normalize-type `(member ,x))) 0)))))

(defvar *atomic-type-hash* (make-hash-table :test 'eql))

(let ((package-list (mapcar 'find-package '(:si :cl :keyword))))
  (defun hashable-atomp (thing)
    (cond ((fixnump thing))
	  ((symbolp thing)
	   (member (symbol-package thing) package-list)))))

(defun object-tp (x &aux (h (hashable-atomp x)))
  (multiple-value-bind
   (f r) (when h (gethash x *atomic-type-hash*))
   (if r f
     (let ((z (object-tp1 x)))
       (when h (setf (gethash x *atomic-type-hash*) z))
       z))))


(let ((m (make-btp))(x (make-btp)))
  (defun comp-tp0 (type &aux (z (nprocess-type (normalize-type type))))

    (when *cmp-verbose* (print (list 'computing type)))

    (btp-xor m m m)
    (btp-xor x x x)

    (when (cadr z)
      (btp-not m m)
      (btp-not x x))

    (if (caddr z)
	(if (cadr z) (btp-not m m) (btp-not x x))
      (dolist (k (car z))
	(let ((a (cdr (assoc (car k) *k-bv*))))
	  (if (cadr z)
	      (btp-andc2 m a m)
	    (btp-ior x a x)))))

    (copy-tp x m (new-tp4 (tp-mask m x) x m 0 z) 0)))

(defvar *typep-defined* nil)

(defun comp-tp (type)
  (if (when *typep-defined* (atomic-type type));FIXME bootstrap NULL
      (object-tp (car (atomic-type (normalize-type type))));e.g. FLOAT coercion
    (comp-tp0 type)))

(defun btp-count (x &aux (j 0))
  (dotimes (i +bit-words+ j)
    (let* ((y (*fixnum (c-array-self x) i nil nil))
	   (q (logcount y)))
      (incf j (if (minusp y) (- fixnum-length q) q)))))

;(defun btp-count (x) (count-if-not 'zerop x))

(defun btp-type2 (x &aux (z +tp-t+))
  (dotimes (i +btp-length+ (ntp-not z))
    (unless (zerop (sbit x i))
      (setq z (ntp-and (ntp-not (cadr (aref *btpa* i))) z)))))

(defun btp-type1 (x)
  (car (nreconstruct-type (btp-type2 x))))

(let ((nn (make-btp)))
  (defun btp-type (x &aux (n (>= (btp-count x) #.(ash +btp-length+ -1))))
    (if n `(not ,(btp-type1 (btp-not x nn))) (btp-type1 x))))

;(defun btp-type (x) (btp-type1 x))


(defun tp-type (x)
  (when x
    (cond ((eq x t))
	  ((atom x) (btp-type x))
	  ((car (nreconstruct-type (caddr x)))))))

(defun num-bnd (x) (if (listp x) (car x) x))

(defun max-bnd (x y op &aux (nx (num-bnd x)) (ny (num-bnd y)))
  (cond ((or (eq x '*) (eq y '*)) '*)
	((eql nx ny) (if (atom x) x y))
	((funcall op nx ny) x)
	(y)))

(defun rng-bnd2 (y x &aux (mx (car x))(xx (cdr x))(my (car y))(xy (cdr y)))
  (let ((rm (max-bnd mx my '<))(rx (max-bnd xx xy '>)))
    (cond ((and (eql rm mx) (eql rx xx)) x)
	  ((and (eql rm my) (eql rx xy)) y)
	  ((cons rm rx)))))

(defun rng-bnd (y x) (if y (rng-bnd2 y x) x))

(defvar *btp-bnds*
  (let ((i -1))
    (mapcan (lambda (x)
	      (incf i)
	      (when (member (when (listp x) (car x)) +range-types+)
		`((,i ,(cons (cadr x) (caddr x))))))
	    +btp-types+)))

(progn
  . #.(flet ((slow-sort (fn key)
			(do* ((x nil (lreduce (lambda (y x &aux (kx (funcall key x)))
						(if (eq (max-bnd kx (funcall key y) fn) kx) x y))
					      r))
			      (r *btp-bnds* (lremove x r))
			      (n nil (cons x n)))
			     ((not r) (nreverse n)))))

	    `((defvar *btp-bnds<* ',(slow-sort '< 'caadr))
	      (defvar *btp-bnds>* ',(slow-sort '> 'cdadr)))))

(defun btp-bnds< (x)
  (dolist (l *btp-bnds<*)
    (unless (zerop (sbit x (car l)))
      (return (caadr l)))))

(defun btp-bnds> (x)
  (dolist (l *btp-bnds>*)
    (unless (zerop (sbit x (car l)))
      (return (cdadr l)))))

(defun btp-bnds (z)
  (let ((m (btp-bnds< z))(x (btp-bnds> z)))
    (when (and m x) (cons m x))))

(defun ntp-bnds (x)
  (lreduce (lambda (y x)
	     (lreduce 'rng-bnd
		      (when (member (car x) +range-types+)
			(if (eq (cadr x) t) (return-from ntp-bnds '(* . *))
			  (cdr x)))
		      :initial-value y))
	   (lreduce (lambda (y z)
		      (when (cadr x)
			(unless (assoc z y)
			  (push (list z t) y))) y)
		    +range-types+ :initial-value (car x))
	   :initial-value nil))

(defun tp-bnds (x)
  (when x
    (if (eq x t) '(* . *)
      (if (atom x) (btp-bnds x) (ntp-bnds (caddr x))))))

(defun xtp (tp) (if (listp tp) (car tp) tp))
(setf (get 'xtp 'cmp-inline) t)
(defun mtp (tp) (if (listp tp) (cadr tp) tp))
(setf (get 'mtp 'cmp-inline) t)

(defun ntp-op (op t1 t2)
  (ecase op
	 (and (ntp-and t1 t2))
	 (or (ntp-or t1 t2))))

(defun min-btp-type2 (x)
  (if (< (btp-count x) #.(ash +btp-length+ -1)) (btp-type2 x)
    (ntp-not (btp-type2 (btp-not x x)))))

(let ((tmp (make-btp)))
  (defun new-tp1 (op t1 t2 xp mp)
    (cond
     ((atom t1)
      (unless (equal xp mp)
	(if (eq op 'and)
	    (ntp-and (caddr t2) (min-btp-type2 (btp-orc2 t1 (xtp t2) tmp)))
	  (ntp-or (caddr t2) (min-btp-type2 (btp-andc2 t1 (mtp t2) tmp))))))
     ((atom t2) (new-tp1 op t2 t1 xp mp))
     ((new-tp4 (tp-mask (pop t1) (pop t1) (pop t2) (pop t2)) xp mp (if (eq op 'and) -1 1)
	       (ntp-op op (car t1) (car t2)))))))


(let ((xp (make-btp))(mp (make-btp)))
  (defun cmp-tp-and (t1 t2)
    (btp-and (xtp t1) (xtp t2) xp)
    (cond ((when (atom t1) (equal xp (xtp t2))) t2)
	  ((when (atom t2) (equal xp (xtp t1))) t1)
	  ((and (atom t1) (atom t2)) (copy-tp xp xp nil -1))
	  ((btp-and (mtp t1) (mtp t2) mp)
	   (cond ((when (atom t1) (equal mp t1)) t1)
		 ((when (atom t2) (equal mp t2)) t2)
		 ((copy-tp xp mp (new-tp1 'and t1 t2 xp mp) -1)))))))

(defun tp-and (t1 t2)
  (when (and t1 t2)
    (cond ((eq t1 t) t2)((eq t2 t) t1)
	  ((cmp-tp-and t1 t2)))))


(let ((xp (make-btp))(mp (make-btp)))
  (defun cmp-tp-or (t1 t2)
    (btp-ior (mtp t1) (mtp t2) mp)
    (cond ((when (atom t1) (equal mp (mtp t2))) t2)
	  ((when (atom t2) (equal mp (mtp t1))) t1)
	  ((and (atom t1) (atom t2)) (copy-tp mp mp nil 1))
	  ((btp-ior (xtp t1) (xtp t2) xp)
	   (cond ((when (atom t1) (equal xp t1)) t1)
		 ((when (atom t2) (equal xp t2)) t2)
		 ((copy-tp xp mp (new-tp1 'or t1 t2 xp mp) 1)))))))

(defun tp-or (t1 t2)
  (cond ((eq t1 t))
	((eq t2 t))
	((not t1) t2)
	((not t2) t1)
	((cmp-tp-or t1 t2))))


(defun cmp-tp-not (tp)
  (if (atom tp)
      (btp-not tp (make-btp))
    (list (btp-not (cadr tp) (make-btp)) (btp-not (car tp) (make-btp)) (ntp-not (caddr tp)))))

(defun tp-not (tp)
  (unless (eq tp t)
    (or (not tp)
	(cmp-tp-not tp))))


(let ((p1 (make-btp))(p2 (make-btp)))
  (defun tp<= (t1 t2)
    (cond ((eq t2 t))
	  ((not t1))
	  ((or (not t2) (eq t1 t)) nil)
	  ((equal *nil-tp* (btp-andc2 (xtp t1) (mtp t2) p1)))
	  ((equal *nil-tp* (btp-andc2 p1 (btp-andc2 (xtp t2) (mtp t1) p2) p1))
	   (eq +tp-nil+ (ntp-and (caddr t1) (ntp-not (caddr t2))))))))

(defun tp>= (t1 t2) (tp<= t2 t1))

(defun tp-p (x)
  (or (null x) (eq x t) (bit-vector-p x)
      (when (listp x)
	(and (bit-vector-p (car x))
	     (bit-vector-p (cadr x))
	     (consp (caddr x))))));FIXME

(defvar *nrm-hash* (make-hash-table :test 'equal))
(defvar *unnrm-hash* (make-hash-table :test 'eq))

(defun hashable-typep (x)
  (or (when (symbolp x)
	(unless (si-find-class x nil)
	  (let ((x (get x 's-data))) (if x (s-data-frozen x) t))))
      (when (listp x)
	(when (eq (car x) 'member)
	  (not (member-if-not 'integerp (cdr x)))))))

(defun comp-tp1 (x &aux (s (hashable-typep x)))
  (multiple-value-bind
   (r f) (when s (gethash x *nrm-hash*))
   (if f r
     (let ((y (comp-tp x)))
       (when (and s (unless (eq y t) y))
	 (setf (gethash y *unnrm-hash*) x)
	 (setf (gethash x *nrm-hash*) y))
       y))))

(defun cmp-norm-tp (x)
  (cond ((if x (eq x t) t) x)
	((eq x '*) x)
	((when (listp x)
	   (case (car x)
		 ((returns-exactly values) (cons (car x) (mapcar 'cmp-norm-tp (cdr x)))))))
	((comp-tp1 x))))

(defun tp-type1 (x)
  (multiple-value-bind
   (r f) (gethash x *unnrm-hash*)
   (if f r (tp-type x))))

(defun cmp-unnorm-tp (x)
  (cond ((tp-p x) (tp-type1 x))
	((when (listp x)
	   (case (car x)
		 ((not returns-exactly values) (cons (car x) (mapcar 'cmp-unnorm-tp (cdr x)))))))
	(x)))

(defun null-list (x) (when (plusp x) (make-list x :initial-element #tnull)))

(defun type-and (x y)
  (cond ((eq x '*) y)
	((eq y '*) x)
	((and (cmpt x) (cmpt y))
	 (let ((lx (length x))(ly (length y)))
	   (cons (if (when (eql lx ly)
		       (when (eq (car x) (car y))
			 (eq (car x) 'returns-exactly)))
		     'returns-exactly 'values)
		 (mapcar 'type-and
			 (append (cdr x) (null-list (- ly lx)))
			 (append (cdr y) (null-list (- lx ly)))))))
	((cmpt x) (type-and (or (cadr x) #tnull) y))
	((cmpt y) (type-and x (or (cadr y) #tnull)))
	((tp-and x y))))

(defun type-or1 (x y)
  (cond ((eq x '*) x)
	((eq y '*) y)
	((and (cmpt x) (cmpt y))
	 (let ((lx (length x))(ly (length y)))
	   (cons (if (when (eql lx ly)
		    (when (eq (car x) (car y))
		      (eq (car x) 'returns-exactly)))
		  'returns-exactly 'values)
		 (mapcar 'type-or1
			 (append (cdr x) (null-list (- ly lx)))
			 (append (cdr y) (null-list (- lx ly)))))))
	((cmpt x) (type-or1 x `(returns-exactly ,y)))
	((cmpt y) (type-or1 `(returns-exactly ,x) y))
	((tp-or x y))))

(defun type<= (x y)
  (cond ((eq y '*))
	((eq x '*) nil)
	((and (cmpt x) (cmpt y))
	 (do ((x (cdr x) (cdr x))(y (cdr y) (cdr y)))
	     ((and (not x) (not y)) t)
	     (unless (type<= (if x (car x) #tnull) (if y (car y) #tnull))
	       (return nil))))
	((cmpt x) (type<= x `(returns-exactly ,y)));FIXME
	((cmpt y) (type<= `(returns-exactly ,x) y))
	((tp<= x y))))

(defun type>= (x y) (type<= y x))

















(defconstant +rn+ '#.(mapcar (lambda (x) (cons (cmp-norm-tp (car x)) (cadr x))) +r+))


(defconstant +tfns1+ '(tp0 tp1 tp2 tp3 tp4 tp5 tp6 tp7 tp8))

(defconstant +rs+ (mapcar (lambda (x)
				 (cons x
				       (mapcar (lambda (y)
						 (cons (car y) (funcall x (eval (cdr y)))))
					       +rn+)))
			       +tfns1+))

(defconstant +kt+ (mapcar 'car +rn+))

(defun tps-ints (a rl)
  (lremove-duplicates (mapcar (lambda (x) (cdr (assoc (cadr x) rl))) a)))

(defun ints-tps (a rl)
  (lreduce (lambda (y x) (if (member (cdr x) a) (type-or1 y (car x)) y)) rl :initial-value nil))


(eval-when
 (compile eval)
 (defun msym (x) (intern (string-concatenate (string x) "-TYPE-PROPAGATOR") :si)))

(defconstant +ktn+ (mapcar (lambda (x) (cons x (tp-not x))) +kt+))

(defun decidable-type-p (x)
  (or (atom x)
      (not (third (third x)))))

(defun type-and-list (tps)
  (mapcan (lambda (x &aux (q x))
	    (mapcan (lambda (y)
		      (unless (tp<= q (cdr y))
			`((,x ,(car y)
			      ,(cond ((tp<= (car y) x) (car y))
				     ((let ((x (type-and (car y) x)))
					(when (decidable-type-p x)
					  x)))
				     (x))))))
		    +ktn+))
	  tps))

(defconstant +rq1+
  (mapcar (lambda (x)
	    (cons (pop x)
		  (lreduce (lambda (y x &aux (nx (tp-not (car x))))
			     (let ((z (rassoc (cdr x) y)))
			       (if z
				   (setf (car z) (tp-and nx (car z)) y y)
				 (cons (cons nx (cdr x)) y))))
			   x :initial-value nil)))
	  +rs+))

(defun norm-tp-ints (tp rl)
  (cmp-norm-tp
   (cons 'member
	 (lreduce (lambda (y x)
	     (if (tp<= tp (car x)) y (cons (cdr x) y)))
	   rl :initial-value nil))))


(progn;FIXME macrolet norm-tp-ints can only compile-file, not compile
  . #.(mapcar (lambda (x &aux (s (msym x)))
		`(let* ((rl (cdr (assoc ',x +rq1+))))
		   (defun ,s (f x)
		     (declare (ignore f))
		     (norm-tp-ints x rl))
		   (setf (get ',x 'type-propagator) ',s)
		   (setf (get ',x 'c1no-side-effects) t)))
	      +tfns1+))



(defun best-type-of (c)
  (let* ((r (lreduce 'set-difference c :key 'car :initial-value +kt+))
	 (tps (nconc (mapcar 'car c) (list r))))
    (or (caar (member-if (lambda (x)
			   (let* ((f (pop x))
				  (z (mapcan
				      (lambda (y)
					(lremove-duplicates
					 (mapcar (lambda (z) (cdr (assoc z x))) y)))
				      tps)))
			     (eq z (lremove-duplicates z))))
			 +rs+))
	(caar +rs+))))

(defun calist2 (a)
  (lreduce (lambda (y x &aux (z (rassoc (cdr x) y :test 'equal)));;aggregate identical subtypes, e.g. undecidable
	     (if z (setf (car z) (cons (caar x) (car z)) y y) (setf y (cons x y))))
	   (mapcar (lambda (x)
		     (cons (list x);; collect specified types intersecting with this tps
			   (mapcan (lambda (y &aux (q (caddr y)))
				     (when (eq x (cadr y))
				       (list (cons (car y) (unless (eq q x) q)))));;only subtypes smaller than tps
				   a)))
		   (lreduce (lambda (y x) (adjoin (cadr x) y)) a :initial-value nil));;unique tps
	   :initial-value nil))

(defconstant +useful-type-list+ `(nil
				  null
				  boolean keyword symbol
				  proper-cons cons proper-list list
				  simple-vector simple-string simple-bit-vector
				  string vector array
				  proper-sequence sequence
				  zero one
				  bit rnkind non-negative-char unsigned-char signed-char char
				  non-negative-short unsigned-short signed-short short
				  seqind non-negative-fixnum
				  non-negative-integer
				  immfix tractable-fixnum fixnum bignum integer
				  negative-short-float positive-short-float
				  non-negative-short-float non-positive-short-float
				  short-float
				  negative-long-float positive-long-float
				  non-negative-long-float non-positive-long-float
				  long-float
				  negative-float positive-float
				  non-negative-float non-positive-float
				  float
				  negative-real positive-real
				  non-negative-real non-positive-real
				  real
				  fcomplex dcomplex
				  complex-integer complex-ratio
				  complex-ratio-integer complex-integer-ratio
				  complex
				  number
				  character structure package hash-table function
				  t))
;; (defconstant +useful-types+ (mapcar 'cmp-norm-tp +useful-type-list+))
(defconstant +useful-types-alist+ (mapcar (lambda (x) (cons x (cmp-norm-tp x))) +useful-type-list+))
