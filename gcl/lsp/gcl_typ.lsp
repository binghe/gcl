(in-package :si)

;; (defun logandc2 (x y) (boole boole-andc2 x y))
;; (defun abs (x) (if (< x 0) (- x) x))

(deftype object nil t);FIXME

(defconstant +bit-types+
  (lremove
   nil
   (mapcar
    (let (y)
      (lambda (x &aux (z y))
	(setq y (car (resolve-type (list 'or x y))))
	(car (resolve-type `(and ,x (not ,z))))))
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
      ,@(mapcar 'car +r+)))))
(defconstant +n-bit-types+ (length +bit-types+))
(defvar *bit-types* +bit-types+)

(defconstant %sz% (1+ (integer-length most-positive-fixnum)))
(defconstant %typ-m% (1- (ash 1 (mod +n-bit-types+ %sz%))))

(defstruct tp
  (f1 0 :type fixnum)
  (f2 0 :type fixnum))

(defstruct (typ (:include tp))
  (x1 -1 :type fixnum)
  (x2 (logand %typ-m% -1) :type fixnum)
  (type nil));FIXME :type list

(defconstant %typ-nil% (make-typ :x1 0 :x2 0))
(defconstant %typ-t% (make-typ :f1 -1 :f2 (logand -1 %typ-m%)))

(defvar *typ-register* (make-typ))
(defvar *typ-mask*     (make-typ));make-tp


(defmacro typ-word-loop (&rest forms &aux x (sz #.(1+ (integer-length most-positive-fixnum))))
  (flet (;(msym (x n) (intern (concatenate 'string (symbol-name x) (write-to-string n))));FIXME
	 (msym (x n)
	   (if (eq x 'typ-f)
	       (ecase n (1 'typ-f1) (2 'typ-f2));FIXME
	       (ecase n (1 'typ-x1) (2 'typ-x2)))))
	(multiple-value-bind
	 (n r) (truncate +n-bit-types+ sz)
	 (dotimes (i (+ n (if (zerop r) r 1)) (cons 'progn (nreverse x)))
	   (let ((typ-f (msym 'typ-f (1+ i)))(typ-x (msym 'typ-x (1+ i))))
	   (push
	    `(flet ((typ-f (x) (,typ-f x))(set-typ-f (typ x) (setf (,typ-f typ) x))
		    (typ-x (x) (,typ-x x))(set-typ-x (typ x) (setf (,typ-x typ) x))
		    (max nil ,(if (< i n) sz r)))
		   ,@forms)
	    x))))))

(defmacro typ-word-boolean (form)
  `(block nil (typ-word-loop (unless ,form (return))) t))

(defun coarse-typ-p (x)
  (typ-word-boolean (eql (typ-f x) (typ-x x))))
     
(defun typ-eql (x y)
  (typ-word-boolean (and (eql (typ-f x) (typ-f y)) (eql (typ-x x) (typ-x y)))))

(defun set-typ-ior (x y z)
  (typ-word-loop
   (set-typ-f x (logior (typ-f y) (typ-f z)))
   (set-typ-x x (logior (typ-x y) (typ-x z))))
  x)

(defun set-typ-and (x y z)
  (typ-word-loop
   (set-typ-f x (logand (typ-f y) (typ-f z)))
   (set-typ-x x (logand (typ-x y) (typ-x z))))
  x)

(defun set-typ-not (x y)
  (typ-word-loop
   (let ((z (if (eql (max) %sz%) -1 %typ-m%)))
     (set-typ-f x (logandc2 z (typ-x y)))
     (set-typ-x x (logandc2 z (typ-f y)))))
  x)
  
(defvar *nstp* 0)
(defun msubtypep (t1 t2)
  (incf *nstp*)
  (values (subtypep t1 t2)))

(defun typ-?un?set-bits (type ?set typ mask &aux (*bit-types* +bit-types+))
  (typ-word-loop
   (let ((res (if ?set (typ-f typ) (typ-x typ)))(mask (typ-f mask)))
     (dotimes (i (max) (if ?set (set-typ-f typ res) (set-typ-x typ res)))
       (let ((x (pop *bit-types*))(a (<< 1 i)))
	 (unless (zerop (logand mask a))
	   (when (if ?set (msubtypep x type) (msubtypep type `(not ,x)))
	     (setf res (if ?set (logior res a) (logandc2 res a))))))))))
(declaim (inline typ-?un?set-bits))

(defun typ-mask (mask typ1 &optional typ2)
  (typ-word-loop
   (let ((x (logxor (typ-f typ1) (typ-x typ1))))
     (set-typ-f mask (if typ2 (logand x (logxor (typ-f typ2) (typ-x typ2))) x))))
  mask)


(defun typ-points-max (items &aux (typ (copy-typ %typ-nil%)))
  (dolist (item items typ)
    (let ((x (position item +bit-types+ :test 'typep)))
      (block nil
	(typ-word-loop
	 (cond ((< x (max)) (set-typ-x typ (logior (typ-x typ) (<< 1 x))) (return))
	       ((decf x (max)))))))))

(defun type-points (type)
  (or (atomic-type type)
      (when (consp type)
	(when (eq (car type) 'member)
	  (cdr type)))))

(defun typ-get-max (type &aux (at (type-points type)))
  (if at
      (typ-points-max at)
      (let ((typ (typ-guess-max type)))
	(typ-?un?set-bits type nil typ (typ-mask *typ-mask* typ))
	typ)))

(defun type-to-typ (type &optional (typ (typ-get-max type) typp))
  (when typp
    (typ-?un?set-bits type nil typ (typ-mask *typ-mask* typ)))
  (typ-?un?set-bits type t   typ (typ-mask *typ-mask* typ))
  (unless (coarse-typ-p typ) (setf (typ-type typ) type))
  typ)

(defun typ-guess-test (type &aux (*nstp* 0))
  (let* ((x (type-to-typ type))
	 (n *nstp*)
	 (y (type-to-typ type (make-typ))))
    (values (equalp x y) n (- *nstp* n))))

(defun copy-atomic-typ (x y &aux (x (copy-typ x)))
  (setf (cadr (typ-type x)) y)
  x)

(defun clean-type (x);FIXME
  (cond ((si::si-classp x) (or (si::si-class-name x) t))
	((si::structurep x) (or (si::s-data-name x) t))
	((atom x) x)
	((let* ((a (car x))(ca (clean-type a))(d (cdr x))(cd (clean-type d)))
	   (if (and (eq a ca) (eq d cd))
	       x
	       (cons ca cd))))))

(defun new-type (x)
  (clean-type (car (resolve-type x))))

(defun typ-nmax-bits (typ &aux (j 0))
  (typ-word-loop
   (incf j (popcount (typ-x typ))))
  j)

(defun atomic-type (tp)
  (cond ((eq tp 'true) '(t));FIXME
	((eq tp 'null) '(nil))
	((when (consp tp)
	   (case (car tp)
		 ((integer ratio short-float long-float)
		  (let* ((d (cdr tp))(dd (cadr d))(da (car d)))
		    (and (numberp da) (numberp dd) (= da dd) d)))
		 ((member eql) (let ((d (cdr tp))) (unless (cdr d) d))))))));FIXME

(defun atomic-typ (typ)
  (when (eql 1 (typ-nmax-bits typ))
    (atomic-type (or (typ-type typ) (typ-min-types typ)))))


(defun typ-and (t1 t2 &aux (ctp1 (coarse-typ-p t1)) (ctp2 (coarse-typ-p t2)))
  (set-typ-and *typ-register* t1 t2)
  (cond ((when (or ctp1 ctp2) (typ-eql *typ-register* t2)) t2)
	((when (or ctp1 ctp2) (typ-eql *typ-register* t1)) t1)
	(t
	 (setf (typ-type *typ-register*)
	       (cond ((coarse-typ-p *typ-register*) nil)
		     (ctp1 (typ-type t2))
		     (ctp2 (typ-type t1))
		     ((let* ((type (new-type `(and ,(typ-type t1) ,(typ-type t2)))))
			;; (let* ((x (cons (logand (mask1 t1) (mask1 t2))
			;; 		(logand (mask2 t1) (mask2 t2))))
			;;        (y (or (assoc x *nbtsal* :test 'equal)
			;; 	      (car (push (cons x 0) *nbtsal*)))))
			;;   ;; (when (and (eql (car x) 1) (eql (cdr x) 0))
			;;   ;;   (pushnew (typ-type t1) *nbttps* :test 'equal)
			;;   ;;   (pushnew (typ-type t2) *nbttps* :test 'equal))
			;;   ;; (when (equal x (cons 4096 256))
			;;   ;;   (print (list t1 t2)) (break))
			;;   (incf (cdr y)))
			(typ-?un?set-bits type nil *typ-register* (typ-mask *typ-mask* t1 t2))
			(unless (coarse-typ-p *typ-register*)
			  type)))))
	(cond ((typ-eql *typ-register* %typ-nil%) %typ-nil%)
	      ((typ-eql *typ-register* %typ-t%) %typ-t%)
	      ((copy-typ *typ-register*))))))


(defconstant +bit-type-guesses+
  (mapcar (lambda (x) (cons x (typ-and (make-typ) (type-to-typ x (make-typ)))))
	  '(integer number list vector array symbol structure)))

(defun typ-guess-max (type)
  (let ((x (cdar (member type +bit-type-guesses+ :key 'car :test 'msubtypep))))
    (if x (copy-typ x) (make-typ))))

(defun typ<= (t1 t2)
  (or (typ-word-boolean (eql (logand (typ-x t1) (typ-f t2)) (typ-x t1)))
      (unless (typ-word-boolean (zerop (typ-f (typ-mask *typ-mask* t1 t2))))
	(msubtypep (typ-type t1) (typ-type t2)))))

(defun typ= (t1 t2)
  (when (typ-eql t1 t2)
    (or (coarse-typ-p t1)
	(let ((t1 (typ-to-type t1))(t2 (typ-to-type t2)))
	  (when (msubtypep t1 t2)
	    (msubtypep t2 t1))))))

(defun typ-or (t1 t2 &aux (ctp1 (coarse-typ-p t1)) (ctp2 (coarse-typ-p t2)))
  (set-typ-ior *typ-register* t1 t2)
  (cond ((when (or ctp1 ctp2) (typ-eql *typ-register* t2)) t2)
	((when (or ctp1 ctp2) (typ-eql *typ-register* t1)) t1)
	(t
	 (setf (typ-type *typ-register*)
	       (cond ((coarse-typ-p *typ-register*) nil)
		     (ctp1 (typ-type t2))
		     (ctp2 (typ-type t1))
		     ((let ((type (new-type `(or ,(typ-type t1) ,(typ-type t2)))))
			(typ-?un?set-bits type t *typ-register* (typ-mask *typ-mask* t1 t2))
			(unless (coarse-typ-p *typ-register*)
			  type)))))
	(cond ((typ-eql *typ-register* %typ-nil%) %typ-nil%)
	      ((typ-eql *typ-register* %typ-t%) %typ-t%)
	      ((copy-typ *typ-register*))))))


(defun typ-not (t1 &aux (typ (make-typ)))
  (set-typ-not typ t1)
  (unless (coarse-typ-p typ)
    (setf (typ-type typ) (new-type `(not ,(typ-type t1)))))
  typ)

(defvar *nbts* 0)
(defvar *nbtsal* nil)
(defvar *nbttps* nil)

(defun typ-min-types (typ &aux res (bit-types +bit-types+))
  (typ-word-loop
   (dotimes (i (max) (if (cdr res) (cons 'or res) (car res)))
     (let ((x (pop bit-types)))
       (unless (zerop (logand (typ-f typ) (<< 1 i)))
	 (push x res))))))

(defun typ-max-types (typ &aux res (bit-types +bit-types+))
  (typ-word-loop
   (dotimes (i (max) (if (cdr res) (cons 'or res) (car res)))
     (let ((x (pop bit-types)))
       (unless (zerop (logand (typ-x typ) (<< 1 i)))
	 (push x res))))))

(defun typ-to-type (typ)
  (identity ;cmp-norm-tp
   (let* ((mins (typ-min-types typ))
	  (maxs (unless (coarse-typ-p typ) `(and ,(typ-type typ) ,(typ-max-types typ)))))
     (cond ((and mins maxs) `(or ,mins ,maxs))
	    (mins)
	    (maxs)))))

(defun max-bnd (x y op &aux (nx (if (atom x) x (car x))) (ny (if (atom y) y (car y))))
  (cond ((or (eq x '*) (eq y '*)) '*)
	((= nx ny) (if (atom x) x y))
	((funcall op nx ny) x)
	(y)))

(defun real-bnds (tp)
  (cond ((and (consp tp) (eq (car tp) 'or))
	 (reduce (lambda (y x &aux (x (real-bnds x))) 
		   (if y
		       (list (max-bnd (car y) (car x) '<) (max-bnd (cadr y) (cadr x) '>))
		       x))
		 (cdr tp) :initial-value nil))
	((cdr tp))))



(defun rep (x)
  (cond ((member x '(returns-exactly values)))
	((atom x) nil)
	((or (rep (car x)) (rep (cdr x))))))

(defvar *utps2* nil)

(defun run nil
  (setq *utps2* nil si::*notify-gbc* t)
  (maphash (lambda (x y) (unless (rep x) (unless (eq x '*) (push (cons x (type-to-typ x)) *utps2*))))
	   *norm-tp-hash*)

  (setq *nstp* 0 *nbtsal* nil)
  (dolist (l *utps2*) (dolist (m *utps2*) (unless (typ= (type-to-typ (type-and (car l) (car m))) (typ-and (cdr l) (cdr m))) (print (setq a (list l m (type-to-typ (type-and (car l) (car m))) (typ-and (cdr l) (cdr m)))))(Break))))
  (time (dolist (l *utps2*) (dolist (m *utps2*) (when t (typ-and (cdr l) (cdr m))))))
  (print (/ (float *nstp*) (* (length *utps2*) (length *utps2*))))

  (setq *nstp* 0 *nbtsal* nil)
  (time (dolist (l *utps2*) (dolist (m *utps2*) (when t (typ<= (cdr l) (cdr m))))))
  (print (/ (float *nstp*) (* (length *utps2*) (length *utps2*))))
  
  (setq *nstp* 0 *nbtsal* nil)
  (time (dolist (l *utps2*) (dolist (m *utps2*) (when t (typ-or (cdr l) (cdr m))))))
  (print (/ (float *nstp*) (* (length *utps2*) (length *utps2*))))
  
  (mapcar (lambda (x)
	    (list (typ-to-type (make-typ :f1 (caar x) :f2 (cdar x) :x1 (caar x) :x2 (cdar x)))
		  (* (cdr x) (+ (logcount (caar x)) (logcount (cdar x))))))
	  (sort  *nbtsal* '>  :key (lambda (x) (* (cdr x) (+ (logcount (caar x)) (logcount (cdar x))))))))


;; (defun cmp-norm-tp (x)
;;   (case x
;;     ((t object *) %typ-t%)
;;     ((nil) %typ-nil%)
;;     (otherwise (if (typ-p x) x (type-to-typ x)))))
;; (defun cmp-unnorm-tp (x) (if (typ-p x) (typ-to-type x) x))
;; (defun type-and (x y) (typ-and (cmp-norm-tp x) (cmp-norm-tp y)))
;; (defun type-or1 (x y) (typ-or (cmp-norm-tp x) (cmp-norm-tp y)))
;; (defun type-not (x) (typ-not (cmp-norm-tp x)))
;; (defun type>= (x y) (typ<= (cmp-norm-tp y) (cmp-norm-tp x)))
;; (defun type<= (x y) (typ<= (cmp-norm-tp x) (cmp-norm-tp y)))
;; (defun compiler::coerce-to-one-value (type) (if (when (listp type) (member (car type) '(returns-exactly values))) (cadr type) type))
;; (defun atomic-tp (tp) (atomic-typ (cmp-norm-tp tp)))
;; (defun type= (t1 t2) (typ= (cmp-norm-tp t1) (cmp-norm-tp t2)))


;; (c-set-symbol-stype '+rn+ 0)
;; (defconstant +rn+ (mapcar (lambda (x) (cons (cmp-norm-tp (car x)) (cadr x))) +r+))
;; (c-set-symbol-stype '+rs+ 0)
;; (defconstant +rs+ (mapcar (lambda (x) (cons x (mapcar (lambda (y) (cons (car y) (funcall x (eval (cdr y))))) +rn+))) +tfns1+))
;; (c-set-symbol-stype '+kt+ 0)
;; (defconstant +kt+ (mapcar 'car +rn+))

;; (defun typ-nil (x) (eq x %typ-nil%))
;; (defun type-nil (x) (typ-nil (cmp-norm-tp x)))

;; (defun type-and-list (tps)
;;   (mapcan (lambda (x) (mapcan (lambda (y &aux (z (type-and x y))) (unless (typ-nil z) `((,x ,y ,z)))) +kt+)) tps))

;; #.(labels ((lcf (shft &optional res)
;; 	   (if (> (abs shft) (integer-length most-positive-fixnum)) 
;; 	       (nreverse res)
;; 	       (lcf (ash shft 1) (cons `(x (+ x (ash x ,shft))) res))))
;; 	 (lc (pat shft)
;; 	   (if (> shft (integer-length most-positive-fixnum)) 
;; 	       pat
;; 	       (lc (logior pat (ash pat shft)) (ash shft 1)))))
;;   `(progn
;;     (defun popcount (x)
;;       (declare (non-negative-fixnum x))
;;       (let* ((x (- x (logand (ash x -1) ,(lc 5 4))))
;; 	     (x (+ (logand x ,(lc 3 4)) (logand (ash x -2) ,(lc 3 4))))
;; 	     (x (logand ,(lc 15 8) (+ x (ash x -4))))
;; 	     ,@(lcf -8))
;; 	(logand x ,(1- (ash (1+ (integer-length most-positive-fixnum)) 1)))))
;;     (declaim (inline popcount))))

;; (defun mkinfm (f tp z &aux (z (?-add 'progn z)))
;;   `(infer-tp ,f ,(cmp-norm-tp tp) ,z))
