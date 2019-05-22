(in-package :si)

(defvar *array-types* (cons (cons nil 'array-nil) +array-type-alist+))

(defvar *kingdom-ops*
  `((integer int^ int~ rng-recon)
    ,@(mapcar (lambda (x) `(,x rng^ rng~ rng-recon))
	      '(ratio short-float long-float))
    (complex-integer cmpi^ cmpi~ cmp-recon)
    (complex-ratio   cmp^  cmpr~ cmp-recon)
    ,@(mapcar (lambda (x) `(,x cmp^ cmp~ cmp-recon))
	      '(complex-short-float complex-long-float))
    (structure str^ str~ str-recon)
    ,@(mapcar (lambda (x) `(,x std^ std~ std-recon))
	      '(std-instance
		standard-generic-compiled-function
		standard-generic-interpreted-function))
    ,@(mapcar (lambda (x) `(,x  cns^ cns~ cns-recon))
	      '(proper-cons improper-cons))
;    (cons cns^ cns~ cns-recon)
    ,@(mapcar (lambda (x) `(,(cdr x) ar^ ar~ ar-recon))
	      *array-types*)
    ,@(mapcar (lambda (x) `(,x sing^ sing~ sing-recon))
	      +singleton-types+)))

#.`(defun k~ (k x)
     (unless (eq x t)
       (case k
	 (integer (int~ x))
	 ((ratio short-float long-float) (rng~ x))
	 (complex-integer (cmpi~ x))
	 (complex-ratio   (cmpr~ x))
	 ((complex-short-float complex-long-float) (cmp~ x))
	 (structure (str~ x))
	 ((std-instance
	   standard-generic-compiled-function
	   standard-generic-interpreted-function) (std~ x))
	 ((proper-cons improper-cons) (cns~ x))
	 (,(mapcar 'cdr *array-types*) (ar~ x))
	 (otherwise (sing~ x)))));,+singleton-types+

#.`(defun k^ (k x y)
     (cond ((eq x t) y)
	   ((eq y t) x)
	   ((case k
	      (integer (int^ x y))
	      ((ratio short-float long-float) (rng^ x y))
	      (complex-integer (cmpi^ x y))
	      ((complex-ratio complex-short-float complex-long-float)   (cmp^ x y))
	      (structure (str^ x y))
	      ((std-instance
		standard-generic-compiled-function
		standard-generic-interpreted-function) (std^ x y))
	      ((proper-cons improper-cons) (cns^ x y))
	      (,(mapcar 'cdr *array-types*) (ar^ x y))
	      (otherwise (sing^ x y))))));,+singleton-types+

(defmacro ntp-ld (ntp tp)
  `(let ((x ,tp))
     (when x
       (push x (car ,ntp)))))

(defmacro negate (lst)
  (let ((l (gensym)))
    `(let ((,l ,lst))
       (cond ((not ,l))
	     ((eq ,l t) nil)
	     ((and (consp ,l) (eq (car ,l) 'not)) (cadr ,l))
	     (`(not ,,l))))))


;;; ARRAY


(defun rnki (x)
  (when (listp x)
    (when (eq (car x) 'rank)
      (cdr x))))

(defun rd^ (x y)
  (cond ((eql x y) x);eq
	((atom x) (when (listp y) (unless (member x y) x)));memq
	((atom y) (rd^ y x))
	((union x y))));test

(defun dim^ (x y)
  (cond ((eq x '*) y)
	((eq y '*) x)
	((rd^ x y))))

(defun dims^ (x y)
  (cond ((and x y)
	 (let* ((a (dim^ (car x) (car y)))
		(d (when a (dims^ (cdr x) (cdr y)))))
	   (when d
	     (list (cons a (car d))))))
	((or x y) nil)
	((list nil))))

(defun adims (x)
  (cond ((arrayp x) (array-dimensions x))
	((when (listp x) (arrayp (car x))) (array-dimensions (car x)))
	(x)))

(defun ar^ (x y &aux (rx (rnki x))(ry (rnki y))(ax (adims x))(ay (adims y)))
  (cond ((and rx ry) (let ((d (rd^ rx ry))) (when d (cons 'rank d))))
	(rx (when (rd^ rx (length (adims y))) y))
	(ry (ar^ y x))
	((and (eq x ax) (eq y ay)) (car (dims^ x y)))
	((eq x ax) (when (ar^ x ay) y))
	((eq y ay) (ar^ y x))
	((ar^ ax ay) (rd^ x y))))

(defun disu (x)
  (when x
    (let* ((cx (pop x))(cx (if (atom cx) (list cx) cx)))
      (mapcan (lambda (y) (mapcar (lambda (x) (cons x y)) cx))
	      (if x (disu x) (list x))))))

(defun ar-recon (x &optional (tp (car (rassoc (pop x) *array-types*)) tpp)(ax (adims x)))
  (cond ((not tpp) (?or (mapcar (lambda (z) (ar-recon z tp)) x)))
	((eq x t) `(array ,tp *));'*
	((consp (rnki x))
	 `(and ,(ar-recon t tp)
	       (not (or ,@(mapcar (lambda (x) (ar-recon (cons 'rank x) tp)) (rnki x))))))
	((rnki x) `(array ,tp ,(rnki x)))
	((when (eq x ax) (member-if 'listp x))
	 `(and ,(ar-recon (mapcar (lambda (x) (if (atom x) x '*)) x) tp)
	       (not (or ,@(mapcar (lambda (x) (ar-recon x tp)) (disu x))))))
	((eq ax x) `(array ,tp ,x))
	((listp x) `(and ,(ar-recon ax) (not (member ,@x))))
	(`(member ,x))))

(defun onot (x)
  (when x
      (let ((d (mapcar (lambda (y) (cons (car x) y)) (onot (cdr x)))))
	(if (eq (car x) '*) d
	    (cons (cons (list (car x)) (make-list (length (cdr x)) :initial-element '*)) d)))))

(defun ar~ (x &aux (ax (adims x)))
  (cond ((consp (rnki x)) (mapcar (lambda (x) (cons 'rank x)) (rnki x)))
	((rnki x) `((rank ,(rnki x))))
	((when (eq x ax) (member-if 'listp x))
	 (nconc (ar~ (substitute-if '* 'listp x)) (disu x)))
	((eq x ax) (nconc (ar~ (cons 'rank (length x))) (onot x)))
	((listp x) (nconc (ar~ (array-dimensions (car x))) x))
	((nconc (ar~ (array-dimensions x)) `((,x))))))

(defun ar-load (ntp type)
  (ntp-ld ntp `(,(cdr (assoc (cadr type) *array-types*))
		 ,(let ((x (caddr type)))
		    (cond ((eq x '*) t)
			  ((integerp x) (cons 'rank x))
			  (x))))))


;;; SINGETON



(defun sing^ (x y) (rd^ y x))

(defun sing~ (x)
  (cond ((listp x) x)
	((list (list x)))))

(defun sing-load (ntp type) (ntp-ld ntp (cons (car type) '(t))))

(defun sing-recon (x &aux (c (pop x)))
  (cond ((eq (car x) t) (case c (null `(member nil)) (true `(member t)) (otherwise c)));FIXME
	((listp (car x)) `(and ,c (not (member ,@(car x)))))
	(`(member ,@x))))


;;; INTEGER



(defun intcmp (x y f)
  (cond ((eq x '*) y)((eq y '*) x)((funcall f x y) y)(x)))

(defun icons (a d)
  (when (or (eq a '*) (eq d '*) (<= a d))
    (if (and (eq a '*) (eq d '*)) t (cons a d))))

(defun int^ (x y)
  (icons (intcmp (car x) (car y) '<) (intcmp (cdr x) (cdr y) '>)))

(defun int~ (x)
  (cond ((eq (car x) '*) (list (cons (1+ (cdr x)) '*)))
	((eq (cdr x) '*) (list (cons '* (1- (car x)))))
	((nconc (int~ (cons (car x) '*)) (int~ (cons '* (cdr x)))))))


;;; RANGES



(defun range-cons (range &aux (a (pop range))(d (car range)))
  (if (and (eq '* a) (eq '* d)) t (cons a d)))

(defun rngnum (x)
  (if (listp x) (car x) x))

(defun rngcmp (x y f &aux (nx (rngnum x))(ny (rngnum y)))
  (cond ((eq x '*) y)((eq y '*) x)((funcall f nx ny) y)((funcall f ny nx) x)((eq y ny) x)(y)))

(defun ncons (a d &aux (na (rngnum a))(nd (rngnum d)))
  (when (or (eq a '*) (eq d '*) (< na nd) (and (eql a d) (eql na nd) (eql a na)))
    (if (and (eq a '*) (eq d '*)) t (cons a d))))

(defun rng^ (x y)
  (ncons (rngcmp (car x) (car y) '<) (rngcmp (cdr x) (cdr y) '>)))

(defun rngi (x)
  (if (listp x) (if (integerp (car x)) x (car x)) (list x)))

(defun rng~ (x)
  (cond ((eq (car x) '*) (list (cons (rngi (cdr x)) '*)))
	((eq (cdr x) '*) (list (cons '* (rngi (car x)))))
	((nconc (rng~ (cons (car x) '*)) (rng~ (cons '* (cdr x)))))))

(defun rng-load (ntp type) (ntp-ld ntp (list (pop type) (range-cons type))))

(defun rng-recon (x &aux (c (pop x)))
  (if (eq (car x) t)
      (list c '* '*)
      (?or (mapcar (lambda (x) (list c (car x) (cdr x))) x))))
  

;;; COMPLEX


(defun cmp-load (ntp type &aux (rtp (cadr type)) (o (eq (car rtp) 'or)))
  (ntp-ld ntp `(,(cadr (assoc (if o (caadr rtp) (car rtp)) +ctps+))
		 ,(let ((x (mapcar (lambda (x) (range-cons (cdr x))) (if o (cdr rtp) (list rtp)))))
		    (cons x x)))))

(defun cmp-cons (a d) (when (and a d) (list (cons a d))))

(defun cmpi~ (x);complex integer
  (let ((a (op-not (car x) 'int^ 'int~))(d (op-not (cdr x) 'int^ 'int~)))
    (nconc (cmp-cons (car x) d) (cmp-cons a (cdr x)) (cmp-cons a d))))

(defun cmpi^ (x y);complex integer
  (car (cmp-cons (op-and (car x) (car y) 'int^)
		 (op-and (cdr x) (cdr y) 'int^))))

(defun rati (x)
  (if (listp x) (car x) (list x)))

(defun rat~ (x)
  (cond ((eq (car x) '*) (list (cons (rati (cdr x)) '*)))
	((eq (cdr x) '*) (list (cons '* (rati (car x)))))
	((nconc (rat~ (cons (car x) '*)) (rat~ (cons '* (cdr x)))))))


(defun cmpr~ (x);complex ratio
  (let ((a (op-not (car x) 'rng^ 'rat~))(d (op-not (cdr x) 'rng^ 'rat~)))
    (nconc (cmp-cons (car x) d) (cmp-cons a (cdr x)) (cmp-cons a d))))

(defun cmp^ (x y)
  (car (cmp-cons (op-and (car x) (car y) 'rng^)
		 (op-and (cdr x) (cdr y) 'rng^))))

(defun cmp~ (x)
  (let ((a (op-not (car x) 'rng^ 'rng~))(d (op-not (cdr x) 'rng^ 'rng~)))
    (nconc (cmp-cons (car x) d) (cmp-cons a (cdr x)) (cmp-cons a d))))

(defun cmp-recon (x &aux (c (car (rassoc (pop x) +ctps+ :key 'car))))
  (cond ((eq (car x) t) `(complex (,c * *)))
	((?or (mapcar (lambda (x) `(complex ,(rng-recon (cons c (car x))))) x)))))


;;; CONS


(defun cns-list (a d &optional m n)
  (when (and (or (car a) (cadr a)) (or (car d) (cadr d)))
    (if (and (unless (car a) (cadr a)) (unless (car d) (cadr d))); (not m) (not n)
	`(t)
	`((,a ,d ,m ,n)))))

(defun cns^ (x y)
  (cond ((let ((a (ntp-and (car x) (car y)))
	       (d (ntp-and (cadr x) (cadr y))))
	   (when (or (car a) (cadr a))
	     (when (or (car d) (cadr d))
	       (let ((mx (caddr x))(nx (cadddr x))(my (caddr y))(ny (cadddr y)))
		 (cond ((and mx my) (when (eql mx my) x))
		       ((and mx ny) (unless (member mx ny) x))
		       (mx x)
		       ((and nx my) (unless (member my nx) y))
		       (my y)
		       ((and nx ny) (car (cns-list a d nil (union nx ny))))
		       (nx x)
		       (ny y)
		       ((car (cns-list a d)))))))))))


(defun cns~ (x)
  (cond ((let ((a (ntp-not (car x)))
	       (d (ntp-not (cadr x))))
	   (nconc (cns-list a (cadr x))
		  (cns-list (car x) d)
		  (cns-list a d)
		  (when (caddr x)
		    (cns-list (car x) (cadr x) nil (list (caddr x))))
		  (mapcan (lambda (y) (cns-list (car x) (cadr x) y)) (cadddr x)))))))


(defvar *pcns-ntp* `(((proper-cons t)(null t)) nil nil))
(defvar *pcns-nntp* `(((proper-cons)(null)) t nil))

(defun ntp-lattice-bound (type bound)
  (let ((u (nprocess-type `(and ,bound (not ,type)))))
    (if (not (or (car u) (cadr u))) +tp-t+
	(nprocess-type `(and ,type ,bound)))))


(defun cns-load (ntp type &aux (c (pop type))(a (nprocess-type (pop type))))
  (ntp-ld ntp (cons 'proper-cons
		    (cns-list a (ntp-lattice-bound (car type) (normalize-type 'proper-list)))))
  (ntp-ld ntp (cons 'improper-cons
		    (cns-list a (ntp-lattice-bound (car type) (normalize-type '(not proper-list)))))))

(defun cns-recon-cap (c x)
  (if (eq (car x) t) x
      (op-and (cns-list +tp-t+ (if (eq c 'proper-cons) *pcns-ntp* *pcns-nntp*))
	      x 'cns^)))

(defun cns-recon (x &optional (c (pop x) cp))
  (cond ((not cp) (?or (mapcar (lambda (x) (cns-recon x c)) (cns-recon-cap c x))))
	((eq x t) c)
	((caddr x) `(member ,(caddr x)))
	((cadddr x) `(not (member ,@(cadddr x))))
	(x `(,c ,(nreconstruct-type-int (pop x))
		,(nreconstruct-type-int (car x))))))


;;; STRUCTURE


(defun str-def (x) (c-structure-def (if (listp x) (car x) x)))

(defun s-data-tokp (x &aux (x (str-def x)))
  (eq x (str-def x)))

(defun str~ (x)
  (cond ((s-data-tokp x) (if (listp x) x `((,x))))
	((nconc (str~ (str-def x)) (if (listp x) x `((,x)))))))

(defun str^ (x y)
  (cond ((eq (str-def x) (str-def y)) (rd^ x y))
	((s-data-tokp x) (when (str^ x (str-def y)) y))
	((s-data-tokp y) (str^ y x))))

(defun str-load (ntp x)
  (ntp-ld ntp (cons (pop x)
		    (or (mapcar (lambda (x) (get x 's-data)) (when x (get-included (car x))))
			'(t)))))

(defun str-recon (x &optional (c (pop x) cp))
  (cond 
    ((not cp) (?or (mapcar (lambda (x) (str-recon x c)) x)))
    ((eq x t) c)
    ((s-data-tokp x) (if (listp x) (cons 'not x) x))
    ((listp x) `(and ,(str-def x) (not (member ,@x))))
    (`(member ,x))))


;;; CLASS


(defun si-subclasses (c)
  (when c 
    (cons c (mapcan 'si-subclasses (si-class-direct-subclasses c)))))
  
(defun std-def (x) (si-class-of (if (listp x) (car x) x)))

(defun s-class-p (x &aux (x (std-def x)))
  (eq x (std-def x)))

(defun std~ (x)
  (cond ((s-class-p x) (if (listp x) x `((,x))))
	((nconc (std~ (std-def x)) (if (listp x) x `((,x)))))))

(defun std^ (x y)
  (cond ((eq (std-def x) (std-def y)) (rd^ x y))
	((s-class-p x) (when (std^ x (std-def y)) y))
	((s-class-p y) (std^ y x))))

(defun std-load (ntp x)
  (ntp-ld ntp (cons (pop x) (or (si-subclasses (car x)) '(t)))))

(defun std-recon (x &optional (c (pop x) cp))
  (cond 
    ((not cp) (?or (mapcar (lambda (x) (std-recon x c)) x)))
    ((eq x t) c)
    ((s-class-p x) (if (listp x) (cons 'not x) x))
    ((listp x) `(and ,(std-def x) (not (member ,@x))))
    (`(member ,x))))


;;; INDIVIDUALS


(defun kktype-of (x)
  (cond ((atom x) x)
	((eq (car x) 'array) (cadr (assoc (cadr x) +atps+)))
	((eq (car x) 'complex) (cadr (assoc (caadr x) +ctps+)))
	((car x))))

(defun ktype-of (x)
  (cond ((typep x 'structure) 'structure);FIXME
	((typep x 'std-instance) 'std-instance);FIXME
	((improper-consp x) 'improper-cons);FIXME
	((consp x) 'proper-cons);FIXME
	((kktype-of (type-of x)))
	((error "unknown type"))))

(defun member-reduce (type) 
  (lreduce (lambda (y x &aux (k (ktype-of x)) (z (assoc k y)))
	     (if z (progn (nconc z (list x)) y)
		 (nconc y `((,k ,x)))))
	   (cdr type) :initial-value nil))
  

(defun cons-to-cns-list (x)
  (cns-list (nprocess-type `(member ,(car x)))
	    (nprocess-type `(member ,(cdr x)))
	    x))

#.`(defun member-load (ntp type)
     (mapc (lambda (x)
	     (case (car x) 
	       ((proper-cons improper-cons) (ntp-ld ntp (cons (pop x) (mapcan 'cons-to-cns-list x))))
	       (,+range-types+	(ntp-ld ntp (cons (pop x) (mapcar (lambda (x) (cons x x)) x))))
	       (,(mapcar 'cadr +ctps+)
		(ntp-ld ntp (cons (pop x)
				  (mapcar (lambda (x &aux (r (realpart x))(i (imagpart x)))
					    `(((,r . ,r)) . ((,i . ,i)))) x))))
	       (otherwise (ntp-ld ntp (substitute t nil x)))))
	   (member-reduce type)))





(defun f^ (x y ^)
  (cond ((eq x t) y)
	((eq y t) x)
	((funcall ^ x y))))

(defun f~ (x ~)
  (cond ((eq x t) nil)
	((funcall ~ x))))

(defun op-and (x y ^)
  (lreduce (lambda (xx x)
	     (nconc xx
		    (lreduce (lambda (yy y &aux (z (f^ x y ^)))
			       (if z (nconc yy (list z)) yy)) y :initial-value nil)))
	   x :initial-value nil))
  
(defun op-not (x ^ ~)
  (lreduce (lambda (xx x)
	     (cond ((eq (car xx) t) (f~ x ~))
		   (xx (op-and (f~ x ~) xx ^)))) x :initial-value '(t)))

(defun op-or (x y ^ ~)
  (op-not (op-not (append x y) ^ ~) ^ ~))


(defun kop-and (k x y)
  (lreduce (lambda (xx x)
	     (nconc xx
		    (lreduce (lambda (yy y &aux (z (k^ k x y)))
			       (if z (nconc yy (list z)) yy)) y :initial-value nil)))
	   x :initial-value nil))

(defun kop-not (k x)
  (lreduce (lambda (xx x)
	     (cond ((eq (car xx) t) (k~ k x))
		   (xx (kop-and k (k~ k x) xx)))) x :initial-value '(t)))

(defun kop-or (k x y)
  (kop-not k (kop-not k (append x y))))


(defun k-op (op x y &aux (k (car x))(n (cdr (assoc k *kingdom-ops*)))(^ (pop n)) (~ (car n)))
  (cons k
	(case op
	  (and (op-and (cdr x) (cdr y) ^))
	  (or (op-or (cdr x) (cdr y) ^ ~))
	  (not (op-not (cdr x) ^ ~)))))


(defun k-op (op x y &aux (k (car x)))
  (cons k
	(case op
	  (and (kop-and k (cdr x) (cdr y)))
	  (or (kop-or k (cdr x) (cdr y)))
	  (not (kop-not k (cdr x))))))

(defun ntp-not (x &aux (z (make-ntp)))
  (mapc (lambda (l) (ntp-ld z (k-op 'not l nil))) (car x))
  (setf (car (cdr z)) (negate (cadr x)))
  (ntp-clean z)
  z)

(defun ntp-and (&rest xy &aux (z (make-ntp)))
  (when xy
    (let ((x (tp-mod (car xy) t)) (y (tp-mod (cadr xy) nil)))
      (mapc (lambda (l &aux (ny (assoc (car l) (car y))))
	      (ntp-ld z (cond (ny (k-op 'and l ny))((cadr y) l)))) (car x))
      (when (cadr x)
	(mapc (lambda (l)
		(unless (assoc (car l) (car x))
		  (ntp-ld z l))) (car y)))
      (setf (car (cdr z)) (and (cadr x) (cadr y)))
      (setf (car (cddr z)) (or  (caddr x) (caddr y)))
      (ntp-clean z)
      z)))

(defun ntp-or (&rest xy &aux (z (make-ntp)))
  (when xy
    (let ((x (tp-mod (car xy) t)) (y (tp-mod (cadr xy) nil)))
      (mapc (lambda (l &aux (ny (assoc (car l) (car y))))
	      (ntp-ld z (cond (ny (k-op 'or l ny))((cadr y) `(,(car l) t))(l)))) (car x))
      (unless (cadr x)
	(mapc (lambda (l)
		(unless (assoc (car l) (car x))
		  (ntp-ld z l))) (car y)))
      (setf (car (cdr z)) (or (cadr x) (cadr y)))
      (setf (car (cddr z)) (or (caddr x) (caddr y)))
      (ntp-clean z)
      z)))


(defconstant +ntypes+ `(,@+singleton-types+
			std-instance structure standard-generic-interpreted-function
			standard-generic-compiled-function
			t nil proper-cons))

(defun normalize-type (type &aux e
			      (type (if (listp type) type (list type)));FIXME
			      (ctp (car type)))

  (cond ((eq ctp 'structure-object) `(structure));FIXME
	((member ctp +ntypes+) type)
	((eq type (setq e (expand-deftype type))) type)
	((normalize-type e))))


#.`(defun ntp-load (type)
     (let ((ntp (make-ntp)))
       (case (car type)
	 ((t) (setq ntp (ntp-not ntp)))
	 ((nil))
	 (proper-cons (ntp-ld ntp `(proper-cons t)));(pcns-load ntp type));(list 'cons t)))
	 (,+range-types+ (rng-load ntp type))
	 (complex (cmp-load ntp type))
	 (cons (cns-load ntp type))
	 ((std-instance
	   standard-generic-interpreted-function
	   standard-generic-compiled-function)
	  (std-load ntp type))
	 (structure (str-load ntp type))
	 (array (ar-load ntp type))
	 (,+singleton-types+ (sing-load ntp type))
	 (member (member-load ntp type))
	 (otherwise (setq ntp (ntp-not ntp)) (setf (car (cddr ntp)) t)))
       ntp))


(defun nprocess-type (type)
  (case (car type)
    (type-min (let ((*tp-mod* -1)) (nprocess-type (cadr type))))
    (type-max (let ((*tp-mod*  1)) (nprocess-type (cadr type))))
    (and (lreduce 'ntp-and (mapcar 'nprocess-type (cdr type))))
    (or (lreduce  'ntp-or  (mapcar 'nprocess-type (cdr type))))
    (not (ntp-not (nprocess-type (cadr type))))
    (otherwise (ntp-load type))))

(defun is-everything (z)
  (not (member-if-not
	(lambda (x) (eq (cadr (assoc (car x) (car z))) t))
	*kingdom-ops*)))

(defun prune-type (z q i w) ;FIXME optional tail recursion
  (declare (seqind i))
  (cond ((= i (length +array-type-alist+))
	 (setf (car (cdar q)) '*)
	 (ldelete-if (lambda (y) (unless (eq y (car q)) (and (consp y) (eq (car y) 'array)))) z))
	((not w) z)
	((or (atom (car w)) (not (eq (caar w) 'array))) (prune-type z q i (cdr w)))
	((not q) (prune-type z w (1+ i) (cdr w)))
	((equal (caddar w) (caddar q)) (prune-type z q (1+ i) (cdr w)))
	(z)))

(defun and-or-flatten (tp &aux (ctp (when (listp tp) (car tp))))
  (if (member ctp '(and or))
      (let ((x (mapcan (lambda (x &aux (x (and-or-flatten x)))
			 (cond ((when (listp x) (eq (car x) ctp)) (cdr x))
			       ((eq x (eq ctp 'and)) nil)
			       ((list x)))) (cdr tp))))
	(cond ((not x) (when (eq ctp 'and) '(t))) ((not (cdr x)) (car x)) ((cons ctp x))))
      tp))

(defun nreconstruct-type-int (x)
  (cond ((caddr x) t)
	((cadr x)
	 (let* ((x (ntp-not x)))
	   (unless (is-everything x)
	     (let ((z (nreconstruct-type-int x)))
	       (or (not z) `(not ,z))))))
	((let* ((z (mapcar (lambda (x) (funcall (fourth (assoc (car x) *kingdom-ops*)) x)) (car x)))
		(z (prune-type z nil 0 z)))
 	(and-or-flatten (if (cdr z) `(or ,@z) (car z)))))))

(defun nreconstruct-type (x)
  (list (nreconstruct-type-int x) (caddr x)))


(defun resolve-type (type)
  (nreconstruct-type (nprocess-type (normalize-type type))))

(defun subtypep (t1 t2 &optional env)
  (declare (ignore env) (optimize (safety 2)))
  (check-type t1 full-type-spec)
  (check-type t2 full-type-spec)
  (if (or (not t1) (eq t2 t))
      (values t t)
    (let* ((rt (resolve-type `(and ,t1 ,(negate t2))))
	   (mt (when (cadr rt) (resolve-type `(and (type-max ,t1) ,(negate `(type-min ,t2))))))
	   (rt (if (or (not (cadr rt)) (car mt)) rt `(nil nil))))
      (values (not (car rt))  (not (cadr rt))))))
