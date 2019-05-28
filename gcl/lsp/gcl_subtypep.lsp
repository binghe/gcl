(in-package :si)

(eval-when (compile eval load)

(defvar *array-types* (cons (cons nil 'array-nil) +array-type-alist+))

(defvar *k-ops*
  `((integer int^ int~ rng-recon)
    ((ratio short-float long-float) rng^ rng~ rng-recon)
    (complex-integer cmpi^ cmpi~ cmp-recon)
    (complex-ratio   cmp^  cmpr~ cmp-recon)
    ((complex-short-float complex-long-float) cmp^ cmp~ cmp-recon)
    (structure str^ str~ str-recon)
    ((std-instance
      standard-generic-compiled-function
      standard-generic-interpreted-function) std^ std~ std-recon)
    ((proper-cons improper-cons) cns^ cns~ cns-recon)
    (,(mapcar 'cdr *array-types*)  ar^ ar~ ar-recon)
    (,+singleton-types+  sing^ sing~ sing-recon))))


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

(defun ar-ld (type)
  `(,(cdr (assoc (cadr type) *array-types*))
     ,(let ((x (caddr type)))
	(cond ((eq x '*) t)
	      ((integerp x) (cons 'rank x))
	      (x)))))

;;; SINGETON



(defun sing^ (x y) (rd^ y x))

(defun sing~ (x)
  (cond ((listp x) x)
	((list (list x)))))

(defun sing-ld (type) (cons (car type) '(t)))

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

(defun rng-ld (type) (list (pop type) (range-cons type)))

(defun rng-recon (x &aux (c (pop x)))
  (if (eq (car x) t)
      (list c '* '*)
      (?or (mapcar (lambda (x) (list c (car x) (cdr x))) x))))
  

;;; COMPLEX


(defun cmp-ld (type &aux (rtp (cadr type)) (o (eq (car rtp) 'or)))
  `(,(cadr (assoc (if o (caadr rtp) (car rtp)) +ctps+))
     ,(let ((x (mapcar (lambda (x) (range-cons (cdr x))) (if o (cdr rtp) (list rtp)))))
	(or (when (listp x) (eq (car x) t)) (cons x x)))))

(defun cmp-cons (a d) (when (and a d) (list (cons a d))))

(defun cmpi~ (x);complex integer
  (let ((a (kop-not 'integer (car x)))(d (kop-not 'integer (cdr x))))
    (nconc (cmp-cons (car x) d) (cmp-cons a (cdr x)) (cmp-cons a d))))

(defun cmpi^ (x y);complex integer
  (car (cmp-cons (kop-and 'integer (car x) (car y))
		 (kop-and 'integer (cdr x) (cdr y)))))

(defun rati (x)
  (if (listp x) (car x) (list x)))

(defun rat~ (x)
  (cond ((eq (car x) '*) (list (cons (rati (cdr x)) '*)))
	((eq (cdr x) '*) (list (cons '* (rati (car x)))))
	((nconc (rat~ (cons (car x) '*)) (rat~ (cons '* (cdr x)))))))


(defun cmpr~ (x);complex ratio
  (let ((a (kop-not 'rational (car x)))(d (kop-not 'rational (cdr x))))
    (nconc (cmp-cons (car x) d) (cmp-cons a (cdr x)) (cmp-cons a d))))

(defun cmp^ (x y)
  (car (cmp-cons (kop-and 'ratio (car x) (car y))
		 (kop-and 'ratio (cdr x) (cdr y)))))

(defun cmp~ (x)
  (let ((a (kop-not 'ratio (car x)))(d (kop-not 'ratio (cdr x))))
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
  (let ((a (ntp-and (car x) (car y)))
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
		((car (cns-list a d)))))))))

(defun cns~ (x)
  (cond ((let ((a (ntp-not (car x)))
	       (d (ntp-not (cadr x))))
	   (nconc (cns-list a (cadr x))
		  (cns-list (car x) d)
		  (cns-list a d)
		  (when (caddr x)
		    (cns-list (car x) (cadr x) nil (list (caddr x))))
		  (mapcan (lambda (y) (cns-list (car x) (cadr x) y)) (cadddr x)))))))

(defvar *pcnsk* '(proper-cons null))
(defvar *pcns-ntp* (list (mapcar (lambda (x) (list x t)) *pcnsk*) nil nil))
(defvar *pcns-nntp* (list (mapcar (lambda (x) (list x t))
				  (set-difference
				   (lreduce (lambda (xx x &aux (x (car x)))
					      (if (listp x) (append x xx) (cons x xx)))
					    *k-ops* :initial-value nil)
				   *pcnsk*))
			  nil nil))

(defun pcdr (u type &aux (z (ntp-and u (nprocess-type type))))
  (ntp-prune (car z) (cadr z) (caddr z) (length (car u))))

(defun pcns (u type)
  (k-mk (pop type) nil (cns-list (nprocess-type (pop type)) (pcdr u (car type)))))

(defun cns-ld (type)
  (pcns (if (eq (car type) 'proper-cons) *pcns-ntp* *pcns-nntp*) type))


(defun cns-recon (x &optional (c (pop x) cp))
  (cond ((not cp) (?or (mapcar (lambda (x) (cns-recon x c)) x)))
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

(defun str-ld (x)
  (cons (pop x)
	(or (mapcar (lambda (x) (get x 's-data)) (when x (get-included (car x))))
	    '(t))))

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

(defun std-ld (x)
  (cons (pop x) (or (si-subclasses (car x)) '(t))))

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
  

(defun cons-to-cns-list (u x)
  (cns-list (nprocess-type `(member ,(car x))) (pcdr u `(member ,(cdr x))) x))

(defun mcns-ld (x &aux (c (pop x))(u (if (eq c 'proper-cons) *pcns-ntp* *pcns-nntp*)))
  (cons c (mapcan (lambda (x) (cons-to-cns-list u x)) x)))

#.`(defun member-ld (type)
     (mapcan (lambda (x)
	     (?cns
	      (case (car x)
		((proper-cons improper-cons) (mcns-ld x))
		(,+range-types+	(cons (pop x) (mapcar (lambda (x) (cons x x)) x)))
		(,(mapcar 'cadr +ctps+)
		 (cons (pop x)
		       (mapcar (lambda (x &aux (r (realpart x))(i (imagpart x)))
				 `(((,r . ,r)) . ((,i . ,i)))) x)))
		(otherwise (substitute t nil x)))
	      nil))
	     (member-reduce type)))


#.`(defun k^ (k x y)
     (case k
       (rational (rng^ x y));FIXME ?
       ,@(mapcar (lambda (x) `(,(car x) (,(cadr x) x y))) (butlast *k-ops*))
       (otherwise (,(cadr (car (last *k-ops*))) x y))))

(defun kop-and (k x y)
  (cond ((eq (car x) t) y)
	((eq (car y) t) x)
	((lreduce (lambda (xx x)
	     (lreduce (lambda (yy y) (?cns (k^ k x y) yy))
		      y :initial-value xx))
		  x :initial-value nil))))


#.`(defun k~ (k x)
     (unless (eq x t)
       (case k
	 (rational (rat~ x));FIXME?
	 ,@(mapcar (lambda (x) `(,(car x) (,(caddr x) x))) (butlast *k-ops*))
	 (otherwise (,(caddr (car (last *k-ops*))) x)))))

(defun kop-not (k x)
  (lreduce (lambda (xx x) (when xx (kop-and k (k~ k x) xx))) x :initial-value '(t)))

(defun kop-or (k x y)
  (kop-not k (kop-not k (append x y))))

(defun k-mk (k d x)
  (unless (eq d (car x))
    (cons k x)))

(defun k-op (op x y d &aux (k (car x)))
  (k-mk
   k d
   (case op
     (and (kop-and k (cdr x) (cdr y)))
     (or  (kop-or  k (cdr x) (cdr y)))
     (not (kop-not k (cdr x))))))


(defun ntp-prune (x y z &rest u)
  (cond
    ((not (or x z u)) (if y +tp-t+ +tp-nil+))
    ((unless (member (not y) x :test-not 'eq :key 'cadr)
       (eql (length x) (or (car u)
			   #.(lreduce (lambda (xx x &aux (x (car x)))
					(+ (if (listp x) (length x) 1) xx))
				      *k-ops* :initial-value 0))))
     (apply 'ntp-prune nil (not y) z u))
    ((list* x y z u))))

(defun ?cns (x y) (if x (cons x y) y))

(defun ntp-not (x &aux (d (not (cadr x))))
  (apply 'ntp-prune
   (lreduce (lambda (ll l) (?cns (k-op 'not l nil d) ll)) (car x) :initial-value nil)
   d
   (cddr x)))

(defun tp-mod (x d)
  (cond ((not (caddr x)) x)
	((/= (let ((x *tp-mod*)) (if (>= x 0) x (- x))) 1) x)
	((= *tp-mod* 1) +tp-t+)
	(+tp-nil+)))

(defun ntp-and (&rest xy)
  (when xy
    (let* ((x (tp-mod (car xy) t)) (y (tp-mod (cadr xy) nil))
	   (lx (car x))(ly (car y))(dx (cadr x))(dy (cadr y))(d (and dx dy)))
      (cond ((not lx) (if dx y x))
	    ((not ly) (if dy x y))
	    ((apply 'ntp-prune
	      (lreduce (lambda (ll l &aux (ny (assoc (car l) ly)))
			 (?cns (cond (ny (k-op 'and l ny d)) (dy l)) ll))
		       lx :initial-value
		       (when dx
			 (lreduce (lambda (ll l) (?cns (unless (assoc (car l) lx) l) ll))
				  ly :initial-value nil)))
	      d (or (caddr x) (caddr y)) (cdddr x)))))))


(defun ntp-or (&rest xy)
  (when xy
    (let* ((x (tp-mod (car xy) t)) (y (tp-mod (cadr xy) nil))
	   (lx (car x))(ly (car y))(dx (cadr x))(dy (cadr y))(d (or dx dy)))
      (cond ((not lx) (if dx x y))
	    ((not ly) (if dy y x))
	    ((apply 'ntp-prune
	      (lreduce (lambda (ll l &aux (ny (assoc (car l) ly)))
			 (?cns (cond (ny (k-op 'or l ny d)) ((not dy) l)) ll))
		       lx :initial-value
		       (unless dx
			 (lreduce (lambda (ll l) (?cns (unless (assoc (car l) lx) l) ll))
				  ly :initial-value nil)))
	      d (or (caddr x) (caddr y)) (cdddr x)))))))

(defconstant +ntypes+ `(,@+singleton-types+
			std-instance structure standard-generic-interpreted-function
			standard-generic-compiled-function
			t nil))

(defun normalize-type (type &aux e
			      (type (if (listp type) type (list type)));FIXME
			      (ctp (car type)))
  (cond ((eq ctp 'structure-object) `(structure));FIXME
	((member ctp +ntypes+) type)
	((eq type (setq e (just-expand-deftype type))) type)
	((normalize-type e))))

(defun mntp (x)
  (case x
    ((t) +tp-t+)
    ((nil) +tp-nil+)
    (unknown (list nil t t))
    (otherwise (list (if (consp (car x)) x (list x)) nil nil))))

#.`(defun ntp-load (type)
     (mntp
      (case (car type)
	 ((t nil) (car type))
	 (,+range-types+ (rng-ld type))
	 (complex (cmp-ld type))
	 ((cons proper-cons improper-cons) (cns-ld type))
	 ((std-instance
	   standard-generic-interpreted-function
	   standard-generic-compiled-function)
	  (std-ld type))
	 (structure (str-ld type))
	 (array (ar-ld type))
	 (,+singleton-types+ (sing-ld type))
	 (member (member-ld type))
	 (otherwise 'unknown))))

(defun nprocess-type (type)
  (case (car type)
    (type-min (let ((*tp-mod* -1)) (nprocess-type (cadr type))))
    (type-max (let ((*tp-mod*  1)) (nprocess-type (cadr type))))
    (and (lreduce 'ntp-and (mapcar 'nprocess-type (cdr type))))
    (or (lreduce  'ntp-or  (mapcar 'nprocess-type (cdr type))))
    (not (ntp-not (nprocess-type (cadr type))))
    (otherwise (ntp-load type))))

#.`(defun k-recon (x)
     (case (car x)
       ,@(mapcar (lambda (x) `(,(car x) (,(cadddr x) x))) (butlast *k-ops*))
       (otherwise (,(cadddr (car (last *k-ops*))) x))))

(defun nreconstruct-type-int (x)
  (cond ((caddr x) t)
	((cadr x)
	 (let* ((x (ntp-not x)))
	   (let ((z (nreconstruct-type-int x)))
	     (or (not z) `(not ,z)))))
	((?or (mapcar 'k-recon (car x))))))

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


