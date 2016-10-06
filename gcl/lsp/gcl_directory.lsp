(in-package :si)

(defconstant +d-type-alist+ (d-type-list))

(defun ?push (x tp)
  (when (and x (eq tp :directory) (vector-push-extend #\/ x)))
  x)

(defun wreaddir (x s &optional y (ls (length s) lsp) &aux (y (if (rassoc y +d-type-alist+) y :unknown)))
  (when lsp (setf (fill-pointer s) ls))
  (let ((r (readdir x (car (rassoc y +d-type-alist+)) s)))
    (typecase r
      (fixnum (wreaddir x (adjust-array s (+ 100 (ash (array-dimension s 0) 1))) y))
      (cons (let ((tp (cdr (assoc (cdr r) +d-type-alist+)))) (cons (?push (car r) tp) tp)))
      (otherwise (?push r y)))))

(defun dot-dir-p (r l) (member-if (lambda (x) (string= x r :start2 l)) '("./" "../")))

(defun vector-push-string (x s &optional (ss 0) (lx (length x)) &aux (ls (- (length s) ss)))
  (let ((x (if (> ls (- (array-dimension x 0) lx)) (adjust-array x (+ ls (ash lx 1))) x)))
    (setf (fill-pointer x) (+ lx ls))
    (replace x s :start1 lx :start2 ss)))

(defun walk-dir (s e f &optional (y :unknown) (d (opendir s)) (l (length s)) (le (length e))
		   &aux (r (wreaddir d s y l)))
  (cond (r (unless (dot-dir-p r l) (funcall f r (vector-push-string e r l le) l))
	   (walk-dir s e f y d l le))
	((setf (fill-pointer s) l (fill-pointer e) le) (closedir d))))

(defun recurse-dir (x y f)
  (funcall f x y)
  (walk-dir x y (lambda (x y l) (declare (ignore l)) (recurse-dir x y f)) :directory))

(defun make-frame (s &aux (l (length s)))
  (replace (make-array l :element-type 'character :adjustable t :fill-pointer l) s))

(defun expand-wild-directory (l f zz &optional (yy (make-frame zz)))
  (let* ((x (member-if 'wild-dir-element-p l))
	 (s (namestring (make-pathname :directory (ldiff l x))))
	 (z (vector-push-string zz s))
	 (l (length yy))
	 (y (link-expand (vector-push-string yy s) l))
	 (y (if (eq y yy) y (make-frame y))))
    (when (or (eq (stat z) :directory) (zerop (length z)))
      (cond ((eq (car x) :wild-inferiors) (recurse-dir z y f))
	    (x (walk-dir z y (lambda (q e l)
			       (declare (ignore l))
			       (expand-wild-directory (cons :relative (cdr x)) f q e)) :directory));FIXME
	    ((funcall f z y))))))

(defun directory (p &key &aux (p (translate-logical-pathname p))(d (pathname-directory p))
		    (c (unless (eq (car d) :absolute) (make-frame (concatenate 'string (getcwd) "/"))))
		    (lc (when c (length c)))
		    (filesp (or (pathname-name p) (pathname-type p)))
		    (v (compile-regexp (to-regexp p)))(*up-key* :back) r)
  (expand-wild-directory d
   (lambda (dir exp &aux (pexp (pathname (if c (vector-push-string c exp 0 lc) exp))))
     (if filesp
	 (walk-dir dir exp
		   (lambda (dir exp pos)
		     (declare (ignore exp))
		     (when (pathname-match-p dir v)
		       (push (merge-pathnames (parse-namestring dir nil *default-pathname-defaults* :start pos) pexp nil) r)))
		   :file)
       (when (pathname-match-p dir v) (push pexp r))))
   (make-frame (if c "./" "")))
  r)
