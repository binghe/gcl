(in-package :si)

(defconstant +d-type-alist+ (d-type-list))


(defun ?push (x tp)
  (when (and x (eq tp :directory) (vector-push-extend #\/ x)))
  x)

(defun wreaddir3 (x s &optional y &aux (y (if (rassoc y +d-type-alist+) y :unknown)))
  (let ((r (readdir3 x (car (rassoc y +d-type-alist+)) s)))
    (typecase r
      (fixnum (wreaddir3 x (adjust-array s (+ 100 (ash (array-dimension s 0) 1))) y))
      (cons (let ((tp (cdr (assoc (cdr r) +d-type-alist+)))) (cons (?push (car r) tp) tp)))
      (otherwise (?push r y)))))


(defun dot-dir-p (r l &aux (z (length r)))
  (cond ((eql z (+ l 2)) (when (eql (aref r l) #\.) (eql (aref r (1+ l)) #\/)))
	((eql z (+ l 3)) (when (eql (aref r l) #\.) (when (eql (aref r (1+ l)) #\.) (eql (aref r (+ l 2)) #\/))))))

(defun walk-dir (s f &optional (y :unknown) (d (opendir s)) (l (length s)) &aux (r (wreaddir3 d s y)))
  (cond (r (unless (dot-dir-p r l) (funcall f r)) (walk-dir s f y d (setf (fill-pointer s) l)))
	((closedir d))))

(defun wild-dir-element-p (x)
  (or (eq x :wild) (eq x :wild-inferiors)
      (when (stringp x) (not (eql -1 (string-match #v"(\\*|\\?|\\[|\\{)" x))))))

(defun recurse-dir (x f)
  (funcall f x)
  (walk-dir x (lambda (x) (recurse-dir x f)) :directory))

(defun vector-push-string (x s &aux (lx (length x))(ls (length s))
			     (x (if (> ls (- (array-dimension x 0) lx)) (adjust-array x (+ ls (ash lx 1))) x)))
  (setf (fill-pointer x) (+ lx ls))
  (replace x s :start1 lx))

(defun expand-wild-directory (l f &optional (zz (make-frame "")))
  (let* ((x (member-if 'wild-dir-element-p l))
	 (z (vector-push-string zz (namestring (make-pathname :directory (ldiff l x))))))
    (cond ((eq (car x) :wild-inferiors) (recurse-dir z f))
	  (x (walk-dir z (lambda (q) (expand-wild-directory (cons :relative (cdr x)) f q)) :directory));FIXME
	  ((funcall f z)))))

(defun make-frame (s &aux (l (length s)))
  (make-array (ash l 1) :element-type 'character :adjustable t :fill-pointer l :initial-contents (coerce s 'list)))

(defun expand-path (p &aux (v (compile-regexp (to-regexp p)))(filesp (or (pathname-name p) (pathname-type p))) r);;version?
  (expand-wild-directory
   (pathname-directory p)
   (lambda (x) (if filesp (walk-dir x (lambda (y) (when (pathname-match-p y v) (push (truename (copy-seq y)) r))) :file)
		 (when (pathname-match-p x p) (push (truename (copy-seq x)) r)))))
  r)

(defun directory (p &key &aux (p (merge-pathnames (translate-logical-pathname p) (parse-namestring (concatenate 'string (getcwd) "/")) nil)))
  (expand-path p))
