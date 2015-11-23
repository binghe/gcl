(in-package :si)

(deftype compiled-regexp nil `(vector unsigned-char))

(defun lenel (x)
  (case x (:wild 1)(:wild-inferiors 2)(:absolute 0)((:unspecific :relative nil :newest) -1)(otherwise (length x))))

(defun mme2 (s lel &optional (b 0) (i 1) r el
	       &aux (e (+ b (lenel (car lel))))(j (match-beginning i))(k (match-end i)))
  (cond
   ((unless (eql j -1) (eql j k)) (mme2 s lel b (1+ i) r el))
   ((< (1- b) j k (1+ e))
    (let ((z (subseq s j k))(r (if el r (cons nil r))))
      (mme2 s lel b (1+ i) (cons (cons z (car r)) (cdr r)) (or el (car lel)))))
   ((< (1- j) b e (1+ k))
    (let ((r (if el r (cons nil r))))
      (mme2 s (cdr lel) (1+ e) i (cons (cons (car lel) (car r)) (cdr r)) (or el (list (car lel))))))
   ((consp el)
    (let* ((cr (nreverse (car r))))
      (mme2 s lel b (1+ i) (cons (cons (car el) (list cr)) (cdr r)))))
   (el
    (let* ((cr (nreverse (car r))))
      (mme2 s (cdr lel) (1+ e) i (cons (cons el cr) (cdr r)))))
   (lel (mme2 s (cdr lel) (1+ e) i (cons (car lel) r)))
   ((nreverse r))))


(defconstant +pathname-keys+ '(:host :device :directory :name :type :version))

(defun do-repl (x y)
  (labels ((r (x l &optional (b 0) &aux (f (string-match #v"\\*" x b)))
	      (if (eql f -1) (if (eql b 0) x (subseq x b))
		(concatenate 'string (subseq x b f) (or (car l) "") (r x (cdr l) (1+ f))))))
    (r y x)))

(defun dir-p (x) (when (consp x) (member (car x) '(:absolute :relative))))

(defun source-portion (x y)
  (cond
   ((or (dir-p x) (dir-p y))
    (mapcan (lambda (z &aux (w (source-portion
				(if y (when (wild-dir-element-p z) (setf x (member-if 'listp x)) (pop x)) z)
				(when y z))))
   	      (if (listp w) w (list w))) (or y x)))
   ((if y (eq y :wild-inferiors) t) (if (listp x) (if (listp (cadr x)) (cadr x) (car x)) x));(or  y)
   ((eq y :wild) (if (listp x) (car x) x));(or  y)
   ((stringp y) (do-repl (when (listp x) (unless (listp (cadr x)) (cdr x))) y))
   (y)))

#.`(defun mlp (p &aux (p (pathname p)))
     (labels ((mrxp (x) (if (listp x) (mapcar #'mrxp x) x)))
       (list
	,@(mapcar (lambda (x &aux (y (intern (concatenate 'string "PATHNAME-" (string-upcase x)))))
		    `(mrxp (,y p))) +pathname-keys+))))

(defun pnl1 (x) (list* (pop x) (pop x) (append (pop x) x)))
(defun lnp (x) (list* (pop x) (pop x) (let ((q (last x 3))) (cons (ldiff x q) q))))


(defun pathname-match-p (p w)
  (zerop (string-match (etypecase w (compiled-regexp w)((or string pathname) (to-regexp w))) (namestring p))))


(defun mme3 (sx px)
  (lnp (mme2 sx (pnl1 (mlp px)))))

(defun list-toggle-case (x f)
  (typecase x
    (string (funcall f x))
    (cons (mapcar (lambda (x) (list-toggle-case x f)) x))
    (otherwise x)))

(defun translate-pathname (source from to &key
				  &aux (psource (pathname source))
				  (pto (pathname to))
				  (nsource (namestring source))
				  (case-toggle-p (when (typep psource 'logical-pathname) (not (typep pto 'logical-pathname))));FIXME
				  (match (pathname-match-p source from)))
  (declare (optimize (safety 1)))
  (check-type source pathname-designator)
  (check-type from pathname-designator)
  (check-type to pathname-designator)
  (check-type match (not null))
  (apply 'make-pathname :host (pathname-host pto)
	 (mapcan 'list +pathname-keys+
		 (mapcar 'source-portion
			 (list-toggle-case (mme3 nsource psource) (if case-toggle-p 'string-downcase 'identity))
			 (mlp pto)))))

(defun translate-logical-pathname (spec &key &aux (p (pathname spec)))
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (typecase p
    (logical-pathname
     (let ((rules (assoc p (logical-pathname-translations (pathname-host p)) :test 'pathname-match-p)))
       (unless rules
	 (error 'file-error :pathname p :format-control "No matching translations"))
       (translate-logical-pathname (apply 'translate-pathname p rules))))
    (otherwise p)))
    
