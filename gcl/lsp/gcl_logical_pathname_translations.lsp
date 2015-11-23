(in-package :si)

(defvar *pathname-logical* nil)

(defun host-key (k) (if (stringp k) (string-right-trim ":" (string-upcase k)) k))

(defun setf-logical-pathname-translations (v k &aux (k (host-key k)))
  (setf (cdr (or (assoc k *pathname-logical* :test 'equal) (car (push (cons k t) *pathname-logical*)))) ;(cons k nil)
	(if (listp v) (mapcar (lambda (x) (list (parse-namestring (car x) k) (parse-namestring (cadr x)))) v) v)))

(defsetf logical-pathname-translations (x) (y) `(setf-logical-pathname-translations ,y ,x))

(defun logical-pathname-translations (k)
  (cdr (assoc (host-key k) *pathname-logical* :test 'equal)))

(remprop 'logical-pathname-translations 'si::setf-update-fn)

