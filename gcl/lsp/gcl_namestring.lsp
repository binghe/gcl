(in-package :si)

(defun msub (a x) (if a (msub (cdr a) (substitute (caar a) (cdar a) x)) x))

(defvar *glob-to-regexp-alist* (list (cons #v"{[^}]*}" (lambda (x) (msub '((#\| . #\,)(#\( . #\{)(#\) . #\})) x)))
				     (cons #v"\\[[^\\]*\\]" (lambda (x)
							      (concatenate 'string "("
									   (substitute #\^ #\! (subseq x 0 2))
									   (subseq x 2) ")")))
					;				   (cons #v"^\\*\\*/" (lambda (x) "([^/].*/)?"))
					;				   (cons #v"^/\\*\\*/" (lambda (x) "/?(.*/)?"))
					;				   (cons #v"\\*\\*/" (lambda (x) "(.*/)?"))
				     (cons #v"\\*" (lambda (x) "([^/.]*)"))
				     (cons #v"\\?" (lambda (x) "([^/.])"))
				     (cons #v"\\." (lambda (x) "\\."))))

(defun mglist (x &optional (b 0))
  (let* ((y (mapcan (lambda (z &aux (w (string-match (car z) x b)))
		      (unless (eql w -1)
			(list (list w (match-end 0) z))))
		    *glob-to-regexp-alist*))
	 (z (when y (reduce (lambda (y x) (if (< (car x) (car y)) x y)) y))))
    (when z
      (cons z (mglist x (cadr z))))))

(defun mgsub (x &optional (l (mglist x)) (b 0) &aux (w (pop l)))
  (if w
      (concatenate 'string
		   (subseq x b (car w))
		   (funcall (cdaddr w) (subseq x (car w) (cadr w)))
		   (mgsub x l (cadr w)))
    (subseq x b)))


(defun lpsub (x lp) (if lp (substitute #\; #\/ x) x))

(defun elsub1 (el x rp lp &aux (pref (car x))(dflt (cadr x))(post (caddr x)))
  (etypecase el
    (string (let ((x (list pref el post))) (unless (zerop (length dflt)) (if rp (mapcar 'mgsub x) x))))
    ((integer 1) (elsub1 (if lp (write-to-string el) "") x rp lp))
    ((eql :wild-inferiors) (if rp (list "(.*/|)") (elsub1 "**" x rp lp)))
    ((eql :wild) (if rp (list dflt) (elsub1 "*" x rp lp)))
    ((eql :newest) (if rp (list dflt) (elsub1 (if lp "NEWEST" "") x rp lp)))
    ((member :up :back) (elsub1 ".." x rp lp))
    ((member nil :unspecific) (list (if rp dflt "")))
    (cons (cons
	   (if (eq (car el) :absolute) (if lp "" "/") (if lp ";" ""));(if (and rp (eq (cadr el) :wild-inferiors)) "/?" (if lp "" "/"))
	   (mapcan (lambda (x) (elsub1 x '("" "([^/]*/|)" "/") rp lp)) (cdr el))))))

;(c-set-symbol-stype '+pp-defs+ 0)
;(c-set-symbol-stype '+lp-defs+ 0)
(defconstant +physical-pathname-defaults+ '(("" "" "")("" "" "")("" "(.*/|)" "")("" "([^/.]*)" "")("." "\\.?([^/]*)" "")("" "" "")))
(defconstant +logical-pathname-defaults+  '(("" "([^:]+:|)" ":")("" "" "")("" "(.*;|)" "")("" "([^;.]*)" "")("." "\\.?([^;.]*)" "")("." "\\.?([1-9][0-9]*|NEWEST|\\*|)" "")))
;(defconstant +lp-defs+ (cons '("" "(.+:|)" ":") (nconc (butlast (cdr +pp-defs+)) '(("." "(\\.[1-9][0-9]*|\\.NEWEST|\\.*|)" "")))))

(defun to-regexp1 (x rp lp)
  (lpsub (apply 'concatenate 'string
		(cons (if rp "^" "")
		      (nconc (mapcan (lambda (x y) (elsub1 x y rp lp))
				     x (if lp +logical-pathname-defaults+ +physical-pathname-defaults+))
			     (list (if rp "$" ""))))) lp))

(defconstant +generic-logical-pathname-regexp+ (compile-regexp (to-regexp1 (make-list (length +logical-pathname-defaults+)) t t)))
(defconstant +generic-physical-pathname-regexp+ (compile-regexp (to-regexp1 (make-list (length +physical-pathname-defaults+)) t nil)))
(defun to-regexp (x &optional (rp t) &aux (px (pathname x))(lp (typep px 'logical-pathname)))
  (to-regexp1 (mlp px) rp lp))

(defun namestring (x)
  (etypecase x
    (string x)
    (pathname (or (c-pathname-namestring x) (to-regexp x nil)))
    (stream (namestring (c-stream-object1 x)))))
