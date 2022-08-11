(in-package :si)

;; (defun pathnamep (x)
;;   (declare (optimize (safety 1)))
;;   (when (typep x 'pathname) t))


(eval-when (compile eval)
  (defun add-dir-sep (s &optional (i 0) (bp 0) (l (length s)))
    (when (< i l)
      (let ((x (aref s i)))
	(append
	 (if (eql x #\/)
	     (if (zerop bp) (list #\[ x #\\ #\]) (list x #\\))
	   (list x))
	 (add-dir-sep s (1+ i) (case x (#\[ (1+ bp))(#\] (1- bp))(otherwise bp)) l)))))

  (defun ads (s) #+winnt (coerce (add-dir-sep s) 'string) #-winnt s))

(defconstant +dirsep+ (compile-regexp #.(ads "/")))

(defconstant +glob-to-regexp-alist+ (list (cons #v"{[^}]*}" (lambda (x) (msub '((#\| . #\,)(#\( . #\{)(#\) . #\})) x)))
					  (cons #v"\\[[^\\]*\\]"
						(lambda (x)
						  (string-concatenate "(" (substitute #\^ #\! (subseq x 0 2)) (subseq x 2) ")")))
					  (cons #v"\\*" (lambda (x) #.(ads "([^/.]*)")))
					  (cons #v"\\?" (lambda (x) #.(ads "([^/.])")))
					  (cons #v"\\." (lambda (x) "\\."))))

(defconstant +physical-pathname-defaults+ '(("" "" "")
					    #+winnt("" "([A-Za-z]:)?" ":") #-winnt("" "()" "")
					    ("" #.(ads "(/?([^/]*/)*)") "" "" #.(ads "([^/]*/)") "/")
					    ("" #.(ads "([^/.]*)") "")
					    ("." #.(ads "(\\.[^/]*)?") "")
					    ("" "" "")))

(defconstant +logical-pathname-defaults+  '(("" "([-0-9A-Z]+:)?" ":")
					    ("" "" "")
					    ("" "(;?((\\*?([-0-9A-Z]+\\*?)+|\\*|\\*\\*);)*)" "" "" "((\\*?([-0-9A-Z]+\\*?)+|\\*);)" ";")
					    ("" "(\\*?([-0-9A-Z]+\\*?)+|\\*)?" "")
					    ("." "(\\.(\\*?([-0-9A-Z]+\\*?)+|\\*))?" "")
					    ("." "(\\.([1-9][0-9]*|newest|NEWEST|\\*))?" "")))

(defun msub (a x) (if a (msub (cdr a) (substitute (caar a) (cdar a) x)) x))


(defun mglist (x &optional (b 0))
  (let* ((y (mapcan (lambda (z &aux (w (string-match (car z) x b)))
		      (unless (eql w -1)
			(list (list w (match-end 0) z))))
		    +glob-to-regexp-alist+))
	 (z (when y (reduce (lambda (y x) (if (< (car x) (car y)) x y)) y))))
    (when z
      (cons z (mglist x (cadr z))))))

(defun mgsub (x &optional (l (mglist x)) (b 0) &aux (w (pop l)))
  (if w
      (string-concatenate
		   (subseq x b (car w))
		   (funcall (cdaddr w) (subseq x (car w) (cadr w)))
		   (mgsub x l (cadr w)))
    (subseq x b)))


(defun elsub (el x rp lp &aux (y x) (pref (pop y))(dflt (pop y))(post (pop y)))
;  (destructuring-bind (pref dflt post &rest y) x
    (etypecase el
      (string (let ((x (list pref el post))) (unless (zerop (length dflt)) (if rp (mapcar 'mgsub x) x))))
      (integer (elsub (write-to-string el) x rp lp))
      ((eql :wild-inferiors) (if rp (list "(" dflt "*)") (elsub "**" x rp lp)))
      ((eql :wild) (if rp (list dflt) (elsub "*" x rp lp)))
      ((eql :newest) (elsub (if rp "(newest|NEWEST)" "NEWEST") x rp lp))
      ((member :up :back) (elsub ".." x rp lp))
      ((member nil :unspecific) (when rp (list dflt)))
      (cons (cons
	     (if (eq (car el) :absolute) (if lp "" "/") (if lp ";" ""))
	     (mapcan (lambda (z) (elsub z y rp lp)) (cdr el)))))
;    )
)



(defun to-regexp-or-namestring (x rp lp)
  (apply 'string-concatenate
	 (mapcan (lambda (x y) (elsub x y rp lp))
		 x (if lp +logical-pathname-defaults+ +physical-pathname-defaults+))))

(defun directory-list-check (l)
  (when (listp l)
    (when (member (car l) '(:absolute :relative))
      (mapl (lambda (x &aux (c (car x))(d (cadr x)))
	      (when (and (member d '(:up :back)) (member c '(:absolute :wild-inferiors)))
		(return-from directory-list-check nil))) l))))
    
(defun canonicalize-pathname-directory (l &aux x)
  (cond ((eq l :wild) (canonicalize-pathname-directory '(:absolute :wild-inferiors)))
	((stringp l) (canonicalize-pathname-directory (list :absolute l)))
	((and (eq (car l) :relative) (stringp (cadr l)) (plusp (length (cadr l))) (eql #\~ (aref (cadr l) 0)))
	 (canonicalize-pathname-directory (nconc (dir-parse (home-namestring (cadr l))) (cddr l))))
	((setq x (member-if (lambda (x) (or (string-equal "" x) (string-equal "." x))) l))
	 (canonicalize-pathname-directory (nconc (ldiff l x) (cdr x))))
	((setq x (member :back l))
	 (let* ((y (ldiff l x))(ll (car (last y))))
	   (canonicalize-pathname-directory (if (or (stringp ll) (eq ll :wild)) (nconc (butlast y) (cdr x)) (nconc y (cons :up (cdr x)))))))
	(l)))

(defvar *default-pathname-defaults* (init-pathname nil nil nil nil nil nil ""))
(declaim (type pathname *default-pathname-defaults*))

(defun toggle-case (x)
  (cond ((symbolp x) x)
	((listp x) (mapcar 'toggle-case x))
	((find-if 'upper-case-p x) (if (find-if 'lower-case-p x) x (string-downcase x)))
	((find-if 'lower-case-p x) (string-upcase x))
	(x)))

(defun logical-pathname (spec &aux (p (pathname spec)))
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (check-type p logical-pathname)
  p)
  
(eval-when (compile eval)
  (defun strsym (p &rest r)
    (declare (:dynamic-extent r))
    (intern (apply 'string-concatenate (mapcar 'string-upcase r)) p)))

#.`(defun make-pathname (&key (host nil hostp) (device nil devicep) (directory nil directoryp)
			      (name nil namep) (type nil typep) (version nil versionp)
			      defaults (case :local) namestring &aux defaulted (def (when defaults (pathname defaults))))
     (declare (optimize (safety 1)))
     (check-type host (or (member nil :unspecific) string))
     (check-type device (or (member nil :unspecific) string))
     (check-type directory (or (member nil :unspecific :wild) string list))
     (check-type name (or string (member nil :unspecific :wild)))
     (check-type type (or string (member nil :unspecific :wild)))
     (check-type version (or (integer 1) (member nil :unspecific :wild :newest)))
     (check-type defaults (or null pathname-designator))
     (check-type case (member :common :local))
     ,(flet ((def? (k) `(let* (,@(when (eq k 'host) `((def (or def *default-pathname-defaults*))))
			       (nk (if ,(strsym :si k "P") ,k (when def (,(strsym :si "C-PATHNAME-" k) def))))
			       (nk (unless (equal "" nk) nk))
			       (nk (progn (unless (eq ,k nk) (setq defaulted t)) nk))
			       (nk (if (eq case :local) nk (progn (setq defaulted t) (toggle-case nk)))))
			  nk)))
	    `(let* ((h ,(def? 'host))
		    (h (cond ((logical-pathname-host-p h) h)(h (setq defaulted t) nil)))
		    (dev ,(def? 'device))
		    (d ,(def? 'directory))
		    (d (let ((d1 (canonicalize-pathname-directory d))) (unless (eq d d1) (setq defaulted t)) d1))
		    (n ,(def? 'name))
		    (typ ,(def? 'type))
		    (v ,(def? 'version))
		    (p (init-pathname h dev d n typ v
				      (or (unless defaulted namestring) (to-regexp-or-namestring (list h dev d n typ v) nil h)))))
	       (when h (c-set-t-tt p 1))
	       (unless (eq d (directory-list-check d))
		 (error 'file-error :pathname p :format-control "Bad directory list"))
	       p)))

(macrolet ((pn-accessor (k &aux (f (strsym :si "PATHNAME-" k)) (c (strsym :si "C-PATHNAME-" k)))
	      `(defun ,f (p &key (case :local) &aux (pn (pathname p)))
		 (declare (optimize (safety 1)))
		 (check-type p pathname-designator)
		 (let ((x (,c pn))) (if (eq case :local) x (toggle-case x))))))
  (pn-accessor host)
  (pn-accessor device)
  (pn-accessor directory)
  (pn-accessor name)
  (pn-accessor type)
  (pn-accessor version))

(defconstant +pathname-keys+ '(:host :device :directory :name :type :version))

#.`(defun mlp (p)
     (list ,@(mapcar (lambda (x) `(,(strsym :si "C-PATHNAME-" x) p)) +pathname-keys+)))

(defun pnl1 (x) (list* (pop x) (pop x) (append (pop x) x)))
(defun lnp (x) (list* (pop x) (pop x) (let ((q (last x 3))) (cons (ldiff x q) q))))
