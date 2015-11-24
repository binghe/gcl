(in-package :si)

(defun sharp-p-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((x (parse-namestring (read stream)))) x))
;  (values (parse-namestring (read stream))));fixme values compiled

(set-dispatch-macro-character #\# #\p 'sharp-p-reader)

(defun dir-conj (x) (if (eq x :relative) :absolute :relative))

(defvar *up-key* :up)

(defun mfr (x b i) (subseq x b i));  (make-array (- i b) :element-type 'character :displaced-to x :displaced-index-offset b)

(defvar *sym-sub-alist* '((:host . nil)
			  (:device . nil)
			  (:directory . (("." . nil)(".." . :up)("*" . :wild)("**" . :wild-inferiors)))
			  (:name . (("*" . :wild)))
			  (:type . (("*" . :wild)))
			  (:version . (("*" . :wild)("NEWEST" . :newest)))))

(defun element (x b i key)
  (let* ((z (when (> i b) (mfr x b i)))
	 (w (assoc (string-upcase z) (cdr (assoc key *sym-sub-alist*))  :test 'string-equal)))
    (subst *up-key* :up (if w (cdr w) z))))

(defun dir-parse (x sep sepfirst &optional (b 0))
  (when (stringp x)
    (let ((i (or (search sep x :start2 b) -1)));string-match spoils outer match results
      (unless (eql i -1)
	(let* ((y (dir-parse x sep sepfirst (1+ i)))
	       (z (element x b i :directory))
	       (y (if z (cons z y) y)))
	  (if (zerop b)
	      (cons (if (zerop i) sepfirst (dir-conj sepfirst)) y)
	    y))))))

(defun match-component (x i k &optional (boff 0) (eoff 0))
  (element x (+ (match-beginning i) boff) (+ (match-end i) eoff) k))

(defun version-parse (x)
  (typecase x
    (string (version-parse (parse-integer x)))
    (integer (locally (check-type x (integer 1)) x))
    (otherwise x)))

(defun logical-pathname-parse (x host dhost &aux (x (string-upcase x)))
  (when (zerop (string-match +generic-logical-pathname-regexp+ x))
    (let ((mhost (match-component x 1 :host 0 -1)))
      (when (and host mhost)
	(unless (string= host mhost)
	    (error 'error :format-control "Host part of ~s does not match ~s" :format-arguments (list x host))))
      (let ((host (or host mhost dhost)))
	(when (logical-pathname-translations host)
	  (list (match-end 0)
		:device :unspecific
		:host host
		:directory (dir-parse (match-component x 2 :none) ";" :relative)
		:name (match-component x 3 :name)
		:type (match-component x 4 :type)
		:version (version-parse (match-component x 5 :version))))))))
  
(defun pathname-parse (x)
  (when (zerop (string-match +generic-physical-pathname-regexp+ x))
    (list (match-end 0)
	  :directory (dir-parse (match-component x 1 :none) "/" :absolute)
	  :name (match-component x 2 :name)
	  :type (match-component x 3 :type))))

(defun synonym-stream-symbol (x) (c-stream-object0 x))
(defun pathname-synonym-stream-p (x) (typep (synonym-stream-symbol x) 'pathname-designator))

(deftype pathname-designator nil '(or string pathname file-stream (and synonym-stream (satisfies pathname-synonym-stream-p))))

(deftype seqind nil `(integer 0))

(defun parse-namestring (thing &optional host (default-pathname *default-pathname-defaults*) &key (start 0) end junk-allowed)
  (declare (optimize (safety 1)))
  (check-type thing pathname-designator)
  (check-type host (or null (satisfies logical-pathname-translations)))
  (check-type default-pathname pathname-designator)
  (check-type start seqind)
  (check-type end (or null seqind))
  
  (typecase thing
    (string (let* ((l (logical-pathname-parse thing host (pathname-host default-pathname)))
		   (l (or l (pathname-parse thing)))
		   (e (pop l)))
	      (values (when l (apply 'make-pathname l)) e)))
    (stream (parse-namestring (c-stream-object1 thing)))
    (pathname
     (when host
       (unless (string= host (pathname-host thing))
	 (error 'file-error :pathname thing :format-control "Host does not match ~s" :format-arguments (list host))))
     (values thing start))))


(defun pathname (spec)
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (if (typep spec 'pathname) spec (values (parse-namestring spec))))

(defun logical-pathname-host-p (x) (logical-pathname-translations (pathname-host x)))

(defun logical-pathname (spec)
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (let* ((p (pathname spec)))
    (check-type p (satisfies logical-pathname-host-p))
    (c-set-t-tt p 1)))

  
