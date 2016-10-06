(in-package :si)

(defun link-expand (str &optional (b 0)	(n (length str)) fr)
  (labels ((frame (b e) (make-array (- n b) :element-type 'character
				    :displaced-to str :displaced-index-offset b :fill-pointer (- e b)))
	   (set-fr (fr e &aux (fr (or fr (frame 0 b)))) (setf (fill-pointer fr) e) fr))
    (let* ((i (string-match #v"/" str b))
	   (fr (set-fr fr (if (eql i -1) n i)))
	   (l (when (eq (stat fr) :link) (readlinkat 0 fr))))
      (cond (l (let ((b (if (eql #\/ (aref l 0)) 0 b)))
		 (link-expand (concatenate 'string (set-fr fr b) l (frame (if (eql i -1) n i) n)) b)))
	    ((eql i -1) str)
	    ((link-expand str (1+ i) n fr))))))

(defun logical-pathname-designator-p (x)
  (typecase x
    (string (logical-pathname-parse x))
    (pathname (typep x 'logical-pathname))
    (stream (logical-pathname-designator-p (pathname x)))))

;(defvar *current-dir* (pathname (concatenate 'string (getcwd) "/"))) FIXME sync with chdir

(defun truename (pd &aux (ppd (translate-logical-pathname pd))(ns (namestring ppd)))
  (declare (optimize (safety 1)))
  (check-type pd pathname-designator)
  (when (wild-pathname-p ns)
    (error 'file-error :pathname pd :format-control "Pathname is wild"))
  (let* ((ns (ensure-dir-string (link-expand ns))))
    (unless (or (zerop (length ns)) (stat ns))
      (error 'file-error :pathname ns :format-control "Pathname does not exist"))
    (let* ((d (pathname-directory ppd))
	   (d1 (subst :back :up d))
	   (ppd (if (eq d d1) ppd (make-pathname :directory d1 :defaults ppd))))
      (if (eq (car d) :absolute) ppd (merge-pathnames ppd (concatenate 'string (getcwd) "/") nil)))))


(defun probe-file (pd &aux (pn (translate-logical-pathname pd)))
  (declare (optimize (safety 1)))
  (check-type pd pathname-designator)
  (when (wild-pathname-p pn)
    (error 'file-error :pathname pn :format-control "Pathname is wild"))
  (when (eq (stat (namestring pn)) :file)
    (truename pn)))
