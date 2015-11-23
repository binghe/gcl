(in-package :si)

(defun ensure-dir-string (str)
  (if (and (eq (car (stat str)) :directory) (not (eql #\/ (aref str (1- (length str))))))
      (concatenate 'string str "/")
    str))

(defun link-expand (str &optional (b 1)	(n (length str)) fr)
  (labels ((frame (b e) (make-array (- n b) :element-type 'character
				    :displaced-to str :displaced-index-offset b :fill-pointer (- e b)))
	   (set-fr (fr e &aux (fr (or fr (frame 0 b)))) (setf (fill-pointer fr) e) fr))
    (let* ((i (string-match #v"/" str b))
	   (fr (set-fr fr (if (eql i -1) n i)))
	   (l (when (eq (car (stat fr)) :link) (readlinkat 0 fr))))
      (cond (l (link-expand (concatenate 'string (set-fr fr (if (eql #\/ (aref l 0)) 0 b)) l (frame (if (eql i -1) n i) n)) b n))
	    ((eql i -1) str)
	    ((link-expand str (1+ i) n fr))))))

(defun truename (pn &aux (pn (translate-logical-pathname pn)) str)
  (cond ((not (eq (car (pathname-directory pn)) :absolute)) (truename (merge-pathnames pn (parse-namestring (concatenate 'string (getcwd) "/")))))
	((wild-pathname-p pn) (error 'file-error :pathname pn :format-control "Pathname is wild"))
	((not (stat (setq str (namestring pn)))) (error 'file-error :pathname pn :format-control "Pathname does not exist"))
	((let ((*up-key* :back)) (pathname (ensure-dir-string (link-expand str)))))))


(defun probe-file (pn &aux (pn (translate-logical-pathname pn)))
  (when (wild-pathname-p pn)
    (error 'file-error :pathname pn :format-control "Pathname is wild"))
  (when (eq (car (stat (namestring pn))) :file)
    (truename pn)))
