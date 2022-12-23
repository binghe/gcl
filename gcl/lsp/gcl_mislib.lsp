;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; This file is IMPLEMENTATION-DEPENDENT.


(in-package :si)

(proclaim '(optimize (safety 2) (space 3)))


(defmacro time (form)
  (let ((real-start (gensym)) (real-end (gensym)) (gbc-time-start (gensym))
	(gbc-time (gensym)) (x (gensym)) (run-start (gensym)) (run-end (gensym))
	(child-run-start (gensym)) (child-run-end (gensym)))
  `(let (,real-start ,real-end (,gbc-time-start (gbc-time)) ,gbc-time ,x)
     (setq ,real-start (get-internal-real-time))
     (multiple-value-bind (,run-start ,child-run-start) (get-internal-run-time)
       (gbc-time 0)
       (setq ,x (multiple-value-list ,form))
       (setq ,gbc-time (gbc-time))
       (gbc-time (+ ,gbc-time-start ,gbc-time))
       (multiple-value-bind (,run-end ,child-run-end) (get-internal-run-time)
	 (setq ,real-end (get-internal-real-time))
	 (fresh-line *trace-output*)
	 (format *trace-output*
		 "real time       : ~10,3F secs~%~
                  run-gbc time    : ~10,3F secs~%~
                  child run time  : ~10,3F secs~%~
                  gbc time        : ~10,3F secs~%"
		 (/ (- ,real-end ,real-start) internal-time-units-per-second)
		 (/ (- (- ,run-end ,run-start) ,gbc-time) internal-time-units-per-second)
		 (/ (- ,child-run-end ,child-run-start) internal-time-units-per-second)
		 (/ ,gbc-time internal-time-units-per-second))))
       (values-list ,x))))


(defun this-tz (&aux (x (current-timezone)))
  (if (current-dstp) (1+ x) x))

(defun decode-universal-time (ut &optional (tz (this-tz) tzp))
  (declare (optimize (safety 2)))
  (check-type ut integer)
  (check-type tz rational)
  (let ((ut (+ ut (* (- (this-tz) tz) 3600) #.(* -1 (+ 17 (* 70 365)) 24 60 60))))
  (multiple-value-bind
	(s n h d m y w yd dstp off) (localtime ut)
    (when (when tzp (> dstp 0))
      (multiple-value-setq (s n h d m y w yd) (localtime (- ut 3600))))
    (values s n h d (1+ m) (+ 1900 y) (if (zerop w) 6 (1- w)) (unless tzp (> dstp 0)) (if tzp tz (+ (truncate (- off) 3600) dstp))))))

(defun encode-universal-time (s n h d m y &optional (tz (this-tz) tzp))
  (declare (optimize (safety 2)))
  (check-type s (integer 0 59))
  (check-type n (integer 0 59))
  (check-type h (integer 0 23))
  (check-type d (integer 1 31))
  (check-type m (integer 1 12))
  (check-type y integer)
  (check-type tz rational)
  (multiple-value-bind
	(tm dstp) (mktime s n h d (1- m) (- y 1900))
    (+ tm #.(* (+ 17 (* 70 365)) 24 60 60) (* (- tz (this-tz)) 3600) (if tzp (* dstp 3600) 0))))

(defun compile-file-pathname (pathname)
  (make-pathname :defaults pathname :type "o"))

(defun constantly (x)
  (lambda (&rest args)
    (declare (ignore args) (:dynamic-extent args))
    x))

(defun complement (fn)
  (lambda (&rest args) (not (apply fn args))))

(defun default-system-banner ()
  (let (gpled-modules)
    (dolist (l '(:unexec :bfd :readline :xgcl))
      (when (member l *features*)
	(push l gpled-modules)))
    (format nil "GCL (GNU Common Lisp)  ~a.~a.~a ~a  ~a  ~a  git: ~a~%~a~%~a ~a~%~a~%~a~%~%~a~%"
	    *gcl-major-version* *gcl-minor-version* *gcl-extra-version* *gcl-release-date*
	    (if (member :ansi-cl *features*) "ANSI" "CLtL1")
	    (if (member :gprof *features*) "profiling" "")
	    *gcl-git-tag*
	    "Source License: LGPL(gcl,gmp), GPL(unexec,bfd,xgcl)"
	    "Binary License: "
	    (if gpled-modules (format nil "GPL due to GPL'ed components: ~a" gpled-modules)
	      "LGPL")
	    "Modifications of this banner must retain notice of a compatible license"
	    "Dedicated to the memory of W. Schelter"
	    "Use (help) to get some basic information on how to use GCL.")))

 (defun lisp-implementation-version nil
   (format nil "GCL ~a.~a.~a git tag ~a"
	   *gcl-major-version*
	   *gcl-minor-version*
	   *gcl-extra-version*
	   *gcl-git-tag*))

(defun objlt (x y)
  (declare (object x y))
  (let ((x (address x)) (y (address y)))
    (declare (fixnum x y))
    (if (< y 0)
	(if (< x 0) (< x y) t)
      (if (< x 0) nil (< x y)))))

(defun reset-sys-paths (s)
  (declare (string s))
  (setq *lib-directory* s)
  (setq *system-directory* (string-concatenate s "unixport/"))
  (let (nl)
    (dolist (l '("cmpnew/" "gcl-tk/" "lsp/" "xgcl-2/"))
      (push (string-concatenate s l) nl))
    (setq *load-path* nl))
  nil)

(defun gprof-output (symtab gmon)
  (with-open-file
     (s (format nil "|gprof -S '~a' '~a' '~a'" symtab (kcl-self) gmon))
     (copy-stream s *standard-output*)))

(defun write-symtab (symtab start end &aux (*package* (find-package "KEYWORD")))

  (with-open-file
   (s symtab :direction :output :if-exists :supersede)

   (format s "~16,'0x T ~a~%" start "GCL_MONSTART")

   (dolist (p (list-all-packages))
     (do-symbols (x p)
      (when (and (eq (symbol-package x) p) (fboundp x))
	(let* ((y (symbol-function x))
	       (y (if (and (consp y) (eq 'macro (car y))) (cdr y) y))
	       (y (if (compiled-function-p y) (function-start y) 0)))
	  (when (<= start y end)
	    (format s "~16,'0x T ~s~%" y x))))))

   (let ((string-register ""))
     (dotimes (i (ptable-alloc-length))
       (multiple-value-bind
	(x y) (ptable i string-register)
	(when (<= start x end)
	  (format s "~16,'0x T ~a~%" x y)))))

   (format s "~16,'0x T ~a~%" end "GCL_MONEND"))

  symtab)

(defun gprof-start (&optional (symtab "gcl_symtab") (adrs (gprof-addresses))
			      &aux (start (car adrs))(end (cdr adrs)))
  (let ((symtab (write-symtab symtab start end)))
    (when (monstartup start end)
      symtab)))

(defun gprof-quit (&optional (symtab "gcl_symtab") &aux (gmon (mcleanup)))
  (when gmon
    (gprof-output symtab gmon)))



