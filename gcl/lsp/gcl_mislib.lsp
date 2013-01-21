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


(in-package 'lisp)


(export 'time)
(export '(reset-sys-paths decode-universal-time encode-universal-time compile-file-pathname complement constantly))


(in-package 'system)


(proclaim '(optimize (safety 2) (space 3)))


(defmacro time (form)
  (let ((real-start (gensym)) (real-end (gensym)) (gbc-time-start (gensym))
	(gbc-time (gensym)) (x (gensym)) (run-start (gensym)) (run-end (gensym))
	(child-run-start (gensym)) (child-run-end (gensym)))
  `(let (,real-start ,real-end (,gbc-time-start (si::gbc-time)) ,gbc-time ,x)
     (setq ,real-start (get-internal-real-time))
     (multiple-value-bind (,run-start ,child-run-start) (get-internal-run-time)
       (si::gbc-time 0)
       (setq ,x (multiple-value-list ,form))
       (setq ,gbc-time (si::gbc-time))
       (si::gbc-time (+ ,gbc-time-start ,gbc-time))
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

(defconstant seconds-per-day #.(* 24 3600))

(defun leap-year-p (y)
  (and (zerop (mod y 4))
       (or (not (zerop (mod y 100))) (zerop (mod y 400)))))

(defun number-of-days-from-1900 (y)
  (let ((y1 (1- y)))
    (+ (* (- y 1900) 365)
       (floor y1 4) (- (floor y1 100)) (floor y1 400)
       -460)))

(eval-when
 (compile eval)
 (defmacro mmd (n &optional lp 
		  &aux (l '(31 28 31 30 31 30 31 31 30 31 30 31))
		  (l (if lp (cons (pop l) (cons (1+ (pop l)) l)) l))(r 0)(s (mapcar (lambda (x) (incf r x)) l)))
  `(defconstant ,n (make-array ,(length s) :element-type '(integer ,(car s) ,(car (last s))) :initial-contents ',s))))
       
(mmd +md+)
(mmd +lmd+ t)

(defun decode-universal-time (ut &optional (tz (current-timezone) tzp) 
				 &aux (dstp (unless tzp (current-dstp))) (ut (- ut (* tz 3600))))
  (declare (optimize (safety 2)))
  (check-type ut (integer 0))
  (check-type tz rational)
  (multiple-value-bind
   (d ut) (floor ut seconds-per-day)
   (let* ((dow (mod d 7))(y (+ 1900 (floor d 366))))
     (labels ((l (y dd &aux (lyp (leap-year-p y))(td (if lyp 366 365))(x (- d dd)))
		 (if (< x td) (values (1+ x) y lyp) (l (1+ y) (+ dd td)))))
	     (multiple-value-bind
	      (d y lyp) (l y (number-of-days-from-1900 y))
	      (let* ((l (if lyp +lmd+ +md+))
		     (m (position d l :test '<=))
		     (d (if (> m 0) (- d (aref l (1- m))) d)))
		(multiple-value-bind
		 (h ut) (floor ut 3600)
		 (multiple-value-bind
		  (min sec) (floor ut 60)
		  (values sec min h d (1+ m) y dow dstp tz)))))))))

(defun encode-universal-time (sec min h d m y &optional (tz (current-timezone)))
  (declare (optimize (safety 2)))
  (check-type sec (integer 0 59))
  (check-type min (integer 0 59))
  (check-type h (integer 0 23))
  (check-type d (integer 1 31))
  (check-type m (integer 1 12))
  (check-type y (integer 1900))
  (check-type tz rational)
  (when (<= 0 y 99)
    (multiple-value-bind
     (sec min h d m y1 dow dstp tz) (get-decoded-time)
     (declare (ignore sec min h d m dow dstp tz))
     (incf y (- y1 (mod y1 100)))
     (cond ((< (- y y1) -50) (incf y 100))
	   ((>= (- y y1) 50) (decf y 100)))))
  (+ (* (+ (1- d) (number-of-days-from-1900 y) (if (> m 1) (aref (if (leap-year-p y) +lmd+ +md+) (- m 2)) 0))
        seconds-per-day)
     (* (+ h tz) 3600) (* min 60) sec))

(defun compile-file-pathname (pathname)
(make-pathname :defaults pathname :type "o"))
(defun constantly (x)
#'(lambda (&rest args)
    (declare (ignore args) (:dynamic-extent args))
x))
(defun complement (fn)
#'(lambda (&rest args) (not (apply fn args))))

(defun default-system-banner ()
  (let (gpled-modules)
    (dolist (l '(:unexec :bfd :readline :xgcl))
      (when (member l *features*)
	(push l gpled-modules)))
    (format nil "GCL (GNU Common Lisp)  ~a.~a.~a ~a  ~a  ~a~%~a~%~a ~a~%~a~%~a~%~%~a~%" 
	    *gcl-major-version* *gcl-minor-version* *gcl-extra-version*
	    (if (member :ansi-cl *features*) "ANSI" "CLtL1")
	    (if (member :gprof *features*) "profiling" "")
	    (si::gcl-compile-time)
	    "Source License: LGPL(gcl,gmp), GPL(unexec,bfd,xgcl)"
	    "Binary License: "
	    (if gpled-modules (format nil "GPL due to GPL'ed components: ~a" gpled-modules)
	      "LGPL")
	    "Modifications of this banner must retain notice of a compatible license"
	    "Dedicated to the memory of W. Schelter"
	    "Use (help) to get some basic information on how to use GCL.")))

 (defun lisp-implementation-version nil
   (format nil "GCL ~a.~a.~a"
	   si::*gcl-major-version*
	   si::*gcl-minor-version*
	   si::*gcl-extra-version*))

(defun objlt (x y)
  (declare (object x y))
  (let ((x (si::address x)) (y (si::address y)))
    (declare (fixnum x y))
    (if (< y 0)
	(if (< x 0) (< x y) t)
      (if (< x 0) nil (< x y)))))

(defun reset-sys-paths (s)
  (declare (string s))
  (setq si::*lib-directory* s)
  (setq si::*system-directory* (si::string-concatenate s "unixport/"))
  (let (nl)
    (dolist (l '("cmpnew/" "gcl-tk/" "lsp/" "xgcl-2/"))
      (push (si::string-concatenate s l) nl))
    (setq si::*load-path* nl))
  nil)
