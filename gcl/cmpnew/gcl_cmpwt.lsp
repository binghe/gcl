;;; CMPWT  Output routines.
;;;
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


(in-package :compiler)

(eval-when (compile eval)
  (require 'FASDMACROS "../cmpnew/gcl_fasdmacros.lsp")


(defmacro data-inits () `(first *data*))

)

(defun wt-comment (message &optional (symbol nil))
  (princ "
/*	" *compiler-output1*)
  (princ message *compiler-output1*)
  (when symbol
        (let ((s (symbol-name symbol)))
             (declare (string s))
             (dotimes** (n (length s))
                        (let ((c (schar s n)))
                             (declare (character c))
                             (unless (char= c #\/)
                                     (princ c *compiler-output1*))))))
  (princ "	*/
" *compiler-output1*)
  nil
  )

(defun wt1 (form)
  (cond ((or (stringp form) (integerp form) (characterp form))
         (princ form *compiler-output1*))
        ((or (typep form 'long-float)
             (typep form 'short-float))
         (format *compiler-output1* "~10,,,,,,'eG" form))
        (t (wt-loc form)))
  nil)

(defun wt-h1 (form)
  (cond ((consp form)
         (let ((fun (get (car form) 'wt)))
              (if fun
                  (apply fun (cdr form))
                  (cmpiler-error "The location ~s is undefined." form))))
        (t (princ form *compiler-output2*)))
  nil)

(defvar *fasd-data*)

(defvar *hash-eq* nil)
(defvar *run-hash-equal-data-checking* t)
(defun memoized-hash-equal (x depth);FIXME implement all this in lisp
  (declare (fixnum depth))
  (when *run-hash-equal-data-checking*
    (unless *hash-eq* (setq *hash-eq* (make-hash-table :test 'eq)))
    (or (gethash x *hash-eq*)
	(setf (gethash x *hash-eq*)
	      (if (> depth 3) 0
		(if (typep x 'cons)
		    (logxor (setq depth (the fixnum (1+ depth)));FIXME?
			    (logxor 
			     (memoized-hash-equal (car x) depth) 
			     (memoized-hash-equal (cdr x) depth)))
		  (si::hash-equal x depth)))))))

(defun push-data-incf (x)
  (incf *next-vv*))

(defun wt-data1 (expr)
  (let ((*print-radix* nil)
        (*print-base* 10)
        (*print-circle* t)
        (*print-pretty* nil)
        (*print-level* nil)
        (*print-length* nil)
        (*print-case* :downcase)
        (*print-gensym* t)
        (*print-array* t)
	;;This forces the printer to add the float type in the .data file.
	(*READ-DEFAULT-FLOAT-FORMAT* t) 
        (si::*print-package* t)
        (si::*print-structure* t))
    (terpri *compiler-output-data*)
    (prin1 expr *compiler-output-data*)))

(defun add-init (x &optional endp &aux (tem (cons (memoized-hash-equal x -1000) x)))
  (if endp
      (nconc (data-inits) (list tem))
    (push tem (data-inits)))
  x)

(defun verify-datum (v)
  (unless (eql (pop v) (memoized-hash-equal v -1000))
    (cmpwarn "A form or constant:~% ~s ~%has changed during the eval compile procedure!.~%  The changed form will be the one put in the compiled file" v))
  v)

(defun wt-fasd-element (x)
  (si::find-sharing-top x (fasd-table (car *fasd-data*)))
  (si::write-fasd-top x (car *fasd-data*)))

(defun wt-data2 (x)
  (if *fasd-data*
      (wt-fasd-element x)
    (wt-data1 x)))

(defun wt-data-file nil
  (when *prof-p* (add-init `(si::mark-memory-as-profiling)))
  (wt-data2 (1+ *next-vv*))
  (dolist (v (nreverse (data-inits)))
    (wt-data2 (verify-datum v)))
  (when *fasd-data*
    (si::close-fasd (car *fasd-data*))))

(defun wt-data-begin ())
(defun wt-data-end ())

(defmacro wt (&rest forms &aux (fl nil))
  (dolist** (form forms (cons 'progn (reverse (cons nil fl))))
    (if (stringp form)
        (push `(princ ,form *compiler-output1*) fl)
        (push `(wt1 ,form) fl))))

(defmacro wt-h (&rest forms &aux (fl nil))
  (cond ((endp forms) '(princ "
" *compiler-output2*))
        ((stringp (car forms))
         (dolist** (form (cdr forms)
                         (list* 'progn `(princ ,(concatenate 'string "
" (car forms)) *compiler-output2*) (reverse (cons nil fl))))
                   (if (stringp form)
                       (push `(princ ,form *compiler-output2*) fl)
                       (push `(wt-h1 ,form) fl))))
        (t (dolist** (form forms
                           (list* 'progn '(princ "
" *compiler-output2*) (reverse (cons nil fl))))
                     (if (stringp form)
                         (push `(princ ,form *compiler-output2*) fl)
                         (push `(wt-h1 ,form) fl))))))

(defmacro wt-nl (&rest forms &aux (fl nil))
  (cond ((endp forms) '(princ "
	" *compiler-output1*))
        ((stringp (car forms))
         (dolist** (form (cdr forms)
                         (list* 'progn `(princ ,(concatenate 'string "
	" (car forms)) *compiler-output1*) (reverse (cons nil fl))))
                   (if (stringp form)
                       (push `(princ ,form *compiler-output1*) fl)
                       (push `(wt1 ,form) fl))))
        (t (dolist** (form forms
                           (list* 'progn '(princ "
	" *compiler-output1*) (reverse (cons nil fl))))
                     (if (stringp form)
                         (push `(princ ,form *compiler-output1*) fl)
                         (push `(wt1 ,form) fl))))))

(defmacro wt-nl1 (&rest forms &aux (fl nil))
  (cond ((endp forms) '(princ "
" *compiler-output1*))
        ((stringp (car forms))
         (dolist** (form (cdr forms)
                         (list* 'progn `(princ ,(concatenate 'string "
" (car forms)) *compiler-output1*) (nreverse (cons nil fl))))
                   (if (stringp form)
                       (push `(princ ,form *compiler-output1*) fl)
                       (push `(wt1 ,form) fl))))
        (t (dolist** (form forms
                           (list* 'progn '(princ "
" *compiler-output1*) (nreverse (cons nil fl))))
                     (if (stringp form)
                         (push `(princ ,form *compiler-output1*) fl)
                         (push `(wt1 ,form) fl))))))

