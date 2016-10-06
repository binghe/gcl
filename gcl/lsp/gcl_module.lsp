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


;;;;    module.lsp
;;;;
;;;;                            module routines


(in-package :si)


(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))


(defvar *modules* nil)


(defun provide (module-name)
  (setq *modules*
        (adjoin (string module-name)
                *modules*
                :test #'string=)))


(defun require (module-name
                &optional (pathname (string-downcase (string module-name))))
  (let ((*default-pathname-defaults* (make-pathname)))
    (unless (member (string module-name)
                    *modules*
                    :test #'string=)
            (if (atom pathname)
                (load pathname)
	      (do ((p pathname (cdr p)))
                    ((endp p))
                  (load (car p)))))))
          

(defun documentation (symbol doc-type)
  (case doc-type
    (variable (get symbol 'variable-documentation))
    (function (get symbol 'function-documentation))
    (structure (get symbol 'structure-documentation))
    (type (get symbol 'type-documentation))
    (setf (get symbol 'setf-documentation))
    (t
     (if (packagep symbol) 
	 (get (find-symbol (package-name symbol) :keyword) 'package-documentation)
       (error "~S is an illegal documentation type." doc-type)))))


(defun find-documentation (body)
  (if (or (endp body) (endp (cdr body)))
      nil
      (let ((form (macroexpand (car body))))
        (if (stringp form)
            form
            (if (and (consp form)
                     (eq (car form) 'declare))
                (find-documentation (cdr body))
                nil)))))


(defun eval-feature (x)
  (cond ((atom x)
         (member x *features*
                 :test #'(lambda (a b)
                           (cond ((symbolp a)
                                  (and (symbolp b)
                                       (string-equal (symbol-name a)
                                                     (symbol-name b))))
                                 (t (eql a b))))))
        ((eq (car x) 'and)
         (dolist (x (cdr x) t) (unless (eval-feature x) (return nil))))
        ((eq (car x) 'or)
         (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))
        ((eq (car x) 'not)
	 (not (eval-feature (cadr x))))
	(t (error "~S is not a feature expression." x))))

(defun sharp-+-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (if (eval-feature (let ((*read-suppress* nil) (*read-base* 10.))
                         (read stream t nil t)))
      (values (read stream t nil t))
      (let ((*read-suppress* t)) (read stream t nil t) (values))))

(set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader
                              (si::standard-readtable))

(defun sharp---reader (stream subchar arg)
  (declare (ignore subchar arg))
  (if (eval-feature (let ((*read-suppress* nil) (*read-base* 10.))
                         (read stream t nil t)))
      (let ((*read-suppress* t)) (read stream t nil t) (values))
      (values (read stream t nil t))))

(set-dispatch-macro-character #\# #\- 'sharp---reader)
(set-dispatch-macro-character #\# #\- 'sharp---reader
                              (si::standard-readtable))

