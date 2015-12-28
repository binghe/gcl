;; -*-Lisp-*-
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


;;;;    assert.lsp


(in-package :si)

(defun read-evaluated-form nil
  (format *query-io* "~&type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defun check-type-symbol (symbol value type &optional type-string 
				 &aux (type-string (when type-string (concatenate 'string ": need a " type-string))))
  (restart-case 
   (cerror "Check type again." 'type-error :datum value :expected-type type)
   (store-value (v) 
		:report (lambda (stream) (format stream "Supply a new value of ~s. ~a" symbol (or type-string "")))
		:interactive read-evaluated-form
		(setf value v)))
  (if (typep value type) value (check-type-symbol symbol value type type-string)))

(defmacro check-type (place typespec &optional string)
  (declare (optimize (safety 2)))
  `(progn (,(if (symbolp place) 'setq 'setf) ,place 
	   (the ,typespec (if (typep ,place ',typespec) ,place (check-type-symbol ',place ,place ',typespec ',string)))) nil))


(defmacro assert (test-form &optional places string &rest args)
  `(do nil;(*print-level* 4) (*print-length* 4)
       (,test-form nil)
     ,(if string
	  `(cerror "" ,string ,@args)
	`(cerror "" "The assertion ~:@(~S~) failed." ',test-form))
     ,@(mapcan (lambda (place)
		 `((format *error-output*
			   "Please input the new value for the place ~:@(~S~): "
			   ',place)
		   (finish-output *error-output*)
		   (setf ,place (read)))) places)))

(defmacro ctypecase (keyform &rest clauses &aux (key (sgen "CTYPECASE")))
  (declare (optimize (safety 2)))
;  (check-type clauses (list-of proper-list))
  `(do nil (nil)
    (typecase ,keyform
      ,@(mapcar (lambda (l)
		  `(,(car l) (return (progn ,@(subst key keyform (cdr l))))))
		clauses))
    (check-type ,keyform (or ,@(mapcar 'car clauses)))))
