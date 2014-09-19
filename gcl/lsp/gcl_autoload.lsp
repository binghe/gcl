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



;;;;    AUTOLOAD


;;; Go into LISP.
(in-package 'lisp)

;(defvar *features*)

(defun lisp-implementation-type () "GNU Common Lisp (GCL)")

(defun machine-type () #+sun "SUN"
  #+hp-ux "HP-UX"
  #+eclipse "ECLIPSE"
  #+vax "VAX"
  )
				 
;(defun machine-type () "DEC VAX11/780")

(defun machine-version () (machine-type))
;(defun machine-version () nil)

(defun machine-instance () (machine-type))
;(defun machine-instance () nil)

(defun software-type ()
  #+aosv "AOS/VS"
  #+bsd "BSD"
  #+system-v "SYSTEM-V"
  #+hp-ux "HP-UX")

;(defun software-type () "UNIX BSD")

(defun software-version () (software-type))
;(defun software-version () "4.2BSD")

;(defun short-site-name () "RIMS")
(defun short-site-name () nil)

;(defun long-site-name ()
;  "Research Institute for Mathematical Sciences, Kyoto University")
(defun long-site-name () nil)



;;; Compiler functions.

(defun proclaim (d)
       (when (eq (car d) 'special) (mapc #'si:*make-special (cdr d))))

(defun proclamation (d)
  (and (eq (car d) 'special)
       (dolist (var (cdr d) t)
               (unless (si:specialp var) (return nil)))))

(defun compile-file (&rest args)
       (error "COMPILE-FILE is not defined in this load module."))
(defun compile (&rest args)
       (error "COMPILE is not defined in this load module."))
(defun disassemble (&rest args)
       (error "DISASSEMBLE is not defined in this load module."))


;;; Editor.

;
(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))

#+never
(defun get-universal-time ()
  (multiple-value-bind (sec min h d m y dow dstp tz)
      (get-decoded-time)
    (encode-universal-time sec min h d m y tz)))


;  Set the default system editor to a fairly certain bet.
#-winnt(defvar *gcl-editor* "vi")
#+winnt(defvar *gcl-editor* "notepad")

(defun new-ed (editor-name)
  "Change the editor called by (ed) held in *gcl-editor*."
  (setf *gcl-editor* editor-name))

(defun ed (&optional name)
  "Edit a file using the editor named in *gcl-editor*; customise with new-ed()."
  (if (null name)
      (system *gcl-editor*)
    (cond ((stringp name) 
	   (system (format nil "~A ~A" *gcl-editor* name))) ; If string, assume file name.
	  ((pathnamep name)
	   (system (format nil "~A ~A" *gcl-editor* (namestring name)))) ; If pathname.
	  (t 
	   (let ((body (symbol-function name)))
	     (cond ((compiled-function-p body) (error "You can't edit compiled functions."))
		   ((and body
			 (consp body)
			 (eq (car body) 'lambda-block)) ; If lambda block, save file and edit.
		    (let ((ed-file (concatenate 'string
						(temp-dir)
						(format nil "~A" (cadr body))
						".lisp")))
		      (with-open-file
		       (st ed-file :direction :output)
		       (print `(defun ,name ,@ (cddr body)) st))
		      (system (format nil "~A ~A" *gcl-editor* ed-file))))
		   (t (system (format nil "~A ~A" *gcl-editor* name))))))))) ; Use symbol as filename

;;; Allocator.

(import 'si::allocate)
(export '(allocate
	  ;allocated-pages maximum-allocatable-pages
          ;allocate-contiguous-pages
          ;allocated-contiguous-pages maximum-contiguous-pages
          ;allocate-relocatable-pages allocated-relocatable-pages 
          sfun gfun cfun cclosure spice structure))

;(defvar type-character-alist
;             '((cons . #\.)
;               (fixnum . #\N)
;               (bignum . #\B)
;               (ratio . #\R)
;               (short-float . #\F)
;               (long-float . #\L)
;               (complex . #\C)
;               (character . #\#)
;               (symbol . #\|)
;               (package . #\:)
;               (hash-table . #\h)
;               (array . #\a)
;               (vector . #\v)
;               (string . #\")
;               (bit-vector . #\b)
;               (structure . #\S)
;	       (sfun . #\g)
;               (stream . #\s)
;               (random-state . #\$)
;               (readtable . #\r)
;               (pathname . #\p)
;               (cfun . #\f)
;	       (vfun . #\V)
;               (cclosure . #\c)
;               (spice . #\!)))
;
;(defun get-type-character (type)
;  (let ((a (assoc type type-character-alist)))
;    (unless a
;            (error "~S is not an implementation type.~%~
;                   It should be one of:~%~
;                   ~{~10T~S~^~30T~S~^~50T~S~%~}~%"
;                   type
;                   (mapcar #'car type-character-alist)))
;    (cdr a)))

;(defun allocate (type quantity &optional really-allocate)
;  (si:alloc (get-type-character type) quantity really-allocate))

;(defun allocated-pages (type)
;  (si:npage (get-type-character type)))

;(defun maximum-allocatable-pages (type)
;  (si:maxpage (get-type-character type)))

;(defun allocate-contiguous-pages (quantity &optional really-allocate)
;  (si::alloc-contpage quantity really-allocate))

;(defun allocated-contiguous-pages ()
;  (si:ncbpage))

;(defun maximum-contiguous-pages ()
;  (si::maxcbpage))

;(defun allocate-relocatable-pages (quantity &optional really-allocate)
;  (si::alloc-relpage quantity))

;(defun allocated-relocatable-pages ()
;  (si::nrbpage))

;; FIXME  This has to come straight from enum.h. CM 20050114
(defvar *type-list*
        '(cons
          fixnum bignum ratio short-float long-float complex
          character symbol package hash-table
          array vector string bit-vector
          structure stream random-state readtable pathname
          cfun cclosure sfun gfun vfun afun closure cfdata spice))

(defun heaprep nil
  
  (let ((f (list
	    "word size:            ~a bits~%"
	    "page size:            ~a bytes~%"
	    "heap start:           0x~x~%"
	    "heap max :            0x~x~%"
	    "shared library start: 0x~x~%"
	    "cstack start:         0x~x~%"
	    "cstack mark offset:   ~a bytes~%"
	    "cstack direction:     ~[downward~;upward~;~]~%"
	    "cstack alignment:     ~a bytes~%"
	    "cstack max:           ~a bytes~%"
	    "immfix start:         0x~x~%"
	    "immfix size:          ~a fixnums~%"
	    "physical memory:      ~a pages~%"))
	(v (multiple-value-list (si::heap-report))))
    
    (do ((v v (cdr v)) (f f (cdr f))) ((not (car v)))
	(format t (car f) 
		(let ((x (car v))) 
		  (cond ((>= x 0) x) 
			((+ x (* 2 (1+ most-positive-fixnum))))))))))

(defun room (&optional x)
  (let ((l (multiple-value-list (si:room-report)))
        maxpage leftpage ncbpage maxcbpage ncb cbgbccount npage maxnpage
        rbused rbfree nrbpage maxrbpage
        info-list link-alist)
    (setq maxpage (nth 0 l) leftpage (nth 1 l)
          ncbpage (nth 2 l) maxcbpage (nth 3 l) ncb (nth 4 l)
          cbgbccount (nth 5 l)
          holepage (nth 6 l)
          rbused (nth 7 l) rbfree (nth 8 l) nrbpage (nth 9 l)
	  maxrbpage (nth 10 l)
          rbgbccount (nth 11 l)
          l (nthcdr 12 l))
    (do ((l l (nthcdr 5 l))
         (tl *type-list* (cdr tl))
         (j 0 (+ j (if (nth 3 l) (nth 3 l) 0)))
         (i 0 (+ i (if (nth 2 l) (nth 2 l) 0))))
        ((null l) (setq npage i maxnpage j))
      (let ((typename (car tl))
            (nused (nth 0 l))
            (nfree (nth 1 l))
            (npage (nth 2 l))
            (maxpage (nth 3 l))
            (gbccount (nth 4 l)))
        (if nused
            (push (list typename npage maxpage
                        (if (zerop (+ nused nfree))
                            0
                            (/ nused 0.01 (+ nused nfree)))
                        (if (zerop gbccount) nil gbccount))
                  info-list)
            (let ((a (assoc (nth nfree *type-list*) link-alist)))
                 (if a
                     (nconc a (list typename))
                     (push (list (nth nfree *type-list*) typename)
                           link-alist))))))
    (terpri)
    (dolist (info (reverse info-list))
      (apply #'format t "~8D/~D~19T~6,1F%~@[~8D~]~35T~{~A~^ ~}"
             (append (cdr info)
                     (if  (assoc (car info) link-alist)
                          (list (assoc (car info) link-alist))
                          (list (list (car info))))))
      (terpri)
      )
    (terpri)
    (format t "~8D/~D~26T~@[~8D~]~35Tcontiguous (~D blocks)~%"
            ncbpage maxcbpage (if (zerop cbgbccount) nil cbgbccount) ncb)
    (format t "~9T~D~35Thole~%" holepage)
    (format t "~8D/~D~19T~6,1F%~@[~8D~]~35Trelocatable~%~%"
            nrbpage maxrbpage (/ rbused 0.01 (+ rbused rbfree))
            (if (zerop rbgbccount) nil rbgbccount))
    (format t "~10D pages for cells~%~%" npage)
    (format t "~10D total pages in core~%" (+ npage ncbpage nrbpage))
    (format t "~10D current core maximum pages~%" (+ maxnpage maxcbpage maxrbpage))
    (format t "~10D pages reserved for gc~%" maxrbpage)
    (format t "~10D pages available for adding to core~%" leftpage)
    (format t "~10D pages reserved for core exhaustion~%~%" (- maxpage (+ maxnpage maxcbpage (ash maxrbpage 1) leftpage)))
    (format t "~10D maximum pages~%" maxpage)
    (values)
    )

 (when x
  (format t "~%~%")
  (format t "Key:~%~%WS: words per struct~%UP: allocated pages~%MP: maximum pages~%FI: fraction of cells in use on allocated pages~%GC: number of gc triggers allocating this type~%~%")
  (heaprep))

 (values))


;;; C Interface.

(defmacro Clines (&rest r) nil)
(defmacro defCfun (&rest r) nil)
(defmacro defentry (&rest r) nil)

(defmacro defla (&rest r) (cons 'defun r))

;;; Help.

(export '(help help*))

(defun help (&optional (symbol nil s))
  (if s (si::print-doc symbol)
      (progn
        (princ "
Welcome to GNU Common Lisp (GCL for short).
Here are some functions you should learn first.

	(HELP symbol) prints the online documentation associated with the
	symbol.  For example, (HELP 'CONS) will print the useful information
	about the CONS function, the CONS data type, and so on.

	(HELP* string) prints the online documentation associated with those
	symbols whose print-names have the string as substring.  For example,
	(HELP* \"PROG\") will print the documentation of the symbols such as
	PROG, PROGN, and MULTIPLE-VALUE-PROG1.

	(SI::INFO <some string>) chooses from a list of all references in the
        on-line documentation to <some string>.

	(APROPOS <some string>) or (APROPOS <some string> '<a package>) list
        all symbols containing <some string>.

	(DESCRIBE '<symbol>) or (HELP '<symbol>) describe particular symbols.

	(BYE) or (BY) ends the current GCL session.

Good luck!				 The GCL Development Team")
        (values))))

(defun help* (string &optional (package (find-package "LISP")))
  (si::apropos-doc string package))

;;; Pretty-print-formats.
;;;
;;;	The number N as the property of a symbol SYMBOL indicates that,
;;;	in the form (SYMBOL f1 ... fN fN+1 ... fM), the subforms fN+1,...,fM
;;;	are the 'body' of the form and thus are treated in a special way by
;;;	the KCL pretty-printer.

(setf (get 'lambda 'si:pretty-print-format) 1)
(setf (get 'lambda-block 'si:pretty-print-format) 2)
(setf (get 'lambda-closure 'si:pretty-print-format) 4)
(setf (get 'lambda-block-closure 'si:pretty-print-format) 5)

(setf (get 'block 'si:pretty-print-format) 1)
(setf (get 'case 'si:pretty-print-format) 1)
(setf (get 'catch 'si:pretty-print-format) 1)
(setf (get 'ccase 'si:pretty-print-format) 1)
(setf (get 'clines 'si:pretty-print-format) 0)
(setf (get 'compiler-let 'si:pretty-print-format) 1)
(setf (get 'cond 'si:pretty-print-format) 0)
(setf (get 'ctypecase 'si:pretty-print-format) 1)
(setf (get 'defcfun 'si:pretty-print-format) 2)
(setf (get 'define-setf-method 'si:pretty-print-format) 2)
(setf (get 'defla 'si:pretty-print-format) 2)
(setf (get 'defmacro 'si:pretty-print-format) 2)
(setf (get 'defsetf 'si:pretty-print-format) 3)
(setf (get 'defstruct 'si:pretty-print-format) 1)
(setf (get 'deftype 'si:pretty-print-format) 2)
(setf (get 'defun 'si:pretty-print-format) 2)
(setf (get 'do 'si:pretty-print-format) 2)
(setf (get 'do* 'si:pretty-print-format) 2)
(setf (get 'do-symbols 'si:pretty-print-format) 1)
(setf (get 'do-all-symbols 'si:pretty-print-format) 1)
(setf (get 'do-external-symbols 'si:pretty-print-format) 1)
(setf (get 'dolist 'si:pretty-print-format) 1)
(setf (get 'dotimes 'si:pretty-print-format) 1)
(setf (get 'ecase 'si:pretty-print-format) 1)
(setf (get 'etypecase 'si:pretty-print-format) 1)
(setf (get 'eval-when 'si:pretty-print-format) 1)
(setf (get 'flet 'si:pretty-print-format) 1)
(setf (get 'labels 'si:pretty-print-format) 1)
(setf (get 'let 'si:pretty-print-format) 1)
(setf (get 'let* 'si:pretty-print-format) 1)
(setf (get 'locally 'si:pretty-print-format) 0)
(setf (get 'loop 'si:pretty-print-format) 0)
(setf (get 'macrolet 'si:pretty-print-format) 1)
(setf (get 'multiple-value-bind 'si:pretty-print-format) 2)
(setf (get 'multiple-value-prog1 'si:pretty-print-format) 1)
(setf (get 'prog 'si:pretty-print-format) 1)
(setf (get 'prog* 'si:pretty-print-format) 1)
(setf (get 'prog1 'si:pretty-print-format) 1)
(setf (get 'prog2 'si:pretty-print-format) 2)
(setf (get 'progn 'si:pretty-print-format) 0)
(setf (get 'progv 'si:pretty-print-format) 2)
(setf (get 'return 'si:pretty-print-format) 0)
(setf (get 'return-from 'si:pretty-print-format) 1)
(setf (get 'tagbody 'si:pretty-print-format) 0)
(setf (get 'the 'si:pretty-print-format) 1)
(setf (get 'throw 'si:pretty-print-format) 1)
(setf (get 'typecase 'si:pretty-print-format) 1)
(setf (get 'unless 'si:pretty-print-format) 1)
(setf (get 'unwind-protect 'si:pretty-print-format) 0)
(setf (get 'when 'si:pretty-print-format) 1)
(setf (get 'with-input-from-string 'si:pretty-print-format) 1)
(setf (get 'with-open-file 'si:pretty-print-format) 1)
(setf (get 'with-open-stream 'si:pretty-print-format) 1)
(setf (get 'with-output-to-string 'si:pretty-print-format) 1)


(in-package 'si)

(defvar *lib-directory* (namestring (truename "../")))

(import '(*lib-directory* *load-path* *system-directory*) 'si::user) 
