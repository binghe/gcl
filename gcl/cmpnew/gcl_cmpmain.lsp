;;; CMPMAIN  Compiler main program.
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


;;;		**** Caution ****
;;;	This file is machine/OS dependant.
;;;		*****************


(in-package 'compiler)


(export '(*compile-print* *compile-verbose*))
(import 'si::*tmp-dir* 'compiler)
(import 'si::*error-p* 'compiler)

;;; This had been true with Linux 1.2.13 a.out or even older
;;; #+linux   (push :ld-not-accept-data  *features*)
;;; its now a bug preventing the :linux feature.


(defvar *compiler-in-use* nil)
(defvar *compiler-compile* nil)
(defvar *compiler-input*)
(defvar *compiler-output1*)
(defvar *compiler-output2*)
(defvar *compiler-output-data*)
(defvar *compiler-output-i*)

(defvar *compile-print* nil)
(defvar *compile-verbose* t)
(defvar *cmpinclude* "\"cmpinclude.h\"")
;;If the following is a string, then it is inserted instead of
;; the include file cmpinclude.h, EXCEPT for system-p calls.
(defvar *cmpinclude-string* t)


;; Let the user write dump c-file etc to  /dev/null.
(defun get-output-pathname (file ext name &optional (dir (pathname-directory *default-pathname-defaults*))
				 (device (pathname-device *default-pathname-defaults*)))
  (cond 
	((equal file "/dev/null") (pathname file))
	#+aix3
	((and (equal name "float")
	      (equal ext "h"))
	 (get-output-pathname file ext "Float" ))
	(t
	 (make-pathname :device (or (and (not (null file))
					 (not (eq file t))
					 (pathname-device file))
				       device)
			:directory (or (and (not (null file))
					    (not (eq file t))
					    (pathname-directory file))
				       dir)
			:name (or (and (not (null file))
				       (not (eq file t))
				       (pathname-name file))
				  name)
			:type ext))))

(defun safe-system (string)
 (multiple-value-bind
  (code result) (system (ts string))
    (unless (and (zerop code) (zerop result))
      (cerror "Continues anyway."
              "(SYSTEM ~S) returned a non-zero value ~D."
              string
              result)
      (setq *error-p* t))
    (values result)))

;; If this is t we use fasd-data on all but system-p files.   If it
;; is :system-p we use it on all files.   If nil use it on none.
(defvar *fasd-data* t)
(defvar *data* nil)
(defvar *default-system-p* nil)
(defvar *default-c-file* nil)
(defvar *default-h-file* nil)
(defvar *default-data-file* nil)
(defvar *keep-gaz* nil)

;;  (list section-length split-file-names next-section-start-file-position)
;;  Many c compilers cannot handle the large C files resulting from large lisp files.
;;  If *split-files* is a number then, separate compilations for sections
;;  *split-files* long, with the 
;;  will be performed for separate chunks of the lisp files.
(defvar *split-files* nil)  ;; if 

(defun check-end (form eof)
  (cond  ((eq form eof)
	  (setf (third *split-files*) nil))
	 ((> (file-position *compiler-input*)
	     (car *split-files*))
	  (setf (third *split-files*)(file-position *compiler-input*)))))
	  

(defun compile-file  (&rest args
			    &aux (*print-pretty* nil)
			    (*package* *package*) (*split-files* *split-files*)
			    (*PRINT-CIRCLE* NIL)
			    (*PRINT-RADIX* NIL)
			    (*PRINT-ARRAY* T)
			    (*PRINT-LEVEL* NIL)
			    (*PRINT-PRETTY* T)
			    (*PRINT-LENGTH* NIL)
			    (*PRINT-GENSYM* T)
			    (*PRINT-CASE* :UPCASE)
			    (*PRINT-BASE* 10)
			    (*PRINT-ESCAPE* T)
			    (section-length *split-files*)
			    tem)
  (loop 
   (compiler::init-env)
   (setq tem (apply 'compiler::compile-file1 args))
   (cond ((atom *split-files*)(return tem))
	 ((and (consp *split-files*)
	       (null (third *split-files*)))
	  (let ((gaz (let ((*DEFAULT-PATHNAME-DEFAULTS* (car args)))
			    			    (gazonk-name)))
		(*readtable* (si::standard-readtable)))
	    (setq gaz (get-output-pathname gaz "lsp" (car args)))
	    (with-open-file (st gaz :direction :output)
	      (print
	       `(eval-when (load eval)
			   (dolist (v ',(nreverse (second *split-files*)))
				   (load (merge-pathnames v si::*load-pathname*))))
	       st))
	    (setq *split-files* nil)
	    (or (member :output-file args)
		(setq args (append args (list :output-file (car args)))))
	    (return 
	     (prog1 (apply 'compile-file gaz (cdr args))
	       (unless *keep-gaz* (mdelete-file gaz))))
	    ))
	 (t nil))
   (if (consp *split-files*)
       (setf (car *split-files*) (+ (third *split-files*) section-length)))
   ))


(defun compile-file1 (input-pathname
                      &key (output-file input-pathname)
                           (o-file t)
                           (c-file *default-c-file*)
                           (h-file *default-h-file*)
                           (data-file *default-data-file*)
			   (c-debug nil)
                           (system-p *default-system-p*)
			   (print nil)
                           (load nil)
                      &aux (*standard-output* *standard-output*)
                           (*error-output* *error-output*)
                           (*compiler-in-use* *compiler-in-use*)
			   (*c-debug* c-debug)
			   (*compile-print* (or print *compile-print*))
                           (*package* *package*)
			   (*DEFAULT-PATHNAME-DEFAULTS* #"")
			   (*data* (list (make-array 50 :fill-pointer 0 :adjustable t) nil nil))
			   *init-name* 	
			   (*fasd-data* *fasd-data*)
                           (*error-count* 0))

  (declare (special *c-debug* *init-name* system-p))

  (cond (*compiler-in-use*
         (format t "~&The compiler was called recursively.~%~
Cannot compile ~a.~%"
                 (namestring (merge-pathnames input-pathname #".lsp")))
         (setq *error-p* t)
         (return-from compile-file1 (values)))
        (t (setq *error-p* nil)
           (setq *compiler-in-use* t)))  

  (unless (probe-file (merge-pathnames input-pathname #".lsp"))
    (format t "~&The source file ~a is not found.~%"
            (namestring (merge-pathnames input-pathname #".lsp")))
    (setq *error-p* t)
    (return-from compile-file1 (values)))

  (when *compile-verbose*
    (format t "~&Compiling ~a.~%" (namestring (merge-pathnames input-pathname #".lsp"))))

  (and *record-call-info* (clear-call-table))

  (with-open-file
   (*compiler-input* (merge-pathnames input-pathname #".lsp"))
   
   
   (cond ((numberp *split-files*)
	  (if (< (file-length *compiler-input*) *split-files*)
	      (setq *split-files* nil)
	    (setq *split-files* (list *split-files* nil 0 nil)))))
   
   (cond ((consp *split-files*)
	  (file-position *compiler-input* (third *split-files*))
	  (setq output-file
		(make-pathname :directory (pathname-directory output-file)
			       :name (format nil "~a~a" (length (second *split-files*)) (pathname-name (pathname output-file)))
			       :type "o"))
	  
	  (push (pathname-name output-file)   (second *split-files*))))
	   
    
  (let* ((eof (cons nil nil))
         (dir (or (and (not (null output-file))
                       (pathname-directory output-file))
                  (pathname-directory input-pathname)))
         (name (or (and (not (null output-file))
                        (pathname-name output-file))
                   (pathname-name input-pathname)))
	 (device (or (and (not (null output-file))
			  (pathname-device output-file))
		     (pathname-device input-pathname)))
	 
         (o-pathname (get-output-pathname o-file "o" name dir device))
         (c-pathname (get-output-pathname c-file "c" name dir device))
         (h-pathname (get-output-pathname h-file "h" name dir device))
         (data-pathname (get-output-pathname data-file "data" name dir device)))

    (declare (special dir name ))
    
    (init-env)
    
    (and (boundp 'si::*gcl-version*)
	 (not system-p)
	 (add-init `(si::warn-version ,si::*gcl-major-version*
				      ,si::*gcl-minor-version*
				      ,si::*gcl-extra-version*)))

    (when (probe-file "./gcl_cmpinit.lsp")
      (load "./gcl_cmpinit.lsp" :verbose *compile-verbose*))

    (with-open-file (*compiler-output-data* data-pathname :direction :output)

      (when *fasd-data*
	(setq *fasd-data* (list (si::open-fasd *compiler-output-data* :output nil nil))))

      (wt-data-begin)

      (if *compiler-compile*
	  (t1expr *compiler-compile*)
	(let* ((rtb *readtable*)
	       (prev (and (eq (get-macro-character #\# rtb)
			      (get-macro-character
			       #\# (si:standard-readtable)))
			  (get-dispatch-macro-character #\# #\, rtb))))
	  (if (and prev (eq prev (get-dispatch-macro-character
				  #\# #\, (si:standard-readtable))))
	      (set-dispatch-macro-character #\# #\, 'si:sharp-comma-reader-for-compiler rtb)
	    (setq prev nil))
	  
	  ;; t1expr the package ops again..
	  (if (consp *split-files*)
	      (dolist (v (fourth *split-files*)) (t1expr v)))
	  (unwind-protect
	      (do ((form (read *compiler-input* nil eof)
			 (read *compiler-input* nil eof))
		   (load-flag (or (eq :defaults *eval-when-defaults*)
				  (member 'load *eval-when-defaults*))))
		  (nil)
		  (cond
		   ((eq form eof))
		   (load-flag (t1expr form))
		   ((maybe-eval nil form)))
		  (cond
		   ((and *split-files* (check-end form eof))
		    (setf (fourth *split-files*) (reverse (third *data*)))
		    (return nil))
		   ((eq form eof) (return nil))))
	    
            (when prev (set-dispatch-macro-character #\# #\, prev rtb)))))
      
      (setq *init-name* (init-name input-pathname system-p))

      (when (zerop *error-count*)
        (when *compile-verbose* (format t "~&End of Pass 1.  ~%"))
        (compiler-pass2 c-pathname h-pathname system-p ))

      (wt-data-end)) ;;; *compiler-output-data* closed.

    (init-env)

    (if (zerop *error-count*)

        (progn
          (when *compile-verbose* (format t "~&End of Pass 2.  ~%"))
	  (cond (*record-call-info*
		 (dump-fn-data (get-output-pathname output-file "fn" name dir device))))
          (cond (o-file
                 (compiler-cc c-pathname o-pathname  )
                 (cond ((probe-file o-pathname)
                        (compiler-build o-pathname data-pathname)
                        (when load (load o-pathname))
                       (when *compile-verbose*
                              (print-compiler-info)
                              (format t "~&Finished compiling ~a.~%" (namestring output-file)
				      )))
                       (t 
                          (format t "~&Your C compiler failed to compile the intermediate file.~%")
                          (setq *error-p* t))))
                 (*compile-verbose*
                  (print-compiler-info)
                  (format t "~&Finished compiling ~a.~%" (namestring output-file)
			  )))
          (unless c-file (mdelete-file c-pathname))
          (unless h-file (mdelete-file h-pathname))
          (unless (or data-file #+ld-not-accept-data t system-p) (mdelete-file data-pathname))
	  o-pathname)

        (progn
          (when (probe-file c-pathname) (mdelete-file c-pathname))
          (when (probe-file h-pathname) (mdelete-file h-pathname))
          (when (probe-file data-pathname) (mdelete-file data-pathname))
          (format t "~&No FASL generated.~%")
          (setq *error-p* t)
	  (values)
	  ))))))

(defun gazonk-name ()
  (dotimes (i 1000)
   (let ((tem (merge-pathnames 
               (format nil "~agazonk_~d_~d.lsp" (if (boundp '*tmp-dir*) *tmp-dir* "") (abs (si::getpid)) i))))
    (unless (probe-file tem)
     (return-from gazonk-name (pathname tem)))))
 (error "1000 gazonk names used already!"))

(defun prin1-cmp (form strm)
  (let ((*compiler-output-data* strm)
	(*fasd-data* nil))
    (wt-data1 form)  ;; this binds all the print stuff
    ))

(defun compile (name &optional def &aux tem gaz (*default-pathname-defaults* #"."))

  (cond ((not(symbolp name)) (error "Must be a name"))
	((and (consp def)
	      (member (car def) '(lambda )))
	 (or name (setf name 'cmp-anon))
	 (setf (symbol-function name)
	       def)
	 (compile name))
	(def (error "def not a lambda expression"))
	((setq tem (macro-function name))
	 (setf (symbol-function 'cmp-anon) tem)
	 (compile 'cmp-anon)
	 (setf (macro-function name) (macro-function name))
	 ;; FIXME -- support warnings-p and failures-p.  CM 20041119
	 (values name nil nil))
	((and (setq tem (symbol-function name))
	      (consp tem))
	 (let ((na (if (symbol-package name) name 'cmp-anon)))
	   (unless (and (fboundp 'si::init-cmp-anon) (or (si::init-cmp-anon) (fmakunbound 'si::init-cmp-anon)))
	     (with-open-file
	      (st (setq gaz (gazonk-name)) :direction :output))
	     (let* ((*compiler-compile* 
		     `(defun ,na 
			,@(ecase (car tem)
				 (lambda (cdr tem))
				 (lambda-block (cddr tem)))))
		    (fi (compile-file gaz)))
	       (when (pathnamep fi)
		 (load fi)
		 (mdelete-file fi)))
	     (unless *keep-gaz* (mdelete-file gaz)))
	   (or (eq na name) (setf (symbol-function name) (symbol-function na)))
	 ;; FIXME -- support warnings-p and failures-p.  CM 20041119
	   (values (symbol-function name) nil nil)
	   ))
	(t (error "can't compile ~a" name))))

(defun disassemble (name &aux tem)
  (cond ((and (consp name)
	      (eq (car name) 'lambda))
	 (eval `(defun cmp-anon ,@ (cdr name)))
	 (disassemble 'cmp-anon))
	((not(symbolp name)) (error "Not a lambda or a name"))
	((setq tem(macro-function name))
	 (setf (symbol-function 'cmp-tmp-macro) tem)
	 (disassemble 'cmp-tmp-macro)
	 (setf (macro-function name) (macro-function name))
	 name)
	((and (setq tem (symbol-function name))
	      (consp tem)
	      (eq (car tem) 'lambda-block))
	 (let ((gaz (gazonk-name)))
	   (with-open-file
	     (st gaz :direction :output)
	     (prin1-cmp `(defun ,name ,@ (cddr tem))       st))
	   (let (*fasd-data*)
	     (compile-file gaz 
			   :h-file t 
			   :c-file t
			   :data-file t
			   :o-file t))
	   (let ((cn (get-output-pathname gaz "c" gaz ))
		 (dn (get-output-pathname gaz "data" gaz ))
		 (hn (get-output-pathname gaz "h" gaz ))
		 (on (get-output-pathname gaz "o" gaz )))
	     (with-open-file (st cn)
			     (do () ((let ((a (read-line st)))
				       (when (>= (si::string-match "gazonk_[0-9]*_[0-9]*.h" a) 0)
					 (format t "~%~d~%" a)
					 a))))
			     (si::copy-stream st *standard-output*))
	     (with-open-file (st dn)
			     (si::copy-stream st *standard-output*))
	     (with-open-file (st hn)
			     (si::copy-stream st *standard-output*))
	     (when (zerop (system "which objdump >/dev/null"))
	       (safe-system (si::string-concatenate "objdump --source " (namestring on))))
	     (mdelete-file cn)
	     (mdelete-file dn)
	     (mdelete-file hn)
	     (mdelete-file on)
	     (unless *keep-gaz* (mdelete-file gaz)))))
	(t (error "can't disassemble ~a" name))))


(defun compiler-pass2 (c-pathname h-pathname system-p
				  &aux 
				  (ci *cmpinclude*)
				  (ci (when (stringp ci) (subseq ci 1 (1- (length ci)))))
				  (ci (concatenate 'string si::*system-directory* "../h/" ci))
				  (system-p (when (probe-file ci) system-p)))
  (declare (special *init-name*))
  (with-open-file (st c-pathname :direction :output)
    (let ((*compiler-output1* st))
      (declare (special *compiler-output1*))
    (with-open-file (*compiler-output2* h-pathname :direction :output)
      (cond ((and 
	      (stringp *cmpinclude-string*)
	      (not system-p)
	      (si::fwrite *cmpinclude-string* 0
			  (length *cmpinclude-string*) *compiler-output1*)))
	    (t (wt-nl1 "#include " *cmpinclude*)))
      (wt-nl1 "#include \""
	      (namestring
	        (make-pathname :name
	          (pathname-name h-pathname)
	           :type (pathname-type h-pathname)))
              "\"")

      (catch *cmperr-tag* (ctop-write *init-name*))

      (terpri *compiler-output1*)
      ;; write ctl-z at end to make sure preprocessor stops!
      #+dos (write-char (code-char 26) *compiler-output1*)
      (terpri *compiler-output2*)))))


(defvar *cc* "cc")
(defvar *ld* "ld")
(defvar *ld-libs* "ld-libs")
(defvar *opt-three* "")
(defvar *opt-two* "")
(defvar *init-lsp* "init-lsp")

(defvar *use-buggy* nil)

(defun  compiler-command (&rest args &aux na )
  (declare (special *c-debug*))
  (let ((dirlist (pathname-directory (first args)))
	(name (pathname-name (first args)))
	dir)
    (cond (dirlist (setq dir (namestring (make-pathname :directory dirlist))))
	  (t (setq dir ".")))
    (setq na  (namestring
	       (make-pathname :name name :type (pathname-type(first args)))))
   #+(or dos winnt)
      (format nil "~a -I~a ~a ~a -c -w ~s -o ~s"
	      *cc*
	      (concatenate 'string si::*system-directory* "../h")
	      (if (and (boundp '*c-debug*) *c-debug*) " -g " "")
	      (case *speed*
		    (3 *opt-three* )
		    (2 *opt-two*) 
		    (t ""))	
	      (namestring (make-pathname  :type "c" :defaults (first args)))
	      (namestring (make-pathname  :type "o" :defaults (first args)))
	      )

   #-(or dos winnt)
   (format nil  "~a -I~a ~a ~a -c ~s -o ~s ~a"
	   *cc*
	   (concatenate 'string si::*system-directory* "../h")
	   (if (and (boundp '*c-debug*) *c-debug*) " -g " "")
           (case *speed*
		 (3 *opt-three* )
		 (2 *opt-two*) 
		 (t ""))	
	   (namestring (first args))
	   (namestring (second args))
	   (prog1
	       #+aix3
	     (format nil " -w ;ar x /lib/libc.a fsavres.o  ; ar qc XXXfsave fsavres.o ; echo init_~a > XXexp ; mv  ~a  XXX~a ; ld -r -D-1 -bexport:XXexp -bgc XXX~a -o ~a XXXfsave ; rm -f XXX~a XXexp XXXfsave fsavres.o"
		     *init-name*
		     (setq na (namestring (get-output-pathname na "o" nil)))
		     na na na na na)
	     #+(or dlopen irix5)
	     (if (not system-p)
		 (format nil
			 " -w ; mv ~a XX~a ; ld  ~a -shared XX~a  -o ~a -lc ; rm -f XX~a"  
			 (setq na (namestring (get-output-pathname na "o" nil)))			    na
			 #+ignore-unresolved "-ignore_unresolved"
			 #+expect-unresolved "-expect_unresolved '*'"
			 na na na))	
			    
	     #+bsd ""; "-w"
	     #-(or aix3 bsd irix3) " 2> /dev/null ")
		  
		 
	   )
   )
  )

#+winnt (defun prep-win-path-acc ( s acc)
  (let ((pos (search "\~" s)))
    (if pos 
	(let ((start (subseq s 0 (1+ pos)))
	      (finish (subseq s (1+ pos))))
	  (prep-win-path-acc finish (concatenate 'string acc start "~")))
      (concatenate 'string acc s))))

#+winnt
(defun no-device (c)
  (let* ((c (namestring (truename c)))
	 (p (search ":" c)))
    (if p (subseq c (1+ p)) c)))

;; #+winnt
;; (defun prep-win-path (c o)
;;   (let* ((w si::*wine-detected*)
;; 	 (c (if w (no-device c) c))
;; 	 (o (if w (no-device o) o)))
;;     (prep-win-path-acc (compiler-command c o) "")))

(defun compiler-cc (c-pathname o-pathname)
  (safe-system
   (format
     nil
     (prog1
	 #+irix5 (compiler-command c-pathname o-pathname )
	 #+vax "~a ~@[~*-O ~]-S -I. -w ~a ; as -J -W -o ~A ~A"
	 #+(or system-v e15 dgux sgi ) "~a ~@[~*-O ~]-c -I. ~a 2> /dev/null"
	 #+winnt (prep-win-path-acc (compiler-command c-pathname o-pathname) "")
	 #-winnt (compiler-command c-pathname o-pathname)
	)
     *cc*
     (if (or (= *speed* 2) (= *speed* 3)) t nil)
            (namestring c-pathname)
             (namestring o-pathname)
  
            ))
  
  #+dont_need
  (let ((cname (pathname-name c-pathname))
        (odir (pathname-directory o-pathname))
        (oname (pathname-name o-pathname)))
    (unless (and (equalp (truename "./")
                         (truename (make-pathname :directory odir)))
                 (equal cname oname))
	(rename-file (make-pathname :name cname :type "o")
        	                  o-pathname)
)))


(defun compiler-build (o-pathname data-pathname)
  #+(and system-v (not e15))
  (safe-system (format nil "echo \"\\000\\000\\000\\000\" >> ~A"
                       (namestring o-pathname)))
    #+(or hp-ux sun sgi)
    (with-open-file (o-file
		    (namestring o-pathname)
		    :direction :output
		    :if-exists :append)
      ; we could do a safe-system, but forking is slow on the Iris
    #+(or hp-ux (and sgi (not irix5)))
    (dotimes (i 12)
      (write-char #\^@ o-file))
    #+sun  ; we could do a safe-system, but forking is slow on the Iris
    (dolist (v '(0 0 4 16 0 0 0 0))
	      (write-byte v o-file))

    )
  #-ld-not-accept-data  
  (when (probe-file o-pathname)
     (nconc-files o-pathname data-pathname)
    #+never
    (safe-system (format nil
			 "cat ~a  >> ~A"
			 (namestring data-pathname)
			 (namestring o-pathname)))))

(defun print-compiler-info ()
  (format t "~&OPTIMIZE levels: Safety=~d~:[ (No runtime error checking)~;~], Space=~d, Speed=~d~%"
          (cond ((null *compiler-check-args*) 0)
                ((null *safe-compile*) 1)
                ((null *compiler-push-events*) 2)
                (t 3))
          *safe-compile* *space* *speed*))

(defun nconc-files (a b)
  (let* ((n 256)
	 (tem (make-string n))
	 (m 0))
    (with-open-file (st-a a :direction :output :if-exists :append)
      (with-open-file (st-b b )
	(sloop::sloop
	   do (setq m (si::fread tem 0 n st-b))
	   while (and m (> m 0))
	   do (si::fwrite tem 0 m st-a))))))

#+dos
(progn
(defun directory (x &aux ans)
  (let* ((pa (pathname x))
	 (temp "XXDIR")
	 tem
	 (name (pathname-name pa)))
    (setq pa (make-pathname :directory (pathname-directory pa)
			    :name (or (pathname-name pa) :wild)
			    :type (pathname-type pa)))
    (setq name (namestring pa))
    (safe-system (format nil "ls -d ~a > ~a" name temp))
    (with-open-file (st temp)
	    (loop (setq tem (read-line st nil nil))
		  (if (and tem (setq tem (probe-file tem)))
		      (push tem ans) (return))))
    ans))

(defvar *old-compile-file* #'compile-file) 
(defun compile-file (f &rest l)
  (let* ((p (pathname f)) dir pwd)
    (setq dir (pathname-directory p))
    (when dir
	  (setq dir (namestring (make-pathname :directory dir
					       :name ".")))
	  (setq pwd (namestring (truename ".")))
	  )
    (unwind-protect
	(progn (if dir (si::chdir dir))
	       (apply *old-compile-file* f l))
      (if pwd (si::chdir pwd)))))

(defun user-homedir-pathname ()
  (or (si::getenv "HOME") "/"))

)

;
;  These functions are added to build custom images requiring
;  the loading of binary objects on systems relocating with dlopen.
;

(defun make-user-init (files outn)

  (let* ((c (pathname outn))
	 (c (merge-pathnames c (make-pathname :directory '(:current))))
	 (o (merge-pathnames (make-pathname :type "o") c))
	 (c (merge-pathnames (make-pathname :type "c") c)))
  
  (with-open-file (st c :direction :output)
		  (format st "#include ~a~%~%" *cmpinclude*)

		  (format st "#define load2(a) do {")
		  (format st "printf(\"Loading %s...\\n\",(a));")
		  (format st "load(a);")
		  (format st "printf(\"Finished %s...\\n\",(a));} while(0)~%~%")

		  (let ((p nil))
		    (dolist (tem files)
		      (when (equal (pathname-type tem) "o")
			(let ((tem (namestring tem)))
			  (push (list (si::find-init-name tem) tem) p))))

		    (setq p (nreverse p))

		    (dolist (tem p)
		      (format st "extern void ~a(void);~%" (car tem)))
		    (format st "~%")

		    (format st "typedef struct {void (*fn)(void);char *s;} Fnlst;~%")
		    (format st "#define NF ~a~%" (length p))
		    (format st "static Fnlst my_fnlst[NF]={")
		    (dolist (tem p)
		      (when (not (eq tem (car p)))
			(format st ",~%"))
		      (format st "{~a,\"~a\"}" (car tem) (cadr tem)))
		    (format st "};~%~%")
		    
		    (format st "static int user_init_run;~%")
		    (format st "#define my_load(a_,b_) {if (!user_init_run && (a_) && (b_)) gcl_init_or_load1((a_),(b_));(a_)=0;(b_)=0;}~%~%")
                    
		    (format st "object user_init(void) {~%")
		    (format st "user_init_run=1;~%")
		    (dolist (tem files)
		      (let ((tem (namestring tem)))
			    (cond ((equal (cadr (car p)) tem)
				   (format st "gcl_init_or_load1(~a,\"~a\");~%"
					   (car (car p)) tem)
				   (setq p (cdr p)))
				  (t 
				   (format st "load2(\"~a\");~%" tem)))))
		    (format st "return Cnil;}~%~%")

		    (format st "static int my_strncmp(const char *s1,const char *s2,unsigned long n) {")
		    (format st "  for (;n--;) if (*s1++!=*s2++) return 1; return 0;}")

		    (format st "int user_match(const char *s,int n) {~%")
		    (format st "  Fnlst *f;~%")
		    (format st "  for (f=my_fnlst;f<my_fnlst+NF;f++){~%")
		    (format st "     if (f->s && !my_strncmp(s,f->s,n)) {~%")
		    (format st "        my_load(f->fn,f->s);~%")
		    (format st "        return 1;~%")
		    (format st "     }~%")
		    (format st "  }~%")
		    (format st "  return 0;~%")
		    (format st "}~%~%")))
		    
  (compiler-cc c o)
  (mdelete-file c)

  o))

(defun mysub (str it new)
  (let ((x (search it str)))
    (unless x
      (return-from mysub str))
    (let ((y (+ (length it) (the fixnum x))))
      (declare (fixnum y))
      (concatenate (type-of str)
		   (subseq str 0 x)
		   new
		   (mysub (subseq str y) it new)))))


(eval-when (compile eval)
(defmacro fcr (x) `(load-time-value (si::compile-regexp ,x)))
(defmacro sml (x y &optional z) 
  (let ((q (gensym)))
    `(let ((,q (si::string-match ,x ,y ,@(when z (list z)))))
       (if (= ,q -1) (length ,y) ,q)))))

(defun ts (s &optional (r ""))
  (declare (string s) (ignorable r))
  #+winnt
  (if (not si::*wine-detected*) s
    (let* ((x (sml (fcr #u"[^ \n\t]") s))
	   (y (sml (fcr #u"[ \n\t]") s x))
	   (f (subseq s x y))
	   (l (subseq s y))
	   (k (when (> (length f) 0) (aref f 0)))
	   (q (if (eql k #\") (string k) ""))
	   (f (if (eql k #\") (subseq f 1 (1- (length f))) f))
	   (f (if (and k (not (eql k #\-))) (namestring (no-device f)) f)))
      (if k (concatenate 'string r q f q (ts l " ")) "")))
  #-winnt s)

(defun mdelete-file (x)
  (delete-file (ts (namestring x))))


(defun link (files image &optional post extra-libs (run-user-init t)) 

  (let* ((ui (make-user-init files "user-init"))
	 (raw (pathname image))
	 (init (merge-pathnames (make-pathname
				 :name (concatenate 'string "init_" (pathname-name raw))
				 :type "lsp") raw))
	 (raw (merge-pathnames raw (truename "./")))
	 (raw (merge-pathnames (make-pathname
				:name (concatenate 'string "raw_" (pathname-name raw)))
			       raw))
	 (map (merge-pathnames (make-pathname
				:name (concatenate 'string (pathname-name raw) "_map")) raw))
	 #+winnt (raw (merge-pathnames (make-pathname :type "exe") raw))
	 )

    (with-open-file (st (namestring map) :direction :output))
    (safe-system 
     (let* ((par (namestring (make-pathname :directory '(:parent))))
	    (i (concatenate 'string " " par))
	    (j (concatenate 'string " " si::*system-directory* par)))
       (format nil "~a ~a ~a ~a -L~a ~a ~a ~a"
	       (mysub *ld* i j) 
	       (namestring raw)
	       (namestring ui)
	       (let ((sfiles ""))
		 (dolist (tem files)
		   (if (equal (pathname-type tem) "o")
		       (setq sfiles (concatenate 'string sfiles " " (namestring tem)))))
		 sfiles) 
	       si::*system-directory*
	       #+gnu-ld (format nil "-Wl,-Map ~a" (namestring map)) #-gnu-ld ""
	       (if (stringp extra-libs) extra-libs "")
	       (mysub *ld-libs* i j))))
    
    (mdelete-file ui)
    
    (with-open-file (st init :direction :output)
		    (unless run-user-init
		      (format st "(fmakunbound 'si::user-init)~%"))
		    (format st "(setq si::*no-init* '(")
		    (dolist (tem files)
		      (format st " \"~a\"" (pathname-name tem)))
		    (format st "))~%")
		    (with-open-file (st1 
				     (format nil "~a~a" si::*system-directory* *init-lsp*))
				    (si::copy-stream st1 st))
		    (if (stringp post) (format st "~a~%" post))
		    (format st "(si::save-system \"~a\")~%" (ts (namestring image))))
    
    (safe-system (format nil "~a ~a < ~a" 
		    (namestring raw)
		    si::*system-directory*
		    (namestring init)))
    
    (mdelete-file raw)
    (mdelete-file init))

  image)
