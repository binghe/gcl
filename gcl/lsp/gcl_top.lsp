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


;;;;  top.lsp
;;;;
;;;;  Top-level loop, break loop, and error handlers
;;;;
;;;;  Revised on July 11, by Carl Hoffman.


(in-package "LISP")
;(export 'lisp)
(export '(+ ++ +++ - * ** *** / // ///))
(export '(break warn))
(export '*break-on-warnings*)
(export '*break-enable*)

(in-package 'system)

(export '*break-readtable*)
(export '(loc *debug-print-level*))

(export '(vs ihs-vs ihs-fun frs-vs frs-bds frs-ihs bds-var bds-val super-go))

(eval-when 
    (compile)
  (proclaim '(optimize (safety 2) (space 3)))
  (defvar *command-args* nil))

(defvar +)
(defvar ++)
(defvar +++)
(defvar -)
(defvar *)
(defvar **)
(defvar ***)
(defvar /)
(defvar //)
(defvar ///)


;; setup file search and autoload

(defvar *fixed-load-path* nil)
(defvar *load-path* nil)
(defvar *load-types* '(".o" ".lsp" ".lisp"))

(defvar *lisp-initialized* nil)
(defconstant +top-level-quit-tag+ (cons nil nil))
(defvar *quit-tag* +top-level-quit-tag+)
(defvar *quit-tags* nil)
(defvar *break-level* '())
(defvar *break-env* nil)
(defvar *ihs-base* 1)
(defvar *ihs-top* 1)
(defvar *current-ihs* 1)
(defvar *frs-base* 0)
(defvar *frs-top* 0)
(defvar *break-enable* t)
(defvar *break-message* "")

(defvar *break-on-warnings* nil)

(defvar *break-readtable* nil)

(defvar *top-level-hook* nil)


(defvar *top-eof* (cons nil nil))
(defvar *no-prompt* nil)

(defun top-level ()
  (let ((+ nil) (++ nil) (+++ nil)
        (- nil) 
        (* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil))
    (setq *lisp-initialized* t)
    (catch *quit-tag*
      (progn 
	(cond
	 (*multiply-stacks* (setq *multiply-stacks* nil))
	 ((probe-file "init.lsp") (load "init.lsp"))))
      (when (if (symbolp *top-level-hook*) (fboundp *top-level-hook*) (functionp *top-level-hook*))
	(funcall *top-level-hook*)))

    (when (boundp '*system-banner*)
      (format t *system-banner*)
      (format t "Temporary directory for compiler files set to ~a~%" *tmp-dir*))

    (loop
      (setq +++ ++ ++ + + -)
      (if *no-prompt* (setq *no-prompt* nil)
	(format t "~%~a>"
		(if (eq *package* (find-package 'user)) ""
                  (package-name *package*))))
      (reset-stack-limits)
      ;; have to exit and re-enter to multiply stacks
      (cond (*multiply-stacks* (Return-from top-level)))
      (when (catch *quit-tag*
              (setq - (locally (declare (notinline read))
                               (read *standard-input* nil *top-eof*)))
              (when (eq - *top-eof*) (bye))
              (let ((values (multiple-value-list
                             (locally (declare (notinline eval)) (eval -)))))
                (setq /// // // / / values *** ** ** * * (car /))
                (fresh-line)
                (dolist (val /)
                  (locally (declare (notinline prin1)) (prin1 val))
                  (terpri))
                nil))
        (setq *evalhook* nil *applyhook* nil)
        (terpri *error-output*)
        (break-current)))))

(defun set-dir (sym val)
   (let ((tem (or val (and (boundp sym) (symbol-value sym)))))
      (if tem (set sym (coerce-slash-terminated tem)))))

(defvar *error-p* nil)

(defun process-some-args (args &optional compile &aux *load-verbose*)
  (when args
    (let ((x (pop args)))
      (cond ((equal x "-load") (load (pop args)))
	    ((equal x "-eval") (eval (read-from-string (pop args))))
	    ((equal x "-batch") (setq *top-level-hook* 'bye))
	    ((equal x "-o-file") (unless (read-from-string (car args))
				   (push (cons :o-file nil) compile)
				   (pop args)))
	    ((equal x "-h-file") (push (cons :h-file t) compile))
	    ((equal x "-data-file") (push (cons :data-file t) compile))
	    ((equal x "-c-file") (push (cons :c-file t) compile))
	    ((equal x "-system-p") (push (cons :system-p t) compile))
	    ((equal x "-compile") (push (cons :compile (pop args)) compile))
	    ((equal x "-o") (push (cons :o (pop args)) compile))
	    ((equal x "-libdir") (set-dir '*lib-directory* (pop args)))
	    ((equal x "-dir") (set-dir '*system-directory* (pop args)))
	    ((equal x "-f") (do-f (car (setq *command-args* args))))
	    ((equal x "--") (setq *command-args* args args nil))))
    (process-some-args args compile))

  (when compile
    (let* (*break-enable* 
	   (file (cdr (assoc :compile compile)))
	   (o (cdr (assoc :o compile)))
	   (compile (remove :o (remove :compile compile :key 'car) :key 'car))
	   (compile (cons (cons :output-file (or o file)) compile))
	   (result (system:error-set `(apply 'compile-file ,file ',(mapcan (lambda (x) (list (car x) (cdr x))) compile)))))
      (bye (if (or *error-p* (equal result '(nil))) 1 0)))))

(defun dbl-read (&optional (stream *standard-input*) (eof-error-p t)
			   (eof-value nil)  &aux tem  ch)
  (tagbody
   top
   (setq ch (read-char stream eof-error-p eof-value))
   (cond ((eql ch #\newline) (go top))
	 ((eq ch eof-value) (return-from dbl-read eof-value)))
   (unread-char ch stream))

  (cond ((eql #\: ch)
	 (setq tem
	       (string-concatenate
		"("
		(read-line stream eof-error-p eof-value)")"))
	 (read  (make-string-input-stream tem)
					 eof-error-p eof-value))
	(t (read stream eof-error-p eof-value))))


(defvar *debug-print-level* 3)

(defun terminal-interrupt (correctablep)
  (let ((*break-enable* t))
    (if correctablep
        (cerror "Type :r to resume execution, or :q to quit to top level."
		"Console interrupt.")
        (error "Console interrupt -- cannot continue."))))


(defun break-call (key args &optional (prop 'si::break-command) &aux fun)
  (setq fun (complete-prop key 'keyword prop))
  (or fun (return-from break-call nil))
  (setq fun (get fun prop))
  (cond (fun
	 (setq args (cons fun args))
	 (or (symbolp fun) (setq args (cons 'funcall args)))
	 (evalhook args nil nil *break-env*)
	 )
	(t (format *debug-io* "~&~S is undefined break command.~%" key))))

(defun break-quit (&optional (level 0)
                   &aux (current-level (length *break-level*)))
  (when (and (>= level 0) (< level current-level))
    (let ((x (nthcdr (- current-level level 1) *quit-tags*))
	  (y (member nil *quit-tags* :key 'cdr)))
      (if (tailp x y)
	  (format *debug-io* "The *quit-tag* is disabled at level ~s.~%" (length y))
	(throw (cdar x) (cdar x)))))
  (break-current))

(defun break-previous (&optional (offset 1))
  (do ((i (1- *current-ihs*) (1- i)))
      ((or (< i *ihs-base*) (<= offset 0))
       (set-env)
       (break-current))
    (when (ihs-visible i)
      (setq *current-ihs* i)
      (setq offset (1- offset)))))

(defun set-current ()
  (do ((i *current-ihs* (1- i)))
      ((or (ihs-visible i) (<= i *ihs-base*))
       (setq *current-ihs* i)
       (set-env)
       (format *debug-io* "Broken at ~:@(~S~).~:[  Type :H for Help.~;~]"
               (ihs-fname *current-ihs*)
               (cdr *break-level*)))))

(defun break-next (&optional (offset 1))
  (do ((i *current-ihs* (1+ i)))
      ((or (> i *ihs-top*) (< offset 0))
       (set-env)
       (break-current))
    (when (ihs-visible i)
      (setq *current-ihs* i)
      (setq offset (1- offset)))))

(defun break-go (ihs-index)
  (setq *current-ihs* (min (max ihs-index *ihs-base*) *ihs-top*))
  (if (ihs-visible *current-ihs*)
      (progn (set-env) (break-current))
      (break-previous)))

(defun break-message ()
  (princ *break-message* *debug-io*)
  (terpri *debug-io*)
  (values))

(defun describe-environment (&optional (env *break-env*) (str *debug-io*))
  (or (eql (length env) 3) (error "bad env"))
    (let ((fmt "~a~#[none~;~S~;~S and ~S~
         ~:;~@{~#[~;and ~]~S~^, ~}~].~%"))
      (apply 'format str fmt "Local variables: "
	     (mapcar #'car (car *break-env*)))
      (apply 'format str fmt "Local functions: "
	     (mapcar #'car (cadr *break-env*)))
      (apply 'format str fmt "Local blocks: "
	     (mapcan #'(lambda (x) (when (eq (cadr x) 'block) (list (car x))))
                 (caddr *break-env*)))
      (apply 'format str fmt "Local tags: "
	     (mapcan #'(lambda (x) (when (eq (cadr x) 'tag) (list (car x))))
                 (caddr *break-env*)))))

(defun break-vs (&optional (x (ihs-vs *ihs-base*)) (y (ihs-vs *ihs-top*)))
  (setq x (max x (ihs-vs *ihs-base*)))
  (setq y (min y (1- (ihs-vs (1+ *ihs-top*)))))
  (do ((ii *ihs-base* (1+ ii)))
      ((or (>= ii *ihs-top*) (>= (ihs-vs ii) x))
       (do ((vi x (1+ vi)))
           ((> vi y) (values))
         (do ()
             ((> (ihs-vs ii) vi))
           (when (ihs-visible ii) (print-ihs ii))
           (incf ii))
         (format *debug-io* "~&VS[~d]: ~s" vi (vs vi))))))

(defun break-local (&optional (n 0) &aux (x (+ (ihs-vs *current-ihs*) n)))
  (break-vs x x))

(defun break-bds (&rest vars &aux (fi *frs-base*))
  (do ((bi (1+ (frs-bds (1- *frs-base*))) (1+ bi))
       (last (frs-bds (1+ *frs-top*))))
      ((> bi last) (values))
    (when (or (null vars) (member (bds-var bi) vars))
      (do ()
          ((or (> fi *frs-top*) (> (frs-bds fi) bi)))
        (print-frs fi)
        (incf fi))
      (format *debug-io* "~&BDS[~d]: ~s = ~s"
              bi (bds-var bi) (bds-val bi)))))

(defun simple-backtrace ()
  (princ "Backtrace: " *debug-io*)
  (do* ((i *ihs-base* (1+ i))
        (b nil t))
       ((> i *ihs-top*) (terpri *debug-io*) (values))
    (when (ihs-visible i)
      (when b (princ " > " *debug-io*))
      (write (ihs-fname i) :stream *debug-io* :escape t
             :case (if (= i *current-ihs*) :upcase :downcase)))))

(defun ihs-backtrace (&optional (from *ihs-base*) (to *ihs-top*))
  (setq from (max from *ihs-base*))
  (setq to (min to *ihs-top*))
  (do* ((i from (1+ i))
        (j (or (sch-frs-base *frs-base* from) (1+ *frs-top*))))
       ((> i to) (values))
    (when (ihs-visible i) (print-ihs i))
    (do () ((or (> j *frs-top*) (> (frs-ihs j) i)))
      (print-frs j)
      (incf j))))

(defun print-ihs (i &aux (*print-level* 2) (*print-length* 4))
  (format t "~&~:[  ~;@ ~]IHS[~d]: ~s ---> VS[~d]"
          (= i *current-ihs*)
          i
          (let ((fun (ihs-fun i)))
            (cond ((or (symbolp fun) (compiled-function-p fun)) fun)
                  ((consp fun)
                   (case (car fun)
                     (lambda fun)
                     ((lambda-block lambda-block-expanded) (cdr fun))
                     (lambda-closure (cons 'lambda (cddddr fun)))
                     (lambda-block-closure (cddddr fun))
                     (t (cond
			 ((and (symbolp (car fun))
			       (or (special-form-p(car fun))
				   (fboundp (car fun))))
			  (car fun))
			 (t '(:zombi))))))
                  (t (print fun)
		   :zombi)))
          (ihs-vs i)))

(defun print-frs (i)
  (format *debug-io* "~&    FRS[~d]: ~s ---> IHS[~d],VS[~d],BDS[~d]"
          i (frs-kind i) (frs-ihs i) (frs-vs i) (frs-bds i)))

(defun frs-kind (i &aux x)
  (case (frs-class i)
    (:catch
     (if (spicep (frs-tag i))
         (or (and (setq x (member (frs-tag i) (vs (+ (frs-vs i) 2))
                                  :key #'caddr :test #'eq))
                  (if (eq (cadar x) 'block)
                      `(block ,(caar x) ***)
                      `(tagbody ,@(reverse (mapcar #'car
                                             (remove (frs-tag i) x
                                                     :test-not #'eq
                                                     :key #'caddr)))
                                ***)))
             `(block/tagbody ,(frs-tag i)))
         `(catch ',(frs-tag i) ***)))
    (:protect '(unwind-protect ***))
    (t `(system-internal-catcher ,(frs-tag i)))))

(defun break-current ()
  (if *break-level*
      (format *debug-io* "Broken at ~:@(~S~)." (ihs-fname *current-ihs*))
      (format *debug-io* "~&Top level."))
  (values))



(defvar *break-hidden-packages* nil)

(defun ihs-visible (i &aux (tem (ihs-fname i)))
  (and tem (not (member tem *break-hidden-packages*))))


(defun ihs-fname (ihs-index)
  (let ((fun (ihs-fun ihs-index)))
    (cond ((symbolp fun) fun)
          ((consp fun)
           (case (car fun)
             (lambda 'lambda)
             ((lambda-block lambda-block-expanded) (cadr fun))
             (lambda-block-closure (nth 4 fun))
             (lambda-closure 'lambda-closure)
             (t (if (and (symbolp (car fun))
			 (or (special-form-p (car fun))
			     (fboundp (car fun))))
		    (car fun) :zombi)
		    )))
          ((compiled-function-p fun)
           (compiled-function-name fun))
          (t :zombi))))

(defun ihs-not-interpreted-env (ihs-index)
  (let ((fun (ihs-fun ihs-index)))
    (cond ((and (consp fun)
		(> ihs-index 3)
		;(<= (ihs-vs ihs-index) (ihs-vs (- ihs-index 1)))
		)
	   nil)
	  (t t))))

(defun set-env ()
  (setq *break-env*
        (if (ihs-not-interpreted-env *current-ihs*)
            nil
            (let ((i (ihs-vs *current-ihs*)))
              (list (vs i) (vs (1+ i)) (vs (+ i 2)))))))

(defun list-delq (x l)
  (cond ((null l) nil)
        ((eq x (car l)) (cdr l))
        (t (rplacd l (list-delq x (cdr l))))))

(defun super-go (i tag &aux x)
  (when (and (>= i *frs-base*) (<= i *frs-top*) (spicep (frs-tag i)))
    (if (setq x (member (frs-tag i) (vs (+ (frs-vs i) 2))
                        :key #'caddr :test #'eq))
        ; Interpreted TAGBODY.
        (when (and (eq (cadar x) 'tag)
                   (member tag (mapcar #'car (remove (frs-tag i) x
                                                     :test-not #'eq
                                                     :key #'caddr))))
          (internal-super-go (frs-tag i) tag t))
        ; Maybe, compiled cross-closure TAGBODY.
        ; But, it may also be compiled cross-closure BLOCK, in which case
        ; SUPER-GO just RETURN-FROMs with zero values.
        (internal-super-go (frs-tag i) tag nil)))
  (format *debug-io* "~s is invalid tagbody identification for ~s." i tag))

(defun break-backward-search-stack (sym &aux string)
  (setq string (string sym))
  (do* ((ihs (1- *current-ihs*) (1- ihs))
        (fname (ihs-fname ihs) (ihs-fname ihs)))
      ((< ihs *ihs-base*)
       (format *debug-io* "Search for ~a failed.~%" string))
    (when (and (ihs-visible ihs)
               (search string (symbol-name fname) :test #'char-equal))
      (break-go ihs)
      (return))))

(defun break-forward-search-stack (sym &aux string)
  (setq string (string sym))
  (do* ((ihs (1+ *current-ihs*) (1+ ihs))
        (fname (ihs-fname ihs) (ihs-fname ihs)))
      ((> ihs *ihs-top*)
       (format *debug-io* "Search for ~a failed.~%" string))
    (when (and (ihs-visible ihs)
               (search string (symbol-name fname) :test #'char-equal))
      (break-go ihs)
      (return))))

(defun break-resume ()
  (if *debug-continue* 
      (invoke-restart *debug-continue*)
    :resume))

(putprop :b 'simple-backtrace 'break-command)
(putprop :r 'break-resume 'break-command)
(putprop :resume (get :r 'break-command) 'break-command)
(putprop :bds 'break-bds 'break-command)
(putprop :blocks 'break-blocks 'break-command)
(putprop :bs 'break-backward-search-stack 'break-command)
(putprop :c 'break-current 'break-command)
(putprop :fs 'break-forward-search-stack 'break-command)
(putprop :functions 'break-functions 'break-command)
(putprop :go 'break-go 'break-command)
(putprop :h 'break-help 'break-command)
(putprop :help 'break-help 'break-command)
(putprop :ihs 'ihs-backtrace 'break-command)
(putprop :env '(lambda () (describe-environment *break-env*)) 'break-command)
(putprop :m 'break-message 'break-command)
(putprop :n 'break-next 'break-command)
(putprop :p 'break-previous 'break-command)
(putprop :q 'break-quit 'break-command)
(putprop :s 'break-backward-search-stack 'break-command)
(putprop :vs 'break-vs 'break-command)

(defun break-help ()
  (dolist (v '( "
Break-loop Command Summary ([] indicates optional arg)
--------------------------

:bl [j]     show local variables and their values, or segment of vs if compiled
              in j stack frames starting at the current one.
:bt [n]     BACKTRACE [n steps]
:down [i]   DOWN i frames (one if no i)
:env        describe ENVIRONMENT of this stack frame (for interpreted).
:fr [n]     show frame n
:loc [i]    return i'th local of this frame if its function is compiled (si::loc i)
"
":r          RESUME (return from the current break loop).
:up [i]     UP i frames (one if no i)

Example: print a bactrace of the last 4 frames

>>:bt 4

Note:  (use-fast-links nil) makes all non system function calls
be recorded in the stack.   (use-fast-links t) is the default

Low level commands:
------------------
:p [i]           make current the i'th PREVIOUS frame (in list show by :b)
:n [i]           make current the i'th NEXT frame (in list show by :b)
:go [ihs-index]  make current the frame corresponding ihs-index
"
":m               print the last break message.
:c               show function of the current ihs frame.
:q [i]           quit to top level
:r               resume from this break loop.
:b               full backtrace of all functions and special forms.
:bs [name]       backward search for frame named 'name'
:fs  [name]      search for frame named 'name'
:vs [from] [to]  Show value stack between FROM and TO
:ihs [from] [to] Show Invocation History Stack
"
"
:bds ['v1 'v2 ..]Show previous special bindings of v1, v2,.. or all if no v1

")) (format  *debug-io* v))
  (format *debug-io* "~%Here is a COMPLETE list of bindings.   Too
add a new one, add a 'si::break-command property:")
  (do-symbols (v (find-package "KEYWORD"))
	      (cond ((get v 'si::break-command)
		     (format  *debug-io*
			      "~%~(~a -- ~a~)" v (get v 'si::break-command)))))
	  (values)
	  )


;;make sure '/' terminated

(defun coerce-slash-terminated (v )
  (declare (string v))
  (or (stringp v) (error "not a string ~a" v))
  (let ((n (length v)))
    (declare (fixnum n))
    (unless (and (> n 0) (eql
			  (the character(aref v (the fixnum (- n 1)))) #\/))
	    (setf v (format nil "~a/" v))))
  v)
(defun fix-load-path (l)
  (when (not (equal l *fixed-load-path*))
      (do ((x l (cdr x)) )
	  ((atom x))
	  (setf (car x) (coerce-slash-terminated (car x))))
      (do ((v l (cdr v)))
	  ((atom v))
	  (do ((w v (cdr w)))
	      ((atom (cdr w)))
	      (cond ((equal (cadr w) (car v))
		     (setf (cdr w)(cddr w)))))))
  (setq *fixed-load-path* l))

(defun file-search (NAME &optional (dirs *load-path*)
			  (extensions *load-types*) (fail-p t) &aux  tem)
  "Search for NAMME in DIRS with EXTENSIONS.
First directory is checked for first name and all extensions etc."
  (fix-load-path dirs)
  (dolist (v dirs)
      (dolist (e extensions)
	  (if (probe-file (setq tem (si::string-concatenate v name e)))
	    (return-from file-search tem))))
  (if fail-p
      (let ((*path* nil))
	(declare (special *path*))
	(cerror
	 "Do (setq si::*path* \"pathname\") for path to use then :r to continue"
	 "Lookup failed in directories:~s for name ~s with extensions ~s"
	 dirs name extensions)
	*path*)))

(defun aload (path)
  (load (file-search path *load-path* *load-types*)))

(defun autoload (sym path &aux (si::*ALLOW-GZIPPED-FILE* t))
  (or (fboundp sym)
      (setf (symbol-function sym)
	    #'(lambda (&rest l)
		(aload path)
		(apply sym l)))))

(defun autoload-macro (sym path &aux (si::*ALLOW-GZIPPED-FILE* t))
  (or (fboundp sym)
      (setf (macro-function sym)
	    #'(lambda (form env)
		(aload path)
		(funcall sym form env)))))

(eval-when (compile) (proclaim '(optimize (safety 0))) )
(defvar si::*command-args* nil)

(defvar *tmp-dir*)

(defun wine-tmp-redirect ()
  (let* ((s (find-symbol "*WINE-DETECTED*" (find-package "SYSTEM"))))
    (when (and s (symbol-value s))
      (list *system-directory*))))
	 

(defun get-temp-dir nil
 (dolist (x `(,@(wine-tmp-redirect) ,@(mapcar 'getenv '("TMPDIR" "TMP" "TEMP")) "/tmp" ""))
   (when (or (stringp x) (pathnamep x))
     (let* ((x (truename (pathname x)))
	    (y (namestring (make-pathname :name (pathname-name x) :type (pathname-type x) :version (pathname-version x))))
	    (y (unless (zerop (length y)) (list y))))
       (when (eq :directory (car (stat x)))
	 (return-from get-temp-dir 
	   (namestring 
	    (make-pathname 
	     :device (pathname-device x)
	     :directory (append (pathname-directory x) y)))))))))

(defun set-up-top-level (&aux (i (argc)) tem)
  (declare (fixnum i))
  (setq *tmp-dir* (get-temp-dir))
  (dotimes (j i) (push (argv j) tem))
  (setq *command-args* (nreverse tem))
  (setq tem *lib-directory*)
  (process-some-args *command-args*)
  (unless *lib-directory*
    (let ((dir (getenv "GCL_LIBDIR")))
      (when dir
	(setq *lib-directory* (coerce-slash-terminated dir)))))
  (unless (and *load-path* (equal tem *lib-directory*))
    (setq *load-path* (cons (string-concatenate *lib-directory* "lsp/") *load-path*))
    (setq *load-path* (cons (string-concatenate *lib-directory* "gcl-tk/") *load-path*))
    (setq *load-path* (cons (string-concatenate *lib-directory*  "xgcl-2/") *load-path*)))
  (unless (boundp '*system-directory*)
    (setq *system-directory* (namestring (truename (make-pathname :name nil :type nil :defaults (argv 0))))))))

(defvar *old-top-level* #'top-level)

(defun gcl-top-level nil
  
  (set-up-top-level)
  
  (in-package :user)
  (setq *ihs-top* (ihs-top))
  (funcall *old-top-level*))
 
(defun do-f (file &aux *break-enable*)
  (catch *quit-tag*
    (labels ((read-loop (st &aux (tem (read st nil 'eof))) (when (eq tem 'eof) (bye)) (eval tem) (read-loop st))
	     (read-file (st) (read-line st nil 'eof) (read-loop st)))
	    (if file
		(with-open-file
		 (st file)
		 (read-file st))
	      (read-file *standard-input*))))
  (bye 1))
