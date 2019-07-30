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


;;;;   iolib.lsp
;;;;
;;;;        The IO library.


;; (in-package 'lisp)

;; (export '(with-open-stream with-input-from-string with-output-to-string parse-integer))
;; (export '(read-from-string))
;; (export '(write-to-string prin1-to-string princ-to-string))
;; (export 'file-string-length)
;; (export 'with-open-file)
;; (export '(y-or-n-p yes-or-no-p))
;; (export 'dribble)
;; (export 'with-standard-io-syntax)
;; (export 'logical-pathname-translations)
;; (export 'load-logical-pathname-translations)
;; (export 'formatter)
;; (export 'pprint-dispatch)
;; (export 'set-pprint-dispatch)
;; (export 'copy-pprint-dispatch)
;; (export 'ensure-directories-exist) ; from ECLS
;; (export 'print-unreadable-object) ; from ECLS
;; (export 'with-compilation-unit)
;; (export '(concatenated-stream-streams 
;; 	  broadcast-stream-streams 
;; 	  two-way-stream-input-stream
;; 	  echo-stream-input-stream
;; 	  two-way-stream-output-stream
;; 	  echo-stream-output-stream
;; 	  synonym-stream-symbol
;; 	  read-byte
;; 	  write-byte
;; 	  read-sequence
;; 	  write-sequence
;; 	  open))

(in-package :system)

(defun concatenated-stream-streams (stream)
  (declare (optimize (safety 2)))
  (check-type stream concatenated-stream)
  (c-stream-object0 stream))
(defun broadcast-stream-streams (stream)
  (declare (optimize (safety 2)))
  (check-type stream broadcast-stream)
  (c-stream-object0 stream))
(defun two-way-stream-input-stream (stream)
  (declare (optimize (safety 2)))
  (check-type stream two-way-stream)
  (c-stream-object0 stream))
(defun echo-stream-input-stream (stream)
  (declare (optimize (safety 2)))
  (check-type stream echo-stream)
  (c-stream-object0 stream))
(defun two-way-stream-output-stream (stream)
  (declare (optimize (safety 2)))
  (check-type stream two-way-stream)
  (c-stream-object1 stream))
(defun echo-stream-output-stream (stream)
  (declare (optimize (safety 2)))
  (check-type stream echo-stream)
  (c-stream-object1 stream))
(defun synonym-stream-symbol (stream)
  (declare (optimize (safety 2)))
  (check-type stream synonym-stream)
  (c-stream-object0 stream))

(defun maybe-clear-input (&optional (x *standard-input*))
  (cond ((not (typep x 'stream)) nil)
	((typep x 'synonym-stream) (maybe-clear-input (symbol-value (synonym-stream-symbol x))))
	((typep x 'two-way-stream) (maybe-clear-input (two-way-stream-input-stream x)))
	((terminal-input-stream-p x) (clear-input t))))

(defun decl-vars (decls);FIXME complete and centralize
  (remove-duplicates
   (mapcan (lambda (x) 
	     (when (eq (car x) 'declare)
	       (mapcan (lambda (x) (cond ((member (car x) '(type ftype)) (cddr x))
					 ((member (car x) '(optimize)) nil)
					 ((cdr x)))) (cdr x)))) (copy-tree decls))))


(defmacro with-open-stream ((var stream) . body)
;  (declare (optimize (safety 2)))
  (multiple-value-bind 
   (ds b)
   (find-declarations body)
   `(let* (,@(mapcar (lambda (x) (list x x)) (remove var (decl-vars ds)))
	     (,var ,stream))
      ,@ds
      (unwind-protect
	  (progn ,@b)
	(close ,var)))))



(defmacro with-input-from-string ((var string &key index start end) . body)
;  (declare (optimize (safety 2)))
  (multiple-value-bind 
   (ds b)
   (find-declarations body)
   (let ((r (gensym)))
     `(let* (,@(mapcar (lambda (x) (list x x)) (remove var (decl-vars ds)))
	       (,var (make-string-input-stream ,string ,start ,end)))
	,@ds 
	(let ((,r (multiple-value-list (progn ,@b))))
	  ,@(when index `((setf ,index (si:get-string-input-stream-index ,var))))
	  (values-list ,r))))))


(defmacro with-output-to-string ((var &optional string &key element-type) . body)
;  (declare (optimize (safety 2)))
  (multiple-value-bind 
   (ds b)
   (find-declarations body)
   (let ((e (gensym)))
     `(let* (,@(mapcar (lambda (x) (list x x)) (remove var (decl-vars ds)))
	      (,var ,(if string `(make-string-output-stream-from-string ,string) `(make-string-output-stream))))
	,@ds 
	(let (,@(when element-type `((,e ,element-type))));FIXME
	  ,@b
	  ,@(unless string `((get-output-stream-string ,var))))))))


(defun read-from-string (string
                         &optional (eof-error-p t) eof-value
                         &key (start 0) (end nil end-p) preserve-whitespace)
  (declare (optimize (safety 2)))
  (check-type string string)
  (unless end-p (setq end (length string)))
  (let ((stream (make-string-input-stream string start end)))
    (if preserve-whitespace
        (values (read-preserving-whitespace stream eof-error-p eof-value)
                (si:get-string-input-stream-index stream))
        (values (read stream eof-error-p eof-value)
                (si:get-string-input-stream-index stream)))))


(defun write (x &key stream 
		(array            *print-array*)
		(base             *print-base*)
		(case             *print-case*)
		(circle           *print-circle*)
		(escape           *print-escape*)
		(gensym           *print-gensym*)
		(length           *print-length*)
		(level            *print-level*)
		(lines            *print-lines*)
		(miser-width      *print-miser-width*)
		(pprint-dispatch  *print-pprint-dispatch*)
		(pretty           *print-pretty*)
		(radix            *print-radix*)
		(readably         *print-readably*)
		(right-margin     *print-right-margin*))
  (write-int x stream array base case circle escape gensym
	     length level lines miser-width pprint-dispatch
	     pretty radix readably right-margin))

(defun write-to-string (object &rest rest &key
			    ( escape nil escape-supplied-p )
			    ( radix nil radix-supplied-p )
			    ( base nil base-supplied-p )
			    ( circle nil circle-supplied-p )
			    ( pretty nil pretty-supplied-p )
			    ( level nil level-supplied-p )
			    ( length nil length-supplied-p )
			    ( case nil case-supplied-p )
			    ( gensym nil gensym-supplied-p )
			    ( array nil array-supplied-p )
			    ( lines nil lines-supplied-p )
			    ( miser-width nil miser-width-supplied-p )
			    ( pprint-dispatch nil pprint-dispatch-supplied-p )
			    ( readably nil readably-supplied-p )
			    ( right-margin nil right-margin-supplied-p )
                        &aux (stream (make-string-output-stream)))
  (declare (optimize (safety 2)))
  (let*((*print-array*
	  (if array-supplied-p array *print-array*))
	(*print-base*
	  (if base-supplied-p base *print-base*))
	(*print-case*
	  (if case-supplied-p case *print-case*))
	(*print-circle*
	  (if circle-supplied-p circle *print-circle*))
	(*print-escape*
	  (if escape-supplied-p escape *print-escape*))
	(*print-gensym*
	  (if gensym-supplied-p gensym *print-gensym*))
	(*print-length*
	  (if length-supplied-p length *print-length*))
	(*print-level*
	  (if level-supplied-p level *print-level*))
	(*print-lines*
	  (if lines-supplied-p lines *print-lines*))
	(*print-miser-width*
	  (if miser-width-supplied-p miser-width *print-miser-width*))
	(*print-pretty*
	  (if pretty-supplied-p pretty *print-pretty*))
	(*print-radix*
	  (if radix-supplied-p radix *print-radix*))
	(*print-readably*
	  (if readably-supplied-p readably *print-readably*))
	(*print-right-margin*
	  (if right-margin-supplied-p right-margin *print-right-margin*))
	(*print-pprint-dispatch*
	  (if pprint-dispatch-supplied-p pprint-dispatch *print-pprint-dispatch*)))
      (apply #'write object :stream stream rest)
      (get-output-stream-string stream)))

(defun prin1-to-string (object
                        &aux (stream (make-string-output-stream)))
  (declare (optimize (safety 2)))
  (prin1 object stream)
  (get-output-stream-string stream))


(defun princ-to-string (object
                        &aux (stream (make-string-output-stream)))
  (declare (optimize (safety 2)))
  (princ object stream)
  (get-output-stream-string stream))

(defun file-string-length (ostream object)
  (declare (optimize (safety 2)))
  (let ((ostream (if (typep ostream 'broadcast-stream) 
		     (car (last (broadcast-stream-streams ostream)))
		   ostream)))
    (cond ((not ostream) 1)
	  ((subtypep1 (stream-element-type ostream) 'character)
	   (length (let ((*print-escape* nil)) (write-to-string object)))))))

(defmacro with-temp-file ((s pn) (tmp ext) &rest body) 
  (multiple-value-bind
   (doc decls ctps body)
   (parse-body-header body)
   (declare (ignore doc))
   `(let* ((,s (temp-stream ,tmp ,ext)) 
	   (,pn (stream-object1 ,s))) 
      ,@decls
      ,@ctps
      (unwind-protect (progn ,@body) (progn (close ,s) (delete-file ,s))))))

(defmacro with-open-file ((stream . filespec) . body)
;  (declare (optimize (safety 2)))
  (multiple-value-bind 
   (ds b)
   (find-declarations body)
   `(let* (,@(mapcar (lambda (x) (list x x)) (remove stream (decl-vars ds)))
	     (,stream (open ,@filespec)))
      ,@ds 
      (unwind-protect
	  (progn ,@b)
	(if ,stream (close ,stream))))))

(defun pprint-dispatch (obj &optional (table *print-pprint-dispatch*))
  (declare (optimize (safety 2)))
  (let ((fun (si:get-pprint-dispatch obj table)))
    (if fun (values fun t) (values 'si:default-pprint-object nil))))

(setq *print-pprint-dispatch* '(pprint-dispatch . nil))

(defun set-pprint-dispatch (type-spec function &optional
			    (priority 0)
			    (table *print-pprint-dispatch*))
  (declare (optimize (safety 2)))
  (unless (typep priority 'real)
    (error 'type-error :datum priority :expected-type 'real))
  (let ((a (assoc type-spec (cdr table) :test 'equal)))
    (if a (setf (cdr a) (list function priority))
	(rplacd (last table) `((,type-spec ,function ,priority)))))
  nil)

(defun copy-pprint-dispatch (&optional table)
  (declare (optimize (safety 2)))
  (unless table
    (setq table *print-pprint-dispatch*))
  (unless (and (eq (type-of table) 'cons)
  	(eq (car table) 'pprint-dispatch))
    (error 'type-error :datum table :expected-type 'pprint-dispatch))
  (copy-seq table ))

(defun y-or-n-p (&optional string &rest args)
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Y or N) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "Y")
           (return-from y-or-n-p t))
          ((string-equal (symbol-name reply) "N")
           (return-from y-or-n-p nil)))))


(defun yes-or-no-p (&optional string &rest args)
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Yes or No) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "YES")
           (return-from yes-or-no-p t))
          ((string-equal (symbol-name reply) "NO")
           (return-from yes-or-no-p nil)))))

(defun sharp-a-reader (stream subchar arg)
  (declare (ignore subchar) (optimize (safety 2)))
  (let ((initial-contents (read stream nil nil t)))
    (unless *read-suppress*
      (do ((i 0 (1+ i))
	   (d nil (cons (length ic) d))
	   (ic initial-contents (if (zerop (length ic)) ic (elt ic 0))))
	  ((>= i arg)
	   (make-array (nreverse d)
		       :initial-contents initial-contents))))))

(set-dispatch-macro-character #\# #\a 'sharp-a-reader)
(set-dispatch-macro-character #\# #\A 'sharp-a-reader)

;; defined in defstruct.lsp
(set-dispatch-macro-character #\# #\s 'sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'sharp-s-reader)

(defvar *dribble-stream* nil)
(defvar *dribble-io* nil)
(defvar *dribble-namestring* nil)
(defvar *dribble-saved-terminal-io* nil)

(defun dribble (&optional (pathname "DRIBBLE.LOG" psp) (f :supersede))
  (declare (optimize (safety 2)))
  (cond ((not psp)
         (when (null *dribble-stream*) (error "Not in dribble."))
         (if (eq *dribble-io* *terminal-io*)
             (setq *terminal-io* *dribble-saved-terminal-io*)
             (warn "*TERMINAL-IO* was rebound while DRIBBLE is on.~%~
                   You may miss some dribble output."))
         (close *dribble-stream*)
         (setq *dribble-stream* nil)
         (format t "~&Finished dribbling to ~A." *dribble-namestring*))
        (*dribble-stream*
         (error "Already in dribble (to ~A)." *dribble-namestring*))
        (t
         (let* ((namestring (namestring pathname))
                (stream (open pathname :direction :output
                                       :if-exists f
                                       :if-does-not-exist :create)))
           (setq *dribble-namestring* namestring
                 *dribble-stream* stream
                 *dribble-saved-terminal-io* *terminal-io*
                 *dribble-io* (make-two-way-stream
                               (make-echo-stream *terminal-io* stream)
                               (make-broadcast-stream *terminal-io* stream))
                 *terminal-io* *dribble-io*)
           (multiple-value-bind (sec min hour day month year)
               (get-decoded-time)
             (format t "~&Starts dribbling to ~A (~d/~d/~d, ~d:~d:~d)."
                     namestring year month day hour min sec))))))

; simple formatter macro

(defmacro formatter ( control-string )
  (declare (optimize (safety 2)))
  `(progn
     (lambda (*standard-output* &rest arguments)                                
       (let ((*format-unused-args* nil))
	 (apply 'format t ,control-string arguments)
	 *format-unused-args*))))

(defun stream-external-format (s)
  (declare (optimize (safety 1)))
  (check-type s stream)
  :default)

;;; copied from ECL under LGPL by Michael Koehne
;;;    with-standard-io-syntax


(defmacro with-standard-io-syntax (&body body)
  (declare (optimize (safety 2)))
  `(let* ((*package* (find-package :cl-user))
	  (*print-array* t)
	  (*print-base* 10)
	  (*print-case* :upcase)
	  (*print-circle* nil)
	  (*print-escape* t)
	  (*print-gensym* t)
	  (*print-length* nil)
	  (*print-level* nil)
	  (*print-lines* nil)
	  (*print-miser-width* nil)
	  (*print-pprint-dispatch* *print-pprint-dispatch*);FIXME
	  (*print-pretty* nil)
	  (*print-radix* nil)
	  (*print-readably* t)
	  (*print-right-margin* nil)
	  (*read-base* 10)
	  (*read-default-float-format* 'single-float)
	  (*read-eval* t)
	  (*read-suppress* nil)
	  (*readtable* (copy-readtable (si::standard-readtable))));FIXME copy?
     ,@body))

(defmacro print-unreadable-object
	  ((object stream &key type identity) &body body)
  (declare (optimize (safety 2)))
  (let ((q `(princ " " ,stream)))
    `(if *print-readably* 
	 (error 'print-not-readable :object ,object)
       (progn
	 (princ "#<" ,stream)
	 ,@(when type `((prin1 (type-of ,object) ,stream) ,q))
	 ,@body
	 ,@(when identity
	     (let ((z `(princ (address ,object) ,stream)))
	       (if (and (not body) type) (list z) (list q z))))
	 (princ ">" ,stream)
	 nil))))
;     (print-unreadable-object-function ,object ,stream ,type ,identity ,(when body `(lambda nil ,@body)))))

; i know this should be in cmpnew - but its easier here.

(defmacro with-compile-file-syntax (&body body)
  `(let ((*print-radix* nil)
	 (*print-base* 10)
	 (*print-circle* t)
	 (*print-pretty* nil)
	 (*print-level* nil)
	 (*print-length* nil)
	 (*print-case* :downcase)
	 (*print-gensym* t)
	 (*print-array* t)
	 (*print-package* t)
	 (*print-structure* t))
     ,@body))

(defmacro with-compilation-unit (opt &rest body)   
  (declare (optimize (safety 2)))
  (declare (ignore opt)) 
  `(let ((res (multiple-value-list (let ((*disable-recompile* t)) ,@body))))
     (do-recompile nil)
     (values-list res)))

(defun get-byte-stream-nchars (s)
  (check-type s stream)
  (let* ((tp (stream-element-type s))
	 (tp (if (consp tp) (cadr tp) char-length))
	 (nc (ceiling tp char-length)))
    nc))

(defun parse-integer (s &key start end (radix 10) junk-allowed)
  (declare (optimize (safety 1)))
  (parse-integer-int s start end radix junk-allowed))


(defun write-byte (j s)
  (declare (optimize (safety 2)))
  (let ((nc (get-byte-stream-nchars s))
	(ff (1- (expt 2 char-length))))
    (do ((k 0 (1+ k))(i j (ash i (- char-length)))) ((= k nc) j)
	(write-char (code-char (logand i ff)) s))))

(defun read-byte (s &optional (eof-error-p t) eof-value)
  (declare (optimize (safety 2)))
  (let ((nc (get-byte-stream-nchars s)))
    (do ((j 0 (1+ j)) 
	 (i 0 (logior i
	       (ash (char-code (let ((ch (read-char s eof-error-p eof-value)))
				 (if (and (not eof-error-p) (eq ch eof-value))
				     (return-from read-byte ch)
				   ch))) (* j char-length)))))
	((= j nc) i))))


(defun read-sequence (seq strm &key (start 0) (end nil))
  (declare (optimize (safety 2)))
  (check-type seq sequence)
  (check-type start (integer 0))
  (when end (check-type end (integer 0)))
  (let* ((end (or end (length seq)))
	 (seq (if (and (consp seq) (> start 0)) (nthcdr start seq) seq))
	 (tp (stream-element-type strm)))
    (if (eq tp 'character)
	(if (consp seq)
	    (do ((i start (1+ i))(seq seq (cdr seq))) ((= i end) i) 
		(declare (seqind i))
		(setf (car seq) (let ((el (read-char strm nil 'eof)))
				  (if (eq el 'eof) (return i) el))))
	  (do ((i start (1+ i))) ((= i end) i) 
	      (declare (seqind i))
	      (setf (aref seq i) (let ((el (read-char strm nil 'eof)))
				   (if (eq el 'eof) (return i) el)))))
      (if (consp seq)
	  (do ((i start (1+ i))(seq seq (cdr seq))) ((= i end) i) 
	      (declare (seqind i))
	      (setf (car seq) (let ((el (read-byte strm nil 'eof)))
				  (if (eq el 'eof) (return i) el))))
	(do ((i start (1+ i))) ((= i end) i) 
	    (declare (seqind i))
	    (setf (aref seq i) (let ((el (read-byte strm nil 'eof)))
				 (if (eq el 'eof) (return i) el))))))))


(defun write-sequence (seq strm &key (start 0) (end nil))
  (declare (optimize (safety 2)))
  (check-type seq sequence)
  (check-type start (integer 0))
  (when end (check-type end (integer 0)))
  (let* ((end (or end (length seq)))
	 (seq (if (and (consp seq) (> start 0)) (nthcdr start seq) seq))
	 (tp (stream-element-type strm)))
    (if (eq tp 'character)
	(if (consp seq)
	    (do ((i start (1+ i))(seq seq (cdr seq))) ((= i end) i) 
		(declare (seqind i))
		(write-char (car seq) strm))
	  (do ((i start (1+ i))) ((= i end) i) 
	      (declare (seqind i))
	      (write-char (aref seq i) strm)))
      (if (consp seq)
	  (do ((i start (1+ i))(seq seq (cdr seq))) ((= i end) i) 
	      (declare (seqind i))
	      (write-byte (car seq) strm))
	(do ((i start (1+ i))) ((= i end) i) 
	    (declare (seqind i))
	    (write-byte (aref seq i) strm)))))
  seq)

(defun restrict-stream-element-type (tp)
  (cond ((member tp '(unsigned-byte signed-byte)) tp)
	((or (member tp '(character :default)) (si::subtypep1 tp 'character)) 'character)
	((si::subtypep1 tp 'integer) 
	 (let* ((ntp (car (expand-ranges (cmp-norm-tp tp))))
		(min (cadr ntp))(max (caddr ntp))
		(s (if (or (eq min '*) (< min 0)) 'signed-byte 'unsigned-byte))
		(lim (unless (or (eq min '*) (eq max '*)) (max (integer-length min) (integer-length max))))
		(lim (if (and lim (eq s 'signed-byte)) (1+ lim) lim)))
	   (if lim `(,s ,lim) s)))
	((check-type tp (member character integer)))))

(defun open (f &key (direction :input)
	       (element-type 'character)
	       (if-exists nil iesp)
	       (if-does-not-exist nil idnesp)
	       (external-format :default))
  (let* ((f (pathname f))
	 (pf (translate-logical-pathname f)))
    (when (wild-pathname-p pf)
      (error 'file-error :pathname pf :format-control "Pathname is wild."))
    (let ((s (open-int pf direction (restrict-stream-element-type element-type)
		       if-exists iesp if-does-not-exist idnesp external-format)))
      (when (typep s 'stream) (c-set-stream-object1 s f) s))))

(defun load-pathname-exists (z)
  (or (probe-file z)
      (when *allow-gzipped-file*
	(when (probe-file (string-concatenate (namestring z) ".gz"))
	  z))))

(defun load-pathname (p print if-does-not-exist external-format
			&aux (pp (merge-pathnames p))
			(epp (reduce (lambda (y x) (or y (load-pathname-exists (translate-pathname x "" p))))
				     '(#P".o" #P".lsp" #P".lisp" #P"") :initial-value nil)));FIXME newest?
  (if epp
      (let* ((*load-pathname* pp)(*load-truename* epp))
	(with-open-file
	 (s epp :external-format external-format)
	 (if (member (peek-char nil s nil 'eof) '#.(mapcar 'code-char (list 127 #xcf #xce #x4c)))
	     (load-fasl s print)
	   (let ((*standard-input* s)) (load-stream s print)))))
    (when if-does-not-exist
      (error 'file-error :pathname pp :format-control "File does not exist."))))

(defun load (p &key (verbose *load-verbose*) (print *load-print*) (if-does-not-exist :error)
	       (external-format :default) &aux (*readtable* *readtable*)(*package* *package*))
  (declare (optimize (safety 1)))
  (check-type p (or stream pathname-designator))
  (when verbose (format t ";; Loading ~s~%" p))
  (prog1
      (typecase p
	(pathname-designator (load-pathname (pathname p) print if-does-not-exist external-format))
	(stream (load-stream p print)))
    (when verbose (format t ";; Finished loading ~s~%" p))))

(defun ensure-directories-exist (ps &key verbose &aux created)
  (declare (optimize (safety 1)))
  (check-type ps pathname-designator)
  (when (wild-pathname-p ps)
    (error 'file-error :pathname ps :format-control "Pathname is wild"))
  (labels ((d (x y &aux (z (ldiff x y)) (n (namestring (make-pathname :directory z))))
	      (when (when z (stringp (car (last z))))
		(unless (eq :directory (stat n))
		  (mkdir n)
		  (setq created t)
		  (when verbose (format *standard-output* "Creating directory ~s~%" n))))
	      (when y (d x (cdr y)))))
    (let ((pd (pathname-directory ps)))
      (d pd (cdr pd)))
    (values ps created)))

