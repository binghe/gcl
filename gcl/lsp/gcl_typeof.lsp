(in-package :si)

(defun array-type-of (array-tp array)
  (list array-tp (nth (c-array-elttype array) +array-types+) (array-dimensions array)))

(defun simple-array-type-of (array) (array-type-of 'simple-array array))
(defun non-simple-array-type-of (array) (array-type-of 'non-simple-array array))

(defun integer-type-of (x) `(integer ,x ,x))
(defun ratio-type-of (x) `(ratio ,x ,x))
(defun short-float-type-of (x) `(short-float ,x ,x))
(defun long-float-type-of (x) `(long-float ,x ,x))

(defun complex-type-of (cmp)
  (declare (complex cmp));FIXME
  `(complex* ,(type-of (realpart cmp)) ,(type-of (imagpart cmp))))

(defun structure-type-of (str)
  (sdata-name (c-structure-def str)))

(defun valid-class-name (class &aux (name (si-class-name class)))
  (when (eq class (si-find-class name nil))
    name))
(setf (get 'valid-class-name 'cmp-inline) t)

(defun std-instance-type-of (x)
  (let* ((c (si-class-of x))) (or (valid-class-name c) c)))

(defun cons-type-of (x);recurse?
  (if (improper-consp x) 'improper-cons 'proper-cons))


(defconstant +vtps+ (mapcar (lambda (x) (list x (intern (string-concatenate "VECTOR-"  (string x))))) +array-types+))
(defconstant +atps+ (mapcar (lambda (x) (list x (intern (string-concatenate "ARRAY-"   (string x))))) +array-types+))
(defconstant +vtpsn+ `((nil vector-nil) ,@+vtps+))
(defconstant +atpsn+ `((nil array-nil) ,@+atps+))


(defun real-rep (x)
  (case x (integer 1) (ratio 1/2) (short-float 1.0s0) (long-float 1.0)))

(defun complex-rep (x)
  (let* ((s (symbolp x))
	 (r (real-rep (if s x (car x))))
	 (i (real-rep (if s x (cadr x)))))
    (complex r i)))

(defconstant +nr+ `((immfix 1 integer) 
		   (bfix  most-positive-fixnum integer)
		   (bignum (1+ most-positive-fixnum) integer)
		   (ratio 1/2 ratio)
		   (short-float 1.0s0 short-float) 
		   (long-float 1.0 long-float)
		   ,@(mapcar (lambda (x &aux (v (complex-rep (car x))))
			       `(,(cadr x) ,v complex)) +ctps+)
		   (standard-char #\a)
		   (non-standard-base-char #\Return)
		   (structure (make-dummy-structure) structure) 
		   (std-instance (set-d-tt 1 (make-dummy-structure)) std-instance) 
		   (non-logical-pathname (init-pathname nil nil nil nil nil nil ""))
		   (logical-pathname (set-d-tt 1 (init-pathname nil nil nil nil nil nil "")))
		   (hash-table-eq (make-hash-table :test 'eq))
		   (hash-table-eql (make-hash-table :test 'eql))
		   (hash-table-equal (make-hash-table :test 'equal))
		   (hash-table-equalp (make-hash-table :test 'equalp))
		   (package *package*)
		   (file-input-stream (let ((s (open-int "/dev/null" :input 'character nil nil nil nil :default))) (close s) s))
		   (file-output-stream (let ((s (open-int "/dev/null" :output 'character nil nil nil nil :default))) (close s) s))
		   (file-io-stream (let ((s (open-int "/dev/null" :io 'character nil nil nil nil :default))) (close s) s))
		   (file-probe-stream (let ((s (open-int "/dev/null" :probe 'character nil nil nil nil :default))) (close s) s))
		   (file-synonym-stream (make-synonym-stream '*standard-output*))
		   (non-file-synonym-stream *debug-io*);FIXME
		   (broadcast-stream (make-broadcast-stream))
		   (concatenated-stream (make-concatenated-stream))
		   (two-way-stream *terminal-io*)
		   (echo-stream (make-echo-stream *standard-output* *standard-output*))
		   (string-input-stream (make-string-input-stream ""))
		   (string-output-stream (make-string-output-stream));FIXME user defined, socket
		   (random-state (make-random-state)) 
		   (readtable (standard-readtable)) 
		   (non-standard-generic-compiled-function (function eq))
		   (non-standard-generic-interpreted-function (set-d-tt 2 (lambda nil nil)))
		   (standard-generic-compiled-function (set-d-tt 1 (lambda nil nil)))
		   (standard-generic-interpreted-function (set-d-tt 3 (lambda nil nil)))
		   ,@(mapcar (lambda (x)
			       `((simple-array ,(car x) 1)
				 (make-vector ',(car x) 1 nil nil nil 0 nil nil)
				 simple-array)) +vtps+)
		   ,@(mapcar (lambda (x)
			       `((matrix ,(car x))
				 (make-array1 ',(car x) nil nil nil 0 '(1 1) t)
				 simple-array)) +atps+)
		   ((non-simple-array character)
		    (make-vector 'character 1 t nil nil 0 nil nil)
		    non-simple-array)
		   ((non-simple-array bit)
		    (make-vector 'bit 1 t nil nil 0 nil nil)
		    non-simple-array)
		   ((non-simple-array t)
		    (make-vector 't 1 t nil nil 0 nil nil)
		    non-simple-array)
                   (spice (alloc-spice))
		   (cons '(1) cons)
		   (keyword :a)
		   (null nil)
		   (true t)
		   (gsym 'a)))


(defconstant +tfns1+ '(tp0 tp1 tp2 tp3 tp4 tp5 tp6 tp7 tp8))

#.`(progn
     ,@(let ((x (lreduce (lambda (y x) (if (> (cadr x) (cadr y)) x y))
	   (mapcar (lambda (x &aux (z (lremove-duplicates
				       (mapcar (lambda (q) (funcall x (eval (cadr q)))) +nr+))))
		     (list x (length z) (lreduce 'min z) (lreduce 'max z)))
		   +tfns1+) :initial-value (list nil 0))))
	 (unless (eql (cadr x) (length +nr+))
	   (print "type-of functions too general"))
	 `((defvar *type-of-dispatch*
	     (make-vector t ,(1+ (- (cadddr x) (caddr x))) nil nil nil 0 nil nil))
	   (defmacro tp7-ind (x) `(- (,',(car x) ,x) ,,(caddr x))))))


(mapc (lambda (x &aux (z (caddr x)))
	(setf (aref *type-of-dispatch* (tp7-ind (eval (cadr x))))
	      (if z (symbol-function (intern (string-concatenate (string z) "-TYPE-OF")))
		(car x))))
      +nr+)
		
(defun type-of (x &aux (z (aref *type-of-dispatch* (tp7-ind x))))
  (declare ((vector t) *type-of-dispatch*));FIXME
  (if (functionp z) (values (funcall z x)) z))
