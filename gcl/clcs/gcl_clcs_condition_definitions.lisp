;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: "CONDITIONS"; Base: 10 -*-

(IN-PACKAGE "CONDITIONS")

(eval-when (compile load eval)
  (pushnew :clos-conditions *features*))

(eval-when (compile load eval)
  (when (and (member :clos-conditions *features*)
	     (member :defstruct-conditions *features*))
    (dolist (sym '(simple-condition-format-control simple-condition-format-arguments
						  type-error-datum type-error-expected-type
						  case-failure-name case-failure-possibilities
						  stream-error-stream file-error-pathname package-error-package
						  cell-error-name arithmetic-error-operation
						  internal-error-function-name))
      (when (fboundp sym) (fmakunbound sym)))
    (setq *features* (remove :defstruct-conditions *features*))))

(define-condition warning (condition) ())
(define-condition style-warning (warning) ())

(define-condition serious-condition (condition) ())
(define-condition error (serious-condition) ())

(define-condition simple-condition (condition)
  ((format-control :type string
		  :initarg :format-control
		  :reader simple-condition-format-control
		  :initform "")
   (format-arguments :initarg :format-arguments
		     :reader simple-condition-format-arguments
		     :initform nil))
  (:report (lambda (c s) 
	     (call-next-method)
	     (apply 'format s 
		    (simple-condition-format-control c)
		    (simple-condition-format-arguments c)))))

(define-condition simple-warning (simple-condition warning) ())
(define-condition simple-error (simple-condition error) ())

(define-condition storage-condition (serious-condition) ())
(define-condition stack-overflow    (storage-condition) ())
(define-condition storage-exhausted (storage-condition) ())

(define-condition type-error (error)
  ((datum :initarg :datum :reader type-error-datum)
   (expected-type :initarg :expected-type :reader type-error-expected-type))
  (:report ("~%~s is not of type ~s." datum expected-type)))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition case-failure (type-error)
 ((name :initarg :name :reader case-failure-name)
  (possibilities :initarg :possibilities
		 :reader case-failure-possibilities))
  (:report ("~%~s fell through ~s expression.~%wanted one of ~:s." datum name possibilities)))

(define-condition PROGRAM-ERROR (ERROR) ())
(define-condition control-error (error) ())
(define-condition parse-error (error) ())

(define-condition print-not-readable (error) 
  ((object :initarg :object :reader print-not-readable-object))
  (:report ("~%Object ~s is unreadable: " object)))

(define-condition stream-error (error)
  ((stream :initarg :stream :reader stream-error-stream))
  (:report ("~%Stream error on stream ~s: " stream)))

(define-condition reader-error (parse-error stream-error) ())

(define-condition end-of-file (stream-error)
  ()
  (:report ("~%Unexpected end of file:")))

(define-condition file-error (error)
  ((pathname :initarg :pathname :reader file-error-pathname))
  (:report ("~%File error on ~s:" pathname)))
(define-condition pathname-error (file-error) ())

(define-condition package-error (error)
  ((package :initarg :package :reader package-error-package))
  (:report ("~%Package error on ~s: " package)))
	      

(define-condition cell-error (error)
  ((name :initarg :name :reader cell-error-name))
  (:report ("~%Cell error on ~s: " name)))

(define-condition unbound-variable (cell-error)
  ()
  (:report ("~%Unbound variable.")))
  
(define-condition unbound-slot (cell-error)
  ((instance :initarg :instance :reader unbound-slot-instance))
  (:report ("~%Slot is unbound in ~s: " instance)))

(define-condition undefined-function (cell-error) nil
  (:report ("~%Undefined function.")))

(define-condition arithmetic-error (error)
  ((operation :initarg :operation :reader arithmetic-error-operation)
   (operands :initarg :operands :reader arithmetic-error-operands))
  (:report ("~%Arithmetic error when performing ~s on ~s: " operation operands)))

(define-condition division-by-zero (arithmetic-error) ())
(define-condition floating-point-overflow (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())
(define-condition floating-point-inexact (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())

(define-condition abort-failure (control-error) () (:report "~%Abort failed."))

(define-condition internal-condition (condition)
  ((function-name :initarg :function-name :reader internal-condition-function-name
		  :initform nil))
  (:report (lambda (condition stream)
	     (when (internal-condition-function-name condition)
	       (format stream "Condition in ~S [or a callee]: "
		       (internal-condition-function-name condition)))
	     (call-next-method))))

(define-condition internal-warning (internal-condition warning)
  ()
  (:report (lambda (condition stream)
	     (when (internal-condition-function-name condition)
	       (format stream "Warning in ~S [or a callee]: "
		       (internal-condition-function-name condition)))
	     (call-next-method))))

(define-condition internal-error (internal-condition error)
  ()
  (:report (lambda (condition stream)
	     (when (internal-condition-function-name condition)
	       (format stream "Error in ~S [or a callee]: "
		       (internal-condition-function-name condition)))
	     (call-next-method))))

(define-condition internal-simple-condition (internal-condition simple-condition) ())
(define-condition internal-simple-error (internal-simple-condition error) ())
(define-condition internal-simple-warning (internal-simple-condition warning) ())

(defun symcat (x y) (values (intern (concatenate 'string (string x) (string y)) 'conditions)))

#.`(progn 
     ,@(mapcar (lambda (x) 
		 `(define-condition ,(symcat "INTERNAL-SIMPLE-" x)  (internal-simple-condition ,x) ())) 
	       `(stack-overflow storage-exhausted print-not-readable end-of-file style-warning type-error
				unbound-variable unbound-slot undefined-function division-by-zero
				case-failure abort-failure
				,@(mapcar (lambda (x) (symcat "FLOATING-POINT-" x)) 
					  '(overflow underflow invalid-operation inexact))
				,@(mapcar (lambda (x) (symcat x "-ERROR"))
					  '(program control parse stream reader file
						    package cell arithmetic pathname)))))



(defvar *simple-condition-class* (find-class 'simple-condition))
(defvar *internal-simple-condition-class* (find-class 'internal-simple-condition))

(defun simple-condition-class-p (type)
  (let ((type (if (symbolp type) (find-class type nil) type)))
    (when (typep type 'standard-class)
      (member *simple-condition-class* 
	      (pcl::class-precedence-list type)))))

(defun internal-simple-condition-class-p (type)
  (when (symbolp type)
    (setq type (find-class type)))
  (and (typep type 'standard-class)
       (member *internal-simple-condition-class* 
	       (pcl::class-precedence-list type))))


