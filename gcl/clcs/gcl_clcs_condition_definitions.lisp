;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: "CONDITIONS"; Base: 10 -*-

(IN-PACKAGE :CONDITIONS)

(define-condition warning (condition) nil)
(define-condition style-warning (warning) nil)

(define-condition serious-condition (condition) nil)
(define-condition error (serious-condition) nil)

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

(define-condition simple-warning (simple-condition warning) nil)
(define-condition simple-error (simple-condition error) nil)

(define-condition storage-condition (serious-condition) nil)
(define-condition stack-overflow    (storage-condition) nil)
(define-condition storage-exhausted (storage-condition) nil)

(define-condition type-error (error)
  ((datum :initarg :datum
	  :reader type-error-datum)
   (expected-type :initarg :expected-type
		  :reader type-error-expected-type))
  (:report ("~s is not of type ~s: " datum expected-type)))
(define-condition simple-type-error (simple-error type-error) nil)

(define-condition program-error (error) nil)
(define-condition control-error (error) nil)
(define-condition parse-error (error) nil)

(define-condition print-not-readable (error) 
  ((object :initarg :object
	   :reader print-not-readable-object))
  (:report ("Object ~s is unreadable: " object)))

(define-condition stream-error (error)
  ((stream :initarg :stream
	   :reader stream-error-stream))
  (:report ("Stream error on stream ~s: " stream)))

(define-condition reader-error (parse-error stream-error) nil)

(define-condition end-of-file (stream-error)
  nil
  (:report ("Unexpected end of file: ")))

(define-condition file-error (error)
  ((pathname :initarg :pathname
	     :reader file-error-pathname))
  (:report ("File error on ~s: " pathname)))

(define-condition pathname-error (file-error) nil)

(define-condition package-error (error)
  ((package :initarg :package
	    :reader package-error-package))
  (:report ("Package error on ~s: " package)))
	      
(define-condition cell-error (error)
  ((name :initarg :name
	 :reader cell-error-name))
  (:report ("Cell error on ~s: " name)))

(define-condition unbound-variable (cell-error)
  nil
  (:report ("Unbound variable: ")))

(define-condition unbound-slot (cell-error)
  ((instance :initarg :instance 
	     :reader unbound-slot-instance))
  (:report ("Slot is unbound in ~s: " instance)))
  
(define-condition undefined-function (cell-error)
  nil
  (:report ("Undefined function: ")))

(define-condition arithmetic-error (ERROR)
  ((operation :initarg :operation
	      :reader arithmetic-error-operation)
   (operands :initarg :operands
	      :reader arithmetic-error-operands))
  (:report ("~%Arithmetic error when performing ~s on ~s: " operation operands)))

(define-condition division-by-zero (arithmetic-error) nil)
(define-condition floating-point-overflow (arithmetic-error) nil)
(define-condition floating-point-invalid-operation (arithmetic-error) nil)
(define-condition floating-point-inexact (arithmetic-error) nil)
(define-condition floating-point-underflow (arithmetic-error) nil)

(define-condition case-failure (type-error)
 ((name :initarg :name
	:reader case-failure-name)
  (possibilities :initarg :possibilities
		 :reader case-failure-possibilities))
  (:report
    (lambda (condition stream)
      (format stream "~s fell through ~s expression.~%wanted one of ~:s."
	      (type-error-datum condition)
	      (case-failure-name condition)
	      (case-failure-possibilities condition)))))

(define-condition abort-failure (control-error) nil (:report "abort failed."))

(define-condition internal-condition (condition)
  ((function-name :initarg :function-name
		  :reader internal-condition-function-name
		  :initform nil))
  (:report (lambda (condition stream)
	     (when (internal-condition-function-name condition)
	       (format stream "Condition in ~S [or a callee]: "
		       (internal-condition-function-name condition)))
	     (call-next-method))))

(define-condition internal-simple-condition (internal-condition simple-condition) nil)

(define-condition internal-simple-error (internal-condition simple-error) nil)
(define-condition internal-simple-type-error (internal-condition simple-type-error) nil)
(define-condition internal-simple-warning (internal-condition simple-warning) nil)

#.`(progn
     ,@(mapcar (lambda (x) 
		 `(define-condition
		    ,(intern (concatenate 'string "INTERNAL-SIMPLE-" (string x)))
		    (internal-condition simple-condition ,x) nil)) 
	       `(stack-overflow storage-exhausted print-not-readable end-of-file style-warning
				unbound-variable unbound-slot undefined-function division-by-zero
				case-failure abort-failure
				,@(mapcar (lambda (x) (intern (concatenate 'string "FLOATING-POINT-" (string x))))
					  '(overflow underflow invalid-operation inexact))
				,@(mapcar (lambda (x) (intern (concatenate 'string (string x) "-ERROR")))
					  '(program control parse stream reader file
						    package cell arithmetic pathname)))))
