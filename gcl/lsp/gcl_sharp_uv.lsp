(in-package :si)

(defun regexp-conv (stream)

  (let ((tem (make-array 10 :element-type 'character :fill-pointer 0)))
    (or (eql (read-char stream) #\")
	(error "sharp-u-reader reader needs a \" right after it"))
    (loop
     (let ((ch (read-char stream)))
       (cond ((eql ch #\") (return tem))
	     ((eql ch #\\)
	      (setq ch (read-char stream))
	      (setq ch (or (cdr (assoc ch '((#\n . #\newline)
					    (#\t . #\tab)
					    (#\r . #\return))))
			   ch))))
       (vector-push-extend ch tem)))
    tem))

(defun sharp-u-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (regexp-conv stream))

(defun sharp-v-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(load-time-value (compile-regexp ,(regexp-conv stream))))

(set-dispatch-macro-character #\# #\u 'sharp-u-reader)
(set-dispatch-macro-character #\# #\v 'sharp-v-reader)
