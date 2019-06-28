(in-package :si)

(defun mask (nbits)
  (if (eql nbits fixnum-length)
      -1
      (~ (<< -1 nbits))))
(declaim (inline mask))

(defun merge-word (x y m) (\| (& x m) (& y (~ m))))
(declaim (inline merge-word))

(defun bit-array-fixnum (a i n)
  (if (<= 0 i n)
      (*fixnum (c-array-self a) i nil 0)
      0))
(declaim (inline bit-array-fixnum))

(defun set-bit-array-fixnum (a i v);(a i n v)
;  (assert (<= 0 i n))
  (*fixnum (c-array-self a) i t v))
(declaim (inline set-bit-array-fixnum))

(defun gw (a i n od)
  (cond ((zerop od) (bit-array-fixnum a i n))
	((plusp od)
	 (merge-word
	  (>> (bit-array-fixnum a i n) od)
	  (<< (bit-array-fixnum a (1+ i) n) (- fixnum-length od))
	  (mask (- fixnum-length od))))
	((merge-word
	  (>> (bit-array-fixnum a (1- i) n) (+ fixnum-length od))
	  (<< (bit-array-fixnum a i n) (- od))
	  (mask (- od))))))
(declaim (inline gw))

(defun bit-array-op (fn ba1 ba2 &optional rba
		     &aux
		       (rba (case rba
			      ((t) ba1)
			      ((nil) (make-array (array-dimensions ba1) :element-type 'bit))
			      (otherwise rba))))
  (let* ((o3 (c-array-offset rba))
	 (y (array-total-size rba))
	 (o1 (c-array-offset ba1))
	 (n1 (ceiling (+ o1 (array-total-size ba1)) fixnum-length))
	 (o1 (- o1 o3))
	 (o2 (c-array-offset ba2))
	 (n2 (ceiling (+ o2 (array-total-size ba2)) fixnum-length))
	 (o2 (- o2 o3)))
    
    (multiple-value-bind
     (nw rem) (floor (+ o3 y) fixnum-length)
      
     (let ((i 0)(n3 (if (zerop rem) nw (1+ nw))))
       
       (when (plusp o3)
	 (set-bit-array-fixnum
	  rba i
	  (merge-word
	   (funcall fn (gw ba1 i n1 o1) (gw ba2 i n2 o2)) 
	   (bit-array-fixnum rba i n3)
	    (<< (mask (min y (- fixnum-length o3))) o3)))
	 (incf i))
       
       (do nil ((>= i nw))
	   (set-bit-array-fixnum
	    rba i
	    (funcall fn (gw ba1 i n1 o1) (gw ba2 i n2 o2)))
	   (incf i))
       
       (when (and (plusp rem) (eql i nw))
	 (set-bit-array-fixnum
	  rba i
	  (merge-word
	   (funcall fn (gw ba1 i n1 o1) (gw ba2 i n2 o2))
	   (bit-array-fixnum rba i n3)
	   (mask rem))))
       
       rba))))
(declaim (inline bit-array-op))

;FIXME array-dimensions allocates....
(defun bit-array-dimension-check (x y &aux (r (array-rank x)))
  (when (eql r (array-rank y))
    (dotimes (i r t)
      (unless (eql (array-dimension x i) (array-dimension y i))
	(return nil)))))
(declaim (inline bit-array-dimension-check))

#.`(progn
     ,@(mapcar (lambda (x &aux (n (eq (car x) 'not))
			  (f (intern (string-concatenate "BIT-" (string (pop x))))))
		 `(defun ,f (x ,@(unless n `(y)) &optional r)
		    (declare (optimize (safety 1)))
		    (check-type x (array bit))
		    ,@(unless n
			`((check-type y (array bit))))
		    (check-type r (or boolean (array bit)))
		    ,@(unless n
			`((assert (bit-array-dimension-check x y)
				  (y)
				  'type-error :datum y
				  :expected-type `(array-bit ,(array-dimensions x)))))
		    (assert (when r
			      (or (eq r t)
				  (bit-array-dimension-check x r)))
			    (y)
			    'type-error :datum r
			    :expected-type `(array-bit ,(array-dimensions x)))
		    (bit-array-op ,x x ,(if n 'x 'y) r)))
	       '((and . #'&)
		 (ior . #'\|)
		 (xor . #'^)
		 (eqv .   (lambda (x y) (~ (^ x y))))
		 (nand .  (lambda (x y) (~ (& x y))))
		 (nor .   (lambda (x y) (~ (\| x y))))
		 (andc1 . (lambda (x y) (& (~ x) y)))
		 (andc2 . (lambda (x y) (& x (~ y))))
		 (orc1 .  (lambda (x y) (\| (~ x) y)))
		 (orc2 .  (lambda (x y) (\| x (~ y))))
		 (not .   (lambda (x y) (~ x))))))


  
;; (defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
;;   (declare (optimize (safety 1)))
;;   (check-type bit-array1 (array bit))
;;   (check-type bit-array2 (array bit))
;;   (check-type result-bit-array (or boolean (array bit)))
;;   (assert (equal (
;;   (bit-dimension-check bit-array1 bit-array2)
;;   (bit-dimension-check bit-array1 result-bit-array)
;;   (bit-array-op #'& bit-array1 bit-array2 result-bit-array))


;; (defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op #'\| bit-array1 bit-array2 result-bit-array))


;; (defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op #'^ bit-array1 bit-array2 result-bit-array))


;; (defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op (lambda (x y) (~ (^ x y))) bit-array1 bit-array2 result-bit-array))

    
;; (defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op (lambda (x y) (~ (& x y))) bit-array1 bit-array2 result-bit-array))

    
;; (defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op (lambda (x y) (~ (\| x y))) bit-array1 bit-array2 result-bit-array))

    
;; (defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op (lambda (x y) (& (~ x) y)) bit-array1 bit-array2 result-bit-array))

    
;; (defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op (lambda (x y) (& x (~ y))) bit-array1 bit-array2 result-bit-array))

    
;; (defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op (lambda (x y) (\| (~ x) y)) bit-array1 bit-array2 result-bit-array))

    
;; (defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
;;   (bit-array-op (lambda (x y) (\| x (~ y))) bit-array1 bit-array2 result-bit-array))

;; (defun bit-not (bit-array &optional result-bit-array)
;;   (bit-array-op (lambda (x y) (~ x)) bit-array bit-array result-bit-array))

