(in-package :si)

(defun mask (nbits)
  (if (eql nbits fixnum-length)
      -1
      (~ (<< -1 nbits))))
(setf (get 'mask 'compiler::cmp-inline) t)

(defun merge-word (x y m) (\| (& x m) (& y (~ m))))
(setf (get 'merge-word 'compiler::cmp-inline) t)

(defun bit-array-fixnum (a i n)
  (if (<= 0 i n)
      (*fixnum (c-array-self a) i nil 0)
      0))
(setf (get 'bit-array-fixnum 'compiler::cmp-inline) t)

(defun set-bit-array-fixnum (a i v);(a i n v)
;  (assert (<= 0 i n))
  (*fixnum (c-array-self a) i t v))
(setf (get 'set-bit-array-fixnum 'compiler::cmp-inline) t)

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
(setf (get 'gw 'compiler::cmp-inline) t)

(defun bit-array-op (fn ba1 ba2 &optional rba
		     &aux
		       (rba (case rba
			      ((t) ba1)
			      ((nil) (make-array (array-dimensions ba1) :element-type 'bit))
			      (otherwise rba))))
  (let* ((o3 (array-offset rba))
	 (y (array-total-size rba))
	 (o1 (array-offset ba1))
	 (n1 (ceiling (+ o1 (array-total-size ba1)) fixnum-length))
	 (o1 (- o1 o3))
	 (o2 (array-offset ba2))
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
(setf (get 'bit-array-op 'compiler::cmp-inline) t)

;FIXME array-dimensions allocates....
(defun bit-array-dimension-check (x y &aux (r (array-rank x)))
  (when (eql r (array-rank y))
    (dotimes (i r t)
      (unless (eql (array-dimension x i) (array-dimension y i))
	(return nil)))))
(setf (get 'bit-array-dimension-check 'compiler::cmp-inline) t)

(eval-when
 (compile eval)
 (defmacro defbitfn (f fn &aux (n (eq f 'bit-not)))
   `(defun ,f (x ,@(unless n `(y)) &optional r)
      (declare (optimize (safety 1)))
      (check-type x (array bit))
      ,@(unless n `((check-type y (array bit))))
      (check-type r (or boolean (array bit)))
      ,@(unless n
	  `((assert (bit-array-dimension-check x y)
		    (y)
		    'type-error :datum y
		    :expected-type `(array-bit ,(array-dimensions x)))))
      (assert (or (if r (eq r t) t)
		  (bit-array-dimension-check x r))
	      (y)
	      'type-error :datum r
	      :expected-type `(array-bit ,(array-dimensions x)))
      (bit-array-op ,fn x ,(if n 'x 'y) r))))

(defbitfn bit-and #'&)
(defbitfn bit-ior #'\|)
(defbitfn bit-xor #'^)
(defbitfn bit-eqv   (lambda (x y) (~ (^ x y))))
(defbitfn bit-nand  (lambda (x y) (~ (& x y))))
(defbitfn bit-nor   (lambda (x y) (~ (\| x y))))
(defbitfn bit-andc1 (lambda (x y) (& (~ x) y)))
(defbitfn bit-andc2 (lambda (x y) (& x (~ y))))
(defbitfn bit-orc1  (lambda (x y) (\| (~ x) y)))
(defbitfn bit-orc2  (lambda (x y) (\| x (~ y))))
(defbitfn bit-not   (lambda (x y) (~ x)))
