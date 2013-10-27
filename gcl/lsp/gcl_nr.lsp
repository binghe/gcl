(in-package :si)


(eval-when
 (compile eval)

 (defmacro defcomp ((fn fn2))
   `(defun ,fn (n1 &optional (n2 n1 n2p) &rest r) 
      (declare (:dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 ,(if (member fn '(= /=)) 'number 'real))
      (check-type n2 ,(if (member fn '(= /=)) 'number 'real))
      (cond ((not n2p))
	    ((not (,fn2 n1 n2)) nil)
	    ((not r))
	    ((apply ',fn n2 (car r) (cdr r))))))

 (defmacro defpt ((fn fn2) &aux (def (if (eq fn '+) 0 1)))
   `(defun ,fn (&optional (n1 ,def) (n2 ,def) &rest r) 
      (declare (:dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 number)
      (check-type n2 number)
      (let ((z (,fn2 n1 n2)))
	(if r (apply ',fn z (car r) (cdr r)) z))))

 (defmacro defmm ((fn c))
   `(defun ,fn (n1 &optional (n2 n1) &rest r) 
      (declare (:dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 real)
      (check-type n2 real)
      (let ((z (if (,c n1 n2) n1 n2)))
	(if r (apply ',fn z (car r) (cdr r)) z))))

 (defmacro defmd ((fn fn2))
   `(defun ,fn (n1 &optional (n2 n1 n2p) &rest r) 
      (declare (:dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 number)
      (check-type n2 number)
      (let* ((n1 (if n2p n1 ,(if (eq fn '-) 0 1)))
	     (z (,fn2 n1 n2)))
	(if r (apply ',fn z (car r) (cdr r)) z)))))

(defcomp (<  <2))
(defcomp (<= <=2))
(defcomp (=  =2))
(defcomp (/= /=2))
(defcomp (>= >=2))
(defcomp (>  >2))

(defpt (+ number-plus))
(defpt (* number-times))
(defmm (max >=))
(defmm (min <=))

(defmd (- number-minus))
(defmd (/ number-divide))

(defun gcd (&optional (n1 0) (n2 0) &rest r) 
  (declare (:dynamic-extent r))
  (declare (optimize (safety 1)))
  (check-type n1 integer)
  (check-type n2 integer)
  (let ((z (if (not (and (typep n1 'fixnum) (typep n2 'fixnum)))
	       (mpz_gcd n1 n2)
	     (let ((i n1)(j n2))
	       (if (or (= i most-negative-fixnum) (= j most-negative-fixnum))
		   (mpz_gcd n1 n2)
		 (labels ((g0 (x y) (if (= y 0) x (g0 y (mod x y)))))
			 (g0 (abs i) (abs j))))))))
	   (if r (apply 'gcd z (car r) (cdr r)) z)))

(defun lcm (&optional (n1 1) (n2 1) &rest r) 
  (declare (:dynamic-extent r))
  (declare (optimize (safety 1)))
  (check-type n1 integer)
  (check-type n2 integer)
  (let ((z (if (not (and (typep n1 'fixnum) (typep n2 'fixnum)))
	       (mpz_lcm n1 n2)
	     (let ((i n1)(j n2))
	       (if (or (= i most-negative-fixnum) (= j most-negative-fixnum))
		   (mpz_lcm n1 n2)
		 (let* ((i (abs i))(j (abs j))(g (gcd i j)))
		   (if (= g 0) 0 (* i (truncate j g)))))))))
	   (if r (apply 'lcm z (car r) (cdr r)) z)))

;; BINARY gcd algorithm slower than traditional Euclidean above, CM 20090701

;; (defun gcd5 (x y &aux k)
;;   (declare (non-negative-fixnum x y))
;;   (labels ((lshft (x y) (lit :fixnum (unbox :fixnum x) (cstr "<<") (unbox :fixnum y)))
;; 	   (gcdo (x y)
;; 		 (declare (non-negative-fixnum x y))
;; 		 (cond
;; 		  ((= 0 (logand 1 x)) (gcdo (ash x -1) y))
;; 		  ((= 0 (logand 1 y)) (gcdo x (ash y -1)))
;; 		  ((= x y) x)
;; 		  ((> x y) (gcdo (ash (- x y) -1) y))
;; 		  ((gcdo x (ash (- y x) -1))))))
;;   (cond ((= x 0) y)
;; 	((= y 0) x)
;; 	((let* ((k (the (integer 0 31) (ctz (logior x y)))))
;; 	   (lshft (gcdo (ash x (- k)) (ash y (- k))) k))))))