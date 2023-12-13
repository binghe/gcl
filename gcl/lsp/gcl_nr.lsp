(in-package :si)


(eval-when
 (compile eval)

 (defmacro defcomp ((fn fn2))
   `(defun ,fn (n1 &optional (n2 n1 n2p) &rest r) 
      (declare (dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 ,(if (member fn '(= /=)) 'number 'real))
      (check-type n2 ,(if (member fn '(= /=)) 'number 'real))
      (cond ((not n2p))
	    ((not (,fn2 n1 n2)) nil)
	    ((not r))
	    ((apply ',fn n2 (car r) (cdr r))))))

 (defmacro defpt ((fn fn2) &aux (def (if (eq fn '+) 0 1)))
   `(defun ,fn (&optional (n1 ,def) (n2 ,def) &rest r) 
      (declare (dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 number)
      (check-type n2 number)
      (let ((z (,fn2 n1 n2)))
	(if r (apply ',fn z (car r) (cdr r)) z))))

 (defmacro defmm ((fn c))
   `(defun ,fn (n1 &optional (n2 n1) &rest r) 
      (declare (dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 real)
      (check-type n2 real)
      (let ((z (if (,c n1 n2) n1 n2)))
	(if r (apply ',fn z (car r) (cdr r)) z))))

  (defmacro defmd ((fn fn2 fn3))
   `(defun ,fn (n1 &optional (n2 n1 n2p) &rest r) 
      (declare (dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 number)
      (check-type n2 number)
      (if n2p
	  (let ((z (,fn2 n1 n2)))
	    (if r (apply ',fn z (car r) (cdr r)) z))
	(,fn ,fn3 n1)))))

(defcomp (<  <2))
(defcomp (<= <=2))
(defcomp (=  =2))
(defun /= (n1 &rest r)
  (declare (optimize (safety 1))(dynamic-extent r))
  (check-type n1 number)
  (if r (unless (member n1 r :test '=) (apply '/= r)) t))
(defcomp (>= >=2))
(defcomp (>  >2))

(defpt (+ number-plus))
(defpt (* number-times))
(defmm (max >=))
(defmm (min <=))

(defmd (- number-minus 0))
(defmd (/ number-divide 1))

(defun zgcd2 (x y) (cond ((= x 0) y) ((= y 0) x) ((fgcd2 x y))))
(defun lgcd2 (x y tt &aux (tt (>> tt (ctzl tt))))
  (if (plusp tt) (setq x tt) (setq y (- tt)))
  (if (= x y) x (lgcd2 x y (- x y))));FIXME too many tagbody iterations
(defun fgcd2 (x y &aux (tx (min (ctzl x) (ctzl y)))(x (>> x tx))(y (>> y tx)))
  (<< (lgcd2 x y (if (oddp x) (- y) (>> x 1))) tx))
(setf (get 'zgcd2 'cmp-inline) t)
(setf (get 'lgcd2 'cmp-inline) t)
(setf (get 'fgcd2 'cmp-inline) t)

(defun gcd (&rest r)
  (declare (optimize (safety 1))(dynamic-extent r))
  (labels ((gcd2 (x y &aux (tp `(integer #.(1+ most-negative-fixnum) #.most-positive-fixnum)))
		 (check-type x integer)
		 (check-type y integer)
		 (if (and (typep x tp) (typep y tp))
		     (zgcd2 (abs x) (abs y))
		   (mpz_gcd x y))))
	  (reduce #'gcd2 r :initial-value 0)))

(defun lcm (&rest r)
  (declare (optimize (safety 1))(dynamic-extent r))
  (labels ((lcm2 (x y &aux (tp `(integer #.(1+ most-negative-fixnum) #.most-positive-fixnum)))
		 (check-type x integer)
		 (check-type y integer)
		 (if (and (typep x tp) (typep y tp))
		     (let* ((x (abs x))(y (abs y))(g (zgcd2 x y)))
		       (if (= 0 g) g (* x (truncate y g))))
		   (mpz_lcm x y))))
	  (reduce #'lcm2 r :initial-value 1)))
;)
