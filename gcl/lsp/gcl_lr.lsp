(in-package :si)

(eval-when
 (compile)

 ;; (dolist (l '(^ \| & ~ >> <<))
 ;;   (unintern l)(import (find-symbol (symbol-name l) 'c) 'si))
 );FIXME

(eval-when
 (compile)

 (defmacro defbltin (n)
  `(progn
     (defun ,n (x)
       (declare (fixnum x))
       (the (integer 0 ,(integer-length most-positive-fixnum))
	    (lit :fixnum ,(strcat "__builtin_" n "(") (:fixnum x) ")")))
     (declaim (inline ,n))))

(defmacro defp nil
  (labels ((lcf (shft &optional res)
		(if (> (abs shft) (integer-length most-positive-fixnum)) 
		    (nreverse res)
		  (lcf (ash shft 1) (cons `(x (+ x (ash x ,shft))) res))))
	   (lc (pat shft)
	       (if (> shft (integer-length most-positive-fixnum)) 
		   pat
		 (lc (logior pat (ash pat shft)) (ash shft 1)))))
	  `(progn
	     (defun popcount (x)
	       (declare (non-negative-fixnum x))
	       (let* ((x (- x (logand (ash x -1) ,(lc 5 4))))
		      (x (+ (logand x ,(lc 3 4)) (logand (ash x -2) ,(lc 3 4))))
		      (x (logand ,(lc 15 8) (+ x (ash x -4))))
		      ,@(lcf -8))
		 (logand x ,(1- (ash (1+ (integer-length most-positive-fixnum)) 1)))))
	     (declaim (inline popcount)))))


 (defmacro defl* ((f d &optional r c c1 c2 (nn f)))
   (let* ((b (symbol-name nn))
	  (fb (intern (concatenate 'string (symbol-name f) "B2")))
	  (ls (intern (concatenate 'string "LOG" b)))
	  (s (cdr (assoc f '((eqv . &)(and . &)(ior . \|)(xor . ^)))))
	  sa;FIXME
;	  (sa (eq s '&))
	  (f `(,s n1 n2))
	  (f (if c `(~ ,f) f))
	  (q `(let* ((n1f (typep n1 'fixnum))(n2f (typep n2 'fixnum))
		     ,@(when sa 
			 `((n1 (if (and n2f (not n1f)) (mpz_get_si n1) n1))
			   (n2 (if (and n1f (not n2f)) (mpz_get_si n2) n2)))))
		(if (,(if sa 'or 'and) n1f n2f) ,f (the integer (,fb n1 n2 ,c)))))
	  (q (if r `(let ((z ,q)) (if r (apply ',ls z (car r) (cdr r)) z)) q)))
     `(defun ,ls ,(if r `(&optional (n1 ,d) (n2 ,d) &rest r) `(n1 n2))
	,@(when r `((declare (dynamic-extent r))))
	(declare (optimize (safety 1)))
	(check-type n1 integer)
	(check-type n2 integer)
	(let (,@(when c1 `((n1 (lognot n1))))
	      ,@(when c2 `((n2 (lognot n2)))))
	  ,q)))))


(defl* (and -1 t))
(defl* (ior  0 t))
(defl* (xor  0 t))
(defl* (xor -1 t t nil nil eqv))

(defl* (and -1 nil t nil nil nand))
(defl* (ior  0 nil t nil nil nor))

(defl* (and -1 nil nil t nil andc1))
(defl* (and -1 nil nil nil t andc2))

(defl* (ior 0 nil nil t nil orc1))
(defl* (ior 0 nil nil nil t orc2))
(defp)

(defun lognot (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (if (typep x 'fixnum) (~ x) (mpz_com x)))

(defun boole (op n1 n2)
  (declare (optimize (safety 1)))
  (check-type op (integer 0 15))
  (check-type n1 integer)
  (check-type n2 integer)
  (case op
	(#.boole-and (logand n1 n2))
	(#.boole-ior (logior n1 n2))
	(#.boole-xor (logxor n1 n2))
	(#.boole-eqv (logeqv n1 n2))

	(#.boole-nand  (lognand n1 n2))
	(#.boole-nor   (lognor n1 n2))
	(#.boole-andc1 (logandc1 n1 n2))
	(#.boole-andc2 (logandc2 n1 n2))
	(#.boole-orc1  (logorc1 n1 n2))
	(#.boole-orc2  (logorc2 n1 n2))

	(#.boole-clr 0)
	(#.boole-set -1)
	(#.boole-1 n1)
	(#.boole-2 n2)
	(#.boole-c1 (lognot n1))
	(#.boole-c2 (lognot n2))))

(deftype shft-integer nil `(integer * ,most-positive-fixnum))

(defbltin clzl)
(defbltin ctzl)
;(defbltin popcountl)
(defbltin parityl)
(defbltin ffsl)

(defun ash (x y &aux (lw #.(- fixnum-length)))
  (declare (optimize (safety 1)))
  (check-type x integer)
  (check-type y shft-integer)
  (if (typep y 'fixnum)
      (let ((y y))
	(if (= y 0) x
	  (if (typep x 'fixnum)
	      (let ((x x))
		(if (< y 0)
		    (let ((y (if (= y most-negative-fixnum) y (- y))))
		      (if (/= 0 (logand y lw))
			  (if (< x 0) -1 0)
			(>> x y)))
		  (if (< y (clzl x)) (<< x y) (mpz_mul_2exp x y))))
	    (if (< y 0) (mpz_fdiv_q_2exp x (if (= y most-negative-fixnum) y (- y))) 
	      (mpz_mul_2exp x y)))))
    (if (< x 0) -1 0)))

(defun integer-length (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (if (typep x 'fixnum)
      (let ((x (if (minusp x) (lognot x) x)))
	(if (= x 0) x
	  (- fixnum-length (clzl x))))
      (mpz_sizeinbase (if (minusp x) (lognot x) x) 2)))
  
(defun logcount (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (if (typep x 'fixnum)
      (popcount (if (< x 0) (lognot x) x))
    (mpz_popcount  (if (< x 0) (lognot x) x))))
  
(defun logbitp (y x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (check-type y (integer 0))
  (if (typep y 'fixnum)
      (if (typep x 'fixnum)
	  (if (<= y #.(1- (integer-length most-positive-fixnum)))
	      (not (zerop (logand x (ash 1 y))))
	    (minusp x))
	(not (zerop (mpz_tstbit x y))))
    (minusp x)))

(defun immfixp (x)
  (lit :boolean "is_imm_fixnum(" (:object x) ")"))
(putprop 'immfixp t 'compiler::cmp-inline)
;(declaim (inline immfixp))
(setf (get 'immfix 'si::type-predicate) 'immfixp)
(setf (get 'immfixp 'si::predicate-type) 'immfix)

(defun mpz_sgn (x)
  (declare (optimize (safety 1)))
  (check-type x bignum)
  (lit :fixnum "mpz_sgn(&(" (:object x) "->big.big_mpz_t))"))
(putprop 'mpz_sgn t 'compiler::cmp-inline)
;(declaim (inline mpz_sgn))
(defun mpz_odd_p (x)
  (declare (optimize (safety 1)))
  (check-type x bignum)
  (lit :fixnum "mpz_odd_p(&(" (:object x) "->big.big_mpz_t))"))
(putprop 'mpz_odd_p t 'compiler::cmp-inline)
;(declaim (inline mpz_odd_p))
(defun mpz_even_p (x)
  (declare (optimize (safety 1)))
  (check-type x bignum)
  (lit :fixnum "mpz_even_p(&(" (:object x) "->big.big_mpz_t))"))
(putprop 'mpz_even_p t 'compiler::cmp-inline)
;(declaim (inline mpz_even_p))

(defun plusp (x)
  (declare (optimize (safety 1)))
  (check-type x real)
  (typecase x
	    (fixnum (> x 0))
	    (bignum (> (mpz_sgn x) 0))
	    (ratio (plusp (numerator x)))
	    (short-float (> x 0))
	    (long-float (> x 0))))

(defun minusp (x)
  (declare (optimize (safety 1)))
  (check-type x real)
  (typecase x
	    (fixnum (< x 0))
	    (bignum (< (mpz_sgn x) 0))
	    (ratio (minusp (numerator x)))
	    (short-float (< x 0))
	    (long-float (< x 0))))

(defun zerop (x)
  (declare (optimize (safety 1)))
  (check-type x number)
  (typecase x
	    (fixnum (= x 0))
	    (short-float (= x 0))
	    (long-float  (= x 0))
	    (fcomplex (= x 0))
	    (dcomplex (= x 0))))

(defun oddp (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (typecase x
	    (fixnum (/= 0 (logand 1 x)))
	    (bignum (/= 0 (mpz_odd_p x)))))
		      
(defun evenp (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (typecase x
	    (fixnum (= 0 (logand 1 x)))
	    (bignum (/= 0 (mpz_even_p x)))))
