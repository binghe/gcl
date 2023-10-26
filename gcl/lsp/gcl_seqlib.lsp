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


;;;;   seqlib.lsp
;;;;
;;;;                           sequence routines


;; (in-package 'lisp)


;; (export '(copy-seq reduce fill replace length elt every some notevery notany
;;           remove remove-if remove-if-not
;;           delete delete-if delete-if-not
;;           count count-if count-if-not
;;           substitute substitute-if substitute-if-not
;;           nsubstitute nsubstitute-if nsubstitute-if-not
;;           find find-if find-if-not
;;           position position-if position-if-not
;;           remove-duplicates delete-duplicates
;;           mismatch search
;; 	  with-hash-table-iterator
;;           sort stable-sort merge))


(in-package :system)


(eval-when
 (compile eval)

 (defmacro comp-key (key) 
   `(if (eq ,key #'identity) 0 1))
 
 (defmacro do-key (key n x) 
   (let ((xx (sgen)))
     `(let ((,xx ,x)) (case ,n (0 ,xx) (otherwise (funcall ,key ,xx))))))
 
 (defmacro comp-red (f) 
   `(let ((,f ,f))
      (if (eq ,f #'+) 0
	(if (eq ,f #'*) 1 
	  (if (eq ,f #'min) 2
	    (if (eq ,f #'max) 3
	      (if (eq ,f #'logior) 4 
		(if (eq ,f #'logxor) 5
		  (if (eq ,f #'logand) 6
		    (if (eq ,f #'cons) 7
		      (if (eq ,f #'list) 8 9)))))))))))
 
 (defmacro comp-test (test test-not) 
   `(+ (if ,test-not 1 0)
       (let ((,test ,test))
	 (if (eq ,test #'eq) 0
	   (if (eq ,test #'eql) 2
	     (if (eq ,test #'equal) 4
	       (if (eq ,test #'equalp) 6 
		 (if (eq ,test #'funcall) 8 10))))))))
 
 (defmacro do-test (test nn x y) 
   (let ((r (sgen))(n (sgen))(nx (sgen))(ny (sgen)))
     `(let* ((,n ,nn)(,nx ,x)(,ny ,y)
	     (,r (case ,n 
		       ((0 1) (eq ,nx ,ny))
		       ((2 3) (eql ,nx ,ny))
		       ((4 5) (equal ,nx ,ny))
		       ((6 7) (equalp ,nx ,ny))
		       ((8 9) (funcall ,nx ,ny))
		       (otherwise (funcall ,test ,nx ,ny)))))
	(if (= (logand ,n 1) 1) (not ,r) ,r))))
 
 (defmacro do-red (f nn x y) 
   (let ((n (sgen))(nx (sgen))(ny (sgen)))
     `(let* ((,n ,nn)(,nx ,x)(,ny ,y))
	(case ,n 
	      (0 (+ ,nx ,ny))
	      (1 (* ,nx ,ny))
	      (2 (min ,nx ,ny))
	      (3 (max ,nx ,ny))
	      (4 (logior ,nx ,ny))
	      (5 (logxor ,nx ,ny))
	      (6 (logand ,nx ,ny))
	      (7 (cons ,nx ,ny))
	      (8 (list ,nx ,ny))
	      (otherwise (funcall f ,nx ,ny))))))
 
 (defmacro bump-test (nn i) 
   (let ((n (sgen)))
     `(let ((,n ,nn))
	(case ,n 
	      ((0 1) ,n) 
	      ((2 3) (- ,n (if (eql-is-eq ,i) 2 0)))
	      ((4 5) (- ,n (if (equal-is-eq ,i) 4 0)))
	      ((6 7) (- ,n (if (equalp-is-eq ,i) 6 0)))
	      (otherwise ,n)))))
 
 
 (defmacro mrotatef (a b &aux (s (sgen "MRF-S"))) `(let ((,s ,a)) (setf ,a ,b ,b ,s)))
 
 (defmacro raref (a seq i j l) 
   `(if ,l 
	(mrotatef (car (aref ,a ,i)) (car (aref ,a ,j)))
      (set-array ,seq ,i ,seq ,j t)))
 
 (defmacro garef (a seq i l) `(if ,l (car (aref ,a ,i)) (aref ,seq ,i)))
 
 (defconstant +seq-ll+ '(from-end key start end))
 
 (defmacro defnseq (n (il seq countp testp ifp &optional ivp) &body body)
   `(progn
      (defun ,n ,(append `(,@il ,seq &key) 
			 (when ivp (list (list 'initial-value nil 'ivsp)))
			 (when testp (list 'test 'test-not))
			 (when countp (list 'count)) +seq-ll+)
	(declare (optimize (safety 1)))
	(check-type ,seq proper-sequence)
	(check-type start (or null seqind))
	(check-type end (or null seqind))
	,@(when countp `((check-type count (or null integer))))
	,@(when testp `((and test test-not (error "both test and test not supplied"))))
	
	(let* ,(when countp `((count (if count count array-dimension-limit))
			      (count (min array-dimension-limit (max 0 count)))))
	  (let* ((startp (when start t))(start (if start start 0))
		 (endp (when end t))(end (if end end array-dimension-limit))
		 ,@(when countp `((count count)))
		 ,@(when testp `((test (or test test-not #'eql))
				 (test (if (functionp test) test (funcallable-symbol-function test)))
				 (test-comp (comp-test test test-not))))
		 (key (or key #'identity))(key (if (functionp key) key (funcallable-symbol-function key)))
		 (key-comp  (comp-key key)))
	    (let* ((l (listp s))
		   (hls (or (and from-end (or endp startp)) (not l)))
		   (ls  (if hls (length s) array-dimension-limit))
		   (end (if (< ls end) ls end)))
	      ,@body))))
      ,@(when ifp
	  `((defun ,(intern (string-concatenate (string n) "-IF")) 
	      ,(append `(,@il ,seq &key) (when countp (list 'count)) +seq-ll+)
	      (declare (optimize (safety 1)))
	      (check-type ,seq proper-sequence)
	      (,n ,@il ,seq :test 'funcall :key key :start start :end end :from-end from-end 
		  ,@(when countp `(:count count))))
	    (defun ,(intern (string-concatenate (string n) "-IF-NOT")) 
	      ,(append `(,@il ,seq &key) (when countp (list 'count)) +seq-ll+)
	      (declare (optimize (safety 1)))
	      (check-type ,seq proper-sequence)
	      (,n ,@il ,seq :test-not 'funcall :key key :start start :end end :from-end from-end 
		  ,@(when countp `(:count count)))))))))


(defun length (x)
  (declare (optimize (safety 1)))
  (check-type x proper-sequence)
  (labels ((ll (x i) (declare (seqind i)) (if x (ll (cdr x) (1+ i)) i)))
	  (if (listp x)
	      (ll x 0)
	    (if (array-has-fill-pointer-p x)
		(fill-pointer x)
	      (array-dimension x 0)))))

;; (defun length (x)
;;   (declare (optimize (safety 2)))
;;   (check-type x sequence)
;;   (labels ((ll (x &optional (i 0)) 
;; 	       (declare (seqind i))
;; 	       (if (endp x) i (ll (cdr x) (1+ i)))))
;; 	  (if (listp x)
;; 	      (ll x 0)
;; 	      (if (array-has-fill-pointer-p x) (fill-pointer x) (array-dimension x 0)))))


(defun elt (seq n)
  (declare (optimize (safety 1)))
  (check-type seq sequence)
  (check-type n seqind)
  (labels ((err nil (error 'type-error :datum n :expected-type `(integer 0 (,(length seq))))))
	  (if (listp seq)
	      (let ((r (nthcdr n seq)))
		(if r (car r) (err)))
	    (if (< n (length seq)) (aref seq n) (err)))))

(defun nreverse (s)
  (declare (optimize (safety 1)))
  (check-type s proper-sequence)
  (labels ((lr (tl &optional hd) (if tl (lr (cdr tl) (rplacd tl hd)) hd))
	   (la (&optional (i 0) (j (1- (length s))))
	       (cond ((< i j) (set-array s i s j t) (la (1+ i) (1- j))) (s))))
	  (if (listp s) (lr s) (la))))

(defun reverse (s)
  (declare (optimize (safety 1)))
  (check-type s sequence);FIXME
  (labels ((lr (tl &optional hd) (if tl (lr (cdr tl) (cons (car tl) hd)) hd))
	   (la (&optional (ls (length s)) (r (make-array ls :element-type (array-element-type s))) (i 0) (j (1- ls)))
	       (cond ((and (< i ls) (>= j 0)) (set-array r i s j) (la ls r (1+ i) (1- j))) (r))))
	  (if (listp s) (lr s) (la))))


(defun subseq (s start &optional end)
  (declare (optimize (safety 1)))
  (check-type s sequence)
  (check-type start seqind)
  (check-type end (or null seqind))

  (let ((s s)(start start)(end (or end (1- array-dimension-limit))));FIXME
    (declare (sequence s) (seqind start end))
    (cond ((listp s)
	   (do ((i start (1+ i))(r)(rp)(p (nthcdr start s) (cdr p))) ((or (>= i end) (endp p)) r)
	       (let ((tmp (cons (car p) nil))) (setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp))))))
	  ((let* ((ls (length s))
		  (end (if (> end ls) ls end))
		  (r (make-array (- end start) :element-type (array-element-type s)))
		  (lr (length r)))
	     (do ((j 0 (1+ j))(i start (1+ i))) ((or (>= i end) (>= j lr)) r)
		  (set-array r j s i)))))))

(defnseq delete-duplicates (nil s nil t nil)

  (when from-end 
    (setq s (nreverse s))
    (let ((tmp start))
      (setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
  (let* ((r (unless (and l (= start 0)) s))(rp (when (and l (> start 0)) (nthcdr (1- start) r))))
  (do ((i start (1+ i))
       (p (when l (if rp (cdr rp) s)) (cdr p))
       (ri start))
      ((or (>= i end) (when l (endp p)))
       (let ((r (cond (l (when rp (rplacd rp p)) (or r p))
		      ((do ((m i (1+ m))) ((>= m ls) 
					   (cond ((array-has-fill-pointer-p r) 
						  (setf (fill-pointer r) ri) r)
						 ((subseq r 0 ri))))
			       (setf (aref r ri) (aref r m) ri (1+ ri)))))))
	 (if from-end (nreverse r) r)))
    (declare (seqind ri))
    (let ((el1 (do-key key key-comp (if l (car p) (aref s i)))))
      (unless
	  (do ((j (1+ i) (1+ j))
	       (q (cdr p) (cdr q))
	       (test-comp (bump-test test-comp el1)))
	      ((or (>= j end) (when l (endp q))))
	    (let ((el2 (do-key key key-comp (if l (car q) (aref s j)))))
	      (when (do-test test test-comp el1 el2)
		(return t))))
	(cond (l (setq rp (if rp (cdr (rplacd rp p)) p) r (or r p)))
	      ((setf (aref r ri) (aref r i) ri (1+ ri)))))))))

(defnseq remove-duplicates (nil s nil t nil)

  (when from-end 
    (setq s (reverse s))
    (let ((tmp start))
      (setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
  (do ((i start (1+ i))
       (k 0)
       (h s)
       (p (when l (nthcdr start s)) (cdr p))
       (r)(rp)(ri 0))
      ((or (>= i end) (when l (endp p)))
       (let ((r (cond (l (nconc r h))
		      ((not r) s)
		      ((do ((m k (1+ m))) ((>= m ls) (setf (fill-pointer r) ri) r) 
			   (set-array r ri s m)(setf ri (1+ ri)))))))
	 (if from-end (nreverse r) r)))
      (declare (seqind ri))
      (let* ((el1 (if l (car p) (aref s i)))
	     (el1 (do-key key key-comp el1)))
	(when
	    (do ((j (1+ i) (1+ j))
		 (q (cdr p) (cdr q))
		 (test-comp (bump-test test-comp el1)))
		((or (>= j end) (when l (endp q))))
		(let* ((el2 (if l (car q) (aref s j)))
		       (el2 (do-key key key-comp el2)))
		  (when (do-test test test-comp el1 el2)
		    (return t))))
	  (unless (or l r) (setq r (make-array ls :element-type (array-element-type s) :fill-pointer 0) ri 0))
	  (do ((m k (1+ m))(hp (when l h) (cdr hp))) ((>= m i) (setq k (1+ i) h (cdr hp)))
	      (if l 
		  (let ((tmp (cons (car hp) nil))) (setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp))))
		(progn (set-array r ri s m)(setf ri (1+ ri)))))))))


(defnseq substitute ((newitem item) s t t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (do ((i start (1+ i))(k 0)(j 0)(h s)(r)(rp)
	 (p (when l (nthcdr start s)) (cdr p)))
	((or (>= i end) (>= j count) (when l (endp p)))
	 (let ((r (cond (l (nconc r h))
			((not r) s)
			((do ((m k (1+ m))) ((>= m ls) (setf (fill-pointer r) m) r) 
			     (setf (aref r m) (aref s m)))))))
	   (if from-end (nreverse r) r)))
	(let* ((el2 (do-key key key-comp (if l (car p) (aref s i)))))
	  (when (do-test test test-comp item el2)
	    (unless (or l r) (setq r (make-array ls :element-type (array-element-type s) :fill-pointer t)))
	    (do ((m k (1+ m))(hp (when l h) (cdr hp))) ((> m i) (setq k (1+ i) j (1+ j) h hp))
		(cond (l (let ((tmp (cons (if (eq p hp) newitem (car hp)) nil))) (setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp)))))
		      ((if (= i m) (setf (aref r m) newitem) (set-array r m s m))))))))))

(defnseq nsubstitute ((newitem item) s t t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (nreverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (let* ()
      (do ((i start (1+ i))(j 0)
	   (p (when l (nthcdr start s)) (cdr p)))
	  ((or (>= i end) (>= j count) (when l (endp p))) (if from-end (nreverse s) s))
	  (let ((el2 (do-key key key-comp (if l (car p) (aref s i)))))
	    (when (do-test test test-comp item el2)
	      (incf j) 
	      (cond (l (rplaca p newitem))
		    ((setf (aref s i) newitem)))))))))

(defnseq count ((item) s nil t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (do ((i start (1+ i))(j 0)
	 (p (when l (nthcdr start s)) (cdr p)))
	((or (>= i end) (when l (endp p))) j)
	(declare (seqind j));FIXME iteration counting
	(let ((el2 (do-key key key-comp (if l (car p) (aref s i)))))
	  (when (do-test test test-comp item el2)
	    (incf j))))))

(defnseq position ((item) s nil t t)
  (let* ((test-comp (bump-test test-comp item))
	 (ls (if (and (not hls) from-end) (length s) ls))
	 (hls (or from-end hls)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (do ((i start (1+ i))
	 (p (when l (nthcdr start s)) (cdr p)))
	((or (>= i end) (when l (endp p))))
	(let ((el2 (do-key key key-comp (if l (car p) (aref s i)))))
	  (when (do-test test test-comp item el2)
	    (return-from position (if from-end (the seqind (- ls (1+ i))) i)))))));FIXME

(defnseq find ((item) s nil t t)
  (let* ((test-comp (bump-test test-comp item))
	 (ls (if (and (not hls) from-end) (length s) ls))
	 (hls (or from-end hls)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (do ((i start (1+ i))
	 (p (when l (nthcdr start s)) (cdr p)))
	((or (>= i end) (when l (endp p))))
	(let* ((el (if l (car p) (aref s i)))
	       (el2 (do-key key key-comp el)))
	  (when (do-test test test-comp item el2)
	    (return-from find el))))))


(defnseq remove ((item) s t t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (let ((end1   (if l end (min (1+ end) array-dimension-limit)))
	  (count1 (if l count (min (1+ count) array-dimension-limit))))
      (do ((i start (1+ i))(k 0)(j 0)(h s)(r)(rp)(ri 0)
	   (p (when l (nthcdr start s)) (cdr p)))
	  ((or (>= i end1) (>= j count1) (when l (endp p)))
	   (let ((r (cond (l (when rp (rplacd rp h)) (or r h))
			  ((not r) s)
			  (t (setf (fill-pointer r) ri) r))))
	     (if from-end (nreverse r) r)))
	  (declare (seqind ri k));FIXME
	  (let* ((e (or (= i end) (= j count)))
		 (i (if e (if r ls k) i)))
	    (when (or e (do-test test test-comp item (do-key key key-comp (if l (car p) (aref s i)))))
	      (unless (or l e r) (setq r (make-array ls :element-type (array-element-type s) :fill-pointer 0)))
	      (do ((m k (1+ m))(hp (when l h) (cdr hp))) ((>= m i) (setq k (1+ i) j (1+ j) h (cdr hp)))
		  (cond (l (let ((tmp (cons (car hp) nil))) (setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp)))))
			(t (set-array r ri s m)(setf ri (1+ ri)))))))))))

(defnseq delete ((item) s t t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (nreverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (let* ((r (unless (and (listp s) (= start 0)) s))(rp (when (and l (> start 0)) (nthcdr (1- start) r))));FIXME compiler aid
      (do ((i start (1+ i))(j 0)(ri start)
	   (p (when (listp s) (if rp (cdr rp) s)) (cdr p)));FIXME compiler aid
	  ((or (>= i end) (>= j count) (when l (endp p)))
	   (let ((r (cond (l (when rp (rplacd rp p)) (or r p))
			  ((do ((m i (1+ m))) 
			       ((>= m ls) 
				(cond ((array-has-fill-pointer-p r) (setf (fill-pointer r) ri) r) 
				      ((subseq r 0 ri))))
			       (set-array r ri r m)(setf ri (1+ ri)))))))
	     (if from-end (nreverse r) r)))
	  (cond ((do-test test test-comp item (do-key key key-comp (if l (car p) (aref s i)))) (incf j))
		((cond (l (setq rp (if rp (cdr (rplacd rp p)) p) r (or r p)))
		       (t (set-array r ri r i)(setf ri (1+ ri))))))))))


(defun tofn (o);FIXME coerce function, elsewhere
  (etypecase 
   o
   (function o) 
   (otherwise (the function (c-symbol-gfdef o)))))
(putprop 'tofn t 'compiler::cmp-inline)

;; (deftype fn nil `(satisfies fnp))
;; (defun fnp (x)
;;   (typecase
;;    x
;;    (function t)
;;    ((and symbol (not boolean))
;;     (and (= 0 (c-symbol-mflag x))
;; 	 (/= 0 (address (c-symbol-gfdef x)))))))
;; (putprop 'fnp t 'compiler::cmp-inline)

(defun reduce (fd s &key key from-end (start 0) end (initial-value nil ivp) 
		 &aux (kf (when key (coerce key 'function)))(f (coerce fd 'function))
		 (l (listp s))(e (or end (if l array-dimension-limit (length s)))))
  (declare (optimize (safety 1)))
  (check-type fd function-designator)
  (check-type s sequence)
  (check-type key (or null function-designator))
  (check-type start seqind)
  (check-type end (or null seqind))
  (labels ((k (s i &aux (z (if l (car s) (aref s i)))) (if kf (funcall kf z) z))
	   (fc (r k) (prog1 (if ivp (funcall f (if from-end k r) (if from-end r k)) k) (setq ivp t)))
	   (rl (s i res)
	       (cond ((or (>= i e) (when l (endp s))) (if (or ivp res) res (values (funcall f))))
		     (from-end (fc (rl (if l (cdr s) s) (1+ i) (if ivp res t)) (k s i)))
		     ((rl (if l (cdr s) s) (1+ i) (fc res (k s i)))))))
    (rl (if l (nthcdr start s) s) start initial-value)))

;; (defun reduce (fd s &key key from-end (start 0) end (initial-value nil ivp) 
;; 		 &aux (key (if key (coerce key 'function) #'identity))(f (coerce fd 'function))
;; 		 (l (listp s))(lim (if l array-dimension-limit (length s)))(ftt ivp)(e (or end lim)))
;;   (declare (optimize (safety 2)))
;;   (check-type fd function-designator)
;;   (check-type s sequence)
;;   (check-type key (or null function-designator))
;;   (check-type start seqind)
;;   (check-type end (or null seqind))
;;   (labels ((rl (s &optional (i 0) (res initial-value) (ft ivp))
;; 	       (if (or (>= i e) (when l (endp s)))
;; 		   (if ft res (values (funcall f)))
;; 	       (let ((k (funcall key (if l (car s) (aref s i)))))
;; 		 (if from-end 
;; 		     (let ((r (rl (if l (cdr s) s) (1+ i) (if ftt res k) t)))
;; 		       (cond (ftt (funcall f k r)) ((setq ftt t) r)))
;; 		   (rl (if l (cdr s) s) (1+ i) (if ft (funcall f res k) k) t))))))
;; 	    (rl (if l (nthcdr start s) s) start)))

;; (defun reduce (f s &key key from-end (start 0) end (initial-value nil ivp) 
;; 		 &aux (key (if key (tofn key) #'identity))(f (tofn f))
;; 		 (l (listp s))(lim (if l array-dimension-limit (length s)))(ftt ivp)(e (or end lim)))
;;   (declare (optimize (safety 2)))
;;   (check-type f fn)
;;   (check-type s sequence)
;;   (check-type key (or null fn))
;;   (check-type start seqind)
;;   (check-type end (or null seqind))
;;   (labels ((rl (s &optional (i 0) (res initial-value) (ft ivp))
;; 	       (if (or (>= i e) (when l (endp s)))
;; 		   (if ft res (funcall f))
;; 	       (let ((k (funcall key (if l (car s) (aref s i)))))
;; 		 (if from-end 
;; 		     (let ((r (rl (if l (cdr s) s) (1+ i) (if ftt res k) t)))
;; 		       (cond (ftt (funcall f k r)) ((setq ftt t) r)))
;; 		   (rl (if l (cdr s) s) (1+ i) (if ft (funcall f res k) k) t))))))
;; 	    (rl (if l (nthcdr start s) s) start)))

;; (defnseq reduce ((f) s nil nil nil t)
;;   (when from-end 
;;     (setq s (reverse s))
;;     (let ((tmp start))
;;       (setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
;;   (do ((p (when l (nthcdr start s)) (cdr p))
;;        (i start (1+ i))
;;        (f (if (functionp f) f (funcallable-symbol-function f)))
;;        (red-comp (comp-red f))
;;        (rx initial-value (let* ((el (do-key key key-comp (if l (car p) (aref s i))))
;; 				(ry (if from-end rx el))
;; 				(rx (if from-end el rx)))
;; 			  (cond (ivsp (do-red f red-comp rx ry))
;; 				((setq ivsp t) el)))))
;;       ((or (>= i end) (when l (endp p))) (if ivsp rx (values (funcall f))))))


(eval-when 
 (compile eval)

 (defmacro locsym (f s) `(sgen (string-concatenate (string ,f) ,s)))

 (defmacro dyncpl (x &aux (l (locsym 'dyncpl "-LOOP")));FIXME this can't cons in a labels as it might be a separate fn.  Get do to unroll too.
   `(labels ((,l (x y) (when x (setf (car x) (car y)) (,l (cdr x) (cdr y)))))
	    (declare (notinline make-list))
	    (let ((tmp (make-list (length ,x))))
	      (declare (dynamic-extent tmp))
	      (,l tmp ,x);Can't be mapl, used by
	     tmp)))

 (defmacro seqend (seq seqs &aux (l (locsym 'seqend "-LOOP")))
   `(labels ((,l (s &aux (x (car s)) (y (length x))) (if s (min y (,l (cdr s))) (length ,seq))))
	    (,l ,seqs)))
 
 (defmacro seqval (seq place i)
   `(if (listp ,seq) (pop ,place) (aref ,seq ,i)))

 (defmacro seqvals (vals ns i)
   `(mapl (lambda (x y &aux (yc (car y))) (setf (car x) (seqval yc (car y) ,i))) ,vals ,ns))
 
 ;; (defmacro defevsm (f o w &aux (l (locsym f "-LOOP")))
 ;;   `(defun ,f (pred seq &rest seqs &aux (end (seqend seq seqs)) (ns (dyncpl seqs)) (vals (dyncpl seqs)))
 ;;      (declare (optimize (safety 2))(:dynamic-extent seqs))
 ;;      (check-type seq proper-sequence)
 ;;      (labels ((,l (i) (,o (>= i end)
 ;; 			     (,w (apply pred (seqval seq seq i) (seqvals vals ns i))
 ;; 				 (,l (1+ i))))))
 ;; 	      (,l 0))))
)


;; (defevsm every or   when)
;; (defevsm some  unless or)

(defun every (pred seq &rest seqs &aux (pred (coerce pred 'function)))
  (declare (optimize (safety 1))(dynamic-extent seqs))
  (check-type pred function-designator)
  (check-type seq proper-sequence)
  (apply 'map nil (lambda (x &rest r) (unless (apply pred x r) (return-from every nil))) seq seqs)
  t)

(defun some (pred seq &rest seqs &aux (pred (coerce pred 'function)))
  (declare (optimize (safety 1))(dynamic-extent seqs))
  (check-type pred function-designator)
  (check-type seq proper-sequence)
  (apply 'map nil (lambda (x &rest r &aux (v (apply pred x r))) (when v (return-from some v))) seq seqs))

(defun notevery (pred seq &rest seqs)
  (declare (optimize (safety 1))(dynamic-extent seqs))
  (check-type pred function-designator)
  (check-type seq proper-sequence)
  (not (apply 'every pred seq seqs)))

(defun notany (pred seq &rest seqs)
  (declare (optimize (safety 1))(dynamic-extent seqs))
  (check-type pred function-designator)
  (check-type seq proper-sequence)
  (not (apply 'some pred seq seqs)))


(defun seqtype (sequence)
  (cond ((listp sequence) 'list)
        ((stringp sequence) 'string)
        ((bit-vector-p sequence) 'bit-vector)
        ((vectorp sequence) (list 'array (array-element-type sequence)))
        (t (error "~S is not a sequence." sequence))))

(defmacro call-test (test test-not item keyx)
  (let ((j1 (sgen)) (j2 (sgen))(tst (sgen))(tstn (sgen)))
    `(let ((,j1 ,item)(,j2 ,keyx)(,tst ,test)(,tstn ,test-not))
       (cond (,tst (funcall ,tst ,j1 ,j2))
	     (,tstn (not (funcall ,tstn ,j1 ,j2)))
	     ((eql ,j1 ,j2))))))


(defun bad-seq-limit (x y)
  (declare (seqind x y))
  (error 'type-error :datum x  :expected-type (if (= y 0) '(integer 0) '(integer 0 y))))


(eval-when (compile eval)
(defmacro f+ (x y) `(the fixnum (+ (the fixnum ,x) (the fixnum ,y))))
(defmacro f- (x y) `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))

(defmacro with-start-end (start end seq &body body)
  `(let ((,seq ,seq)(,start (the-start ,start)))
     (check-type ,seq sequence)
     (let ((,end (the-end ,end (length ,seq))))
       (or (<= ,start ,end) (bad-seq-limit  ,start ,end))
       ,@body)))

(defmacro with-start-end-length (start end length seq &body body)
  `(let ((,seq ,seq)(,start (the-start ,start)))
     (check-type ,seq sequence)
     (let* ((,length (length ,seq))(,end (the-end ,end ,length)))
       (or (<= ,start ,end) (bad-seq-limit  ,start ,end))
       ,@body))))

(defun the-end (x y)
  (declare (seqind y))
  (cond ((seqindp x)
	 (unless (<= x y)
	   (bad-seq-limit x y))
	 x)
	((null x) y)
	(t (error 'type-error :datum x :expected-type '(or null seqind)) y)))
	
(defun the-start (x)
  (cond ((seqindp x)
	 (unless (>= x 0)
	     (bad-seq-limit x 0))
	 x)
	((null x) 0)
	(t (error 'type-error :datum x :expected-type '(or null seqind)))))
  

(defun fill (sequence item &key start end );FIXME
  (declare (optimize (safety 1)))
  (with-start-end start end sequence
		  (do ((i start (f+ 1 i)))
		      ((>= i end) sequence)
		     (declare (fixnum i))
		     (setf (elt sequence i) item))))


(defun replace (s1 s2 &key (start1 0) end1 (start2 0) end2 &aux (os1 s1) s3)
  (declare (optimize (safety 1))(notinline make-list)(dynamic-extent s3))
  (check-type s1 sequence)
  (check-type s2 sequence)
  (check-type start1 seqind)
  (check-type start2 seqind)
  (check-type end1 (or null seqind))
  (check-type end2 (or null seqind))
  (when (and (eq s1 s2) (> start1 start2))
    (setq s3 (make-list (length s2)) s2 (replace s3 s2)))
  (let* ((lp1 (listp s1)) (lp2 (listp s2))
	 (e1 (or end1 (if lp1 array-dimension-limit (length s1))))
	 (e2 (or end2 (if lp2 array-dimension-limit (length s2)))))
    (do ((i1 start1 (1+ i1))(i2 start2 (1+ i2))
	 (s1 (if lp1 (nthcdr start1 s1) s1) (if lp1 (cdr s1) s1))
	 (s2 (if lp2 (nthcdr start2 s2) s2) (if lp2 (cdr s2) s2)))
	((or (not s1) (>= i1 e1) (not s2) (>= i2 e2)) os1)
	(let ((e2 (if lp2 (car s2) (aref s2 i2))))
	  (if lp1 (setf (car s1) e2) (setf (aref s1 i1) e2))))))

       

(defun mismatch (sequence1 sequence2
		 &key from-end test test-not
		      (key #'identity)
		      start1 start2
		      end1 end2)
  (declare (optimize (safety 1)))
  (and test test-not (error "both test and test not supplied"))
  (with-start-end start1 end1 sequence1
   (with-start-end start2 end2 sequence2
    (if (not from-end)
        (do ((i1 start1 (f+ 1  i1))
             (i2 start2  (f+ 1  i2)))
            ((or (>= i1 end1) (>= i2 end2))
             (if (and (>= i1 end1) (>= i2 end2)) nil i1))
          (declare (fixnum i1 i2))
          (unless (call-test test test-not
                             (funcall key (elt sequence1 i1))
                             (funcall key (elt sequence2 i2)))
                  (return i1)))
        (do ((i1 (f+ -1  end1) (f+ -1  i1))
             (i2 (f+ -1  end2)  (f+ -1  i2)))
            ((or (< i1 start1) (< i2 start2))
             (if (and (< i1 start1) (< i2 start2)) nil (f+ 1 i1)))
          (declare (fixnum i1 i2))
          (unless (call-test test test-not
                             (funcall key (elt sequence1 i1))
                             (funcall key (elt sequence2 i2)))
                  (return (f+ 1 i1))))))))


(defun search (sequence1 sequence2
               &key from-end test test-not
                    key
		    (start1 0) (start2 0)
		    end1 end2)
  (declare (optimize (safety 1)))
  (and test test-not (error "both test and test not supplied"))
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (check-type start1 seqind)
  (check-type start2 seqind)
  (when end1 (check-type end1 seqind))
  (when end2 (check-type end2 seqind))

  (let ((s1 sequence1)(s2 sequence2)(i1 start1)(i2 start2)(e1 end1)(e2 end2)(st (or test test-not)))
    (declare (sequence s1 s2)(seqind i1 i2)((or null seqind) e1 e2))
    (let* ((eq (unless st (every (lambda (x) (eql-is-eq (if key (funcall key x) x))) s1))) m (mv 0) x1
	   (l1 (listp s1))(l2 (listp s2))
	   (e1 (or e1 (unless l1 (length s1))))(e2 (or e2 (unless l2 (length s2)))))
      (do ((is2 i2 (1+ is2))
	   (ps2 s2 p2)
	   (p2 (when l2 (nthcdr i2 s2)) (cdr p2))
	   (p1 (when l1 (nthcdr i1 s1))))
	  ((if e2 (> is2 e2) (endp ps2)) (when m mv))
	  (declare (seqind is2))
	  (do ((p1 p1 (cdr p1))
	       (p2 p2 (cdr p2))
	       (i1 i1  (1+ i1))
	       (i2 is2 (1+ i2)))
	      ((or (setq x1 (if e1 (>= i1 e1) (endp p1)))
		   (if e2 (>= i2 e2) (endp p2)))
	       (when x1 (if from-end (setq m t mv is2) (return-from search is2))))
	      (declare (seqind i1 i2))
	      (let ((el1 (if l1 (car p1) (aref s1 i1)))
		    (el2 (if l2 (car p2) (aref s2 i2))))
		(when key (setq el1 (funcall key el1) el2 (funcall key el2)))
		(unless
		    (cond (eq (eq el1 el2))
			  ((not st) (eql el1 el2))
			  (test (funcall test el1 el2))
			  ((not (funcall test-not el1 el2))))
		  (return nil))))))))

(defun sort (seq pred &key (key 'identity))
  (declare (optimize (safety 1)))
  (check-type seq proper-sequence)
  (let* ((k (comp-key key))
	 (ll (length seq))
	 (list (listp seq))
	 (a (when list (make-array ll))))
    (when list
      (do ((fi 0 (1+ fi)) (l seq (cdr l))) ((>= fi ll)) (setf (aref a fi) l)))
    (do ((ii (list ll 0))) ((not ii) seq)
	(declare (dynamic-extent ii))
	(let* ((ls (pop ii)) (fi (pop ii)))
	  (declare (seqind ls fi))
	  (do nil ((>= fi (1- ls)))
	    (let* ((spi (+ fi (random (- ls fi))))
		   (sp (do-key key k (garef a seq spi list))))
	      (raref a seq fi spi list)
	      (do ((lf fi) (rt ls)) ((>= lf rt))
		(declare (seqind lf rt));FIXME
		(do ((q t)) 
		    ((or (>= (if q (incf lf) lf) (if q rt (decf rt)))
			 (let ((f (do-key key k (garef a seq (if q lf rt) list))))
			   (and (not (funcall pred (if q f sp) (if q sp f)))
				(setq q (not q)))))))
		(let* ((r (< lf rt))
		       (f (if r lf fi))
		       (s (if r rt (setq spi (1- lf)))))
		  (raref a seq f s list)))
	      (let* ((ospi (1+ spi))
		     (b   (< (- ls ospi) (- spi fi)))
		     (lf  (if b ospi 0))
		     (rt  (if b 0 spi))
		     (b1  (if b (> (- ls lf) 1) (> (- rt fi) 1)))
		     (ns  (if b lf fi))
		     (ns1 (if b ls rt))
		     (nls (if b spi ls))
		     (nfi (if b fi ospi)))
		(when b1
		  (push ns ii) (push ns1 ii))
		(setq ls nls fi nfi))))))))

(defun list-merge-sort (l pred key k)

  (let* ((ll (length l)))
    (if (< ll 2) l
      (let* ((i (ash ll -1))
	     (lf l)
	     (l1 (nthcdr (1- i) l))
	     (rt (prog1 (cdr l1) (rplacd l1 nil)))
	     (lf (list-merge-sort lf pred key k))
	     (rt (list-merge-sort rt pred key k)))
	(do (l0 l1) ((not (and lf rt)) l0)
	  (cond ((funcall pred (do-key key k (car rt)) (do-key key k (car lf)))
		 (setq l1 (if l1 (cdr (rplacd l1 rt)) (setq l0 rt)) rt (cdr rt))
		 (unless rt (rplacd l1 lf)))
		(t (setq l1 (if l1 (cdr (rplacd l1 lf)) (setq l0 lf)) lf (cdr lf))
		   (unless lf (rplacd l1 rt)))))))))



(defun stable-sort (sequence predicate &key (key #'identity))
  (declare (optimize (safety 1)))
  (check-type sequence proper-sequence)
  (typecase 
   sequence
   (list (list-merge-sort sequence predicate key (comp-key key)))
   (string (sort sequence predicate :key key))
   (bit-vector (sort sequence predicate :key key))
   (otherwise 
    (coerce (list-merge-sort (coerce sequence 'list) predicate key (comp-key key))
	    (seqtype sequence)))))

(defun merge (result-type sequence1 sequence2 predicate
	      &key (key #'identity)
	      &aux (l1 (length sequence1)) (l2 (length sequence2)))
  (declare (optimize (safety 1)))
  (declare (fixnum l1 l2))
  (when (equal key 'nil) (setq key #'identity))
  (do ((newseq (make-sequence result-type (the fixnum (f+ l1 l2))))
       (j 0 (f+ 1  j))
       (i1 0)
       (i2 0))
      ((and (= i1 l1) (= i2 l2)) newseq)
    (declare (fixnum j i1 i2))
    (cond ((and (< i1 l1) (< i2 l2))
	   (cond ((funcall predicate
			   (funcall key (elt sequence1 i1))
			   (funcall key (elt sequence2 i2)))
		  (setf (elt newseq j) (elt sequence1 i1))
		  (setf  i1 (f+ 1  i1)))
		 ((funcall predicate
			   (funcall key (elt sequence2 i2))
			   (funcall key (elt sequence1 i1)))
		  (setf (elt newseq j) (elt sequence2 i2))
		  (setf  i2 (f+ 1  i2)))
		 (t
		  (setf (elt newseq j) (elt sequence1 i1))
		  (setf  i1 (f+ 1  i1)))))
          ((< i1 l1)
	   (setf (elt newseq j) (elt sequence1 i1))
	   (setf  i1 (f+ 1  i1)))
	  (t
	   (setf (elt newseq j) (elt sequence2 i2))
	   (setf  i2 (f+ 1  i2))))))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (declare (optimize (safety 1)))
  (let ((table (sgen))
	(ind (sgen))
	(size (sgen)))
    `(let* ((,table ,hash-table)
	    (,ind -1)
	    (,size (1- (hash-table-size ,table))))
       (macrolet ((,name nil
			 `(do nil ((>= ,',ind ,',size))
			      (let* ((e (hashtable-self ,',table (incf ,',ind)))
				     (k (htent-key e)))
				(unless (eql +objnull+ k)
				  (return (values t (nani k) (htent-value e))))))))
		 ,@body))))
		 

(defun copy-seq (s) 
  (declare (optimize (safety 1)))
  (check-type s sequence)
  (if (listp s)
      (copy-list s)
    (let* ((n (length s))
	   (o (make-array n :element-type (array-element-type s))))
      (do ((i 0 (1+ i))) ((>= i n) o) 
	  (set-array o i s i)))))
