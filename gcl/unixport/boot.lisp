(in-package :compiler)(cdebug)(setq *compile-print* t si::*notify-gbc* t)



#+pre-gcl
(progn
  (declaim (optimize (safety 3)))
  (unless (fboundp 'logandc2) (defun logandc2 (x y) (boole boole-andc2 x y)))
  (unless (fboundp 'lognot) (defun lognot (x) (boole boole-c1 x 0)))
  (unless (fboundp 'abs) (defun abs (x) (if (< x 0) (- x) x))))

(progn (setq si::*code-block-reserve* (make-array 30000000 :element-type 'character :static t)) nil)

(mapc 'compile (nconc #-pre-gcl '(sbit si::improper-consp mapcar mapcan mapc mapl maplist
					 member member-if member-if-not
					 assoc assoc-if assoc-if-not
					 rassoc rassoc-if rassoc-if-not)
		      'si::(listp type-spec-p ibb ib typep <= coerce < > >= + - set-array 0-byte-array-self set-0-byte-array-self  concatenate mta mtv eql-is-eq)
		      '(info-p info-ref info-type info-flags info-ch info-ref-ccb info-ref-clb c1constant-value-object
			     var-p var-name var-kind var-ref var-ref-ccb var-loc var-dt var-type var-mt var-tag var-store
			     c-array-rank c-array-dim c-array-elttype c-array-self c-array-hasfillp
			     array-dimension array-row-major-index row-major-aref si::row-major-aset
			     si::row-major-aref-int aref si::aset
			     array-has-fill-pointer-p length)))
(setf (symbol-function 'array-rank) (symbol-function 'c-array-rank)
      (symbol-function 'array-total-size) (symbol-function 'c-array-dim))

(in-package :user)

(defun doitf (l dir ld? cmpl?)
  (time (funcall ld? (funcall cmpl? (concatenate 'string "../" dir "/gcl_" (string-downcase (string l)) 
						 (if (eq cmpl? 'compile-file) ".lsp" ""))))))

(defun doit (ld? cmpl?)

  (dolist (l '(s sf))
    (doitf l "lsp" ld? cmpl?))
  (dolist (l '(c listlib))
    (doitf l "lsp" #+pre-gcl 'identity #-pre-gcl ld? cmpl?));fixme
  (dolist (l '(predlib deftype subtypep bit type typep typecase arraylib
		       seq seqlib bnum fle dl rm nr lr sym hash sharp))
    (doitf l "lsp" ld? cmpl?))
  
  (dolist (l '(cmptype cmpeval cmpvar cmpwt cmpif cmplet cmptag cmpinline cmpenv cmplam cmptop
		       cmpbind cmpblock cmpcall cmpcatch cmpflet cmpfun cmplabel cmploc cmpmap
		       cmpmulti cmpspecial cmputil cmpvs cmpmain))
    (doitf l "cmpnew" ld? cmpl?))
  
  (with-open-file (s "../lsp/gcl_recompile.lsp" :direction :output))
  (dolist (l '(recompile callhash assert defmacro defstruct describe evalmacros sc
			 logical_pathname_translations make_pathname parse_namestring merge_pathnames
			 pathname_match_p namestring wild_pathname_p translate_pathname truename directory
			 rename_file restart
			 iolib mislib module numlib packlib setf top trace sloop debug info serror mnum fpe))
    (doitf l "lsp" 'identity cmpl?)))

(doit (if (boundp 'noload) 'identity 'load) 'compile-file)
