
(lisp::defun compiler::cmp-anon (#0=#:g9081 #1=#:g9082 #2=#:g9083 #3=#:g9085) (lisp::function (lisp::lambda (pcl::.arg0. pcl::.arg1. pcl::.arg2.) (lisp::locally (lisp::declare (lisp::optimize (lisp::speed 3) (lisp::safety 0))) (lisp::let ((pcl::emf (lisp::if (lisp::typep pcl::.arg0. (lisp::quote lisp::generic-function)) (pcl::mlookup pcl::.arg1. #0# #1# lisp::t :simple) #2#))) (lisp::progn lisp::nil (lisp::cond ((lisp::typep pcl::emf (lisp::quote pcl::fast-method-call)) (lisp::funcall (pcl::fast-method-call-function pcl::emf) (pcl::fast-method-call-pv-cell pcl::emf) (pcl::fast-method-call-next-method-call pcl::emf) . #4=(pcl::.arg0. pcl::.arg1. pcl::.arg2.))) (lisp::t (lisp::let ((#5=#:g9084 pcl::emf)) (lisp::if (lisp::typep #5# (lisp::quote pcl::method-call)) (lisp::progn (lisp::let ((pcl::.function. (pcl::method-call-function pcl::emf)) (pcl::.args. (lisp::list . #4#)) (pcl::.cm-args. (pcl::method-call-call-method-args pcl::emf))) (lisp::declare (lisp::type lisp::function pcl::.function.)) (lisp::if (lisp::if pcl::.cm-args. (lisp::null (lisp::cdr pcl::.cm-args.))) (lisp::funcall pcl::.function. pcl::.args. (lisp::car pcl::.cm-args.)) (lisp::apply pcl::.function. pcl::.args. pcl::.cm-args.)))) (lisp::if (lisp::typep #5# (lisp::quote lisp::function)) (lisp::progn (lisp::funcall (lisp::the lisp::function pcl::emf) . #4#)) (lisp::error (system::typecase-error-string (lisp::quote pcl::emf) #5# #3#)))))))))))))