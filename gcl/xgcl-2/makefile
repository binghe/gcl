-include ../makedefs


all: objects #docs

objects: $(LISP)
	echo '(load "sysdef.lisp")(load "sys-proclaim.lisp")(xlib::compile-xgcl)' | $(LISP)

saved_xgcl: $(LISP)
	echo '(load "sysdef.lisp")(load "sys-proclaim.lisp")(xlib::compile-xgcl)(xlib::save-xgcl "$@")' | $(LISP)

sys-proclaim.lisp:
	echo '(load "sysdef.lisp")(compiler::emit-fn t)(xlib::compile-xgcl)(compiler::make-all-proclaims "*.fn")' | $(LISP)

docs: dwdoc/dwdoccontents.html dwdoc.pdf

dwdoc/dwdoccontents.html: $(LISP)
	mkdir -p $(@D) && \
	cd $(@D) && \
	echo '(load "../sysdef.lisp")(in-package :xlib)(defmacro while (test &rest forms) `(loop (unless ,test (return)) ,@forms))(load "../gcl_tohtml.lsp")(load "../gcl_index.lsp")(tohtml "../dwdoc.tex" "dwdoc")(with-open-file (s "dwdoccontents.html" :direction :output) (let ((*standard-output* s)) (xlib::makecont "../dwdoc.tex" 1 "dwdoc")))(with-open-file (s "dwdocindex.html" :direction :output) (let ((*standard-output* s)) (xlib::printindex indexdata "dwdoc")))' | ../$<


dwdoc.pdf: dwdoc.tex
	pdflatex $<

clean:
	rm -f *.o *.data saved_*  cmpinclude.h dwdoc.aux dwdoc.log gmon.out
	rm -f gcl*c gcl*h gcl*data gcl_xrecompile* user-init* *fn

clean-docs:
	rm -rf dwdoc dwdoc.pdf

install:
	-mkdir -p $(DESTDIR)$(INFO_DIR)../doc
	-cp -r dwdoc $(DESTDIR)$(INFO_DIR)../doc
	-cp *tex *.pdf $(DESTDIR)$(INFO_DIR)../doc

#.INTERMEDIATE: saved_xgcl
