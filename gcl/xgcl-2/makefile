-include ../makedefs

all: objects docs

LISP?=../unixport/saved_gcl
APPEND=../xbin/append

objects: $(shell ls -1 *.c *.o) #$(LISP)
	echo '(load "sysdef.lisp")(load "sys-proclaim.lisp")(compiler::cdebug)(xlib::compile-xgcl)' | $(LISP)
	touch $@

gprof_objs: objects $(addprefix ../gprof/,$(shell ls -1 *.o))

../gprof/%.o: %.c #$(DECL)
	$(CC) -I../h -c $(filter-out -fomit-frame-pointer,$(CFLAGS)) $(DEFS) -pg $*.c $(AUX_INFO) -o $@
	touch $*.data
	${APPEND} ${NULLFILE} $*.data  $@


sys-proclaim.lisp:
	echo '(load "sysdef.lisp")(compiler::emit-fn t)(xlib::compile-xgcl)(compiler::make-all-proclaims "*.fn")' | $(LISP)

docs: dwdoc/dwdoccontents.html #dwdoc.pdf

dwdoc/dwdoccontents.html: $(LISP)
	mkdir -p $(@D) && \
	cd $(@D) && \
	echo '(load "../sysdef.lisp")(in-package :xlib)(defmacro while (test &rest forms) `(loop (unless ,test (return)) ,@forms))(load "../gcl_tohtml.lsp")(load "../gcl_index.lsp")(tohtml "../dwdoc.tex" "dwdoc")(with-open-file (s "dwdoccontents.html" :direction :output) (let ((*standard-output* s)) (xlib::makecont "../dwdoc.tex" 1 "dwdoc")))(with-open-file (s "dwdocindex.html" :direction :output) (let ((*standard-output* s)) (xlib::printindex indexdata "dwdoc")))' | ../$<

dwdoc.pdf: dwdoc.tex
	pdflatex $<

clean:
	rm -f *.o *.data saved_*  cmpinclude.h dwdoc.pdf dwdoc.aux dwdoc.log gmon.out dwdoc.dvi dwdoc.html dwdoc.ps
	rm -f gcl_xrecompile* user-init* gcl*c gcl*h gcl*data *fn
	rm -rf dwdoc objects

install: docs
	-mkdir -p $(DESTDIR)$(INFO_DIR)../doc
	-cp -r dwdoc $(DESTDIR)$(INFO_DIR)../doc
	-cp *tex $(DESTDIR)$(INFO_DIR)../doc
	find -name "*.pdf" -exec cp {} $(DESTDIR)$(INFO_DIR)../doc \;
