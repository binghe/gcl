# Compiling gcl:
#   ./configure
#   make
# For more details see the file readme

prefix=/usr/local
# This would cause make install to create /usr/local/bin/gcl and
# /usr/local/lib/gcl-x.yy/* with some basic files.
# This prefix may be overridden e.g. with
# ./configure --prefix=/usr/share

# Allow platform defs file to override this.
TK_LISP_LIB=gcl-tk/tkl.o gcl-tk/tinfo.o gcl-tk/decode.tcl gcl-tk/demos/*.lsp gcl-tk/demos/*.lisp gcl-tk/demos/*.o
TCL_EXES=gcl-tk/gcl.tcl gcl-tk/gcltkaux$(EXE)

GCL_DVI=gcl-tk.dvi gcl-si.dvi gcl.dvi
GCL_HTML=gcl-si_toc.html gcl-tk_toc.html gcl_toc.html
BINSCPT:=gcl

-include makedefs

BINDIR	= bin
HDIR	= h/
CDIR	= c
ODIR	= o
LSPDIR	= lsp
CMPDIR	= cmpnew
PORTDIR	= unixport
CLCSDIR = clcs
MODDIR = mod
PCLDIR = pcl
MPDIR	= mp
TESTDIR = ansi-tests
#GMP_DIR = gmp3/

VERSION=`cat majvers`.`cat minvers`

all: $(BUILD_BFD) system command cmpnew/gcl_collectfn.o lsp/gcl_info.o do-gcl-tk do-info release #pargcl_all FIXME

release: majvers minvers
	date >$@

TARGET:=$(shell if [ "$(FLISP)" != "saved_ansi_gcl" ] ; then echo $(PORTDIR)/$(FLISP); else echo ansi-tests/test_results; fi)

system: $(TARGET) $(GPROF)
#	$(MAKE) -C $(PORTDIR) final
	touch $@

gprof_objs: $(addprefix $(PORTDIR)/,$(addsuffix _gprof,$(FLISP)))
	echo $<

$(PORTDIR)/saved_gcl_gprof: $(PORTDIR)/saved_gcl
	mkdir -p gprof
	for i in o lsp cmpnew xgcl-2; do $(MAKE) -C $$i gprof_objs; done
	$(MAKE) -C unixport $(@F)

$(PORTDIR)/saved_ansi_gcl_gprof: $(PORTDIR)/saved_gcl_gprof $(PORTDIR)/saved_ansi_gcl
	for i in mod pcl clcs; do $(MAKE) -C $$i gprof_objs; done
	$(MAKE) -C unixport $(@F)

ASRC:=$(shell ls -1 o/*.c o/*.d o/*.h h/*.h lsp/*.lsp cmpnew/*.lsp mod/*.lsp pcl/*sp clcs/*sp xgcl-2/*p) #o/*.d o/*.h h/*.h
TAGS: $(ASRC)
	etags \
	--regex='/[ \n\t\#\.`]*(defun[ \n\t]+\([^ \n\t]+\)/' \
	--regex='/[ \n\t\#\.`]*(defmfun[ \n\t]+"\([^ \n\t"]+\)"[ \n\t]*)/\1/' \
	--regex='/[ \n\t\#\.`]*(defmfun[ \n\t]+"[^ \n\t"]+"[ \n\t]+\([^ \n\t]+\)/\1/' \
	$^

xgcl: $(PORTDIR)/saved_xgcl

$(PORTDIR)/saved_xgcl: $(PORTDIR)/saved_gcl
	cd xgcl-2 && $(MAKE)

#binutils/intl/libintl.a:
#	cd $(@D) && $(MAKE)

#binutils/bfd/libbfd.a binutils/libiberty/libiberty.a: binutils/intl/libintl.a
#	cd $(@D) && $(MAKE)

copy_iberty: $(LIBIBERTY)
	mkdir -p binutils/libiberty && cd binutils/libiberty && ar x $<

copy_bfd: $(LIBBFD) copy_iberty
	mkdir -p binutils/bfd && cd binutils/bfd && ar x $<

#h/bfd.h: binutils/bfd/libbfd.a binutils/libiberty/libiberty.a
#	cp $(<D)/$(@F) $@

#h/bfdlink.h h/ansidecl.h h/symcat.h: binutils/bfd/libbfd.a binutils/libiberty/libiberty.a
#	cp $(<D)/../include/$(@F) $@

$(PORTDIR)/saved_pre_gcl: $(HDIR)cmpinclude.h
	(cd $(BINDIR); $(MAKE) all)
	$(MAKE) mpfiles
	rm -f o/cmpinclude.h ; cp h/cmpinclude.h o
	(cd $(ODIR); $(MAKE) all)
	$(MAKE) $<
	rm -f o/cmpinclude.h ; cp h/cmpinclude.h o
	cd $(@D) && $(MAKE) $(@F)
	touch $< $@

$(PORTDIR)/saved_gcl0: $(PORTDIR)/saved_pre_gcl $(HDIR)cmpinclude.h
	cd $(@D) && echo '(time (load "boot.lisp"))' | ../$<
	cd $(@D) && $(MAKE) saved_gcl && mv saved_gcl $(@F)

$(PORTDIR)/saved_gcl1: $(PORTDIR)/saved_gcl0 $(HDIR)cmpinclude.h
	cd $(@D) && echo '(time (load "boot.lisp"))' | ./$(<F)
	cd $(@D) && $(MAKE) saved_gcl && mv saved_gcl $(@F)

$(PORTDIR)/saved_gcl2: $(PORTDIR)/saved_gcl1 $(HDIR)cmpinclude.h
	cd $(@D) && echo '(time (load "boot.lisp"))' | ./$(<F)
	cd $(@D) && $(MAKE) saved_gcl && mv saved_gcl $(@F)

$(PORTDIR)/saved_gcl: $(PORTDIR)/saved_gcl2 $(HDIR)cmpinclude.h
	cd $(@D) && echo '(time (load "boot.lisp"))' | ./$(<F) && $(MAKE) $(@F)
	echo '(si::do-recomp)' | $@ && cd $(@D) && $(MAKE) $(@F)

$(PORTDIR)/saved_mod_gcl: $(PORTDIR)/saved_gcl
	(cd $(MODDIR); $(MAKE) clean; $(MAKE) all)
	cd $(@D) && $(MAKE) $(@F)
	echo '(si::do-recomp)' | $@ && cd $(@D) && $(MAKE) $(@F)

$(PORTDIR)/saved_pcl_gcl: $(PORTDIR)/saved_mod_gcl
	(cd $(PCLDIR); $(MAKE) clean; $(MAKE) all)
	cd $(@D) && $(MAKE) $(@F)

$(PORTDIR)/saved_ansi_gcl: $(PORTDIR)/saved_pcl_gcl
	(cd $(CLCSDIR); $(MAKE) clean; $(MAKE) all)
	cd $(@D) && $(MAKE) $(@F)

ansi-tests/test_results: $(PORTDIR)/saved_ansi_gcl
	cd $(@D) && $(MAKE) $(@F)
#	cd $(@D) && rm -f *.o rt/*.o && echo '(load "gclload.lsp")' | ../$<  2>&1 |tee $(@F) & j=$$! ; \
#		tail -f --pid=$$j --retry $@ & wait $$j

#$(PCLDIR)/saved_gcl_pcl: $(PORTDIR)/saved_gcl
#	cd $(@D) &&  $(MAKE) compile LISP="../$<" &&  $(MAKE) $(@F) LISP="../$<"

#$(CLCSDIR)/saved_full_gcl: $(PCLDIR)/saved_gcl_pcl
#	cd $(@D) &&  $(MAKE) compile LISP="../$<" &&  $(MAKE) $(@F) LISP="../$<"

#$(PORTDIR)/saved_ansi_gcl: $(CLCSDIR)/saved_full_gcl
#	cd $(@D) &&  $(MAKE) $(@F)

cmpnew/gcl_collectfn.o lsp/gcl_info.o:
	cd $(@D) && $(MAKE) $(@F)

do-gcl-tk:
	if [ -d "$(TK_CONFIG_PREFIX)" ] ; then \
		cd gcl-tk && $(MAKE) ; \
	else \
		echo "gcl-tk not made..missing include or lib" ; \
	fi 

do-info:
	cd info && $(MAKE)

mpfiles: $(MPFILES)

$(MPDIR)/libmport.a:
	(cd mp ; $(MAKE) all)

$(GMPDIR)/libgmp.a: $(GMPDIR)/Makefile
	cd $(GMPDIR) && $(MAKE) && rm -f libgmp.a &&  ar qc libgmp.a *.o */*.o

PWD_CMD?=pwd

gmp_all: $(GMPDIR)/Makefile
	cd $(GMPDIR) && echo '#include <stdio.h>' >> gmp.h && echo "#include \"`$(PWD_CMD)`/../h/prelink.h\"" >> gmp.h && $(MAKE) 
	touch $@

$(GMPDIR)/mpn/mul_n.o $(GMPDIR)/mpn/lshift.o $(GMPDIR)/mpn/rshift.o: $(GMPDIR)/Makefile
	cd $(@D) && $(MAKE) $(@F)

# This target can only be made once gcl exists.
# So, the dependencies here should match those of "all", except for pargcl_all.
# By setting make variables, pargcl_all should work for build, and for install
pargcl_all: $(BUILD_BFD) $(PORTDIR)/$(FLISP) command cmpnew/gcl_collectfn.o \
		lsp/gcl_info.o do-gcl-tk do-info
# MINSTALL_LIB_DIR=`echo $(INSTALL_LIB_DIR) | sed -e 'sX^\([a-z]\):X/\1Xg'`
# $(MAKE) install_command "INSTALL_LIB_DIR=$(INSTALL_LIB_DIR)" "prefix=$(prefix)" "DESTDIR=$(DESTDIR)" "BINDIR=$$MINSTALL_LIB_DIR/unixport"
# Logic copied from `command' target, but with FLISP modified:
	if test "$(PARGCLDIR)" != ""; then \
	  MGCLDIR=`echo $(GCLDIR) | sed -e 'sX^\([a-z]\):X/\1Xg'` ; \
	  GCLDIR=`echo $(GCLDIR)` ; \
	  $(MAKE) install-command FLISP=saved_pargcl BINSCPT=pargcl \
	    "INSTALL_LIB_DIR=$$GCLDIR" "prefix=$$GCLDIR" \
	    "BINDIR=$$MGCLDIR/$(PORTDIR)" \
	  && cat bin/pargcl \
	  	| sed -f $(PARGCLDIR)/src/pargcl.sed > bin/pargcl_temp \
	  && mv -f bin/pargcl_temp bin/pargcl \
	  && chmod a+x bin/pargcl \
	  && cat $(PARGCLDIR)/bin/procgroup \
	  	| sed -e s^pargcl/bin/par^bin/par^ \
		| sed -e s^bin/par${SYSTEM}^bin/pargcl^ > bin/procgroup ; \
	  (cd $(PARGCLDIR) && $(MAKE)) \
	  && cp ${PARGCLDIR}/src/saved_pargcl unixport/ \
	  && cd unixport \
	  && mv saved_pargcl temp$(EXE) \
	  && echo '(si::reset-sys-paths "$(GCLDIR)/")(si::save-system "saved_pargcl")' | ./temp$(EXE) \
	  && echo '#' \
	  && echo '#' \
	  && echo '# ParGCL built.  To test it:  cd bin; ./pargcl ' \
	  && echo '#' \
	  && echo '#' ; \
	  rm -f temp$(EXE); \
	fi

command:
	rm -f bin/gcl xbin/gcl
	MGCLDIR=`echo $(GCLDIR) | sed -e 'sX^\([a-z]\):X/\1Xg'` ; \
	GCLDIR=`echo $(GCLDIR)` ; \
	$(MAKE) install-command "INSTALL_LIB_DIR=$$GCLDIR" "prefix=$$GCLDIR" "BINDIR=$$MGCLDIR/$(PORTDIR)"
	(cd xbin ; cp ../bin/gcl .)

#	GCLDIR=`echo $(GCLDIR) | sed -e 'sX^/cygdrive/\([a-z]\)X\1!Xg' -e 'sX^//\([a-z]\)X\1!Xg'` ; \

merge:
	$(CC) -o merge merge.c

LISP_LIB=cmpnew/gcl_collectfn.o xgcl-2/sysdef.lisp xgcl-2/gcl_dwtest.lsp xgcl-2/gcl_dwtestcases.lsp lsp/gcl_export.lsp lsp/gcl_autoload.lsp cmpnew/gcl_cmpopt.lsp cmpnew/gcl_lfun_list.lsp lsp/gcl_auto_new.lsp h/cmpinclude.h  unixport/init_raw.lsp $(shell ls -1 unixport/*.a) gcl-tk/tk-package.lsp $(TK_LISP_LIB) $(RL_LIB) $(FIRST_FILE) $(LAST_FILE) $(addsuffix /sys-proclaim.lisp,lsp cmpnew pcl clcs xgcl-2) unixport/gcl.script unixport/libboot.so lsp/gcl_defmacro.lsp lsp/gcl_evalmacros.lsp lsp/gcl_top.lsp lsp/gcl_module.lsp xgcl-2/sysdef.lisp pcl/package.lisp clcs/package.lisp

install-command:
	rm -f bin/$(BINSCPT)
	(echo '#!/bin/sh' ; \
	echo exec $(BINDIR)/$(FLISP)$(EXE) \\ ; \
	echo '   -dir' $(INSTALL_LIB_DIR)/unixport/ \\ ; \
	echo '   -libdir' $(INSTALL_LIB_DIR)/ \\ ; \
	echo '   -eval '\''(setq si::*allow-gzipped-file* t)'\' \\ ;\
	! [ -d "$(TK_CONFIG_PREFIX)" ] || echo '   -eval '\''(setq si::*tk-library* '\"$(TK_LIBRARY)\"')'\' \\;\
	echo '     '\"\$$@\" ) > bin/$(BINSCPT);
	echo '#' other options: -load "/tmp/foo.o" -load "jo.lsp" -eval '"(joe 3)"' >> bin/$(BINSCPT)
	chmod a+x bin/$(BINSCPT)

install_windows_command:
	rm -f $(prefix)/bin/gcl
	(echo '#!/bin/sh' ; \
	echo exec $(BINDIR)/$(FLISP)$(EXE) \\ ; \
	echo '   -dir' $(INSTALL_LIB_DIR)/unixport/ \\ ; \
	echo '   -libdir' $(INSTALL_LIB_DIR)/ \\ ; \
	echo '   -eval '\''(setq si::*allow-gzipped-file* t)'\' \\ ;\
	! [ -d "$(TK_CONFIG_PREFIX)" ] || echo '   -eval '\''(setq si::*tk-library* '\"$(TK_LIBRARY)\"')'\' \\;\
	echo '     '\"\$$@\" ) > $(prefix)/bin/gcl;
	chmod a+x $(prefix)/bin/gcl
	rm -f $(prefix)/bin/gclm.bat
	(echo '@SET cd='; \
	 echo '@SET promp$=%prompt%'; \
	 echo '@PROMPT SET cd$Q$P'; \
	 echo '@CALL>%temp%.\setdir.bat'; \
	 echo '@'; \
	 echo '% do not delete this line %'; \
	 echo '@ECHO off'; \
	 echo 'PROMPT %promp$%'; \
	 echo 'FOR %%c IN (CALL DEL) DO %%c %temp%.\setdir.bat'; \
	 echo 'set cwd=%cd%'; \
	 echo 'set libdir=%cd%\..\lib\gcl-$(VERSION)'; \
	 echo 'set unixportdir=%libdir%\unixport'; \
	 echo 'path %cd%\..\mingw\bin;%PATH%'; \
	 echo "start %unixportdir%\$(FLISP).exe -dir %unixportdir% -libdir %libdir% -eval \"(setq si::*allow-gzipped-file* t)\" %1 %2 %3 %4 %5 %6 %7 %8 %9" ) > $(DESTDIR)$(prefix)/bin/gclm.bat ; fi
	rm -f $(DESTDIR)$(prefix)/bin/gclfinal.bat
	if gcc --version | grep -i mingw >/dev/null 2>&1 ; then (echo 'ECHO path %1\mingw\bin;%PATH% > gcli.bat'; \
	 echo "ECHO start %1\lib\gcl-$(VERSION)\unixport\$(FLISP).exe -dir %1\lib\gcl-$(VERSION)\unixport -libdir %1\lib\gcl-$(VERSION) -eval \"(setq si::*allow-gzipped-file* t)\" %1 %2 %3 %4 %5 %6 %7 %8 %9 >> gcli.bat" ) > $(DESTDIR)$(prefix)/bin/gclfinal.bat ; fi

install: 
	$(MAKE) install1 "INSTALL_LIB_DIR=$(prefix)/lib/gcl-`cat majvers`.`cat minvers`" "prefix=$(prefix)" "DESTDIR=$(DESTDIR)"

INSTALL_LIB_DIR=
install1:
# When the make variables for `${MAKE} install_unix', below, are added
#  in, please also add them for`${MAKE} pargcl_all
# Only the FLISP and BINSCPT variables need to be added, to convert to pargcl
	if gcc --version | grep -i mingw >/dev/null 2>&1 ; then \
		${MAKE} install_windows; \
	else \
		${MAKE} install_unix; \
		[ "$(PARGCLDIR)" = "" ] || ${MAKE} pargcl_all FLISP=saved_pargcl BINSCPT=pargcl \
			 INSTALL_LIB_DIR=$(INSTALL_LIB_DIR) prefix=$(prefix) \
			 DESTDIR=$(DESTDIR) GCLDIR=$(INSTALL_LIB_DIR); \
	fi

install_unix:
	mkdir -p $(DESTDIR)$(prefix)/lib 
	mkdir -p $(DESTDIR)$(prefix)/bin
	mkdir -p $(DESTDIR)$(prefix)/share
	cp -a man $(DESTDIR)$(prefix)/share/
	mkdir -p $(DESTDIR)$(INSTALL_LIB_DIR)
	MINSTALL_LIB_DIR=`echo $(INSTALL_LIB_DIR) | sed -e 'sX^\([a-z]\):X/\1Xg'` ; \
	$(MAKE) install-command "INSTALL_LIB_DIR=$(INSTALL_LIB_DIR)" "prefix=$(prefix)" "DESTDIR=$(DESTDIR)" "BINDIR=$$MINSTALL_LIB_DIR/unixport"
	cp bin/$(BINSCPT) $(DESTDIR)$(prefix)/bin/
	rm -f $(DESTDIR)$(prefix)/bin/gcl.exe
	tar cf - $$(find $(PORTDIR) -name saved_gcl) $$(find $(PORTDIR) -name saved_ansi_gcl) \
           $$(find $(PORTDIR) -name saved_gcl_gprof) $$(find $(PORTDIR) -name saved_ansi_gcl_gprof) info/*.info* $(LISP_LIB) \
	   $(TCL_EXES)  |  (cd $(DESTDIR)$(INSTALL_LIB_DIR) ;tar xf -)
	if gcc --version | grep -i mingw >/dev/null 2>&1 ; then if grep -i oncrpc makedefs >/dev/null 2>&1 ; then cp /mingw/bin/oncrpc.dll $(DESTDIR)$(INSTALL_LIB_DIR)/$(PORTDIR); fi ; fi
	cd $(DESTDIR)$(INSTALL_LIB_DIR)/$(PORTDIR) && \
		for i in saved_*; do \
			echo '(si::reset-sys-paths "$(INSTALL_LIB_DIR)/")(si::save-system "foo")' | ./$$i && \
			mv foo $$i ; \
		done
	if [ -e "unixport/rsym$(EXE)" ] ; then cp unixport/rsym$(EXE) $(DESTDIR)$(INSTALL_LIB_DIR)/unixport/ ; fi
#	ln $(SYMB) $(INSTALL_LIB_DIR)/$(PORTDIR)/$(FLISP)$(EXE) \
#	 $(DESTDIR)$(prefix)/bin/gcl.exe
	if [ -d "$(TK_CONFIG_PREFIX)" ] ; then  \
	cat gcl-tk/gcltksrv$(BAT) | \
	sed -e "s!GCL_TK_DIR=.*!GCL_TK_DIR=$(INSTALL_LIB_DIR)/gcl-tk!g"  \
	-e "s!TK_LIBRARY=.*!TK_LIBRARY=$(TK_LIBRARY)!g" > \
	$(DESTDIR)$(INSTALL_LIB_DIR)/gcl-tk/gcltksrv$(BAT) ; \
	chmod a+x $(DESTDIR)$(INSTALL_LIB_DIR)/gcl-tk/gcltksrv$(BAT) ; fi
#	if [ -d "$(TK_CONFIG_PREFIX)" ] ; then  \
#	(cd $(DESTDIR)$(INSTALL_LIB_DIR)/gcl-tk/demos ; \
#	echo '(load "../tkl.o")(TK::GET-AUTOLOADS (directory "*.lisp"))' | ../../$(PORTDIR)/$(FLISP)$(EXE)) ; fi
	if test "$(EMACS_SITE_LISP)" != "" ; then (cd elisp ; $(MAKE) install DESTDIR=$(DESTDIR)) ; fi
	if test "$(INFO_DIR)" != "unknown"; then (cd info ; $(MAKE) ; $(MAKE) install DESTDIR=$(DESTDIR)) ; fi
	[ "$(X_LIBS)" == "" ] || if test "$(INFO_DIR)" != "unknown"; then (cd xgcl-2 ; $(MAKE) install LISP=$$(pwd)/../unixport/saved_pre_gcl DESTDIR=$(DESTDIR)) ; fi

install_windows:
	mkdir -p $(DESTDIR)$(prefix)/lib 
	mkdir -p $(DESTDIR)$(prefix)/bin
	mkdir -p $(DESTDIR)$(INSTALL_LIB_DIR)
	MINSTALL_LIB_DIR=`echo $(INSTALL_LIB_DIR) | sed -e 'sX^\([a-z]\):X/\1Xg'` ; \
	$(MAKE) install_windows_command "INSTALL_LIB_DIR=$(INSTALL_LIB_DIR)" "prefix=$(prefix)" "DESTDIR=$(DESTDIR)" "BINDIR=$$MINSTALL_LIB_DIR/unixport"
	rm -f $(DESTDIR)$(prefix)/bin/gcl.exe
	tar cf - $(PORTDIR)/$(FLISP)$(EXE) info/*.info* $(LISP_LIB) \
	$(TCL_EXES)  |  (cd $(DESTDIR)$(INSTALL_LIB_DIR) ;tar xf -)
	if grep -i oncrpc makedefs >/dev/null 2>&1 ; then cp /mingw/bin/oncrpc.dll $(DESTDIR)$(INSTALL_LIB_DIR)/$(PORTDIR); fi
	if test -f /mingw/bin/libgmp.dll ; then cp /mingw/bin/libgmp.dll $(DESTDIR)$(INSTALL_LIB_DIR)/$(PORTDIR); fi
	cd $(DESTDIR)$(INSTALL_LIB_DIR)/$(PORTDIR) && \
		mv $(FLISP)$(EXE) temp$(EXE) && \
		echo '(si::reset-sys-paths "$(INSTALL_LIB_DIR)/")(si::save-system "$(FLISP)$(EXE)")' | ./temp$(EXE) $(LISPFLAGS) && \
		rm -f temp$(EXE)
	if [ -e "unixport/rsym$(EXE)" ] ; then cp unixport/rsym$(EXE) $(DESTDIR)$(INSTALL_LIB_DIR)/unixport/ ; fi
	if [ -d "$(TK_CONFIG_PREFIX)" ] ; then  \
	cat gcl-tk/gcltksrv$(BAT) | \
	sed -e "s!GCL_TK_DIR=.*!GCL_TK_DIR=$(INSTALL_LIB_DIR)/gcl-tk!g"  \
	-e "s!TK_LIBRARY=.*!TK_LIBRARY=$(TK_LIBRARY)!g" > \
	$(DESTDIR)$(INSTALL_LIB_DIR)/gcl-tk/gcltksrv$(BAT) ; \
	chmod a+x $(DESTDIR)$(INSTALL_LIB_DIR)/gcl-tk/gcltksrv$(BAT) ; fi
	if test "$(EMACS_SITE_LISP)" != "" ; then (cd elisp ; $(MAKE) install DESTDIR=$(DESTDIR)) ; fi
	if test "$(INFO_DIR)" != "unknown"; then (cd info ; $(MAKE) ; $(MAKE) install DESTDIR=$(DESTDIR)) ; fi
	cp COPYING.LIB-2.0 readme-bin.mingw $(prefix)
	cp gcl.ico $(prefix)/bin
	rm -rf $(prefix)/install; mkdir $(prefix)/install ; cp windows/install.lsp $(prefix)/install ; cp windows/sysdir.bat $(prefix)/bin

gclclean:
	(cd $(BINDIR); $(MAKE) clean)
	(cd mp ; $(MAKE) clean)
	(cd $(ODIR); $(MAKE) clean)
	(cd $(LSPDIR); $(MAKE) clean)
	(cd $(CMPDIR); $(MAKE) clean)
	(cd $(PORTDIR); $(MAKE) clean)
	(cd gcl-tk ; $(MAKE) clean)
	cd $(CLCSDIR) && $(MAKE) clean
	cd $(PCLDIR) && $(MAKE) clean
	cd $(MODDIR) && $(MAKE) clean
	cd xgcl-2 && $(MAKE) clean
	(cd $(TESTDIR); $(MAKE) clean)
	(cd info ; $(MAKE) clean)
#	find binutils -name "*.o" -exec rm {} \;
	rm -rf binutils
	rm -f foo.tcl config.log makedefs makedefsafter config.cache config.status makedefc
	rm -f h/config.h h/gclincl.h h/cmpinclude.h
	rm -f xbin/gcl foo foo.c bin/gclm.bat gmp_all
	rm -f h/*-linux.defs h/bfd.h h/bfdlink.h h/ansidecl.h h/symcat.h
	rm -f windows/gcl.iss bin/gcl.bat windows/gcl.ansi.iss windows/install.ansi.lsp \
		windows/install.lsp windows/sysdir.bat
	rm -rf windows/Output h/mcompdefs.h gprof
	rm -f ansi-tests/test_results ansi-tests/gazonk* \
		ansi-tests/generated-eval-when-test-file.lisp
	rm -f config.log config.cache config.status $(PORTDIR)/gmon.out gcl.script machine system


clean: gclclean
	-[ -z "$(GMPDIR)" ] || (cd $(GMPDIR) && $(MAKE) distclean)
	-[ -z "$(GMPDIR)" ] || rm -rf $(GMPDIR)/.deps $(GMPDIR)/libgmp.a
	-(cd pargcl && $(MAKE) distclean)
	-[ -z "$(PARGCLDIR)" ] || (rm -f unixport/saved_pargcl unixport/saved_prepargcl bin/pargcl bin/procgroup unixport/_rsym1)
#	-cd binutils/intl && $(MAKE) distclean
#	-cd binutils/bfd && $(MAKE) distclean
#	-cd binutils/libiberty && $(MAKE) distclean

CMPINCLUDE_FILES=$(HDIR)cmpincl1.h $(HDIR)gclincl.h $(HDIR)compbas.h $(HDIR)type.h $(HDIR)mgmp.h \
	$(HDIR)lu.h $(HDIR)globals.h  $(HDIR)vs.h \
	$(HDIR)bds.h $(HDIR)frame.h \
	$(HDIR)lex.h \
	$(HDIR)mstdint.h $(HDIR)compbas2.h \
	$(HDIR)compprotos.h  $(HDIR)immnum.h

OTHERS=$(HDIR)notcomp.h $(HDIR)rgbc.h $(HDIR)stacks.h 

$(HDIR)new_decl.h:
	(cd o && $(MAKE) ../$@)

$(HDIR)mstdint.h:
	echo "#include <stdint.h>" | $(CC) -E -I./$(HDIR) - | $(AWK) '/_u?int/ {print}' >$@

$(HDIR)mcompdefs.h: $(HDIR)compdefs.h $(HDIR)new_decl.h
	$(AWK) 'BEGIN {print "#include \"include.h\"";print "#include \"page.h\"";print "---"} {a=$$1;gsub("\\.\\.\\.","",a);print "\"#define " $$1 "\" " a}' $< |\
	$(CC) -E -P -I./$(HDIR) - |\
	$(AWK) '/^\-\-\-$$/ {i=1;next} {if (!i) next} {gsub("\"","");print}' >$@

$(HDIR)cmpinclude.h: $(HDIR)mcompdefs.h $(CMPINCLUDE_FILES) $(HDIR)config.h $(HDIR)cmponly_last.h
	cp $< $(@F)
	cat $(CMPINCLUDE_FILES) | $(CC) -E -I./$(HDIR) - | $(AWK) '/^# |^$$|^#pragma/ {next}{print}' >> $(@F)
	cat $(HDIR)cmponly_last.h >>$(@F)
	./xbin/move-if-changed mv $(@F) $@
	./xbin/move-if-changed cp $@ o/$(@F)

go:
	mkdir go
	(cd go ; cp -s ../o/makefile ../o/*.o ../o/*.c ../o/*.d ../o/*.ini  .)
	(cd go ; $(MAKE)  go)

tar:
	rm -f gcl-`cat majvers`.`cat minvers`
	xbin/distribute ../ngcl-`cat majvers`.`cat minvers`-beta.tgz

configure: configure.in
	autoconf configure.in > configure
	chmod a+rx configure

kcp:
	(cd go ; $(MAKE)  "CFLAGS = -I../h -pg  -c -g ")
	(cd unixport ; $(MAKE) gcp)

.INTERMEDIATE: $(HDIR)mcompdefs.h $(HDIR)mstdint.h
