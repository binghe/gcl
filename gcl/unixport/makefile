SPECIAL_RSYM = rsym.c

LIBC	= -lc

-include ../makedefs

RSYM=

HDIR	= ../h
ODIR	= ../o
MDIR	= ../mod
LSPDIR	= ../lsp
CMPDIR	= ../cmpnew
XDIR	= ../xgcl-2
CLCSDIR = ../clcs
PCLDIR  = ../pcl
PORTDIR = $(shell pwd)

LD_FLAGS:=$(LDFLAGS)
ifneq ($(FIRST_FILE),)
LD_FLAGS:=$(LD_FLAGS) $(ODIR)/$(FIRST_FILE)
endif
LD_LIBS_PRE=$(addprefix -u ,$(PATCHED_SYMBOLS))
LD_LIBS_POST=$(LIBS) $(LIBC) -lgclp
ifneq ($(LAST_FILE),)
LD_LIBS_POST:=$(LD_LIBS_POST) $(ODIR)/$(LAST_FILE)
endif

ifeq ($(ARRS),)
ARRS:=ar rs
endif

libgclp.a: $(ODIR)/gcllib.a
	cp $< $@
	ranlib $@

gmpfiles: $(shell find ../$(GMPDIR) -name "*.o" |grep -v '\.lib')
	rm -rf gmp
	mkdir gmp
	a="$^" ; \
	for i in $$a ; do \
		cp $$i gmp/$$(echo $$i | sed -e 's,\.\./,,1' -e 's,/,_,g') ; \
	done
	touch $@

bfdfiles: $(shell find ../binutils -name "*.o")
	rm -rf bfd
	mkdir bfd
	a="$^" ; \
	for i in $$a ; do \
		cp $$i bfd/$$(echo $$i | sed -e 's,\.\./,,1' -e 's,/,_,g') ; \
	done 
	touch $@

OOBJS:=$(shell j=$$(ar t $(ODIR)/gcllib.a) ; for i in $$(ls -1 $(ODIR)/*.o) ; do if ! echo $$j |grep $$(basename $$i) >/dev/null 2>&1 ; then echo $$i ; fi ; done)
OOBJS:=$(filter-out $(FIRST_FILE),$(OOBJS))
OOBJS:=$(filter-out $(LAST_FILE),$(OOBJS))
OOBJS:=$(filter-out $(ODIR)/plttest.o,$(OOBJS))
OOBJS:=$(filter-out $(ODIR)/boot.o,$(OOBJS))
OBJS:=$(OOBJS) $(shell ls -1 $(LSPDIR)/*.o | grep -v recompile.o)
OBJS:=$(OBJS) $(shell ls -1 $(XDIR)/*.o)
OBJS:=$(OBJS) $(shell ls -1 $(CMPDIR)/*.o | grep -v collectfn.o)
ROBJS:=$(shell ls -1 gcl_recompile?*.o)
ifeq ($(ROBS),"")
ROBJS:=$(shell ls -1 gcl_recompile.o)
endif
OBJS:=$(OBJS) $(ROBJS)

MODOBJS:=$(shell ls -1 $(MDIR)/*.o)
PCLOBJS:=$(MODOBJS) $(shell ls -1 $(PCLDIR)/*.o)
ANSIOBJS:=$(PCLOBJS) $(shell ls -1 $(CLCSDIR)/*.o)

$(LSPDIR)/auto_new.lsp: $(LSPDIR)/auto.lsp
	cp $< $@
	[ "$(RL_OBJS)" = "" ] || \
		echo "(AUTOLOAD 'init-readline '|readline|)" >>$@

final: $(FLISP)
#	echo "(let ((si::*disable-recompile* nil)(compiler::*default-c-file* t)(compiler::*default-h-file* t)(compiler::*default-system-p* t)(compiler::*default-data-file* t))(si::compiler-state-fns)(si::do-recompile nil))" | ./$<
	echo "(setq si::*optimize-maximum-pages* nil)" >pre_init.lsp;
	echo "(setq si::*optimize-maximum-pages* t si::*disable-recompile* t)" >post_init.lsp;
#	echo "(when (> (length si::*needs-recompile*) 0) (format t \"Warning, eliminating recompiles ~s~%\" si::*needs-recompile*)(setf (fill-pointer si::*needs-recompile*) 0))" >>post_init.lsp;
	$(MAKE) $(FLISP)
	rm -f pre_init.lsp post_init.lsp #recompile.h #gcl_recompile*
	touch $@

FORCE:

# recompile.h: FORCE $(ROBJS)
# 	echo -n $(ROBJS) | sed 's,gcl_recompile.o,gcl_recompileq.o,g' |\
# 		sed -e 's,gcl_recompile,,g' -e 's,\.o,,g' | \
# 		tr ' ' '\012' | sort -n | \
# 		awk '{a=$$1;if (a=="q") a="";printf("  ar_check_init(gcl_recompile%s,no_init);\n",a)}'  >$@;

#sys_ansi_gcl.o sys_gcl.o: recompile.h

pre_init.lsp post_init.lsp:
	touch $@

libboot.so: ../o/boot.o
	$(CC) -shared -Wl,-undefined -Wl,dynamic_lookup $< -o $@

init_raw.lsp: init_raw.lsp.in

	cat $< | sed \
		-e "s#@LI-VERS@#(`cat ../majvers`.`cat ../minvers`) `date`#1" \
		-e "s#@LI-EXTVERS@#`cat ../minvers | cut -f2 -d.`#1" \
		-e "s#@LI-MINVERS@#`cat ../minvers | cut -f1 -d.`#1" \
		-e "s#@LI-MAJVERS@#`cat ../majvers`#1" \
		-e "s#@LI-CC@#\"$(CC) -c $(FINAL_CFLAGS)\"#1" \
		-e "s#@LI-LD@#\"$(CC) $(LD_FLAGS) -o \"#1" \
		-e "s#@LI-LD-LIBS@#\" $(LD_LIBS_PRE) -l$* $(LD_LIBS_POST)\"#1" \
		-e "s#@LI-OPT-THREE@#\"$(O3FLAGS)\"#1" \
		-e "s#@LI-OPT-TWO@#\"$(O2FLAGS)\"#1" \
		-e "s#@LI-INIT-LSP@#\"$@\"#1" >$@

saved_%:raw_% $(RSYM) init_raw.lsp raw_%_map msys \
		$(CMPDIR)/gcl_cmpmain.lsp \
		$(CMPDIR)/gcl_lfun_list.lsp \
		$(CMPDIR)/gcl_cmpopt.lsp $(HDIR)/cmpinclude.h \
		$(LSPDIR)/gcl_auto_new.lsp

	cp init_raw.lsp foo
	echo "(unless si::*quit-tags* (in-package \"USER\")(system:save-system \"$@\"))" >>foo
	ar x lib$*.a $$(ar t lib$*.a |grep ^gcl_)
	$(PORTDIR)/raw_$*$(EXE) $(PORTDIR)/ -libdir $(GCLDIR)/ < foo
#       check that saved image can be prelinked
	[ "$(PRELINK_CHECK)" = "" ] || \
		! [ -x /usr/bin/objdump ] || \
		! /usr/bin/objdump -f $@ | grep "file format" | grep "elf" || \
		! /usr/bin/objdump -R $@ |grep R_.*_COPY || \
		! echo "$@ cannot be prelinked"

$(RSYM): $(SPECIAL_RSYM) $(HDIR)/mdefs.h
	$(CC) $(CFLAGS) -I$(HDIR) -I$(ODIR) -o $(RSYM) $(SPECIAL_RSYM)

$(HDIR)/mdefs.h: $(HDIR)/include.h
	cat $(HDIR)/include.h | sed -e "/include/d" > $(HDIR)/mdefs.h

sys_%.o: sys_init.c sys.c
	$(CC) $(CFLAGS) -c -D $* -D FLAVOR="$*" $< -o $@

libpre_gcl.a: $(OOBJS) sys_pre_gcl.o gmpfiles bfdfiles libboot.so
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp bfd -name "*.o")

libgcl.a: $(OBJS) sys_gcl.o gmpfiles bfdfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp bfd -name "*.o")

libmod_gcl.a: $(OBJS) $(MODOBJS) sys_mod_gcl.o gmpfiles bfdfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp bfd -name "*.o")

libxgcl.a: libgcl.a
	ln -snf $< $@

libpcl_gcl.a: $(OBJS) $(PCLOBJS) sys_pcl_gcl.o gmpfiles bfdfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp bfd -name "*.o")

libansi_gcl.a: $(OBJS) $(ANSIOBJS) sys_ansi_gcl.o gmpfiles bfdfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp bfd -name "*.o")

raw_%_map raw_%: lib%.a libgclp.a $(SYSTEM_OBJS) $(EXTRAS)
	touch raw_$*_map
ifeq ($(GNU_LD),1)
	$(CC) $(LD_FLAGS) -o raw_$*$(EXE) $(filter %.o,$^) -rdynamic -L. $(EXTRA_LD_LIBS) $(LD_STACK_FLAGS) -Wl,-Map raw_$*_map $(LD_LIBS_PRE) -l$* $(LD_LIBS_POST)
else
	$(CC) $(LD_FLAGS) -o raw_$*$(EXE) $(filter %.o,$^) -L. $(EXTRA_LD_LIBS) $(LD_LIBS_PRE) -l$* $(LD_LIBS_POST)
endif

map_%:
	touch $@

clean:
	rm -rf  saved_*$(EXE) raw_*$(EXE) *.o core a.out $(RSYM) \
		$(LSPDIR)/auto_new.lsp foo *maxima* init_*.lsp lib*.a gmp* bfd* *.lsp.tmp \
		gazonk*.lsp *_map saved_* lib* raw_* gcl.script gcl_recompile* \
		pre_init.lsp post_init.lsp final recompile.h init_raw.lsp

.INTERMEDIATE: raw_gcl raw_ansi_gcl recompile.h pre_init.lsp post_init.lsp
.PRECIOUS: libboot.so
