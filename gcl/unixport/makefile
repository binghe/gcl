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
CFLAGS += -I../h

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

gmpfiles: $(shell [ -z "$(GMPDIR)" ] || find ../$(GMPDIR) -name "*.o" |grep -v '\.lib')
	rm -rf gmp
	mkdir gmp
	a="$^" ; \
	for i in $$a ; do \
		cp $$i gmp/$$(echo $$i | sed -e 's,\.\./,,1' -e 's,/,_,g') ; \
	done
	touch $@

OOBJS:=$(shell j=$$(ar t $(ODIR)/gcllib.a) ; for i in $$(find $(ODIR) -name "*.o") ; do if ! echo $$j |grep $$(basename $$i) >/dev/null 2>&1 ; then echo $$i ; fi ; done)
OOBJS:=$(filter-out $(ODIR)/$(FIRST_FILE),$(OOBJS))
OOBJS:=$(filter-out $(ODIR)/$(LAST_FILE),$(OOBJS))
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

MODOBJS:=$(shell find $(MDIR) -name "*.o")
PCLOBJS:=$(MODOBJS) $(shell find $(PCLDIR) -name "*.o")
ANSIOBJS:=$(PCLOBJS) $(shell find $(CLCSDIR) -name "*.o")

$(LSPDIR)/auto_new.lsp: $(LSPDIR)/auto.lsp
	cp $< $@
	[ "$(RL_OBJS)" = "" ] || \
		echo "(AUTOLOAD 'init-readline '|readline|)" >>$@

libboot.so: ../o/boot.o
	$(CC) -shared -Wl,-undefined -Wl,dynamic_lookup $< -o $@

saved_%:raw_% $(RSYM) init_raw.lsp.in raw_%_map \
		$(CMPDIR)/gcl_cmpmain.lsp \
		$(CMPDIR)/gcl_lfun_list.lsp \
		$(CMPDIR)/gcl_cmpopt.lsp $(HDIR)/cmpinclude.h \
		$(LSPDIR)/gcl_auto_new.lsp

	cat init_raw.lsp.in | sed \
		-e "s#@LI-VERS@#(`cat ../majvers`.`cat ../minvers`) `cat ../release`#1" \
		-e "s#@LI-EXTVERS@#`cat ../minvers | cut -f2 -d.`#1" \
		-e "s#@LI-MINVERS@#`cat ../minvers | cut -f1 -d.`#1" \
		-e "s#@LI-MAJVERS@#`cat ../majvers`#1" \
		-e "s#@LI-RELEASE@#`cat ../release`#1" \
		-e "s#@LI-CC@#\"$(GCL_CC) -c $(filter-out -pg,$(FINAL_CFLAGS))\"#1" \
		-e "s#@LI-DFP@#\"$(filter -pg,$(FINAL_CFLAGS))\"#1" \
		-e "s#@LI-LD@#\"$(GCL_CC) $(LD_FLAGS) -o \"#1" \
		-e "s#@LI-LD-LIBS@#\" $(LD_LIBS_POST)\"#1" \
		-e "s#@LI-OPT-THREE@#\"$(O3FLAGS)\"#1" \
		-e "s#@LI-OPT-TWO@#\"$(O2FLAGS)\"#1" \
		-e "s#@LI-INIT-LSP@#\"$@\"#1" >foo

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
	$(CC) $(LD_FLAGS) $(CFLAGS) -I$(HDIR) -I$(ODIR) -o $(RSYM) $(SPECIAL_RSYM)

msys: msys.c
	PATH=/usr/bin:$$PATH gcc $< -o $@ # Unix binary if running wine

$(HDIR)/mdefs.h: $(HDIR)/include.h
	cat $(HDIR)/include.h | sed -e "/include/d" > $(HDIR)/mdefs.h

sys_%.o: sys_init.c sys.c
	$(CC) $(CFLAGS) -c -D $* -D FLAVOR="$*" $< -o $@

libpre_gcl.a: $(OOBJS) sys_pre_gcl.o gmpfiles libboot.so
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp -name "*.o")

libgcl.a: $(OBJS) sys_gcl.o gmpfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp -name "*.o")

libgcl_gprof.a: $(addprefix ../gprof/,$(notdir $(OBJS))) sys_gcl.o gmpfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp -name "*.o")

libmod_gcl.a: $(OBJS) $(MODOBJS) sys_mod_gcl.o gmpfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp -name "*.o")

libmod_gcl.a: $(OBJS) $(MODOBJS) sys_mod_gcl.o gmpfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp -name "*.o")

libxgcl.a: libgcl.a
	ln -snf $< $@

libpcl_gcl.a: $(OBJS) $(PCLOBJS) sys_pcl_gcl.o gmpfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp -name "*.o")

libansi_gcl.a: $(OBJS) $(ANSIOBJS) sys_ansi_gcl.o gmpfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp -name "*.o")

libansi_gcl_gprof.a: $(addprefix ../gprof/,$(notdir $(OBJS) $(ANSIOBJS))) sys_ansi_gcl.o gmpfiles
	rm -rf $@
	$(ARRS) $@ $(filter %.o,$^) $(shell find gmp -name "*.o")

raw_%_map raw_%: lib%.a libgclp.a $(SYSTEM_OBJS) #$(EXTRAS)
	touch raw_$*_map
ifeq ($(GNU_LD),1)
	$(CC) $(LD_FLAGS) -o raw_$*$(EXE) $(filter %.o,$^) \
		$(shell if echo $@ | grep -q gprof ; then echo "-pg"; else echo ""; fi) \
		-rdynamic -L. $(EXTRA_LD_LIBS) $(LD_STACK_FLAGS) \
		-Wl,-Map raw_$*_map $(LD_LIBS_PRE) -l$* $(LD_LIBS_POST)
else
	$(CC) $(LD_FLAGS) -o raw_$*$(EXE) $(filter %.o,$^) $(shell if grep -q gprof $@ ; then echo "-pg"; else echo ""; fi) -L. $(EXTRA_LD_LIBS) $(LD_LIBS_PRE) -l$* $(LD_LIBS_POST)
endif

map_%:
	touch $@

clean:
	rm -rf  saved_*$(EXE) raw_*$(EXE) *.o core a.out $(RSYM) sys_init.lsp \
		$(LSPDIR)/auto_new.lsp foo *maxima* init_*.lsp lib*.a gmp* *.lsp.tmp \
		gazonk*.lsp plt*h *_map saved_* lib* raw_* out* log* tmp* gcl.script

.INTERMEDIATE: raw_gcl raw_ansi_gcl recompile.h pre_init.lsp post_init.lsp
.PRECIOUS: libboot.so
