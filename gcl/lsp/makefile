
.SUFFIXES:
.SUFFIXES: .fn .o .c .lsp

-include ../makedefs


PORTDIR	= ../unixport
CAT=cat
APPEND=../xbin/append

PREFS:= s sf dl fle rm typep type typecase hash sym lr nr recompile sharp callhash arraylib assert defmacro defstruct \
	  describe evalmacros \
	  iolib listlib mislib module numlib mnum \
	  packlib predlib \
	  seq seqlib setf top trace sloop \
          debug info serror bnum c sc restart bit deftype directory fpe \
	  logical_pathname_translations make_pathname merge_pathnames \
	  namestring parse_namestring pathname_match_p rename_file subtypep \
	  translate_pathname truename typeof wild_pathname_p
NPREFS:= export autoload auto_new
SRC:= $(addprefix gcl_,$(addsuffix .lsp,$(PREFS) $(NCPREFS)))
OBJS:=$(addprefix gcl_,$(addsuffix .o,$(PREFS)))

LISP:=$(shell ls -1rt ../unixport/saved_pre_gcl ../unixport/saved_gcl|tail -n 1)

COMPILE_FILE=$(LISP) $(PORTDIR) $(LISPFLAGS) -system-p -c-file -data-file -h-file -compile
#CFLAGS	= -c -O -I../h 

%.o: %.lsp $(LISP) 
	$(COMPILE_FILE) $<


all:	$(OBJS)

gprof_objs: $(addprefix ../gprof/,$(OBJS))

../gprof/%.o: %.c $(DECL)
	$(CC) -I../h -c $(filter-out -fomit-frame-pointer,$(CFLAGS)) $(DEFS) -pg $*.c $(AUX_INFO) -o $@
	${APPEND} ${NULLFILE} $*.data  $@

.c.o:
	$(CC) $(OFLAG) $(CFLAGS) -c $*.c
	${APPEND} ${NULLFILE} $*.data  $*.o

.lsp.fn: ../cmpnew/gcl_collectfn.o
	../xbin/make-fn $*.lsp ../unixport/saved_pre_gcl $(LISPFLAGS) 

all:	$(OBJS)

fns1:	$(FNS)

fns:	../cmpnew/gcl_collectfn.o
	$(MAKE) fns1 -e "FNS=`echo ${OBJS} | sed -e 's:\.o:\.fn:g'`"

#../cmpnew/gcl_collectfn.o: ../cmpnew/gcl_collectfn.lsp
#	(cd ../cmpnew ; $(PORTDIR)/saved_gcl $(PORTDIR)/ gcl_collectfn.lisp gcl_collectfn S1000)


clean:
	rm -f *.o core a.out *.fn *.c *.h *.data gcl_recompile.lsp
allclean:
	rm -f *.h *.data *.c

dummy3 $(NEWCFILES): sys-proclaim.lisp

sys-proclaim.lisp: fns
	echo '(in-package "SYSTEM")' \
	'(load "../cmpnew/gcl_collectfn")'\
	'(compiler::make-all-proclaims "*.fn")' | ../unixport/saved_pre_gcl $(LISPFLAGS) 


newc:	
	$(MAKE) $(OBJS) -e "NEWCFILES=`echo $(OBJS) | sed -e 's:\.o:.c:g'`"


gcl_recompile.lsp:
	touch $@

