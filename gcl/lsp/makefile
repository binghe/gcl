
.SUFFIXES:
.SUFFIXES: .fn .o .c .lsp

-include ../makedefs


PORTDIR	= ../unixport
CAT=cat
APPEND=../xbin/append

OBJS	= gcl_arraylib.o gcl_assert.o gcl_defmacro.o gcl_defstruct.o \
	  gcl_describe.o gcl_evalmacros.o \
	  gcl_iolib.o gcl_listlib.o gcl_mislib.o gcl_module.o gcl_numlib.o \
	  gcl_packlib.o gcl_predlib.o \
	  gcl_seq.o gcl_seqlib.o gcl_setf.o gcl_top.o gcl_trace.o gcl_sloop.o \
          gcl_debug.o gcl_info.o gcl_serror.o \
	  gcl_destructuring_bind.o gcl_defpackage.o gcl_make_defpackage.o gcl_loop.o $(EXTRA_LOBJS)
# export.o autoload.o auto_new.o


COMPILE_FILE=$(PORTDIR)/saved_pre_gcl $(PORTDIR) -system-p -c-file -data-file \
	-o-file nil -h-file -compile
#CFLAGS	= -c -O -I../h 

.lsp.c: 
	@ ../xbin/if-exists $(PORTDIR)/saved_pre_gcl \
	"rm -f $*.c $*.h $*.data $*.o" \
	"$(COMPILE_FILE) $* "

.lsp.o: 
	 @ ../xbin/if-exists $(PORTDIR)/saved_pre_gcl \
	"rm -f $*.c $*.h $*.data $*.o" \
	"$(COMPILE_FILE) $* " \
	"$(CC) $(OFLAG) $(CFLAGS) -c $*.c " \
	"${APPEND} ${NULLFILE} $*.data $*.o "


all:	$(OBJS) #$(RL_OBJS)



.c.o:
	$(CC) $(OFLAG) $(CFLAGS) -c $*.c
	${APPEND} ${NULLFILE} $*.data  $*.o

.lsp.fn: ../cmpnew/gcl_collectfn.o
	../xbin/make-fn $*.lsp

all:	$(OBJS)

fns1:	$(FNS)

fns:	../cmpnew/gcl_collectfn.o
	$(MAKE) fns1 -e "FNS=`echo ${OBJS} | sed -e 's:\.o:\.fn:g'`"

#../cmpnew/gcl_collectfn.o: ../cmpnew/gcl_collectfn.lsp
#	(cd ../cmpnew ; $(PORTDIR)/saved_gcl $(PORTDIR)/ gcl_collectfn.lisp gcl_collectfn S1000)


clean:
	rm -f *.o core a.out *.fn *.c *.data *.h
allclean:
	rm -f *.h *.data *.c

dummy3 $(NEWCFILES): sys-proclaim.lisp

sys-proclaim.lisp: fns
	echo '(in-package "SYSTEM")' \
	'(load "../cmpnew/gcl_collectfn")'\
	'(compiler::make-all-proclaims "*.fn")' | ../xbin/gcl


newc:	
	$(MAKE) $(OBJS) -e "NEWCFILES=`echo $(OBJS) | sed -e 's:\.o:.c:g'`"


