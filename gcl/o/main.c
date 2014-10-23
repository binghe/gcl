/*
 Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

This file is part of GNU Common Lisp, herein referred to as GCL

GCL is free software; you can redistribute it and/or modify it under
the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
License for more details.

You should have received a copy of the GNU Library General Public License 
along with GCL; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

/*
	main.c
	IMPLEMENTATION-DEPENDENT
*/

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>

static void
init_main(void);

static void
initlisp(void);

static int
multiply_stacks(int);

#define IN_MAIN

#ifdef KCLOVM
#include <ovm/ovm.h>
void change_contexts();
int ovm_process_created; 
void initialize_process();
#endif

#include "include.h"
#include <signal.h>
#include "page.h"

bool saving_system=FALSE;

#ifdef BSD
#include <sys/time.h>
#ifndef SGI
#include <sys/resource.h>
#endif
#endif

#ifdef AOSVS

#endif

#ifdef _WIN32
#include <fcntl.h>
#endif

#define LISP_IMPLEMENTATION_VERSION "April 1994"

char *system_directory;

#define EXTRA_BUFSIZE 8
char stdin_buf[BUFSIZ + EXTRA_BUFSIZE];
char stdout_buf[BUFSIZ + EXTRA_BUFSIZE];

#include "stacks.h"

int debug;			/* debug switch */
int raw_image = TRUE;		/* raw or saved image -- CYGWIN will only place this in .data and not in .bss if initialized to non-zero */
bool GBC_enable=FALSE;

long real_maxpage;
object sSAlisp_maxpagesA;

object siClisp_pagesize;

object sStop_level;


object sSAmultiply_stacksA;
int stack_multiple=1;
static object stack_space;

#ifdef _WIN32
unsigned int _dbegin = 0x10100000;
#endif
#ifdef __CYGWIN__
unsigned long _dbegin = 0;
#endif

#ifdef SGC
int sgc_enabled;
#endif
void install_segmentation_catcher(void);

int
cstack_dir(fixnum j) {
  static fixnum n;
  if (!n) {
    n=1;
    return cstack_dir((fixnum)&j);
  }
  return (fixnum)&j<j ? -1 : 1;
}

fixnum log_maxpage_bound=sizeof(fixnum)*8-1;

inline int
mbrk(void *v) {
  ufixnum uv=(ufixnum)v,uc=(ufixnum)sbrk(0),ux,um;
  fixnum m=((1UL<<(sizeof(fixnum)*8-1))-1);

#ifdef MAX_BRK /*GNU Hurd fragmentation bug*/
  if ((ufixnum)v>MAX_BRK) return -1;
#endif

  if (uv<uc) {
    um=uv;
    ux=uc;
  } else {
    um=uc;
    ux=uv;
  }
  if (((fixnum)(ux-um))<0)
    return mbrk((void *)uc+(uv<uc ? -m : m)) || mbrk(v);
  return uc==(ufixnum)sbrk(uv-uc) ? 0 : -1;
}
    
#if defined(__CYGWIN__)||defined(__MINGW32__)

#include <Windows.h>

ufixnum
get_phys_pages_no_malloc(void) {
  MEMORYSTATUS m;

  m.dwLength=sizeof(m);
  GlobalMemoryStatus(&m);
  return m.dwTotalPhys>>PAGEWIDTH;

}

#elif defined (DARWIN)

#include <sys/sysctl.h>

ufixnum
get_phys_pages_no_malloc(void) {
  uint64_t s;
  size_t z=sizeof(s);
  int m[2]={CTL_HW,HW_MEMSIZE};
  
  if (sysctl(m,2,&s,&z,NULL,0)==0)
    return s>>PAGEWIDTH;

  return 0;

}

#elif defined(__sun__)

ufixnum
get_phys_pages_no_malloc(void) {

  return sysconf(_SC_PHYS_PAGES);

}

#else 

ufixnum
get_phys_pages_no_malloc(void) {
  int l;
  char b[PAGESIZE],*c;
  const char *k="MemTotal:",*f="/proc/meminfo";
  ufixnum res=0,n;
  
  if ((l=open(f,O_RDONLY))!=-1) {
    if ((n=read(l,b,sizeof(b)))<sizeof(b) && 
	!(b[n]=0) && 
	(c=strstr(b,k)) && 
	sscanf(c+strlen(k),"%lu",&n)==1)
      res=n;
    close(l);
  }
  return res>>(PAGEWIDTH-10);
}

#endif

int
update_real_maxpage(void) {

  ufixnum i,j,k;
  void *end,*cur,*beg;
#ifdef __MINGW32__
  static fixnum n;

  if (!n) {
    init_shared_memory();
    n=1;
  }
#endif

  massert(cur=sbrk(0));
  beg=data_start ? data_start : cur;
  for (i=0,j=(1L<<log_maxpage_bound);j>PAGESIZE;j>>=1)
    if ((end=beg+i+j-PAGESIZE)>cur)
      if (!mbrk(end)) {
	real_maxpage=page(end);
	i+=j;
      }
  massert(!mbrk(cur));

  phys_pages=get_phys_pages_no_malloc();

#ifdef BRK_DOES_NOT_GUARANTEE_ALLOCATION
  if (phys_pages>0 && real_maxpage>phys_pages+page(beg)) real_maxpage=phys_pages+page(beg);
#endif

  available_pages=real_maxpage-page(beg);
  for (i=t_start,j=0;i<t_other;i++) {
    k=tm_table[i].tm_maxpage;
    if (tm_table[i].tm_type==t_relocatable)
      k*=2;
    else
      j+=k;
    available_pages-=k;
  }
  resv_pages=40<available_pages ? 40 : available_pages;
  available_pages-=resv_pages;

  new_holepage=available_pages/starting_hole_div;
  k=available_pages/20;
  j*=starting_relb_heap_mult;
  j=j<k ? j : k;
  if (maxrbpage<j)
    set_tm_maxpage(tm_table+t_relocatable,j);

  return 0;

}

static int
minimize_image(void) {

#ifdef SGC
  int in_sgc=sgc_enabled;
#else
  int in_sgc=0;
#endif
  extern long new_holepage;
  fixnum old_holepage=new_holepage,i;
  void *new;
  
  if (in_sgc) sgc_quit();
  holepage=new_holepage=1;
  GBC(t_relocatable);
  if (in_sgc) sgc_start();
  new = (void *)(((((ufixnum)rb_pointer)+ PAGESIZE-1)/PAGESIZE)*PAGESIZE);
  core_end = new;
  rb_end=rb_limit=new;
  set_tm_maxpage(tm_table+t_relocatable,(nrbpage=((char *)new-REAL_RB_START)/PAGESIZE));
  new_holepage=old_holepage;
  
#ifdef GCL_GPROF
  gprof_cleanup();
#endif
  
#if defined(BSD) || defined(ATT)  
  mbrk(core_end);
#endif
  
  cbgbccount = tm_table[t_contiguous].tm_adjgbccnt = tm_table[t_contiguous].tm_opt_maxpage = 0;
  rbgbccount = tm_table[t_relocatable].tm_adjgbccnt = tm_table[t_relocatable].tm_opt_maxpage = 0;
  for (i = 0;  i < (int)t_end;  i++)
    tm_table[i].tm_gbccount = tm_table[i].tm_adjgbccnt = tm_table[i].tm_opt_maxpage = 0;
  
  return 0;
  
}

DEFUN_NEW("SET-LOG-MAXPAGE-BOUND",object,fSset_log_maxpage_bound,SI,1,1,NONE,II,OO,OO,OO,(fixnum l),"") {

  void *end,*dend;
  fixnum def=sizeof(fixnum)*8-1;

  l=l<def ? l : def;
  end=data_start+(1L<<l)-PAGESIZE;
  GBC(t_relocatable);
  dend=heap_end+PAGESIZE+(((rb_pointer-REAL_RB_START)+PAGESIZE-1)&(-PAGESIZE));
  if (end >= dend) {
    minimize_image();
    log_maxpage_bound=l;
    update_real_maxpage();
  }

  return (object)log_maxpage_bound;

}

#ifdef NEED_STACK_CHK_GUARD

unsigned long __stack_chk_guard=0;

static unsigned long
random_ulong() {
 
  object y;
  
  vs_top=vs_base;
  vs_push(Ct);
  Lmake_random_state();
  y=vs_pop;
  vs_push(number_negate(find_symbol(make_simple_string("MOST-NEGATIVE-FIXNUM"),system_package)->s.s_dbind));
  vs_push(y);
  Lrandom();

  return fixint(vs_pop);

}
#endif

#ifdef HAVE_MPROTECT
#include <sys/mman.h>
int
gcl_mprotect(void *v,unsigned long l,int p) {

  int i;
  char b[80];

  if ((i=mprotect(v,l,p))) {
    snprintf(b,sizeof(b),"mprotect failure: %p %lu %d\b",v,l,p);
    perror(b);
  }

  return i;

}
#endif

int
main(int argc, char **argv, char **envp) {

  gcl_init_alloc(&argv);

#ifdef GET_FULL_PATH_SELF
  GET_FULL_PATH_SELF(kcl_self);
#else
  kcl_self = argv[0];
#endif
#ifdef __MINGW32__
  {
    char *s=kcl_self;
    for (;*s;s++) if (*s=='\\') *s='/';
  }
#endif	
  *argv=kcl_self;
  
#ifdef CAN_UNRANDOMIZE_SBRK
#include <stdio.h>
#include <stdlib.h>
#include "unrandomize.h"
#endif
  
#ifdef LD_BIND_NOW
#include <stdio.h>
#include <stdlib.h>
#include "ld_bind_now.h"
#endif
  
  setbuf(stdin, stdin_buf); 
  setbuf(stdout, stdout_buf);
#ifdef _WIN32
  _fmode = _O_BINARY;
  _setmode( _fileno( stdin ), _O_BINARY );
  _setmode( _fileno( stdout ), _O_BINARY );
  _setmode( _fileno( stderr ), _O_BINARY );
#endif
  ARGC = argc;
  ARGV = argv;
  ENVP = envp;
  
  vs_top = vs_base = vs_org;
  ihs_top = ihs_org-1;
  bds_top = bds_org-1;
  frs_top = frs_org-1;

  if (raw_image) {

    printf("GCL (GNU Common Lisp)  %s  %ld pages\n",LISP_IMPLEMENTATION_VERSION,real_maxpage);
    fflush(stdout);
    
    if (argc>1) {
      massert(argv[1][strlen(argv[1])-1]=='/');
      system_directory=argv[1];
    }

    initlisp();
    lex_new();
    
    GBC_enable = TRUE;
    
    gcl_init_init();
  
    sLApackageA->s.s_dbind = user_package;
    
  } else {

    terminal_io->sm.sm_object0->sm.sm_fp = stdin;
    terminal_io->sm.sm_object1->sm.sm_fp = stdout;
#ifdef LD_BIND_NOW /*FIXME currently mips only, verify that these two requirements are the same*/
    reinit_gmp();
#endif
    gcl_init_big1();
#ifdef HAVE_READLINE
    gcl_init_readline_function();
#endif
#ifdef NEED_STACK_CHK_GUARD
  __stack_chk_guard=random_ulong();/*Cannot be safely set inside a function which returns*/
#endif

  }

#ifdef _WIN32
  detect_wine();
#endif

  sSAlisp_maxpagesA->s.s_dbind = make_fixnum(real_maxpage);

  ihs_push(Cnil);
  lex_new();
  vs_base = vs_top;
  
  interrupt_enable = TRUE;
  install_default_signals();
    
  do 
    super_funcall(sStop_level);
  while (type_of(sSAmultiply_stacksA->s.s_dbind)==t_fixnum && multiply_stacks(fix(sSAmultiply_stacksA->s.s_dbind)));
    
  return 0;

}

/* catch certain signals */
void install_segmentation_catcher(void)
{
  unblock_signals(SIGSEGV,SIGSEGV);
  unblock_signals(SIGBUS,SIGBUS);
  (void) gcl_signal(SIGSEGV,segmentation_catcher);
  (void) gcl_signal(SIGBUS,segmentation_catcher);
}

int catch_fatal=1;
void
error(char *s)
{
        if (catch_fatal>0 && interrupt_enable )
            {catch_fatal = -1;
#ifdef SGC
	   if (sgc_enabled)
	     { sgc_quit();}
	   if (sgc_enabled==0)
#endif
	     { install_segmentation_catcher() ;}
	   FEerror("Caught fatal error [memory may be damaged]",0); }
	printf("\nUnrecoverable error: %s.\n", s);
	fflush(stdout);
	abort();
}

static void
initlisp(void) {

        void *v=&v;
	
	if (NULL_OR_ON_C_STACK(v) == 0
#if defined(IM_FIX_BASE)
             || NULL_OR_ON_C_STACK(IM_FIX_BASE) == 0
             || NULL_OR_ON_C_STACK((IM_FIX_BASE|IM_FIX_LIM)) == 0
#endif
	    /* || NULL_OR_ON_C_STACK(vv) */
	    || NULL_OR_ON_C_STACK(pagetoinfo(first_data_page))
	    || NULL_OR_ON_C_STACK(core_end-1)) {
	  /* check person has correct definition of above */
	  fprintf(stderr,"%p %d "
#if defined(IM_FIX_BASE)
		  "%p %d %p %d "
#endif
		  "%p %d %p %d\n",
		  v,NULL_OR_ON_C_STACK(v),
#if defined(IM_FIX_BASE)
		  (void *)IM_FIX_BASE,NULL_OR_ON_C_STACK(IM_FIX_BASE),
		  (void *)(IM_FIX_BASE|IM_FIX_LIM),NULL_OR_ON_C_STACK(IM_FIX_BASE|IM_FIX_LIM),
#endif
		  pagetoinfo(first_data_page),NULL_OR_ON_C_STACK(pagetoinfo(first_data_page)),
		  core_end-1,NULL_OR_ON_C_STACK(core_end-1));
	  error("NULL_OR_ON_C_STACK macro invalid");
	}
	
	Cnil->fw=0;
	set_type_of(Cnil,t_symbol);
 	Cnil->c.c_cdr=Cnil;
	Cnil_body.s.s_dbind = Cnil;
	Cnil_body.s.s_sfdef = NOT_SPECIAL;
	Cnil_body.s.s_fillp = 3;
	Cnil_body.s.s_self = "NIL";
	Cnil_body.s.s_gfdef = OBJNULL;
	Cnil_body.s.s_plist = Cnil;
	Cnil_body.s.s_hpack = Cnil;
	Cnil_body.s.s_stype = (short)stp_constant;
	Cnil_body.s.s_mflag = FALSE;
	
	Ct->fw=0;
	set_type_of(Ct,t_symbol);
	Ct_body.s.s_dbind = Ct;
	Ct_body.s.s_sfdef = NOT_SPECIAL;
	Ct_body.s.s_fillp = 1;
	Ct_body.s.s_self = "T";
	Ct_body.s.s_gfdef = OBJNULL;
	Ct_body.s.s_plist = Cnil;
	Ct_body.s.s_hpack = Cnil;
	Ct_body.s.s_stype = (short)stp_constant;
	Ct_body.s.s_mflag = FALSE;
	
	gcl_init_symbol();

	gcl_init_package();

	Cnil->s.s_hpack = lisp_package;
	import(Cnil, lisp_package);
	export(Cnil, lisp_package);

	Ct->s.s_hpack = lisp_package;
	import(Ct, lisp_package);
	export(Ct, lisp_package);

#ifdef ANSI_COMMON_LISP
/*  	Cnil->s.s_hpack = common_lisp_package; */
	import(Cnil, common_lisp_package);
	export(Cnil, common_lisp_package);

/*  	Ct->s.s_hpack = common_lisp_package; */
	import(Ct, common_lisp_package);
	export(Ct, common_lisp_package);
#endif

/* 	sLquote = make_ordinary("QUOTE"); */
/* 	sLfunction = make_ordinary("FUNCTION"); */
	sLlambda = make_ordinary("LAMBDA");
	sLlambda_block = make_ordinary("LAMBDA-BLOCK");
	sLlambda_closure = make_ordinary("LAMBDA-CLOSURE");
	sLlambda_block_closure = make_ordinary("LAMBDA-BLOCK-CLOSURE");
	sLspecial = make_ordinary("SPECIAL");

	
	NewInit();
	gcl_init_typespec();
	gcl_init_number();
	gcl_init_character();

	gcl_init_read();
	gcl_init_bind();
	gcl_init_pathname();
	gcl_init_print();
	gcl_init_GBC();

	gcl_init_unixfasl();
	gcl_init_unixsys();
	gcl_init_unixsave();

	gcl_init_alloc_function();
	gcl_init_array_function();
	gcl_init_character_function();
	gcl_init_file_function();
	gcl_init_list_function();
	gcl_init_package_function();
	gcl_init_pathname_function();
	gcl_init_predicate_function();
	gcl_init_print_function();
	gcl_init_read_function();
	gcl_init_sequence_function();
#if  defined(KCLOVM) || defined(RUN_PROCESS)
	gcl_init_socket_function();
#endif	
	gcl_init_structure_function();
	gcl_init_string_function();
	gcl_init_symbol_function();
	gcl_init_typespec_function();
	gcl_init_hash();
	gcl_init_cfun();

	gcl_init_unixfsys();
	gcl_init_unixtime();
	gcl_init_eval();
	gcl_init_lex();
	gcl_init_prog();
	gcl_init_catch();
	gcl_init_block();
        gcl_init_macros();
	gcl_init_conditional();
	gcl_init_reference();
	gcl_init_assignment();
	gcl_init_multival();
	gcl_init_error();
	gcl_init_let();
	gcl_init_mapfun();
	gcl_init_iteration();
	gcl_init_toplevel();

	gcl_init_cmpaux();

	init_main();

	gcl_init_format();
	gcl_init_links();

	gcl_init_fat_string();
	gcl_init_sfasl();
#ifdef CMAC
	gcl_init_cmac();
#endif	
#ifdef HAVE_READLINE
	gcl_init_readline();
#endif

}
object
vs_overflow(void)
{
	if (vs_limit > vs_org + stack_multiple *  VSSIZE)
		error("value stack overflow");
	vs_limit += STACK_OVER*VSGETA;
	FEerror("Value stack overflow.", 0);
	return Cnil;
}

void
bds_overflow(void) {
	--bds_top;
	if (bds_limit > bds_org + stack_multiple *  BDSSIZE) {
            error("bind stack overflow");
        }
	bds_limit += STACK_OVER  *BDSGETA;
	FEerror("Bind stack overflow.", 0);
}

void
frs_overflow(void) {
	--frs_top;
	if (frs_limit > frs_org + stack_multiple *  FRSSIZE)
		error("frame stack overflow");
	frs_limit += STACK_OVER* FRSGETA;
	FEerror("Frame stack overflow.", 0);
}

void
ihs_overflow(void) {
	--ihs_top;
	if (ihs_limit > ihs_org + stack_multiple *  IHSSIZE)
		error("invocation history stack overflow");
	ihs_limit += STACK_OVER*IHSGETA;
	FEerror("Invocation history stack overflow.", 0);
}

void
segmentation_catcher(int i) {
  error("Segmentation violation.");
}

/* static void */
/* cs_overflow(void) { */
/* #ifdef AV */
/* 	if (cs_limit < cs_org - cssize) */
/* 		error("control stack overflow"); */
/* 	cs_limit -= CSGETA; */
/* #endif */
/* #ifdef MV */



/* #endif */
/* 	FEerror("Control stack overflow.", 0); */
/* } */

/* static void */
/* end_of_file(void) { */
/* 	error("end of file"); */
/* } */

DEFUNO_NEW("BYE",object,fLbye,LISP
       ,0,1,NONE,OO,OO,OO,OO,void,Lby,(object exitc),"")
{	int n=VFUN_NARGS;
	int exit_code;
	if (n>=1) exit_code=fix(exitc);else exit_code=0;

/*	printf("Bye.\n"); */
	exit(exit_code);

}


DEFUN_NEW("QUIT",object,fLquit,LISP
       ,0,1,NONE,OO,OO,OO,OO,(object exitc),"")
{	return FFN(fLbye)(exitc); }
 
/* DEFUN_NEW("EXIT",object,fLexit,LISP */
/*        ,0,1,NONE,OI,OO,OO,OO,(fixnum exitc),"") */
/* {	return fLbye(exitc); } */
 

/*  c_trace(void) */
/*  { */
/*  #ifdef AOSVS */

/*  #endif */
/*  } */

static void
FFN(siLargc)(void) {
  check_arg(0);
  vs_push(make_fixnum(ARGC));
}

static void
FFN(siLargv)(void) {
  int i=0;
  
  check_arg(1);
  if (type_of(vs_base[0]) != t_fixnum ||
      (i = fix(vs_base[0])) < 0 ||
      i >= ARGC)
    FEerror("Illegal argument index: ~S.", 1, vs_base[0]);
  vs_base[0] = make_simple_string(ARGV[i]);

}

static void
FFN(siLgetenv)(void) {

  char name[256];
  int i;
  char *value;
  extern char *getenv(const char *);
  
  check_arg(1);
  check_type_string(&vs_base[0]);
  if (vs_base[0]->st.st_fillp >= 256)
    FEerror("Too long name: ~S.", 1, vs_base[0]);
  for (i = 0;  i < vs_base[0]->st.st_fillp;  i++)
    name[i] = vs_base[0]->st.st_self[i];
  name[i] = '\0';
  if ((value = getenv(name)) != NULL)
    {vs_base[0] = make_simple_string(value);
#ifdef FREE_GETENV_RESULT
    free(value);
    
#endif		
    }
  else
    vs_base[0] = Cnil;

}

object *vs_marker;

static void
FFN(siLmark_vs)(void) {
  check_arg(0);
  vs_marker = vs_base;
  vs_base[0] = Cnil;
}

static void
FFN(siLcheck_vs)(void) {
  check_arg(0);
  if (vs_base != vs_marker)
    FEerror("Value stack is flawed.", 0);
  vs_base[0] = Cnil;
}

static object
FFN(siLcatch_fatal)(int i) {
  catch_fatal=i;
  return Cnil;
}

LFD(siLreset_stack_limits)(void)
{
  long i=0;

  check_arg(0);
  if(catch_fatal <0) catch_fatal=1;
#ifdef SGC	
  {extern int fault_count ; fault_count = 0;}
#endif 
  if (vs_top < vs_org + stack_multiple *  VSSIZE)
    vs_limit = vs_org + stack_multiple *  VSSIZE;
  else
    error("can't reset vs_limit");
  if (bds_top < bds_org + stack_multiple *  BDSSIZE)
    bds_limit = bds_org + stack_multiple *  BDSSIZE;
  else
    error("can't reset bds_limit");
  if (frs_top < frs_org + stack_multiple *  FRSSIZE)
    frs_limit = frs_org + stack_multiple *  FRSSIZE;
  else
    error("can't reset frs_limit");
  if (ihs_top < ihs_org + stack_multiple *  IHSSIZE)
    ihs_limit = ihs_org + stack_multiple *  IHSSIZE;
  else
    error("can't reset ihs_limit");
  if (cs_base==cs_org)
    cs_org=(void *)&i;
#ifdef __ia64__
 {
   extern void * GC_save_regs_in_stack();
   if (cs_base2==cs_org2)
     cs_org2=GC_save_regs_in_stack();
 }
#endif
  /* reset_cstack_limit(i); */
  vs_base[0] = Cnil;
}

#define COPYSTACK(org,p,typ,lim,top,geta,size) \
  {unsigned long topl=top-org;\
   bcopy(org,p,(lim-org)*sizeof(typ));\
   org=p;\
   top=org+topl;\
   lim=org+stack_multiple*size;\
   p=lim+(STACK_OVER+1)*geta;\
   }

static int
multiply_stacks(int m) {  
  void *p;
  int vs,bd,frs,ihs;
  stack_multiple=stack_multiple*m;
#define ELTSIZE(x) (((char *)((x)+1)) - ((char *) x))
  vs  = (stack_multiple*VSSIZE  + (STACK_OVER+1)*VSGETA)* ELTSIZE(vs_org);
  bd  = (stack_multiple*BDSSIZE + (STACK_OVER+1)*BDSGETA)*ELTSIZE(bds_org);
  frs = (stack_multiple*FRSSIZE + (STACK_OVER+1)*FRSGETA)*ELTSIZE(frs_org);
  ihs = (stack_multiple*IHSSIZE + (STACK_OVER+1)*IHSGETA)*ELTSIZE(ihs_org);
  if (stack_space==0) {enter_mark_origin(&stack_space);}
  stack_space = alloc_simple_string(vs+bd+frs+ihs);
  array_allocself(stack_space,1,code_char(0));
  p=stack_space->st.st_self;
  COPYSTACK(vs_org,p,object,vs_limit,vs_top,VSGETA,VSSIZE);
  COPYSTACK(bds_org,p,struct bds_bd,bds_limit,bds_top,BDSGETA,BDSSIZE);
  COPYSTACK(frs_org,p,struct frame,frs_limit,frs_top,FRSGETA,FRSSIZE);
  COPYSTACK(ihs_org,p,struct invocation_history,ihs_limit,ihs_top,
	    IHSGETA,IHSSIZE);
  vs_base=vs_top;
  return stack_multiple;
}

DEFVAR("*NO-INIT*",sSAno_initA,SI,Cnil,"");

LFD(siLinit_system)(void) {
  check_arg(0);
  gcl_init_system(sSAno_initA);
  vs_base[0] = Cnil;
}

static void
FFN(siLuser_init)(void) {
  check_arg(0);
  sLApackageA->s.s_dbind = user_package;
  user_init();
  vs_base[0] = Cnil;
}

/* static void */
/* FFN(siLaddress)(void) { */
/*   check_arg(1); */
/*   vs_base[0] = make_fixnum((long)vs_base[0]); */
/* } */

DEFUN_NEW("NANI",object,fSnani,SI,1,1,NONE,OI,OO,OO,OO,(fixnum address),"") {

  RETURN1((object)address);

}

DEFUN_NEW("ADDRESS",object,fSaddress,SI,1,1,NONE,IO,OO,OO,OO,(object x),"") {

  RETURN1(x);

}

/* static void */
/* FFN(siLnani)(void) { */
/*   check_arg(1); */

/*   switch (type_of(vs_base[0])) { */
/*   case t_fixnum: */
/*     vs_base[0]=(object)fix(vs_base[0]); */
/*     break; */
/*   default: */
/*     FEerror("Cannot coerce ~s to an address",1,vs_base[0]); */
/*   } */

/* } */

static void
FFN(siLinitialization_failure)(void) {
  check_arg(0);
  printf("lisp initialization failed\n");
  exit(0);
}

DEFUNO_NEW("IDENTITY",object,fLidentity,LISP
       ,1,1,NONE,OO,OO,OO,OO,void,Lidentity,(object x0),"")
{
	/* 1 args */
  RETURN1 (x0);
}

DEFUNO_NEW("GCL-COMPILE-TIME",object,fSgcl_compile_time,SI
       ,0,0,NONE,OO,OO,OO,OO,void,Lgcl_compile_time,(void),"")
{
  RETURN1 (make_simple_string(__DATE__ " " __TIME__));
}

DEFUNO_NEW("LDB1",object,fSldb1,SI
       ,3,3,NONE,OI,II,OO,OO,void,Lldb1,(fixnum a,fixnum b, fixnum c),"")
{
  RETURN1 (make_fixnum(((((~(-1 << (a))) << (b)) & (c)) >> (b))));
}

DEFUN_NEW("LISP-IMPLEMENTATION-VERSION",object,fLlisp_implementation_version,LISP
       ,0,0,NONE,OO,OO,OO,OO,(void),"")
{
	/* 0 args */
	RETURN1((make_simple_string(LISP_IMPLEMENTATION_VERSION)));
}


static void
FFN(siLsave_system)(void) {
  
#ifdef HAVE_YP_UNBIND
  extern object truename(),namestring();
  check_arg(1);
  /* prevent subsequent consultation of yp by getting
     truename now*/
  vs_base[0]=namestring(truename(vs_base[0]));
  {char name[200];
  char *dom = name;
  if (0== getdomainname(dom,sizeof(name)))
    yp_unbind(dom);}
#endif
  
#ifdef DO_BEFORE_SAVE
  DO_BEFORE_SAVE
#endif	
    
  saving_system = TRUE;

  minimize_image();

  saving_system = FALSE;

  Lsave();
  alloc_page(-(holepage+nrbpage));

}

DEFVAR("*LISP-MAXPAGES*",sSAlisp_maxpagesA,SI,make_fixnum(real_maxpage),"");
DEFVAR("*SYSTEM-DIRECTORY*",sSAsystem_directoryA,SI,make_simple_string(system_directory),"");
DEFVAR("*MULTIPLY-STACKS*",sSAmultiply_stacksA,SI,Cnil,"");
DEF_ORDINARY("TOP-LEVEL",sStop_level,SI,"");
DEFVAR("*COMMAND-ARGS*",sSAcommand_argsA,SI,sLnil,"");

static void
init_main(void) {

  make_function("BY", Lby);
  make_si_function("ARGC", siLargc);
  make_si_function("ARGV", siLargv);
  
  make_si_function("GETENV", siLgetenv);
  
  make_si_function("MARK-VS", siLmark_vs);
  make_si_function("CHECK-VS", siLcheck_vs);
  make_si_function("RESET-STACK-LIMITS", siLreset_stack_limits);
  make_si_function("INIT-SYSTEM", siLinit_system);
  make_si_function("USER-INIT", siLuser_init);
  /* make_si_function("ADDRESS", siLaddress); */
  /* make_si_function("NANI", siLnani); */
  make_si_function("INITIALIZATION-FAILURE",
		   siLinitialization_failure);
  
  siClisp_pagesize =
    make_si_constant("LISP-PAGESIZE", make_fixnum(PAGESIZE));
  
  
  {object features;
  
#define ADD_FEATURE(name) \
	 features=  make_cons(make_keyword(name),features)
  
  features=    make_cons(make_keyword("COMMON"),
			 make_cons(make_keyword("KCL"), Cnil));
  ADD_FEATURE("AKCL");
  ADD_FEATURE("GCL");
#ifdef BROKEN_O4_OPT
  ADD_FEATURE("BROKEN_O4_OPT");
#endif
#ifdef GMP
  ADD_FEATURE("GMP");
#endif	 
#ifdef GCL_GPROF
  ADD_FEATURE("GPROF");
#endif	 
  
#ifndef _WIN32	 
  ADD_FEATURE("UNIX");
#endif	

#ifdef _WIN32
  ADD_FEATURE("WINNT");
  ADD_FEATURE("WIN32");
#endif

#ifdef IEEEFLOAT
  ADD_FEATURE("IEEE-FLOATING-POINT");
#endif
#ifdef SGC
  ADD_FEATURE("SGC");
#endif	 
/* #ifdef  ADDITIONAL_FEATURES */
/*   ADDITIONAL_FEATURES; */
/* #endif */
  ADD_FEATURE(HOST_CPU);
  ADD_FEATURE(HOST_KERNEL);
#ifdef HOST_SYSTEM
  ADD_FEATURE(HOST_SYSTEM);
#endif
#ifdef  BSD
  ADD_FEATURE("BSD");
#endif
  
#if !defined(DOUBLE_BIGENDIAN)
  ADD_FEATURE("CLX-LITTLE-ENDIAN");
#endif
  
#ifndef PECULIAR_MACHINE
#define BIGM    (int)((((unsigned int)(-1))/2))	 
  { 
/*      int ONEM = -1; */
    int Bigm  = BIGM;
    int Smallm = -BIGM-1;
    int Seven = 7;
    int Three = 3;
    if ( (Smallm / Seven)  < 0
	 && (Smallm / (-Seven))  > 0
	 && (Bigm / (-Seven)) < 0 
	 && ((-Seven) / Three) == -2
	 && (Seven / (-Three)) == -2
	 && ((-Seven)/ (-Three)) == 2)
      { ADD_FEATURE("TRUNCATE_USE_C");
      }  }
#endif	 
  
#ifdef HAVE_READLINE
  ADD_FEATURE("READLINE");
#endif
#if !defined(USE_DLOPEN)
  ADD_FEATURE("NATIVE-RELOC");
#if defined(HAVE_LIBBFD) 
  ADD_FEATURE("BFD");
#endif
#endif
  ADD_FEATURE("UNEXEC");
#ifdef HAVE_XGCL
  ADD_FEATURE("XGCL");
#endif

#ifdef HAVE_GNU_LD
  ADD_FEATURE("GNU-LD");
#endif
  
#ifdef STATIC_LINKING
  ADD_FEATURE("STATIC");
#endif	 

  make_special("*FEATURES*",features);}
  
  make_si_function("SAVE-SYSTEM", siLsave_system);
  make_si_sfun("CATCH-FATAL",siLcatch_fatal,ARGTYPE1(f_fixnum));
  make_si_function("WARN-VERSION",Lidentity);
  
}

#ifdef SGC
#include "writable.h"
#endif

#ifdef HAVE_PRINT_INSN_I386

#include "dis-asm.h"

static char b[4096],*bp;

static int
my_fprintf(void *v,const char *f,...) {
  va_list va;
  int r;
  va_start(va,f);
  bp+=(r=vsnprintf(bp,sizeof(b)-(bp-b),f,va));
  va_end(va);
  return r;
}

static int
my_read(bfd_vma memaddr, bfd_byte *myaddr, unsigned int length, struct disassemble_info *dinfo) {
  memcpy(myaddr,(void *)(long)memaddr,length);
  return 0;
}

static void
my_pa(bfd_vma addr,struct disassemble_info *dinfo) {
  dinfo->fprintf_func(dinfo->stream,"%p",(void *)(long)addr);
}

#endif

DEFUN_NEW("DISASSEMBLE-INSTRUCTION",object,fSdisassemble_instruction,SI,1,1,NONE,OI,OO,OO,OO,(fixnum addr),"") {

#ifdef HAVE_PRINT_INSN_I386

  static disassemble_info i;
  void *v;
  int (*s)();
  int j;

  memset(&i,0,sizeof(i));
#ifdef __i386__
  i.disassembler_options="i386";
#endif
  i.fprintf_func=my_fprintf;
  i.read_memory_func=my_read;
  i.print_address_func=my_pa;
  bp=b;

  if ((v=dlopen("libopcodes.so",RTLD_NOW))) {
    if ((s=dlsym(v,"print_insn_i386"))) {
      j=s(addr,&i);
      my_fprintf(NULL," ;");
      return MMcons(make_simple_string(b),make_fixnum(j));
    }
    massert(!dlclose(v));
  }

#endif

  return MMcons(make_simple_string("fnop ;"),make_fixnum(0));

}

typedef struct {
  enum type tt;
  struct typemanager *tp;
} Tbl;

#define Tblof(a_)       {(a_),tm_of(a_)}
#define tblookup(a_,b_) ({Tbl *tb=tb1;(b_)=(a_);for (;tb->tt && tb->b_!=(b_);tb++);tb->tt;})
#define mtm_of(a_)      (a_)>=t_other ? NULL : tm_of(a_)

DEFUN_NEW("FUNCTION-BY-ADDRESS",object,fSfunction_by_address,SI,1,1,NONE,OI,OO,OO,OO,(fixnum ad),"") {

  ufixnum m=-1,mm,j;
  void *o;
  object x,xx=Cnil;
  Tbl tb1[]={Tblof(t_sfun),Tblof(t_cfun),Tblof(t_vfun),Tblof(t_afun),Tblof(t_gfun),Tblof(t_closure),Tblof(t_cclosure),{0}};
  struct typemanager *tp;
  enum type tt;
  struct pageinfo *v;

  if (VALID_DATA_ADDRESS_P(ad))
    for (v=cell_list_head;v;v=v->next)
      if (tblookup(mtm_of(v->type),tp))
	for (o=pagetochar(page(v)),j=tp->tm_nppage;j--;o+=tp->tm_size)
	  if (tblookup(type_of((x=o)),tt))
	    if (!is_free(x) && (mm=ad-(ufixnum)x->sfn.sfn_self)<m) {
	      m=mm;
	      xx=x;
	    }
  
  return xx;

}
