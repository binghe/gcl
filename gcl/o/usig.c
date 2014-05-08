/*
 Copyright (C) 1994  W. Schelter

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

#ifdef __MINGW32__
#include <sys/types.h>          /* sigset_t */
#endif

#ifndef IN_UNIXINT
#include "include.h"


#include <signal.h>
#endif

#ifdef USIG
#include USIG
#else

#ifdef HAVE_SIGACTION
#define HAVE_SIGPROCMASK
#endif


#include "usig.h"

extern char signals_handled[];

void
gcl_signal(int signo, void (*handler) (/* ??? */))
{
  char *p = signals_handled;
  while (*p)
    { if (*p==signo)
	{our_signal_handler[signo] = handler;
	 handler = main_signal_handler;
	 break;
       }
      p++;}
 
  {      
  
#ifdef HAVE_SIGACTION
    struct sigaction action;
    action.sa_handler = handler;
/*    action.sa_flags =  SA_RESTART | ((signo == SIGSEGV || signo == SIGBUS) ? SV_ONSTACK : 0) */
   action.sa_flags = SA_RESTART | ((signo == SIGSEGV || signo == SIGBUS) ? SA_ONSTACK : 0)  
#ifdef SA_SIGINFO
    | SA_SIGINFO
#endif      
      ;
    sigemptyset(&action.sa_mask);
    /* sigaddset(&action.sa_mask,signo); */
    sigaction(signo,&action,0);
#else
#ifdef HAVE_SIGVEC
    struct sigvec vec;
    vec.sv_handler =  handler;
    vec.sv_flags =  (signo == SIGSEGV || signo == SIGBUS ? SV_ONSTACK : 0);
    vec.sv_mask = sigmask(signo);
    sigvec(signo,&vec,0);
#else
    signal(signo,handler);
#endif
#endif
  }
}

/* remove the signal n from the signal mask */
int
unblock_signals(int n, int m)
{
  int result = 0;
  int current_mask;
#ifdef  SIG_UNBLOCK_SIGNALS
  SIG_UNBLOCK_SIGNALS(result,n,n);
#else  
#ifdef HAVE_SIGPROCMASK
  /* posix */
  { sigset_t set,oset;
    sigemptyset(&set);
    sigaddset(&set,n);
    sigaddset(&set,m);
    sigprocmask(SIG_UNBLOCK,&set,&oset);
    current_mask=0;
    result =((sigismember(&oset,n) ? signal_mask(n) : current_mask)
              |(sigismember(&oset,m) ? signal_mask(m) : current_mask));
  }
#else

  current_mask = sigblock(0);
  sigsetmask(~(sigmask(m)) & ~(sigmask(n)) & current_mask);
  result = (current_mask & sigmask(m) ? signal_mask(m) : 0)
    | (current_mask & sigmask(n) ? signal_mask(n) : 0);
#endif
#endif
  return result;
}

void
unblock_sigusr_sigio(void)
{ 
#ifdef HAVE_SIGPROCMASK
  /* posix */
  { sigset_t set;
    sigemptyset(&set);
    sigaddset(&set,SIGUSR1);
    sigaddset(&set,SIGIO);
    sigprocmask( SIG_UNBLOCK,&set,0);
  }
#else
  int current_mask = sigblock(0);
  return sigsetmask(~(sigmask(SIGIO))&~(sigmask(SIGUSR1)) & current_mask);
#endif
}

DEFCONST("+MC-CONTEXT-OFFSETS+",sSPmc_context_offsetsP,SI,FPE_INIT,"");

#define ASM __asm__ __volatile__

DEFUN_NEW("FLD",object,fSfld,SI,1,1,NONE,OI,OO,OO,OO,(fixnum val),"") {
  double d;
  ASM ("fldt %1;fstpl %0" : "=m" (d): "m" (*(char *)val));
  RETURN1(make_longfloat(d));
}
DEFUN_NEW("*FIXNUM",fixnum,fSAfixnum,SI,1,1,NONE,II,OO,OO,OO,(fixnum addr),"") {
  RETURN1(*(fixnum *)addr);
}
DEFUN_NEW("*FLOAT",object,fSAfloat,SI,1,1,NONE,OI,OO,OO,OO,(fixnum addr),"") {
  RETURN1(make_shortfloat(*(float *)addr));
}
DEFUN_NEW("*DOUBLE",object,fSAdouble,SI,1,1,NONE,OI,OO,OO,OO,(fixnum addr),"") {
  RETURN1(make_longfloat(*(double *)addr));
}

#define _GNU_SOURCE 1
#include <fenv.h>

DEFUN_NEW("FEENABLEEXCEPT",fixnum,fSfeenableexcept,SI,1,1,NONE,II,OO,OO,OO,(fixnum x),"") {

#ifdef HAVE_FEENABLEEXCEPT

  x=feenableexcept(x);

#else
#define ASM __asm__ __volatile__
  {
    fixnum j;
    unsigned short s;
    unsigned int i;
    ASM("fnstcw %0" :: "m" (s));
    s=(s|FE_ALL_EXCEPT)&(~x);
    ASM("fldcw %0" : "=m" (s));
    ASM("stmxcsr %0" :: "m" (i));
    i=(i|(FE_ALL_EXCEPT<<7))&(~(x<<7));
    ASM("ldmxcsr %0" : "=m" (i));
  }    
#endif

  RETURN1(x);

}

DEFUN_NEW("FETESTEXCEPT",fixnum,fSfetestexcept,SI,1,1,NONE,II,OO,OO,OO,(fixnum excepts),"") {
#ifdef HAVE_FEENABLEEXCEPT
  excepts=fetestexcept(excepts);
#else
  excepts=(FFN(fSfnstsw)()&excepts)|(~(FFN(fSstmxcsr)()>>7)&excepts);
#endif  
  RETURN1(excepts);
}

DEFUN_NEW("FPE_CODE",fixnum,fSfpe_code,SI,0,0,NONE,II,OO,OO,OO,(void),"") {

  RETURN1(FFN(fSfetestexcept)(FE_DIVBYZERO) ? FPE_FLTDIV :
	  (FFN(fSfetestexcept)(FE_INVALID) ? FPE_FLTINV :
	   (FFN(fSfetestexcept)(FE_OVERFLOW) ? FPE_FLTOVF :
	    (FFN(fSfetestexcept)(FE_UNDERFLOW) ? FPE_FLTUND :
	     (FFN(fSfetestexcept)(FE_INEXACT) ? FPE_FLTRES : 0)))));
}

DEFUN_NEW("FNSTSW",fixnum,fSfnstsw,SI,0,0,NONE,II,OO,OO,OO,(void),"") {
  unsigned short t;
  asm ("fnstsw %0" :: "m" (t));
  RETURN1(t);
}
DEFUN_NEW("STMXCSR",fixnum,fSstmxcsr,SI,0,0,NONE,II,OO,OO,OO,(void),"") {
  unsigned int t;
  asm ("stmxcsr %0" :: "m" (t));
  RETURN1(t);
}

DEFUN_NEW("FEDISABLEEXCEPT",fixnum,fSfedisableexcept,SI,0,0,NONE,IO,OO,OO,OO,(void),"") {

  fixnum x;

#ifdef HAVE_FEENABLEEXCEPT

  feclearexcept(FE_ALL_EXCEPT);
  x=fedisableexcept(FE_ALL_EXCEPT);

#else
#define ASM __asm__ __volatile__
  {
    unsigned int i;
    ASM("fnclex");
    ASM("stmxcsr %0" :: "m" (i));
    i=(i|(FE_ALL_EXCEPT<<7));
    ASM("ldmxcsr %0" : "=m" (i));
    x=0;
  }
#endif

  RETURN1(x);
}


DEFCONST("+FE-LIST+",sSPfe_listP,SI,list(5,
					 list(3,sLdivision_by_zero,make_fixnum(FPE_FLTDIV),make_fixnum(FE_DIVBYZERO)),
					 list(3,sLfloating_point_overflow,make_fixnum(FPE_FLTOVF),make_fixnum(FE_OVERFLOW)),
					 list(3,sLfloating_point_underflow,make_fixnum(FPE_FLTUND),make_fixnum(FE_UNDERFLOW)),
					 list(3,sLfloating_point_inexact,make_fixnum(FPE_FLTRES),make_fixnum(FE_INEXACT)),
					 list(3,sLfloating_point_invalid_operation,make_fixnum(FPE_FLTINV),make_fixnum(FE_INVALID))),"");

DEF_ORDINARY("FLOATING-POINT-ERROR",sSfloating_point_error,SI,"");

static void
sigfpe3(int sig,siginfo_t *i,void *p) {

  ucontext_t *v=p;

  unblock_signals(SIGFPE,SIGFPE);
  ifuncall3(sSfloating_point_error,FPE_CODE(i),FPE_ADDR(i,v),FPE_CTXT(v));

}

static void
sigpipe(void)
{
	gcl_signal(SIGPIPE, sigpipe);
	perror("");
	FEerror("Broken pipe", 0);
}


void
sigint(void)
{
  unblock_signals(SIGINT,SIGINT);
  terminal_interrupt(1);
}



static void
sigalrm(void)
{
  unblock_signals(SIGALRM,SIGALRM);
  raise_pending_signals(sig_try_to_delay);
}

DEFVAR("*INTERRUPT-ENABLE*",sSAinterrupt_enableA,SI,sLt,"");

DEF_ORDINARY("SIGUSR1-INTERRUPT",sSsigusr1_interrupt,SI,"");
DEF_ORDINARY("SIGIO-INTERRUPT",sSsigio_interrupt,SI,"");

static void
sigusr1(void)
{ifuncall1(sSsigusr1_interrupt,Cnil);}

static void
sigio(void)
{ifuncall1(sSsigio_interrupt,Cnil);}



void
install_default_signals(void)
{	gcl_signal(SIGFPE, sigfpe3);
	gcl_signal(SIGPIPE, sigpipe);
	gcl_signal(SIGINT, sigint);
	gcl_signal(SIGUSR1, sigusr1);
	gcl_signal(SIGIO, sigio);
	gcl_signal(SIGALRM, sigalrm);
	
	/*install_segmentation_catcher(); */
	signals_allowed = sig_normal;
      }


	

#endif
