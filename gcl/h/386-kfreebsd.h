#include "linux.h"

#ifdef IN_GBC
#if !defined(SIGNAL_H_HAS_SIGCONTEXT) && !defined(HAVE_SIGCONTEXT)
#error Need sigcontext on 386 linux
#else
#include <signal.h>
#ifndef SIGNAL_H_HAS_SIGCONTEXT
#ifdef  HAVE_ASM_SIGCONTEXT_H     
#include <asm/sigcontext.h>
#endif
#ifdef  HAVE_ASM_SIGNAL_H          
#include <asm/signal.h>
#endif
#endif     
#endif

#undef MPROTECT_ACTION_FLAGS
#define MPROTECT_ACTION_FLAGS SA_RESTART|SA_SIGINFO
#define GET_FAULT_ADDR(sig,code,sv,a) ((siginfo_t *)code)->si_addr
#endif

#define ADDITIONAL_FEATURES \
		     ADD_FEATURE("BSD386"); \
      	             ADD_FEATURE("MC68020")


#define	I386
#define SGC

#define RELOC_H "elf32_i386_reloc.h"

#define BRK_DOES_NOT_GUARANTEE_ALLOCATION
#define FREEBSD
