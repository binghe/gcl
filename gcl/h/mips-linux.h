#include "linux.h"

/*  #ifdef IN_GBC */
/*  #define GET_FAULT_ADDR(sig,code,sv,a) \ */
/*      ((void *)(*((char ***)(&code)))[17]) */
/*  #endif */

/*#define NULL_OR_ON_C_STACK(x) ((x)==0 || ((unsigned int)x) > (unsigned int)(pagetochar(MAXPAGE+1)))*/

/*  #define ADDITIONAL_FEATURES \ */
/*  		     ADD_FEATURE("BSD386"); \ */
/*        	             ADD_FEATURE("MC68020") */


/*  #define	I386 */
/*  #define SGC */

/*  #define CLEAR_CACHE do {void *v=memory->cfd.cfd_start,*ve=v+memory->cfd.cfd_size; for (;v<ve;v+=32)   asm __volatile__ ("dcbst 0,%0\n\tsync\n\ticbi 0,%0\n\tsync\n\tisync": : "r" (v) : "memory");} while(0) */

#include <asm/cachectl.h>
int cacheflush(void *,int,int);
#define CLEAR_CACHE_LINE_SIZE 32
#define CLEAR_CACHE do {void *v=memory->cfd.cfd_start,*ve=v+memory->cfd.cfd_size; \
                        v=(void *)((unsigned long)v & ~(CLEAR_CACHE_LINE_SIZE - 1));\
                        cacheflush(v,ve-v,BCACHE);\
                    } while(0)


#undef MPROTECT_ACTION_FLAGS
#define MPROTECT_ACTION_FLAGS SA_RESTART|SA_SIGINFO
#ifdef IN_GBC
#define GET_FAULT_ADDR(sig,code,scp,addr) \
  ((siginfo_t *)code )->si_addr
#endif

/* Reenable when recent mips kernel bug fixed -- SIGBUS passed on
   occasion instead of SIGSEGV with no address passed in siginfo_t*/
/* kernel bug now fixed, but likely not everywhere.  Add additional
   memprotect test in sgbc.c to ensure we have a working kernel */
#define SGC 

#define RELOC_H "elf32_mips_reloc.h"
#define SPECIAL_RELOC_H "elf32_mips_reloc_special.h"

/*Remove when .MIPS.stubs are replaced with callable .plt entries*/
#define LD_BIND_NOW
