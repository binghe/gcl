#include "linux.h"

#define MUST_COPY_VA_LIST
/* #define NULL_OR_ON_C_STACK(x) ((x)==0 || ((((unsigned long)x) > 0x100000000) && ((unsigned long)x) < 0x120000000)) */

#undef MPROTECT_ACTION_FLAGS
#define MPROTECT_ACTION_FLAGS SA_RESTART|SA_SIGINFO
/* #ifdef IN_GBC */
/* #include <ucontext.h> */
/* #define GET_FAULT_ADDR(sig,code,scp,addr) \ no longer working*/
/*   (char *)((struct ucontext *)scp )->uc_mcontext.sc_traparg_a0 */
/*#define GET_FAULT_ADDR(sig,code,sv,a) ((siginfo_t *)code)->si_addr  perhaps try this when get access*/
/* #endif */
/* #define SGC */
#undef SGC

#define RELOC_H "elf64_alpha_reloc.h"
#define SPECIAL_RELOC_H "elf64_alpha_reloc_special.h"
#define PAL_imb		134
#define imb() __asm__ __volatile__ ("call_pal %0 #imb" : : "i" (PAL_imb) : "memory")
#define CLEAR_CACHE imb()

/*FIXME probe broken in recent kernels, no access*/
#define DEFINED_REAL_MAXPAGE (1UL<<18) /*FIXME brk probe broken*/
