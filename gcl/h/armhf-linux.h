#include "linux.h"

#ifdef IN_GBC
#undef MPROTECT_ACTION_FLAGS
#define MPROTECT_ACTION_FLAGS SA_RESTART|SA_SIGINFO
#define GET_FAULT_ADDR(sig,code,sv,a) \
 ((siginfo_t *)code)->si_addr
/*  #define GET_FAULT_ADDR(sig,code,sv,a) \ */
/*      ((void *)(*((char ***)(&code)))[44]) */
#endif

#define SGC

#define RELOC_H "elf32_armhf_reloc.h"
#define SPECIAL_RELOC_H "elf32_armhf_reloc_special.h"

#define NEED_STACK_CHK_GUARD
