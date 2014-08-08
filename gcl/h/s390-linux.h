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

#if SIZEOF_LONG == 8
#define C_GC_OFFSET 4
#define RELOC_H "elf64_s390_reloc.h"
#define SPECIAL_RELOC_H "elf64_sparc_reloc_special.h"
#else
#define RELOC_H "elf32_s390_reloc.h"
#endif
