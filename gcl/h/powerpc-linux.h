#include "linux.h"

#ifdef IN_GBC
#undef MPROTECT_ACTION_FLAGS
#define MPROTECT_ACTION_FLAGS SA_RESTART|SA_SIGINFO
#define GET_FAULT_ADDR(sig,code,sv,a) \
 ((siginfo_t *)code)->si_addr
#endif

#define SGC

#define CLEAR_CACHE_LINE_SIZE 32
#define CLEAR_CACHE do {void *v=memory->cfd.cfd_start,*ve=v+memory->cfd.cfd_size; \
                        void *p=(void *)((unsigned long)v & ~(PAGESIZE-1));	\
			void *pe=(void *)((unsigned long)ve & ~(PAGESIZE-1)) + PAGESIZE-1; \
                        if (mprotect(p,pe-p,PROT_READ|PROT_WRITE|PROT_EXEC)) {		\
			  fprintf(stderr,"%p %p\n",p,pe);		\
			  perror("");					\
			  FEerror("Cannot mprotect", 0);		\
			}						\
                        v=(void *)((unsigned long)v & ~(CLEAR_CACHE_LINE_SIZE - 1));\
                        for (;v<ve;v+=CLEAR_CACHE_LINE_SIZE) \
                           asm __volatile__ ("dcbst 0,%0\n\tsync\n\ticbi 0,%0\n\tsync\n\tisync": : "r" (v) : "memory");\
                        } while(0)

#if SIZEOF_LONG == 4
#define RELOC_H "elf32_ppc_reloc.h"
#else
#define RELOC_H "elf64_ppc_reloc.h"
#define SPECIAL_RELOC_H "elf64_ppc_reloc_special.h"
#define STATIC_FUNCTION_POINTERS
#define C_GC_OFFSET 4
#endif
