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

#define MUST_COPY_VA_LIST
#define NULL_OR_ON_C_STACK(x) ((x)==0 || ((((unsigned long)x) > 0x100000000) && ((unsigned long)x) < 0x120000000))
