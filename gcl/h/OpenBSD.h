/*
 * OpenBSD.h for gcl 1.1
 *
 *
 */

#include "bsd.h"

#undef LD_COMMAND
#define LD_COMMAND(command,main,start,input,ldarg,output) \
  sprintf(command, "ld -dc -N -x -A %s -T %x %s %s -o %s", \
            main,start,input,ldarg,output)

#define ADDITIONAL_FEATURES \
		     ADD_FEATURE("386BSD");\
                     ADD_FEATURE("OpenBSD");


#define I386
#define	IEEEFLOAT

/* #undef HAVE_XDR */

#define USE_ATT_TIME

#undef LISTEN_FOR_INPUT
#define LISTEN_FOR_INPUT(fp) \
  if (((FILE *)fp)->_r <=0 && (c=0, ioctl(((FILE *)fp)->_file, FIONREAD, &c), c<=0)) \
	return(FALSE)

#ifdef IN_GBC
#include <sys/types.h>
#endif

#define DATA_BEGIN (char *) N_DATADDR(header);
#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#define start_of_data() &etext
#define start_of_text() ((char *)(sizeof(struct exec) + getpagesize()))

#undef UNIXSAVE
#ifdef UNIXSAVE 
 extern char etext;
#endif

#define RELOC_FILE "rel_sun3.c"	/* for SFASL - enabled in bsd.h */

#ifdef CLOCKS_PER_SEC
#define HZ CLOCKS_PER_SEC
#else
#define HZ 128
#endif
#define ss_base ss_sp

/* begin for GC */
#define PAGEWIDTH 12		/* i386 sees 4096 byte pages */
/* end for GC */

/*
 * The next two defines are for SGC,
 *	one of which needs to go in cmpinclude.h.
 */
#define SIGPROTV SIGBUS

/* Begin for cmpinclude */
#define SGC	/* can mprotect pages and so selective gc will work */

/* End for cmpinclude */

#if defined(IN_UNIXTIME)
# include <time.h>
#endif
