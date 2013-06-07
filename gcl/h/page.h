#ifndef DBEGIN
#define DBEGIN 0
#endif

#define MAYBE_DATA_P(pp) ((char *)(pp)> (char *) DBEGIN)
#define VALID_DATA_ADDRESS_P(pp) (MAYBE_DATA_P(pp) &&  ((char *)(pp) < heap_end))


#ifndef page
#define page(p)	((unsigned long)(((unsigned long)(((char *)(p))-DBEGIN)>>PAGEWIDTH)))
#define	pagetochar(x)	((char *)((((unsigned long)x) << PAGEWIDTH) + DBEGIN + sizeof(struct pageinfo)))
#define pageinfo(x) ((struct pageinfo *)((((ufixnum)x)>>PAGEWIDTH)<<PAGEWIDTH))
#define pagetoinfo(x) ((struct pageinfo *)((((ufixnum)x)<<PAGEWIDTH)+DBEGIN))
#endif
  
#ifdef UNIX
#define CHECK_FOR_INTERRUPT \
   if (interrupt_flag) sigint()
#else
#define CHECK_FOR_INTERRUPT
#endif

/* alignment required for pointers */
#ifndef PTR_ALIGN
#define PTR_ALIGN SIZEOF_LONG
#endif

#define ROUND_UP_PTR(n)	(((long)(n) + (PTR_ALIGN-1)) & ~(PTR_ALIGN-1))
#define ROUND_DOWN_PTR(n) (((long)(n)  & ~(PTR_ALIGN-1)))

/* alignment required for contiguous pointers */
#if PTR_ALIGN < SIZEOF_CONTBLOCK
#define CPTR_ALIGN SIZEOF_CONTBLOCK
#else
#define CPTR_ALIGN PTR_ALIGN
#endif

#define ROUND_UP_PTR_CONT(n)	(((long)(n) + (CPTR_ALIGN-1)) & ~(CPTR_ALIGN-1))
#define ROUND_DOWN_PTR_CONT(n) (((long)(n)  & ~(CPTR_ALIGN-1)))


#ifdef SGC

#define NORMAL_PAGE 0

/* Contains objects which will be gc'd */
#define SGC_PAGE_FLAG  1       

/* keep writable eg malloc's for system call */
#define SGC_PERM_WRITABLE 2    

#define SGC_WRITABLE  (SGC_PERM_WRITABLE | SGC_PAGE_FLAG)

#define GCL_PAGE(p)         ((unsigned long)p<MAXPAGE)
#define WRITABLE_PAGE_P(p)  (GCL_PAGE(p) && IS_WRITABLE(p))
#define ON_WRITABLE_PAGE(x) WRITABLE_PAGE_P(page(x))

#define  IF_WRITABLE(x,if_code) ({unsigned long xSG= page(x);if(xSG < MAXPAGE && IS_WRITABLE(xSG)) {if_code;}})/*FIXME maxpage*/

#define sgc_mark_object(x) IF_WRITABLE(x,if(!is_marked(x)) sgc_mark_object1(x))

/* When not 0, the free lists in the type manager are freelists
   on SGC_PAGE's, for those types supporting sgc.
   Marking and sweeping is done specially */
   
int sgc_on;


/* for the S field of the FIRSTWORD */
enum sgc_type { SGC_NORMAL,   /* not allocated since the last sgc */
                SGC_RECENT    /* allocated since last sgc */
		};


#define TM_BASE_TYPE_P(i) (tm_table[i].tm_type == i)

/* check if a relblock address is new relblock */
#define SGC_RELBLOCK_P(x)  ((char *)(x) >= rb_start)

/* the following assumes that the char s,m fields of first word
   have same length as a short
   (x->d.m || x->d.s) would be an equivalent for our purposes */
/* struct sgc_firstword {short t; short sm;}; */
#define SGC_OR_M(x)  (is_marked_or_free((object)x) || (valid_cdr((object)x) ? pageinfo(x)->sgc_flags&SGC_PAGE_FLAG : ((object)x)->d.s))

#ifndef SIGPROTV
#define SIGPROTV SIGSEGV
#endif

#ifndef INSTALL_MPROTECT_HANDLER
#define INSTALL_MPROTECT_HANDLER gcl_signal(SIGPROTV, memprotect_handler)
#endif

#else  /* END SGC */
#define sgc_quit()
#define sgc_start()
#define sgc_count_type(x) 0
#endif     

extern int sgc_enabled;
#define TM_NUSED(pt) (((pt).tm_npage*(pt).tm_nppage) - (pt).tm_nfree - (pt).tm_alt_nfree)


extern long resv_pages;
extern int reserve_pages_for_signal_handler;
#define	available_pages	((fixnum)(real_maxpage-page(heap_end)-2*nrbpage-resv_pages))

extern struct pageinfo *cell_list_head,*cell_list_tail,*contblock_list_head,*contblock_list_tail;

#define PAGE_MAGIC 0x2e

extern unsigned long first_data_page;
extern unsigned char *wrimap;
extern fixnum writable_pages;

#define CLEAR_WRITABLE(i) set_writable(i,0)
#define SET_WRITABLE(i) set_writable(i,1)
#define IS_WRITABLE(i) is_writable(i)


#if !defined(IN_MAIN) && defined(SGC)
#include "writable.h"
#endif
