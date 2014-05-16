/*
 Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

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

/*
	object.h
*/

/*
	Some system constants.
*/

#if SIZEOF_LONG < 8
#define SPAD object pad
#else
#define SPAD
#endif

#define	TRUE		1	/*  boolean true value  */
#define	FALSE		0	/*  boolean false value  */

#define	NBPP		4	/*  number of bytes per pointer  */

#ifndef PAGEWIDTH
#define	PAGEWIDTH	11	/*  page width  */
#endif
				/*  log2(PAGESIZE)  */
#undef PAGESIZE
#define	PAGESIZE	(1L << PAGEWIDTH)	/*  page size in bytes  */


#define	CHCODELIM	256	/*  character code limit  */
				/*  ASCII character set  */
#define	CHFONTLIM	1	/*  character font limit  */
#define	CHBITSLIM	1	/*  character bits limit  */
#define	CHCODEFLEN	8	/*  character code field length  */
#define	CHFONTFLEN	0	/*  character font field length  */
#define	CHBITSFLEN      0	/*  character bits field length  */

#define	PHTABSIZE	512	/*  number of entries  */
				/*  in the package hash table  */

#define	ARANKLIM	64	/*  array rank limit  */

#define	RTABSIZE	CHCODELIM
				/*  read table size  */

#define	CBMINSIZE	64	/*  contiguous block minimal size  */

#ifndef CHAR_SIZE
#define CHAR_SIZE        8     /* number of bits in a char */
#endif

#undef bool
typedef int bool;
typedef long fixnum;
typedef unsigned long ufixnum;
typedef float shortfloat;
typedef double longfloat;
typedef unsigned short fatchar;

#ifndef plong
#define plong long
#endif


#define SIGNED_CHAR(x) (((char ) -1) < (char )0 ? (char) x \
		  : (x >= (1<<(CHAR_SIZE-1)) ? \
		     x - (((int)(1<<(CHAR_SIZE-1))) << 1) \
		     : (char ) x))


/*
	Definition of the type of LISP objects.
*/
typedef union lispunion *object;

typedef union int_object iobject;
union int_object {object o; fixnum i;};

/*
	OBJect NULL value.
	It should not coincide with any legal object value.
*/
/* #define	OBJNULL		((object)NULL) */

/*
	Definition of each implementation type.
*/

/* #define	Mfix(obje)	(obje)->FIX.FIXVAL */
/* #define fix(x) Mfix(x) */

#define	SMALL_FIXNUM_LIMIT	1024


enum stype {			/*  symbol type  */
	stp_ordinary,		/*  ordinary  */
	stp_constant,		/*  constant  */
        stp_special		/*  special  */
};

#define	Cnil			((object)&Cnil_body)
#define	Ct			((object)&Ct_body)
#define sLnil Cnil
#define sLt Ct

#define	NOT_SPECIAL		((void (*)())Cnil)
#define	s_fillp		st_fillp
#define	s_self		st_self

#define NOT_OBJECT_ALIGNED(a_) ({union lispunion _t={.vw=(void *)(a_)};_t.td.emf;})

/*
	The values returned by intern and find_symbol.
	File_symbol may return 0.
*/
#define	INTERNAL	1
#define	EXTERNAL	2
#define	INHERITED	3

/*
	All the packages are linked through p_link.
*/
EXTER struct package *pack_pointer;	/*  package pointer  */

#ifdef WIDE_CONS
#define Scdr(a_) (a_)->c.c_cdr
#else
#define Scdr(a_) ({union lispunion _t={.vw=(a_)->c.c_cdr};unmark(&_t);_t.vw;})
#endif

enum httest {			/*  hash table key test function  */
	htt_eq,			/*  eq  */
	htt_eql,		/*  eql  */
	htt_equal		/*  equal  */
};

enum aelttype {			/*  array element type  */
	aet_object,		/*  t  */
	aet_ch,			/*  string-char  */
	aet_bit,		/*  bit  */
	aet_fix,		/*  fixnum  */
	aet_sf,			/*  short-float  */
	aet_lf,			/*  plong-float  */
	aet_char,               /* signed char */
        aet_uchar,               /* unsigned char */
	aet_short,              /* signed short */
	aet_ushort,             /*  unsigned short   */
	aet_last
	  };

#define USHORT_GCL(x,i) (((unsigned short *)(x)->ust.ust_self)[i])
#define SHORT_GCL(x,i) ((( short *)(x)->ust.ust_self)[i])

#define BV_OFFSET(x) ((type_of(x)==t_bitvector ? x->bv.bv_offset : \
		       type_of(x)== t_array ? x->a.a_offset : (abort(),0)))

#define SET_BV_OFFSET(x,val) ((type_of(x)==t_bitvector ? x->bv.bv_offset = val : \
		       type_of(x)== t_array ? x->a.a_offset=val : (abort(),0)))

#define S_DATA(x) ((struct s_data *)((x)->str.str_self))
#define SLOT_TYPE(def,i) (((S_DATA(def))->raw->ust.ust_self[i]))
#define SLOT_POS(def,i) USHORT_GCL(S_DATA(def)->slot_position,i)
#define STREF(type,x,i) (*((type *)(((char *)((x)->str.str_self))+(i))))
#define STSET(type,x,i,val)  do{SGC_TOUCH(x);STREF(type,x,i) = (val);} while(0)


enum smmode {			/*  stream mode  */
	smm_input,		/*  input  */
	smm_output,		/*  output  */
	smm_io,			/*  input-output  */
	smm_probe,		/*  probe  */
	smm_synonym,		/*  synonym  */
	smm_broadcast,		/*  broadcast  */
	smm_concatenated,	/*  concatenated  */
	smm_two_way,		/*  two way  */
	smm_echo,		/*  echo  */
	smm_string_input,	/*  string input  */
	smm_string_output,	/*  string output  */
	smm_user_defined,        /*  for user defined */
	smm_socket		/*  Socket stream  */
};

/* for any stream that takes writec_char, directly (not two_way or echo)
   ie. 	 smm_output,smm_io, smm_string_output, smm_socket
 */
#define STREAM_FILE_COLUMN(str) ((str)->sm.sm_int1)

/* for smm_echo */
#define ECHO_STREAM_N_UNREAD(strm) ((strm)->sm.sm_int0)

/* file fd for socket */
#define SOCKET_STREAM_FD(strm) ((strm)->sm.sm_fd)
#define SOCKET_STREAM_BUFFER(strm) ((strm)->sm.sm_object1)

/*  for     smm_string_input  */
#define STRING_INPUT_STREAM_NEXT(strm) ((strm)->sm.sm_int0)
#define STRING_INPUT_STREAM_END(strm) ((strm)->sm.sm_int1)

/* for smm_two_way and smm_echo */
#define STREAM_OUTPUT_STREAM(strm) ((strm)->sm.sm_object1)
#define STREAM_INPUT_STREAM(strm) ((strm)->sm.sm_object0)

/* for smm_string_{input,output} */
#define STRING_STREAM_STRING(strm) ((strm)->sm.sm_object0)

/* flags */
#define GET_STREAM_FLAG(strm,name) ((strm)->sm.sm_flags & (1<<(name)))
#define SET_STREAM_FLAG(strm,name,val) {if (val) (strm)->sm.sm_flags |= (1<<(name)); else (strm)->sm.sm_flags &= ~(1<<(name));} 

#define GCL_MODE_BLOCKING 1
#define GCL_MODE_NON_BLOCKING 0
#define GCL_TCP_ASYNC 1
     
enum gcl_sm_flags {
  gcl_sm_blocking=1,
  gcl_sm_tcp_async,
  gcl_sm_input,
  gcl_sm_output,
  gcl_sm_had_error
  
  
};
  
#ifdef BSD
#ifdef SUN3
#define	BASEFF		(unsigned char *)0xffffffff
#else
#define	BASEFF		(char *)0xffffffff
#endif
#endif

#ifdef ATT
#define	BASEFF		(unsigned char *)0xffffffff
#endif

#ifdef E15
#define	BASEFF		(unsigned char *)0xffffffff
#endif

#ifdef MV


#endif

enum chattrib {			/*  character attribute  */
	cat_whitespace,		/*  whitespace  */
	cat_terminating,	/*  terminating macro  */
	cat_non_terminating,	/*  non-terminating macro  */
	cat_single_escape,	/*  single-escape  */
	cat_multiple_escape,	/*  multiple-escape  */
	cat_constituent		/*  constituent  */
};

struct rtent {				/*  read table entry  */
	enum chattrib	rte_chattrib;	/*  character attribute  */
	object		rte_macro;	/*  macro function  */
	object		*rte_dtab;	/*  pointer to the  */
					/*  dispatch table  */
					/*  NULL for  */
					/*  non-dispatching  */
					/*  macro character, or  */
					/*  non-macro character  */
};


/* struct character character_table1[256+128]; */

/* EXTER */
/* union lispunion small_fixnum_table[2*SMALL_FIXNUM_LIMIT]; */

/* #define	small_fixnum(i)  \ */
/* 	(object)(small_fixnum_table+SMALL_FIXNUM_LIMIT+(i)) */



#define address_int unsigned long

/*
	The struct of free lists.
*/
struct freelist {
	FIRSTWORD;
	address_int f_link;
};
#ifndef INT_TO_ADDRESS
#define INT_TO_ADDRESS(x) ((object )(long )x)
#endif

#define F_LINK(x) ((struct freelist *)(long) x)->f_link
#define FL_LINK F_LINK
#define SET_LINK(x,val) F_LINK(x) = (address_int) (val)
#define OBJ_LINK(x) ((object) INT_TO_ADDRESS(F_LINK(x)))

#define	FREE	(-1)		/*  free object  */

/*
	Type_of.
*/
/* #define	type_of(obje)	((enum type)(((object)(obje))->d.t)) */

/*
	Storage manager for each type.
*/
struct typemanager {
  enum type tm_type;             /*  type  */
  long	    tm_size;             /*  element size in bytes  */
  long      tm_nppage;           /*  number per page  */
  object    tm_free;             /*  free list  */
				 /*  Note that it is of type object.  */
  long	    tm_nfree;            /*  number of free elements  */
  long	    tm_npage;            /*  number of pages  */
  long	    tm_maxpage;          /*  maximum number of pages  */
  char	   *tm_name;             /*  type name  */
  long	    tm_gbccount;         /*  GBC count  */
  object    tm_alt_free;         /*  Alternate free list (swap with tm_free) */
  long      tm_alt_nfree;        /*  Alternate nfree (length of nfree) */
  long	    tm_alt_npage;        /*  number of pages  */
  long      tm_sgc;              /*  this type has at least this many sgc pages */
  long      tm_sgc_minfree;      /*  number free on a page to qualify for being an sgc page */
  long      tm_sgc_max;          /* max on sgc pages */
  long      tm_min_grow;         /* min amount to grow when growing */
  long      tm_max_grow;         /* max amount to grow when growing */
  long      tm_growth_percent;   /* percent to increase maxpages */
  long      tm_percent_free;     /* percent which must be free after a gc for this type */
  long      tm_distinct;         /* pages of this type are distinct */
  float     tm_adjgbccnt;
  long      tm_opt_maxpage;
};


/*
	The table of type managers.
*/
EXTER struct typemanager tm_table[ 32  /* (int) t_relocatable */];

#define	tm_of(t)	(&(tm_table[(int)tm_table[(int)(t)].tm_type]))

/*
	Contiguous block header.
*/
EXTER bool prefer_low_mem_contblock;
struct contblock {		/*  contiguous block header  */
	int	cb_size;	/*  size in bytes  */
	struct contblock
		*cb_link;	/*  contiguous block link  */
};

/*
	The pointer to the contiguous blocks.
*/
EXTER struct contblock *cb_pointer;	/*  contblock pointer  */

/* SGC cont pages: After SGC_start, old_cb_pointer will be a linked
   list of free blocks on non-SGC pages, and cb_pointer will be
   likewise for SGC pages.  CM 20030827*/
EXTER struct contblock *old_cb_pointer;	/*  old contblock pointer when in SGC  */

/*
	Variables for memory management.
*/
EXTER long ncb;			/*  number of contblocks  */
#define ncbpage tm_table[t_contiguous].tm_npage
#define maxcbpage tm_table[t_contiguous].tm_maxpage
#define cbgbccount tm_table[t_contiguous].tm_gbccount  
  

EXTER long holepage;			/*  hole pages  */
#define nrbpage tm_table[t_relocatable].tm_npage
#define maxrbpage tm_table[t_relocatable].tm_maxpage
#define rbgbccount tm_table[t_relocatable].tm_gbccount
EXTER long new_holepage,starting_hole_div,starting_relb_heap_mult;
  

#ifdef SGC
EXTER char *old_rb_start;			/*  read-only relblock start  */
#endif
EXTER char *rb_start;			/*  relblock start  */
EXTER char *rb_end;			/*  relblock end  */
EXTER char *rb_limit;			/*  relblock limit  */
EXTER char *rb_pointer;		/*  relblock pointer  */
EXTER char *rb_start1;		/*  relblock start in copy space  */
EXTER char *rb_pointer1;		/*  relblock pointer in copy space  */

EXTER char *heap_end;			/*  heap end  */
EXTER char *core_end;			/*  core end  */
EXTER 
char *tmp_alloc;

/* make f allocate enough extra, so that we can round
   up, the address given to an even multiple.   Special
   case of size == 0 , in which case we just want an aligned
   number in the address range
   */

#define ALLOC_ALIGNED(f, size,align) \
  (align <= sizeof(plong) ? (char *)((f)(size)) : \
   (tmp_alloc = (char *)((f)(size+(size ?(align)-1 : 0)))+(align)-1 , \
   (char *)(align * (((unsigned long)tmp_alloc)/align))))
#define AR_ALLOC(f,n,type) (type *) \
  (ALLOC_ALIGNED(f,(n)*sizeof(type),sizeof(type)))


/* FIXME  Make all other page constants scale similarly by default. */
/* #ifndef HOLEPAGE */
/* #define	HOLEPAGE	(MAXPAGE/10) */
/* #endif */


/* /\* #define	INIT_HOLEPAGE	150 *\/ */
/* /\* #define	INIT_NRBPAGE	50 *\/ */
/* /\* #define	RB_GETA		512 *\/ */

/* #define	INIT_HOLEPAGE	(6*HOLEPAGE/5) */
/* #define	INIT_NRBPAGE	(INIT_HOLEPAGE/30) */
#define	RB_GETA		PAGESIZE


#ifdef AV
#define	STATIC	register
#endif
#ifdef MV

#endif

#define	TIME_ZONE	(-9)
/* EXTER  */
/* fixnum FIXtemp; */

/*  For IEEEFLOAT, the double may have exponent in the second word
(little endian) or first word.*/

#if !defined(DOUBLE_BIGENDIAN)
#define HIND 1  /* (int) of double where the exponent and most signif is */
#define LIND 0  /* low part of a double */
#else /* big endian */
#define HIND 0
#define LIND 1
#endif
#ifndef VOL
#define VOL volatile
#endif


#define	isUpper(xxx)	(((xxx)&0200) == 0 && isupper((int)xxx))
#define	isLower(xxx)	(((xxx)&0200) == 0 && islower((int)xxx))
#define	isDigit(xxx)	(((xxx)&0200) == 0 && isdigit((int)xxx))
enum ftype {f_object,f_fixnum};
EXTER 
char *alloca_val;
/*          ...xx|xx|xxxx|xxxx|   
		     ret  Narg     */

/*    a9a8a7a6a5a4a3a4a3a2a1a0rrrrnnnnnnnn
         ai=argtype(i)         ret   nargs
 */
#define SFUN_NARGS(x) (x & 0xff) /* 8 bits */
#define RESTYPE(x) (x<<8)   /* 3 bits */
   /* set if the VFUN_NARGS = m ; has been set correctly */
#define VFUN_NARG_BIT (1 <<11) 
#define ARGTYPE(i,x) ((x) <<(12+(i*2)))
#define ARGTYPE1(x)  (1 | ARGTYPE(0,x))
#define ARGTYPE2(x,y) (2 | ARGTYPE(0,x)  | ARGTYPE(1,y))
#define ARGTYPE3(x,y,z) (3 | ARGTYPE(0,x) | ARGTYPE(1,y) | ARGTYPE(2,z))

object make_si_sfun();
EXTER object MVloc[10];

/* Set new to be an (object *) whose [i]'th elmt is the
   ith elmnt in a va_list
   if 
   ((vl[0] == va_arg(ap,object)) ||
    (vl[1] == va_arg(ap,object)) || .. vl[n-1] == va_arg(ap,object))
   you may set
   #define DONT_COPY_VA_LIST
   In recent versions of gcc, i think the builtin_alist stuff does not
   allow setting this.
 */
#ifdef DONT_COPY_VA_LIST
#define COERCE_VA_LIST(new,vl,n) new = (object *) (vl)
#else
#define COERCE_VA_LIST(new,vl,n) \
 object Xxvl[65]; \
 {int i; \
  new=Xxvl; \
  if (n >= 65) FEerror("Too plong vl",0); \
  for (i=0 ; i < (n); i++) new[i]=va_arg(vl,object);}
#endif

#ifdef DONT_COPY_VA_LIST
#error Cannot set DONT_COPY_VA_LIST in ANSI C
#else
#define COERCE_VA_LIST_NEW(new,fst,vl,n) \
 object Xxvl[65]; \
 {int i; \
  new=Xxvl; \
  if (n >= 65) FEerror("va_list too long",0); \
  for (i=0 ; i < (n); i++) new[i]=i ? va_arg(vl,object) : fst;}
#endif



#define make_si_vfun(s,f,min,max) \
  make_si_vfun1(s,f,min | (max << 8))

/* Number of args supplied to a variable arg t_vfun
 Used by the C function to set optionals */
#define  VFUN_NARGS fcall.argd

#define RETURN2(x,y) do{/*  object _x = (void *) x;  */\
			  fcall.values[2]=y;fcall.nvalues=2; \
			  return (x) ;} while(0)
#define RETURN1(x) do{fcall.nvalues=1; return (x) ;} while(0)
#define RETURN0  do{fcall.nvalues=0; return Cnil ;} while(0)

#define RV(x) (*_p++ = x)

#define RETURNI(n,val1,listvals) RETURN(n,int,val1,listvals)
#define RETURNO(n,val1,listvals) RETURN(n,object,val1,listvals)

/* eg: RETURN(3,object,val1,(RV(val2),RV(val3))) */
#define RETURN(n,typ,val1,listvals) \
   do{typ _val1 = val1; object *_p=&fcall.values[1]; listvals; fcall.nvalues= n; return _val1;}while(0)
/* #define CALL(n,form) (VFUN_NARGS=n,form) */

	

object funcall_cfun(void(*)(),int,...);
object clear_compiler_properties();
EXTER object sSlambda_block_expanded;

# ifdef __GNUC__ 
# define assert(ex)\
{if (!(ex)){(void)fprintf(stderr, \
		  "Assertion failed: file \"%s\", line %d\n", __FILE__, __LINE__);exit(1);}}
# else
# define assert(ex)
# endif

#ifndef FIX_PATH_STRING
#define FIX_PATH_STRING(file) file
#endif
	

#define CHECK_INTERRUPT   if (signals_pending) raise_pending_signals(sig_safe)

#define BEGIN_NO_INTERRUPT \
 plong old_signals_allowed = signals_allowed; \
  signals_allowed = 0

#define END_NO_INTERRUPT \
  signals_allowed = old_signals_allowed
/* could add:   if (signals_pending)
   raise_pending_signals(sig_use_signals_allowed_value) */


#define END_NO_INTERRUPT_SAFE \
  signals_allowed = old_signals_allowed; \
  if (signals_pending) \
    do{ if(signals_allowed ==0) /* should not get here*/abort(); \
	  raise_pending_signals(sig_safe)}while(0)

void raise_pending_signals();

EXTER unsigned plong signals_allowed, signals_pending  ;

EXTER struct symbol Dotnil_body;
#define Dotnil ((object)&Dotnil_body)

#if defined (LOW_SHFT)

#define LOW_IM_FIX (1L<<(LOW_SHFT-1))
#define INT_IN_BITS(a_,b_) ({fixnum _a=(fixnum)(a_);_a>>(b_)==_a>>(CHAR_SIZE*SIZEOF_LONG-1);})

#define      make_imm_fixnum(a_)        ((object)a_)
#define       fix_imm_fixnum(a_)        ((fixnum)a_)
#define      mark_imm_fixnum(a_)        ((a_)=((object)((fixnum)(a_)+(LOW_IM_FIX<<1))))
#define    unmark_imm_fixnum(a_)        ((a_)=((object)((fixnum)(a_)-(LOW_IM_FIX<<1))))
#define        is_imm_fixnum(a_)        ((fixnum)(a_)<(fixnum)OBJNULL)
#define is_unmrkd_imm_fixnum(a_)        ((fixnum)(a_)<LOW_IM_FIX)
#define is_marked_imm_fixnum(a_)        (is_imm_fixnum(a_)*!is_unmrkd_imm_fixnum(a_))
#define           is_imm_fix(a_)        INT_IN_BITS(a_,LOW_SHFT-1)
#elif defined (IM_FIX_BASE) && defined(IM_FIX_LIM)
#define      make_imm_fixnum(a_)        ((object)((a_)+(IM_FIX_BASE+(IM_FIX_LIM>>1))))
#define       fix_imm_fixnum(a_)        (((fixnum)(a_))-(IM_FIX_BASE+(IM_FIX_LIM>>1)))
#define      mark_imm_fixnum(a_)        ((a_)=((object)(((fixnum)(a_)) | IM_FIX_LIM)))
#define    unmark_imm_fixnum(a_)        ((a_)=((object)(((fixnum)(a_)) &~ IM_FIX_LIM)))
#define        is_imm_fixnum(a_)        (((ufixnum)(a_))>=IM_FIX_BASE)
#define is_unmrkd_imm_fixnum(a_)        (is_imm_fixnum(a_)&&!is_marked_imm_fixnum(a_))
#define is_marked_imm_fixnum(a_)        (((fixnum)(a_))&IM_FIX_LIM)
#define           is_imm_fix(a_)        (!(((a_)+(IM_FIX_LIM>>1))&-IM_FIX_LIM))
/* #define        un_imm_fixnum(a_)        ((a_)=((object)(((fixnum)(a_))&~(IM_FIX_BASE)))) */
#else
#define      make_imm_fixnum(a_)        make_fixnum1(a_)
#define       fix_imm_fixnum(a_)        ((a_)->FIX.FIXVAL)
#define      mark_imm_fixnum(a_)        
#define    unmark_imm_fixnum(a_)        
#define        is_imm_fixnum(a_)        0
#define is_unmrkd_imm_fixnum(a_)        0
#define is_marked_imm_fixnum(a_)        0
#define           is_imm_fix(a_)        0
/* #define        un_imm_fixnum(a_)         */
#endif

#define make_fixnum(a_)  ({register fixnum _q1=(a_);register object _q4; \
      _q4=is_imm_fix(_q1) ? make_imm_fixnum(_q1) : make_fixnum1(_q1);_q4;})
#define fix(a_)          ({register object _q2=(a_);register fixnum _q3;		\
      _q3=is_imm_fixnum(_q2) ? fix_imm_fixnum(_q2) :  (_q2)->FIX.FIXVAL;_q3;})
#define Mfix(a_)         fix(a_)
#define small_fixnum(a_) make_fixnum(a_) /*make_imm_fixnum(a_)*/
#define set_fix(a_,b_)   ((a_)->FIX.FIXVAL=(b_))

#define Zcdr(a_)                 (*(object *)(a_))/* ((a_)->c.c_cdr) */ /*FIXME*/

#ifndef WIDE_CONS

#ifndef USE_SAFE_CDR
#define SAFE_CDR(a_)             a_
#define imcdr(a_)                is_imm_fixnum(Zcdr(a_))
#else
#define SAFE_CDR(a_)             ({object _a=(a_);is_imm_fixnum(_a) ? make_fixnum1(fix(_a)) : _a;})
#ifdef DEBUG_SAFE_CDR
#define imcdr(a_)                (is_imm_fixnum(Zcdr(a_)) && (error("imfix cdr"),1))
#else
#define imcdr(a_)                0
#endif
#endif

#else

#define SAFE_CDR(a_)             a_
#define imcdr(a_)                0

#endif

#define is_marked(a_)            (imcdr(a_) ? is_marked_imm_fixnum(Zcdr(a_)) : (a_)->d.m)
#define is_marked_or_free(a_)    (imcdr(a_) ? is_marked_imm_fixnum(Zcdr(a_)) : (a_)->md.mf)
#define mark(a_)                 if (imcdr(a_)) mark_imm_fixnum(Zcdr(a_)); else (a_)->d.m=1
#define unmark(a_)               if (imcdr(a_)) unmark_imm_fixnum(Zcdr(a_)); else (a_)->d.m=0
#define is_free(a_)              (!is_imm_fixnum(a_) && !imcdr(a_) && (a_)->d.f)
#define make_free(a_)            ({(a_)->fw=0;(a_)->d.f=1;(a_)->fw|=(fixnum)OBJNULL;})/*set_type_of(a_,t_other)*/
#define make_unfree(a_)          {(a_)->d.f=0;}

#ifdef WIDE_CONS
#define valid_cdr(a_)            0
#else
#define valid_cdr(a_)            (!(a_)->d.e || imcdr(a_))
#endif

#define type_of(x)       ({register object _z=(object)(x);\
                           (is_imm_fixnum(_z) ? t_fixnum : \
			    (valid_cdr(_z) ?  (_z==Cnil ? t_symbol : t_cons)  : _z->d.t));})

#ifdef WIDE_CONS
#define TYPEWORD_TYPE_P(y_) 1
#else
#define TYPEWORD_TYPE_P(y_) (y_!=t_cons)
#endif
  
/*Note preserve sgc flag here                                         VVV*/
#define set_type_of(x,y) ({object _x=(object)(x);enum type _y=(y);_x->d.f=0;\
    if (TYPEWORD_TYPE_P(_y)) {_x->d.e=1;_x->d.t=_y;_x->fw|=(fixnum)OBJNULL;}})

#ifndef WIDE_CONS

#define cdr_listp(x)     valid_cdr(x)
#define consp(x)         ({register object _z=(object)(x);\
                           (!is_imm_fixnum(_z) && valid_cdr(_z) && _z!=Cnil);})
#define listp(x)         ({register object _z=(object)(x);\
                           (!is_imm_fixnum(_z) && valid_cdr(_z));})
#define atom(x)          ({register object _z=(object)(x);\
                           (is_imm_fixnum(_z) || !valid_cdr(_z) || _z==Cnil);})

#else

#define cdr_listp(x)     listp(x)
#define consp(x)         (type_of(x)==t_cons)
#define listp(x)         ({object _x=x;type_of(_x)==t_cons || _x==Cnil;})
#define atom(x)          !consp(x)

#endif

/* #define eql_is_eq(a_)    (is_imm_fixnum(a_) || ({enum type _tp=type_of(a_); _tp == t_cons || _tp > t_complex;})) */
/* #define equal_is_eq(a_)  (is_imm_fixnum(a_) || type_of(a_)>t_bitvector) */



#define	Msf(obje)	(obje)->SF.SFVAL
#define sf(x) Msf(x)

#define	Mlf(obje)	(obje)->LF.LFVAL
#define lf(x) Mlf(x)

#define endp_prop(a) (consp(a) ? FALSE : ((a)==Cnil ? TRUE : (FEwrong_type_argument(sLlist, (a)),FALSE)))
#define endp(a) endp_prop(a)
    
#define proper_list(a) (type_of(a)==t_cons || (a)==Cnil)

#define IMMNIL(x) (is_imm_fixnum(x)||x==Cnil)

#define eql(a_,b_)    ({register object _a=(a_);register object _b=(b_);_a==_b || (!IMMNIL(_a)&&!IMMNIL(_b)&&eql1(_a,_b));})
#define equal(a_,b_)  ({register object _a=(a_);register object _b=(b_);_a==_b || (!IMMNIL(_a)&&!IMMNIL(_b)&&equal1(_a,_b));})
#define equalp(a_,b_) ({register object _a=(a_);register object _b=(b_);_a==_b || (_a!=Cnil&&_b!=Cnil&&equalp1(_a,_b));})

#define character_table (character_table1+128)
#define	code_char(c)		(object)(character_table+((unsigned char)(c)))
#define	char_code(obje)		((object)obje)->ch.ch_code
#define	char_font(obje)		((object)obje)->ch.ch_font
#define	char_bits(obje)		((object)obje)->ch.ch_bits

/* we sometimes have to touch the header of arrays or structures
   to make sure the page is writable */
#ifdef SGC
#define SGC_TOUCH(x) if (is_marked(x)) system_error(); unmark(x)
#else
#define SGC_TOUCH(x)
#endif

