#define mjoin(a_,b_) a_ ## b_
#define Mjoin(a_,b_) mjoin(a_,b_)

#include "arth.h"

#define LM(a_) AM(AT(SIZEOF_LONG,8),a_)
#if SIZEOF_LONG == 4
#define LL 2
#elif SIZEOF_LONG == 8
#define LL 3
#else
#error "unknown SIZEOF_LONG"
#endif 
#define POW AM(PAGEWIDTH,AP(LL,1))

struct pageinfo {
  unsigned long type:6;
  unsigned long magic:7;
  unsigned long sgc_flags:2;
  unsigned long in_use:LM(15);
  struct pageinfo *next;
};
  
#ifndef WORDS_BIGENDIAN

#define FIRSTWORD unsigned long    e:1,m:1,f:1,s:1,tt:4,t:5,st:3,w:LM(16)
#define FSTPWORD  unsigned long emfs:4,            tp:9,    st:3,w:LM(16)
#define MARKWORD  unsigned long    e:1,   mf:2,s:1,tt:4,t:5,x:LM(13)
#define SGCMWORD  unsigned long    e:1,mfs:3,      tt:4,t:5,x:LM(13)
#define TYPEWORD  unsigned long  emf:3,        s:1,tt:4,t:5,x:LM(13)
#define FUNWORD   unsigned long    e:1,m:1,f:1,s:1,tt:4,t:5,fun_minarg:6,fun_maxarg:6,fun_neval:5,fun_vv:1,y:LM(31)

#else

#define FIRSTWORD unsigned long w:LM(16),st:3,t:5,tt:4,s:1,f:1,m:1,e:1
#define FSTPWORD  unsigned long w:LM(16),st:3,tp:9,             emfs:4
#define MARKWORD  unsigned long x:LM(13),     t:5,tt:4,s:1,   mf:2,e:1
#define SGCMWORD  unsigned long x:LM(13),     t:5,tt:4,      mfs:3,e:1
#define TYPEWORD  unsigned long x:LM(13),     t:5,tt:4,s:1,      emf:3
#define FUNWORD   unsigned long y:LM(31),fun_vv:1,fun_neval:5,fun_maxarg:6,fun_minarg:6,t:5,tt:4,s:1,f:1,m:1,e:1

#endif

#undef bool
typedef int bool;
typedef long fixnum;
typedef unsigned long ufixnum;
typedef float shortfloat;
typedef double longfloat;
typedef unsigned short fatchar;

#if SIZEOF_LONG < 8
#define SPAD object pad
#else
#define SPAD
#endif

typedef union lispunion *object;

struct fixnum_struct {
  FIRSTWORD;
  fixnum FIXVAL;                /*  fixnum value  */
};

struct shortfloat_struct {
  FIRSTWORD;
  shortfloat SFVAL;             /*  shortfloat value  */
};

struct longfloat_struct {
  FIRSTWORD;
  longfloat LFVAL;              /*  longfloat value  */
  SPAD;
};

struct bignum {
  FIRSTWORD;
  __mpz_struct big_mpz_t;
};

struct ratio {
  FIRSTWORD;
  object rat_den;               /*  denominator  */
  object rat_num;               /*  numerator  */
  SPAD;
  
};

struct ocomplex {
  FIRSTWORD;
  object cmp_real;              /*  real part  */
  object cmp_imag;              /*  imaginary part  */
  SPAD;
};

struct character {
  FIRSTWORD;
  unsigned short ch_code;       /*  code  */
  unsigned char ch_font;        /*  font  */
  unsigned char ch_bits;        /*  bits  */
};

#define	s_fillp		st_fillp
#define	s_self		st_self

struct symbol {
  FIRSTWORD;
  object s_dbind;               /*  dynamic binding  */
  void (*s_sfdef)();            /*  special form definition, coincides with c_car  */
  char *s_self;                 /*  print name, coincides with st_self  */
  int s_fillp;                  /*  print name length, coincides with st_fillp  */
  object s_gfdef;               /*  global function definition, for a macro, its expansion function.  */
  object s_plist;               /*  property list  */
  object s_hpack;               /*  home package, Cnil for uninterned symbols  */
  short s_stype;                /*  symbol type of enum stype  */
  short s_mflag;                /*  macro flag  */
  SPAD;

};

struct package {
  FIRSTWORD;
  object p_name;                /*  package name, a string  */
  object p_nicknames;           /*  nicknames, list of strings  */
  object p_shadowings;          /*  shadowing symbol list  */
  object p_uselist;             /*  use-list of packages  */
  object p_usedbylist;          /*  used-by-list of packages  */
  object *p_internal;           /*  hashtable for internal symbols  */
  object *p_external;           /*  hashtable for external symbols  */
  int p_internal_size;          /* size of internal hash table*/
  int p_external_size;          /* size of external hash table */
  int p_internal_fp;            /* [rough] number of symbols */
  int p_external_fp;            /* [rough]  number of symbols */
  struct package *p_link;       /*  package link  */
  SPAD;
};

struct cons {
#ifdef WIDE_CONS
  FIRSTWORD;
#endif
  object c_cdr;                 /*  cdr  */
  object c_car;                 /*  car  */
};

struct htent {			/*  hash table entry  */
  object	hte_key;	/*  key  */
  object	hte_value;	/*  value  */
};

struct hashtable {              /*  hash table header  */
  FIRSTWORD;
  struct htent *ht_self;        /*  pointer to the hash table  */
  object ht_rhsize;             /*  rehash size  */
  object ht_rhthresh;           /*  rehash threshold  */
  int ht_nent;                  /*  number of entries  */
  int ht_size;                  /*  hash table size  */
  short ht_test;                /*  key test function, of enum httest  */
  SPAD;
};

struct array {                  /*  array header  */
  FIRSTWORD;
  object a_displaced;           /*  displaced  */
  short a_rank;                 /*  array rank  */
  short a_elttype;              /*  element type  */
  object *a_self;               /*  pointer to the array  */
  short a_adjustable;           /*  adjustable flag  */
  short a_offset;               /*  bitvector offset  */
  int a_dim;                    /*  dimension  */
  int *a_dims;                  /*  table of dimensions  */
  SPAD;

};

struct vector {                 /*  vector header  */
  FIRSTWORD;
  object v_displaced;           /*  displaced  */
  short v_hasfillp;             /*  has-fill-pointer flag  */
  short v_elttype;              /*  element type  */
  object *v_self;               /*  pointer to the vector  */
  int v_fillp;                  /*  fill pointer, For simple vectors, v_fillp is equal to v_dim.  */
  int v_dim;                    /*  dimension  */
  short v_adjustable;           /*  adjustable flag  */
  short v_offset;               /*  not used  */
  SPAD;
};

struct string {                 /*  string header  */
  FIRSTWORD;
  object st_displaced;          /*  displaced  */
  short st_hasfillp;            /*  has-fill-pointer flag  */
  short st_adjustable;          /*  adjustable flag  */
  char *st_self;                /*  pointer to the string  */
  int st_fillp;                 /*  fill pointer, For simple strings, st_fillp is equal to st_dim.  */
  int st_dim;                   /*  dimension  */
};

struct ustring {
  FIRSTWORD;
  object ust_displaced;
  short ust_hasfillp;
  short ust_adjustable;  
  unsigned char *ust_self;
  int ust_fillp;
  int ust_dim;
};

struct bitvector {              /*  bitvector header  */
  FIRSTWORD;
  object bv_displaced;          /*  displaced  */
  short bv_hasfillp;            /*  has-fill-pointer flag  */
  short bv_elttype;             /*  not used  */
  char *bv_self;                /*  pointer to the bitvector  */
  int bv_fillp;                 /*  fill pointer, For simple bitvectors, st_fillp is equal to st_dim.  */
  int bv_dim;                   /*  dimension, number of bits  */
  short bv_adjustable;          /*  adjustable flag  */
  short bv_offset;              /*  bitvector offset, the position of the first bit in the first byte  */
  SPAD;
};

struct fixarray {               /*  fixnum array header  */
  FIRSTWORD;
  object fixa_displaced;        /*  displaced  */
  short fixa_rank;              /*  array rank  */
  short fixa_elttype;           /*  element type  */
  fixnum *fixa_self;            /*  pointer to the array  */
  short fixa_adjustable;        /*  adjustable flag  */
  short fixa_offset;            /*  not used  */
  int fixa_dim;                 /*  dimension  */
  int *fixa_dims;               /*  table of dimensions  */
  SPAD;

};

struct sfarray {                /*  short-float array header  */
  FIRSTWORD;
  object sfa_displaced;         /*  displaced  */
  short sfa_rank;               /*  array rank  */
  short sfa_elttype;            /*  element type  */
  shortfloat  *sfa_self;        /*  pointer to the array  */
  short sfa_adjustable;         /*  adjustable flag  */
  short sfa_offset;             /*  not used  */
  int sfa_dim;                  /*  dimension  */
  int *sfa_dims;                /*  table of dimensions  */
  SPAD;

};

struct lfarray {                /*  plong-float array header  */
  FIRSTWORD;
  object lfa_displaced;         /*  displaced  */
  short lfa_rank;               /*  array rank  */
  short lfa_elttype;            /*  element type  */
  longfloat *lfa_self;          /*  pointer to the array  */
  short lfa_adjustable;         /*  adjustable flag  */
  short lfa_offset;             /*  not used  */
  int lfa_dim;                  /*  dimension  */
  int *lfa_dims;                /*  table of dimensions  */
  SPAD;

};

struct s_data {
  object name;
  fixnum length;
  object raw;
  object included;
  object includes;
  object staticp;
  object print_function;
  object slot_descriptions;
  object slot_position;
  fixnum   size;
  object has_holes;
};

struct structure {              /*  structure header  */
  FIRSTWORD;
  object str_def;               /*  structure definition (a structure)  */
  object *str_self;             /*  structure self  */
  SPAD;
};

struct stream {
  FIRSTWORD;
  void *sm_fp;                  /*  file pointer  */
  object sm_object0;            /*  some object  */
  object sm_object1;            /*  some object */
  int sm_int0;                  /*  some int  */
  int sm_int1;                  /*  column for input or output, stream */
  char   *sm_buffer;            /*  ptr to BUFSIZE block of storage */
  char sm_mode;                 /*  stream mode  */
  unsigned char sm_flags;       /* flags from gcl_sm_flags */
  short sm_fd;                  /* stream fd */
};

struct random {
  FIRSTWORD;
  __gmp_randstate_struct  rnd_state;
};

struct readtable {              /*  read table  */
  FIRSTWORD;
  struct rtent *rt_self;        /*  read table itself  */
};

struct pathname {
  FIRSTWORD;
  object pn_host;
  object pn_device;
  object pn_directory;
  object pn_name;
  object pn_type;
  object pn_version;
  SPAD;
};

struct cfun {                   /*  compiled function header  */
  FIRSTWORD;
  object cf_name;               /*  compiled function name  */
  void (*cf_self)();            /*  entry address  */
  object cf_data;               /*  data the function uses  */
};

struct cclosure {               /*  compiled closure header  */
  FIRSTWORD;
  object cc_name;               /*  compiled closure name  */
  void (*cc_self)();            /*  entry address  */
  object cc_env;                /*  environment  */
  object cc_data;               /*  data the closure uses  */
  int cc_envdim;
  object *cc_turbo;             /*  turbo charger */
  SPAD;
};

struct closure {
  FIRSTWORD; 
  object cl_name;               /* name */
  object (*cl_self)();          /* C start address of code */
  object cl_data;               /* To object holding VV vector */
  int cl_argd;                  /* description of args + number */
  int cl_envdim;                /* length of the environment vector */
  object *cl_env;               /* environment vector referenced by cl_self()*/
  SPAD;
};

struct sfun {
  FIRSTWORD; 
  object sfn_name;             /* name */
  object (*sfn_self)();        /* C start address of code */
  object sfn_data;             /* To object holding VV vector */
  int sfn_argd;                /* description of args + number */
  SPAD;
};

struct vfun {
  FIRSTWORD; 
  object vfn_name;             /* name */
  object (*vfn_self)();        /* C start address of code */
  object vfn_data;             /* To object holding VV data */
  unsigned short vfn_minargs;  /* Min args and where varargs start */
  unsigned short vfn_maxargs;  /* Max number of args */
  SPAD;
 
};
struct cfdata {
  FIRSTWORD;
  char *cfd_start;             /* beginning of contblock for fun */
  int cfd_size;                /* size of contblock */
  int cfd_fillp;               /* size of self */
  object *cfd_self;            /* body */
  SPAD;
};

struct spice {
  FIRSTWORD;
  int spc_dummy;
};

struct dummy {
  FIRSTWORD;
};

struct ff         {ufixnum ff;};
struct fstpw      {FSTPWORD;};
union  fstp       {ufixnum ff;struct fstpw t;};
struct mark       {MARKWORD;};
struct typew      {TYPEWORD;};
struct sgcm       {SGCMWORD;};

union lispunion {
  struct fixnum_struct      FIX; /*  fixnum  */
  struct bignum             big; /*  bignum  */
  struct ratio              rat; /*  ratio  */
  struct shortfloat_struct   SF; /*  short floating-point number  */
  struct longfloat_struct    LF; /*  plong floating-point number  */
  struct ocomplex           cmp; /*  complex number  */
  struct character           ch; /*  character  */
  struct symbol               s; /*  symbol  */
  struct package              p; /*  package  */
  struct cons                 c; /*  cons  */
  struct hashtable           ht; /*  hash table  */
  struct array                a; /*  array  */
  struct vector               v; /*  vector  */
  struct string              st; /*  string  */
  struct ustring            ust;
  struct bitvector           bv; /*  bit-vector  */
  struct structure          str; /*  structure  */
  struct stream              sm; /*  stream  */
  struct random             rnd; /*  random-states  */
  struct readtable           rt; /*  read table  */
  struct pathname            pn; /*  path name  */
  struct cfun                cf; /*  compiled function  uses value stack] */
  struct cclosure            cc; /*  compiled closure  uses value stack */
  struct closure             cl; /*  compiled closure  uses c stack */
  struct sfun               sfn;    /*  simple function */
  struct vfun               vfn;    /*  function with variable number of args */
  struct cfdata             cfd;    /* compiled fun data */
  struct spice              spc; /*  spice  */
  
  struct dummy                d; /*  dummy  */
  
  struct fstpw             fstp; /*  fast type  */
  struct ff                  ff; /*  fast type  */
  struct mark                md; /*  mark dummy  */
  struct sgcm               smd; /*  sgc mark dummy  */
  struct typew               td; /*  type dummy  */
  fixnum                     fw;
  void *                     vw;
  
  struct fixarray          fixa; /*  fixnum array  */
  struct sfarray            sfa; /*  short-float array  */
  struct lfarray            lfa; /*  plong-float array  */

};

struct call_data { 
  object fun;
  int argd;
  int nvalues;
  object values[50];
  double double_return;
};

EXTER struct call_data fcall;
EXTER union lispunion Cnil_body OBJ_ALIGN;
EXTER union lispunion Ct_body OBJ_ALIGN;
EXTER union lispunion character_table1[256+128] OBJ_ALIGN;
