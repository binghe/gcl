#include "pbits.h"

#undef bool
typedef int bool;

typedef long long lfixnum;
typedef unsigned long long ulfixnum;

typedef long fixnum;
typedef unsigned long ufixnum;

typedef float shortfloat;
typedef double longfloat;

typedef union lispunion *object;

#ifndef WORDS_BIGENDIAN

#define FIRSTWORD ufixnum    e:1,m:1,f:1,s:1,tt:4,t:5,st:3,w:LM(16)
#define FSTPWORD  ufixnum emfs:4,            tp:9,    st:3,w:LM(16)
#define MARKWORD  ufixnum    e:1,   mf:2,s:1,tt:4,t:5,x:LM(13)
#define SGCMWORD  ufixnum    e:1,mfs:3,      tt:4,t:5,x:LM(13)
#define TYPEWORD  ufixnum  emf:3,        s:1,tt:4,t:5,x:LM(13)
#define FUNWORD   ufixnum    e:1,m:1,f:1,s:1,tt:4,t:5,fun_minarg:6,fun_maxarg:6,fun_neval:5,fun_vv:1,y:LM(31)

#else

#define FIRSTWORD ufixnum w:LM(16),st:3,t:5,tt:4,s:1,f:1,m:1,e:1
#define FSTPWORD  ufixnum w:LM(16),st:3,tp:9,             emfs:4
#define MARKWORD  ufixnum x:LM(13),     t:5,tt:4,s:1,   mf:2,e:1
#define SGCMWORD  ufixnum x:LM(13),     t:5,tt:4,      mfs:3,e:1
#define TYPEWORD  ufixnum x:LM(13),     t:5,tt:4,s:1,      emf:3
#define FUNWORD   ufixnum y:LM(31),fun_vv:1,fun_neval:5,fun_maxarg:6,fun_minarg:6,t:5,tt:4,s:1,f:1,m:1,e:1

#endif

#if SIZEOF_LONG < 8
#define SPAD object pad
#else
#define SPAD
#endif

struct fixnum_struct {
  FIRSTWORD;
  fixnum FIXVAL;
};

struct shortfloat_struct {
  FIRSTWORD;
  shortfloat SFVAL;
};

struct longfloat_struct {
  FIRSTWORD;
  longfloat LFVAL;
  SPAD;
};

struct bignum {
  FIRSTWORD;
#ifdef GMP
  __mpz_struct big_mpz_t;
#else
  plong *big_self;
  int big_length;
#endif
};

struct ratio {
  FIRSTWORD;
  object rat_den;
  object rat_num;
  SPAD;

};

struct ocomplex {
  FIRSTWORD;
  object cmp_real;
  object cmp_imag;
  SPAD;
};

struct character {
  FIRSTWORD;
  unsigned short ch_code;
  unsigned char ch_font;
  unsigned char ch_bits;
};


struct symbol {
  FIRSTWORD;
  object s_dbind;
  void (*s_sfdef) ();
  char *s_self;
  int s_fillp;
  object s_gfdef;
  object s_plist;
  object s_hpack;
  short s_stype;
  short s_mflag;
  SPAD;

};

struct package {
  FIRSTWORD;
  object p_name;
  object p_nicknames;
  object p_shadowings;
  object p_uselist;
  object p_usedbylist;
  object *p_internal;
  object *p_external;
  int p_internal_size;
  int p_external_size;
  int p_internal_fp;
  int p_external_fp;
  struct package *p_link;
  SPAD;
};

struct cons {
#ifdef WIDE_CONS
  FIRSTWORD;
#endif
  object c_cdr;
  object c_car;
};

struct htent {
  object hte_key;
  object hte_value;
};

struct hashtable {
  FIRSTWORD;
  struct htent *ht_self;
  object ht_rhsize;
  object ht_rhthresh;
  int ht_nent;
  int ht_size;
  short ht_test;
  SPAD;

};

struct array {
  FIRSTWORD;
  object a_displaced;
  short a_rank;
  short a_elttype;
  object *a_self;
  short a_adjustable;
  short a_offset;
  int a_dim;
  int *a_dims;
  SPAD;

};



struct vector {
  FIRSTWORD;
  object v_displaced;
  short v_hasfillp;
  short v_elttype;
  object *v_self;
  int v_fillp;
  int v_dim;
  short v_adjustable;
  short v_offset;
  SPAD;
};

struct string {
  FIRSTWORD;
  object st_displaced;
  short st_hasfillp;
  short st_adjustable;
  char *st_self;
  int st_fillp;
  int st_dim;
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

struct bitvector {
  FIRSTWORD;
  object bv_displaced;
  short bv_hasfillp;
  short bv_elttype;
  char *bv_self;
  int bv_fillp;
  int bv_dim;
  short bv_adjustable;
  short bv_offset;
  SPAD;
};

struct fixarray {
  FIRSTWORD;
  object fixa_displaced;
  short fixa_rank;
  short fixa_elttype;
  fixnum *fixa_self;
  short fixa_adjustable;
  short fixa_offset;
  int fixa_dim;
  int *fixa_dims;
  SPAD;
};

struct sfarray {
  FIRSTWORD;
  object sfa_displaced;
  short sfa_rank;
  short sfa_elttype;
  shortfloat *sfa_self;
  short sfa_adjustable;
  short sfa_offset;
  int sfa_dim;
  int *sfa_dims;
  SPAD;
};

struct lfarray {
  FIRSTWORD;
  object lfa_displaced;
  short lfa_rank;
  short lfa_elttype;
  longfloat *lfa_self;
  short lfa_adjustable;
  short lfa_offset;
  int lfa_dim;
  int *lfa_dims;
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
  fixnum size;
  object has_holes;
};

struct structure {
  FIRSTWORD;
  object str_def;
  object *str_self;
  SPAD;
};

struct stream {
  FIRSTWORD;
  void *sm_fp;
  object sm_object0;
  object sm_object1;
  int sm_int0;
  int sm_int1;
  char *sm_buffer;
  char sm_mode;
  unsigned char sm_flags;
  short sm_fd;
};

struct random {
  FIRSTWORD;
  __gmp_randstate_struct rnd_state;
};


struct readtable {
  FIRSTWORD;
  struct rtent *rt_self;
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

struct cfun {
  FIRSTWORD;
  object cf_name;
  void (*cf_self) ();
  object cf_data;
};

struct cclosure {
  FIRSTWORD;
  object cc_name;
  void (*cc_self) ();
  object cc_env;
  object cc_data;
  int cc_envdim;
  object *cc_turbo;
  SPAD;
};

struct closure {
  FIRSTWORD;
  object cl_name;
  object (*cl_self) ();
  object cl_data;
  int cl_argd;
  int cl_envdim;
  object *cl_env;
};

struct sfun {
  FIRSTWORD;
  object sfn_name;
  object (*sfn_self) ();
  object sfn_data;
  int sfn_argd;
  SPAD;
};

struct vfun {
  FIRSTWORD;
  object vfn_name;
  object (*vfn_self) ();
  object vfn_data;
  unsigned short vfn_minargs;
  unsigned short vfn_maxargs;
  SPAD;
};
struct cfdata {
  FIRSTWORD;
  char *cfd_start;
  int cfd_size;
  int cfd_fillp;
  object *cfd_self;
  SPAD;
};

struct spice {
  FIRSTWORD;
  int spc_dummy;
};

struct dummy {
  FIRSTWORD;
};
struct ff {
  ufixnum ff;
};
struct fstpw {
  FSTPWORD;
};
union fstp
{
  ufixnum ff;
  struct fstpw t;
};
struct mark {
  MARKWORD;
};
struct typew {
  TYPEWORD;
};
struct sgcm {
  SGCMWORD;
};

union lispunion {
  struct fixnum_struct FIX;
  struct bignum big;
  struct ratio rat;
  struct shortfloat_struct SF;
  struct longfloat_struct LF;
  struct ocomplex cmp;
  struct character ch;
  struct symbol s;
  struct package p;
  struct cons c;
  struct hashtable ht;
  struct array a;
  struct vector v;
  struct string st;
  struct ustring ust;
  struct bitvector bv;
  struct structure str;
  struct stream sm;
  struct random rnd;
  struct readtable rt;
  struct pathname pn;
  struct cfun cf;
  struct cclosure cc;
  struct closure cl;
  struct sfun sfn;
  struct vfun vfn;
  struct cfdata cfd;
  struct spice spc;
  struct dummy d;
  struct fstpw fstp;
  struct ff ff;
  struct mark md;
  struct sgcm smd;
  struct typew td;
  fixnum fw;
  void *vw;
  struct fixarray fixa;
  struct sfarray sfa;
  struct lfarray lfa;
};
