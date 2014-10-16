enum type {
  t_cons,
  t_start = 0,
  t_fixnum,
  t_bignum,
  t_ratio,
  t_shortfloat,
  t_longfloat,
  t_complex,
  t_pathname,
  t_string,
  t_bitvector,
  t_vector,
  t_array,
  t_hashtable,
  t_structure,
  t_character,
  t_symbol,
  t_package,
  t_stream,
  t_random,
  t_readtable,
  t_cfun,
  t_cclosure,
  t_sfun,
  t_gfun,
  t_vfun,
  t_afun,
  t_closure,
  t_cfdata,
  t_spice,
  t_contiguous,
  t_end=t_contiguous,
  t_relocatable,
  t_other
};


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

#define SPP(a_,b_) (type_of(a_)==Join(t_,b_))
#define streamp(a_)    SPP(a_,stream)
#define packagep(a_)   SPP(a_,package)
#define hashtablep(a_) SPP(a_,hashtable)
#define randomp(a_)    SPP(a_,random)
#define characterp(a_) SPP(a_,character)
#define symbolp(a_)    SPP(a_,symbol)
#define stringp(a_)    SPP(a_,string)
#define fixnump(a_)    SPP(a_,fixnum)
#define readtablep(a_) SPP(a_,readtable)
#define functionp(a_)  ({enum type _t=type_of(a_);_t>=t_cfun && _t<=t_closure;})
#define compiled_functionp(a_)  functionp(a_)

#define integerp(a_) ({enum type _tp=type_of(a_); _tp >= t_fixnum     && _tp <= t_bignum;})
#define non_negative_integerp(a_) ({enum type _tp=type_of(a_); (_tp == t_fixnum && fix(a_)>=0) || (_tp==t_bignum && big_sign(a_)>=0);})
#define rationalp(a_)({enum type _tp=type_of(a_); _tp >= t_fixnum     && _tp <= t_ratio;})
#define floatp(a_)   ({enum type _tp=type_of(a_); _tp == t_shortfloat || _tp == t_longfloat;})
#define realp(a_)    ({enum type _tp=type_of(a_); _tp >= t_fixnum     && _tp < t_complex;})
#define numberp(a_)  ({enum type _tp=type_of(a_); _tp >= t_fixnum     && _tp <= t_complex;})
#define arrayp(a_)   ({enum type _tp=type_of(a_); _tp >= t_string     && _tp <= t_array;})
#define vectorp(a_)  ({enum type _tp=type_of(a_); _tp >= t_string     && _tp < t_array;})

#define string_symbolp(a_)                 ({enum type _tp=type_of(a_); _tp == t_string || _tp == t_symbol;})
#define pathname_string_symbolp(a_)        ({enum type _tp=type_of(a_); _tp==t_pathname || _tp == t_string\
                                                                     || _tp == t_symbol;})
#define pathname_string_symbol_streamp(a_) ({enum type _tp=type_of(a_); _tp==t_pathname || _tp == t_string\
                                                                     || _tp == t_symbol || _tp==t_stream;})
