#include <stdarg.h>
#define _VA_LIST_DEFINED

#include <setjmp.h>
#include <stdio.h>

/*  #define	endp(obje)	endp1(obje) */
			   
#define STSET(type,x,i,val)  do{SGC_TOUCH(x);STREF(type,x,i) = (val);} while(0)

#ifndef HAVE_MATH_H
#error Need math.h
#endif
#include <math.h>

#ifndef HAVE_COMPLEX_H
#error Need complex.h
#endif
#include <complex.h>

#ifdef HAVE_ALLOCA_H /*FIXME check if this is truly optional*/
#include <alloca.h>
#endif

#ifdef IM_FIX_BASE
#define fimf(a) ((((ufixnum)a)&(IM_FIX_BASE+IM_FIX_LIM))==IM_FIX_BASE)
#define fimoff  (IM_FIX_BASE+(IM_FIX_LIM>>1))
#define fimb    IM_FIX_BASE
#else
#define fimf(a) 0
#define fimoff  0
#define fimb    0
#endif


#if defined (LOW_SHFT)

#define LOW_IM_FIX (1L<<(LOW_SHFT-1))

#define immnum_comp(x,y,c) ({register object _x=x,_y=y;\
      is_unmrkd_imm_fixnum(_x)&&is_unmrkd_imm_fixnum(_y) ? ((fixnum)_x c (fixnum)_y) : (number_compare(_x,_y) c 0);})
#define immnum_plus(x,y) \
  ({object _x=x,_y=y,_z=((object)((fixnum)_x+(fixnum)_y));is_unmrkd_imm_fixnum(_z) ? _z : number_plus(_x,_y);})
#define immnum_minus(x,y)						\
   ({object _xx=x,_yy=y;is_unmrkd_imm_fixnum(_yy) ? immnum_plus(_xx,((object)(-(fixnum)_yy))) : number_minus(_xx,_yy);}) 
#define immnum_negate(x) \
  ({object _x=x;is_unmrkd_imm_fixnum(_x)&&_x!=(object)(-LOW_IM_FIX) ? ((object)-(fixnum)_x) : number_negate(_x);})
#define immnum_times(x,y) \
  ({object _x=x,_y=y;is_unmrkd_imm_fixnum(_x)&&is_unmrkd_imm_fixnum(_y) ? make_fixnum((fixnum)_x*(fixnum)_y) : number_times(_x,_y);})
#else
/* #define immnum_comp(x,y,c) (fimf(((ufixnum)x)&((ufixnum)y)) ? (x c y) : (number_compare(x,y) c 0)) */
/* #define immnum_comp(x,y,c) ({register object _x=x,_y=y;\ */
/*       fimf(((ufixnum)_x)&((ufixnum)_y)) ? (_x c _y) : (number_compare(_x,_y) c 0);})/\*FIXME? comparisons with marked immfix*\/ */
/* #define tand(_x,_z) fimf(((((ufixnum)_x)&((ufixnum)_z))|(IM_FIX_LIM&((ufixnum)_z)))) */

#define immnum_comp(x,y,c) ({register object _x=x,_y=y;\
      is_imm_fixnum(_x)&&is_imm_fixnum(_y) ? (_x c _y) : (number_compare(_x,_y) c 0);})/*FIXME? comparisons with marked immfix*/

#define immnum_plus(x,y) \
  ({object _x=x,_y=y;is_imm_fixnum(_x)&&is_imm_fixnum(_y) ? make_fixnum(((ufixnum)_x)+((ufixnum)_y)-(fimoff<<1)) : number_plus(_x,_y);})
#define immnum_minus(x,y) \
  ({object _x=x,_y=y;is_imm_fixnum(_x)&&is_imm_fixnum(_y) ? make_fixnum(((ufixnum)_x)-((ufixnum)_y)) : number_minus(_x,_y);})
#define immnum_negate(x) \
  ({object _x=x;is_imm_fixnum(_x) ? make_fixnum((fimoff)-((ufixnum)_x)) : number_negate(_x);})


#define immnum_ior(x,y) \
  ({object _x=x,_y=y;is_imm_fixnum(_x)&&is_imm_fixnum(_y) ? make_fixnum(fix(_x)|fix(_y)) : fS2logior(_x,_y);})
#define immnum_and(x,y) \
  ({object _x=x,_y=y;is_imm_fixnum(_x)&&is_imm_fixnum(_y) ? make_fixnum(fix(_x)&fix(_y)) : fS2logand(_x,_y);})
#define immnum_xor(x,y) \
  ({object _x=x,_y=y;is_imm_fixnum(_x)&&is_imm_fixnum(_y) ? make_fixnum(fix(_x)^fix(_y)) : fS2logxor(_x,_y);})
#define immnum_not(x) \
  ({object _x=x;is_imm_fixnum(_x) ? make_fixnum(~fix(_x)) : fS1lognot(_x);})


/* /\* #define immnum_plus(x,y) \ */
/*   ({object _x=x,_y=y,_z=(object)(((ufixnum)_x)+((ufixnum)_y)-fimoff);tand(_x,_z) ? _z : number_plus(_x,_y);}) */

/* #define immnum_minus(x,y) \ */
/*   ({object _x=x,_y=y,_z=(object)(((ufixnum)_x)-((ufixnum)_y)+fimoff);tand(_x,_z) ? _z : number_minus(_x,_y);}) */

/* #define immnum_negate(x) \ */
/*   ({object _x=x,_z=(object)((fimoff<<1)-((ufixnum)_x));fimf(_z) ? _z : number_negate(_x);}) */

#endif
#define immnum_lt(x,y) immnum_comp(x,y,<)
#define immnum_le(x,y) immnum_comp(x,y,<=)
#define immnum_eq(x,y) immnum_comp(x,y,==)
#define immnum_ne(x,y) immnum_comp(x,y,!=)
#define immnum_gt(x,y) immnum_comp(x,y,>)
#define immnum_ge(x,y) immnum_comp(x,y,>=)
