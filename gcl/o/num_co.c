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
	num_co.c
	IMPLEMENTATION-DEPENDENT

	This file contains those functions
	that know the representation of floating-point numbers.
*/	
#define IN_NUM_CO

#define NEED_MP_H
#define NEED_ISFINITE

#include "include.h"
#include "num_include.h"

object plus_half, minus_half;
extern void zero_divisor(void);
#ifdef CONVEX
#define VAX
#endif

/*   A number is normal when:
   * it is finite,
   * it is not zero, and
   * its exponent is non-zero.
*/

#ifndef IEEEFLOAT
#error this file needs IEEEFLOAT
#endif

int 
gcl_isnormal_double(double d) {

  union {double d;int i[2];} u;
  
  if (!ISFINITE(d) || !d)
    return 0;

  u.d = d;
  return (u.i[HIND] & 0x7ff00000) != 0;

}

int gcl_isnormal_float(float f)
{
  union {float f;int i;} u;

  if (!ISFINITE(f) || !f)
    return 0;

  u.f = f;
  return (u.i & 0x7f800000) != 0;

}

static void
integer_decode_double(double d, int *hp, int *lp, int *ep, int *sp)
{
	int h, l;
	union {double d;int i[2];} u;

	if (d == 0.0) {
		*hp = *lp = 0;
		*ep = 0;
		*sp = 1;
		return;
	}
	u.d=d;
	h=u.i[HIND];
	l=u.i[LIND];
	if (ISNORMAL(d)) {
	  *ep = ((h & 0x7ff00000) >> 20) - 1022 - 53;
	  h = ((h & 0x000fffff) | 0x00100000);
	} else {
	  *ep = ((h & 0x7fe00000) >> 20) - 1022 - 53 + 1;
	  h = (h & 0x001fffff);
	}
	if (32-BIG_RADIX)
	  /* shift for making bignum */
	  { h = h << (32-BIG_RADIX) ; 
	    h |= ((l & (-1 << (32-BIG_RADIX))) >> (32-BIG_RADIX));
	    l &=  ~(-1 << (32-BIG_RADIX));
	  }
	*hp = h;
	*lp = l;
	*sp = (d > 0.0 ? 1 : -1);
}

static void
integer_decode_float(double d, int *mp, int *ep, int *sp)
{
	float f;
	int m;
	union {float f;int i;} u;

	f = d;
	if (f == 0.0) {
		*mp = 0;
		*ep = 0;
		*sp = 1;
		return;
	}
	u.f=f;
	m=u.i;
/* 	m = *(int *)(&f); */
	if (ISNORMAL(f)) {
	  *ep = ((m & 0x7f800000) >> 23) - 126 - 24;
	  *mp = (m & 0x007fffff) | 0x00800000;
	} else {
	  *ep = ((m & 0x7f000000) >> 23) - 126 - 24 + 1;
	  *mp = m & 0x00ffffff;
	}
	*sp = (f > 0.0 ? 1 : -1);
}

static int
double_exponent(double d)
{
	union {double d;int i[2];} u;

	if (d == 0.0)
		return(0);
	u.d=d;
	return (((u.i[HIND] & 0x7ff00000) >> 20) - 1022);
}

static double
set_exponent(double d, int e)
{
	union {double d;int i[2];} u;

	if (d == 0.0)
		return(0.0);
	  
	u.d=d;
	u.i[HIND]= (u.i[HIND] & 0x800fffff) | (((e + 1022) << 20) & 0x7ff00000);
	return(u.d);
}


object
double_to_integer(double d) {

  int h, l, e, s;
  object x;
  vs_mark;
  
  if (d == 0.0)
    return(small_fixnum(0));
  integer_decode_double(d, &h, &l, &e, &s);

  if (e <= -BIG_RADIX) {
    e = (-e) - BIG_RADIX;
    if (e >= BIG_RADIX)
      return(small_fixnum(0));
    h >>= e;
    return(make_fixnum(s*h));
  }
  if (h != 0 || l<0)
    x = bignum2(h, l);
  else
    x = make_fixnum(l);
  vs_push(x);
  x = integer_fix_shift(x, e);
  if (s < 0) {
    vs_push(x);
    x = number_negate(x);
  }
  vs_reset;
  return(x);
}

static object
num_remainder(object x, object y, object q)
{
	object z;

	z = number_times(q, y);
	vs_push(z);
	z = number_minus(x, z);
	vs_popp;
	return(z);
}

/* Coerce X to single-float if one arg,
   otherwise coerce to same float type as second arg */

LFD(Lfloat)(void)
{
	double	d;
	int narg;
	object	x;
	enum type t=t_longfloat;

	narg = vs_top - vs_base;
	if (narg < 1)
		too_few_arguments();
	else if (narg > 2)
		too_many_arguments();
	if (narg == 2) {
		check_type_float(&vs_base[1]);
		t = type_of(vs_base[1]);
	}
	x = vs_base[0];
	switch (type_of(x)) {
	case t_fixnum:
		if (narg > 1 && t == t_shortfloat)
		  x = make_shortfloat((shortfloat)(fix(x)));
		else
		  x = make_longfloat((double)(fix(x)));
		break;

	case t_bignum:
	case t_ratio:
		d = number_to_double(x);
		if (narg > 1 && t == t_shortfloat)
		  x = make_shortfloat((shortfloat)d);
		else
		  x = make_longfloat(d);		
		break;

	case t_shortfloat:
		if (narg > 1 && t == t_shortfloat);
		  else
		    x = make_longfloat((double)(sf(x)));
		break;

	case t_longfloat:
		if (narg > 1 && t == t_shortfloat)
			x = make_shortfloat((shortfloat)(lf(x)));
		break;

	default:
		FEwrong_type_argument(TSor_rational_float, x);
	}
	vs_base = vs_top;
	vs_push(x);
}

LFD(Lnumerator)(void)
{
	check_arg(1);
	check_type_rational(&vs_base[0]);
	if (type_of(vs_base[0]) == t_ratio)
		vs_base[0] = vs_base[0]->rat.rat_num;
}

LFD(Ldenominator)(void)
{
	check_arg(1);
	check_type_rational(&vs_base[0]);
	if (type_of(vs_base[0]) == t_ratio)
		vs_base[0] = vs_base[0]->rat.rat_den;
	else
		vs_base[0] = small_fixnum(1);
}

inline void
intdivrem(object x,object y,fixnum d,object *q,object *r) {

  enum type tx=type_of(x),ty=type_of(y);
  object z,q2,q1;

  if (number_zerop(y)==TRUE)
    zero_divisor();

  switch(tx) {
  case t_fixnum:
  case t_bignum:
    switch (ty) {
    case t_fixnum:
    case t_bignum:
      integer_quotient_remainder_1(x,y,q,r,d);
      return;
    case t_ratio:
      z=integer_divide1(number_times(y->rat.rat_den,x),y->rat.rat_num,d);
      if (q) *q=z;
      if (r) *r=num_remainder(x,y,z);
      return;
    default:
      break;
    }
    break;
  case t_ratio:
    switch (ty) {
    case t_fixnum:
    case t_bignum:
      z=integer_divide1(x->rat.rat_num,number_times(x->rat.rat_den,y),d);
      if (q) *q=z;
      if (r) *r=num_remainder(x,y,z);
      return;
    case t_ratio:
      z=integer_divide1(number_times(x->rat.rat_num,y->rat.rat_den),number_times(x->rat.rat_den,y->rat.rat_num),d);
      if (q) *q=z;
      if (r) *r=num_remainder(x,y,z);
      return;
    default:
      break;
    }
    break;
  default:
    break;
  }

  q2=number_divide(x,y);
  q1=double_to_integer(number_to_double(q2));
  if (d && (d<0 ? number_minusp(q2) : number_plusp(q2)) && number_compare(q2, q1))
    q1 = d<0 ? one_minus(q1) : one_plus(q1);
  if (q) *q=q1;
  if (r) *r=num_remainder(x,y,q1);
  return;
  
}

object
number_ldb(object x,object y) {
  return ifuncall2(sLldb,x,y);
}

object
number_ldbt(object x,object y) {
  return ifuncall2(sLldb_test,x,y);
}

object
number_dpb(object x,object y,object z) {
  return ifuncall3(sLdpb,x,y,z);
}

object
number_dpf(object x,object y,object z) {
  return ifuncall3(sLdeposit_field,x,y,z);
}


LFD(Lfloor)(void) {

  object x, y;
  int n = vs_top - vs_base;

  if (n == 0)
    too_few_arguments();
  if (n > 2)
    too_many_arguments();

  x = vs_base[0];
  y = n>1 ? vs_base[1] : small_fixnum(1);

  intdivrem(x,y,-1,&x,&y);

  vs_top=vs_base;
  vs_push(x);
  vs_push(y);

}

LFD(Lceiling)(void) {

  object x, y;
  int n = vs_top - vs_base;

  if (n == 0)
    too_few_arguments();
  if (n > 2)
    too_many_arguments();

  x = vs_base[0];
  y = n>1 ? vs_base[1] : small_fixnum(1);

  intdivrem(x,y,1,&x,&y);

  vs_top=vs_base;
  vs_push(x);
  vs_push(y);

}

LFD(Ltruncate)(void) {

  object x, y;
  int n = vs_top - vs_base;

  if (n == 0)
    too_few_arguments();
  if (n > 2)
    too_many_arguments();

  x = vs_base[0];
  y = n>1 ? vs_base[1] : small_fixnum(1);

  intdivrem(x,y,0,&x,&y);

  vs_top=vs_base;
  vs_push(x);
  vs_push(y);

}

LFD(Lround)(void)
{
	object x, y, q, q1, r;
	double d;
	int n, c;
	object one_plus(object x), one_minus(object x);

	n = vs_top - vs_base;
	if (n == 0)
		too_few_arguments();
	if (n > 1)
		goto TWO_ARG;
	x = vs_base[0];
	switch (type_of(x)) {

	case t_fixnum:
	case t_bignum:
		vs_push(small_fixnum(0));
		return;

	case t_ratio:
		q = x;
		y = small_fixnum(1);
		goto RATIO;

	case t_shortfloat:
		d = (double)(sf(x));
		if (d >= 0.0)
			q = double_to_integer(d + 0.5);
		else
			q = double_to_integer(d - 0.5);
		d -= number_to_double(q);
		if (d == 0.5 && number_oddp(q)) {
			vs_push(q);
			q = one_plus(q);
			d = -0.5;
		}
		if (d == -0.5 && number_oddp(q)) {
			vs_push(q);
			q = one_minus(q);
			d = 0.5;
		}
		vs_base = vs_top;
		vs_push(q);
		vs_push(make_shortfloat((shortfloat)d));
		return;

	case t_longfloat:
		d = lf(x);
		if (d >= 0.0)
			q = double_to_integer(d + 0.5);
		else
			q = double_to_integer(d - 0.5);
		d -= number_to_double(q);
		if (d == 0.5 && number_oddp(q)) {
			vs_push(q);
			q = one_plus(q);
			d = -0.5;
		}
		if (d == -0.5 && number_oddp(q)) {
			vs_push(q);
			q = one_minus(q);
			d = 0.5;
		}
		vs_base = vs_top;
		vs_push(q);
		vs_push(make_longfloat(d));
		return;

	default:
		FEwrong_type_argument(TSor_rational_float, x);
	}

TWO_ARG:
	if (n > 2)
		too_many_arguments();
	x = vs_base[0];
	y = vs_base[1];
	check_type_or_rational_float(&vs_base[0]);
	check_type_or_rational_float(&vs_base[1]);
	q = number_divide(x, y);
	vs_push(q);
	switch (type_of(q)) {
	case t_fixnum:
	case t_bignum:
		vs_base = vs_top;
		vs_push(q);
		vs_push(small_fixnum(0));
		break;
	
	case t_ratio:
	RATIO:
	  q1 = integer_divide1(q->rat.rat_num, q->rat.rat_den,0);/*FIXME*/
		vs_push(q1);
		r = number_minus(q, q1);
		vs_push(r);
		if ((c = number_compare(r, plus_half)) > 0 ||
		    (c == 0 && number_oddp(q1)))
			q1 = one_plus(q1);
		if ((c = number_compare(r, minus_half)) < 0 ||
		    (c == 0 && number_oddp(q1)))
			q1 = one_minus(q1);
		vs_base = vs_top;
		vs_push(q1);
		vs_push(num_remainder(x, y, q1));
		return;

	case t_shortfloat:
	case t_longfloat:
		d = number_to_double(q);
		if (d >= 0.0)
			q1 = double_to_integer(d + 0.5);
		else
			q1 = double_to_integer(d - 0.5);
		d -= number_to_double(q1);
		if (d == 0.5 && number_oddp(q1)) {
			vs_push(q1);
			q1 = one_plus(q1);
		}
		if (d == -0.5 && number_oddp(q1)) {
			vs_push(q1);
			q1 = one_minus(q1);
		}
		vs_base = vs_top;
		vs_push(q1);
		vs_push(num_remainder(x, y, q1));
		return;
	default:
	  break;
	}
}


LFD(Lmod)(void) {
  check_arg(2);
  intdivrem(vs_base[0],vs_base[1],-1,NULL,vs_base);
  vs_top=vs_base+1;
}
  

LFD(Lrem)(void) {
  check_arg(2);
  intdivrem(vs_base[0],vs_base[1],0,NULL,vs_base);
  vs_top=vs_base+1;
}


LFD(Ldecode_float)(void)
{
	object x;
	double d;
	int e, s;

	check_arg(1);
	check_type_float(&vs_base[0]);
	x = vs_base[0];
	if (type_of(x) == t_shortfloat)
		d = sf(x);
	else
		d = lf(x);
	if (d >= 0.0)
		s = 1;
	else {
		d = -d;
		s = -1;
	}
	e=0;
	if (!ISNORMAL(d)) {
	  int hp,lp,sp;

	  integer_decode_double(d,&hp,&lp,&e,&sp);
	  if (hp!=0 || lp<0)
	    d=number_to_double(bignum2(hp, lp));
	  else
	    d=lp;
	}
	e += double_exponent(d);
	d = set_exponent(d, 0);
	vs_top = vs_base;
	if (type_of(x) == t_shortfloat) {
		vs_push(make_shortfloat((shortfloat)d));
		vs_push(make_fixnum(e));
		vs_push(make_shortfloat((shortfloat)s));
	} else {
		vs_push(make_longfloat(d));
		vs_push(make_fixnum(e));
		vs_push(make_longfloat((double)s));
	}
}

LFD(Lscale_float)(void)
{
	object x;
	double d;
	int e, k=0;

	check_arg(2);
	check_type_float(&vs_base[0]);
	x = vs_base[0];
	if (type_of(vs_base[1]) == t_fixnum)
		k = fix(vs_base[1]);
	else
		FEerror("~S is an illegal exponent.", 1, vs_base[1]);
	if (type_of(x) == t_shortfloat)
		d = sf(x);
	else
		d = lf(x);
	e = double_exponent(d) + k;
#ifdef VAX
	if (e <= -128 || e >= 128)
#endif
#ifdef IBMRT

#endif
#ifdef IEEEFLOAT
	  /* Upper bound not needed, handled by floating point overflow */
	  /* this checks if we're in the denormalized range */
	if (!ISNORMAL(d) || (type_of(x) == t_shortfloat && e <= -126/*  || e >= 130 */) ||
	    (type_of(x) == t_longfloat && (e <= -1022 /* || e >= 1026 */)))
#endif
#ifdef MV

#endif
#ifdef S3000
	if (e < -64 || e >= 64)
#endif
/* 		FEerror("~S is an illegal exponent.", 1, vs_base[1]); */
	  {
	    for (;k>0;d*=2.0,k--);
	    for (;k<0;d*=0.5,k++);
	  }
	else
	  d = set_exponent(d, e);
	vs_popp;
	if (type_of(x) == t_shortfloat)
		vs_base[0] = make_shortfloat((shortfloat)d);
	else
		vs_base[0] = make_longfloat(d);
}

LFD(Lfloat_radix)(void)
{
	check_arg(1);
	check_type_float(&vs_base[0]);
#ifdef VAX
	vs_base[0] = small_fixnum(2);
#endif
#ifdef IBMRT

#endif
#ifdef IEEEFLOAT
	vs_base[0] = small_fixnum(2);
#endif
#ifdef MV

#endif
#ifdef S3000
	vs_base[0] = small_fixnum(16);
#endif
}

LFD(Lfloat_sign)(void)
{
	object x;
	int narg;
	double d, f;

	narg = vs_top - vs_base;
	if (narg < 1)
		too_few_arguments();
	else if (narg > 2)
		too_many_arguments();
	check_type_float(&vs_base[0]);
	x = vs_base[0];
	if (type_of(x) == t_shortfloat)
		d = sf(x);
	else
		d = lf(x);
	if (narg == 1)
		f = 1.0;
	else {
		check_type_float(&vs_base[1]);
		x = vs_base[1];
		if (type_of(x) == t_shortfloat)
			f = sf(x);
		else
			f = lf(x);
		if (f < 0.0)
			f = -f;
	}
	if (d < 0.0)
		f = -f;
	vs_top = vs_base;
	if (type_of(x) == t_shortfloat)
		vs_push(make_shortfloat((shortfloat)f));
	else
		vs_push(make_longfloat(f));
}

LFD(Lfloat_digits)(void)
{
	check_arg(1);
	check_type_float(&vs_base[0]);
	if (type_of(vs_base[0]) == t_shortfloat)
		vs_base[0] = small_fixnum(24);
	else
		vs_base[0] = small_fixnum(53);
}

LFD(Lfloat_precision)(void)
{
	object x;

	check_arg(1);
	check_type_float(&vs_base[0]);
	x = vs_base[0];
	if (type_of(x) == t_shortfloat)
		if (sf(x) == 0.0)
			vs_base[0] = small_fixnum(0);
		else
			vs_base[0] = small_fixnum(24);
	else
		if (lf(x) == 0.0)
			vs_base[0] = small_fixnum(0);
		else
#ifdef VAX
			vs_base[0] = small_fixnum(53);
#endif
#ifdef IBMRT

#endif
#ifdef IEEEFLOAT
			vs_base[0] = small_fixnum(53);
#endif
#ifdef MV

#endif
#ifdef S3000
			vs_base[0] = small_fixnum(53);
#endif
}

LFD(Linteger_decode_float)(void)
{
	object x;
	int h, l, e, s;

	check_arg(1);
	check_type_float(&vs_base[0]);
	x = vs_base[0];
	vs_base = vs_top;
	if (type_of(x) == t_longfloat) {
		integer_decode_double(lf(x), &h, &l, &e, &s);
		if (h != 0 || l<0)
			vs_push(bignum2(h, l));
		else
			vs_push(make_fixnum(l));
		vs_push(make_fixnum(e));
		vs_push(make_fixnum(s));
	} else {
		integer_decode_float((double)(sf(x)), &h, &e, &s);
		vs_push(make_fixnum(h));
		vs_push(make_fixnum(e));
		vs_push(make_fixnum(s));
	}
}

LFD(Lcomplex)(void)
{
	object	r, i;
	int narg;

	narg = vs_top - vs_base;
	if (narg < 1)
		too_few_arguments();
	if (narg > 2)
		too_many_arguments();
	check_type_or_rational_float(&vs_base[0]);
	r = vs_base[0];
	if (narg == 1)
		i = small_fixnum(0);
	else {
		check_type_or_rational_float(&vs_base[1]);
		i = vs_base[1];
	}
	vs_top = vs_base;
	vs_push(make_complex(r, i));
}

LFD(Lrealpart)(void)
{
	object	x;

	check_arg(1);
	check_type_number(&vs_base[0]);
	x = vs_base[0];
	if (type_of(x) == t_complex)
		vs_base[0] = x->cmp.cmp_real;
}

LFD(Limagpart)(void)
{
	object x;

	check_arg(1);
	check_type_number(&vs_base[0]);
	x = vs_base[0];
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		vs_base[0] = small_fixnum(0);
		break;
	case t_shortfloat:
		vs_base[0] = shortfloat_zero;
		break;
	case t_longfloat:
		vs_base[0] = longfloat_zero;
		break;
	case t_complex:
		vs_base[0] = x->cmp.cmp_imag;
		break;
	default:
	  break;
	}
}

void
gcl_init_num_co(void)
{
	float smallest_float, smallest_norm_float, biggest_float;
	double smallest_double, smallest_norm_double, biggest_double;
	float float_epsilon, float_negative_epsilon;
	double double_epsilon, double_negative_epsilon;
	union {double d;int i[2];} u;
	union {float f;int i;} uf;


#ifdef VAX
	l[0] = 0x80;
	l[1] = 0;
	smallest_float = *(float *)l;
	smallest_double = *(double *)l;
#endif

#ifdef IEEEFLOAT
#ifdef NS32K





#else
	uf.i=1;
	u.i[HIND]=0;
	u.i[LIND]=1;
	smallest_float=uf.f;
	smallest_double=u.d;

/* 	((int *) &smallest_float)[0]= 1; */
/* 	((int *) &smallest_double)[HIND] = 0; */
/* 	((int *) &smallest_double)[LIND] = 1; */

#endif
#endif

#ifdef MV




#endif

#ifdef S3000
	l[0] = 0x00100000;
	l[1] = 0;
	smallest_float = *(float *)l;
	smallest_double = *(double *)l;
#endif

#ifdef VAX
	l[0] = 0xffff7fff;
	l[1] = 0xffffffff;
	biggest_float = *(float *)l;
	biggest_double = *(double *)l;
#endif

#ifdef IBMRT




#endif

#ifdef IEEEFLOAT
#ifdef NS32K





#else

	uf.i=0x7f7fffff;
	u.i[HIND]=0x7fefffff;
	u.i[LIND]=0xffffffff;
	
	biggest_float=uf.f;
	biggest_double=u.d;

/* 	((int *) &biggest_float)[0]= 0x7f7fffff; */
/* 	((int *) &biggest_double)[HIND] = 0x7fefffff; */
/* 	((int *) &biggest_double)[LIND] = 0xffffffff; */

#ifdef BAD_FPCHIP
 /* &&&& I am adding junk values to get past debugging */
        biggest_float = 1.0e37;
        smallest_float = 1.0e-37;
        biggest_double = 1.0e308;
        smallest_double = 1.0e-308;
        printf("\n Used fake values for float max and mins ");
#endif
#endif
#endif

#if defined(S3000) && ~defined(DBL_MAX_10_EXP)
	l[0] = 0x7fffffff;
	l[1] = 0xffffffff;
	l[0] = 0x7fffffff;
	l[1] = 0xffffffff;
	biggest_float = *(float *)l;
	biggest_float = *(float *)l;
	biggest_float = *(float *)l;
	biggest_float = 0.0;
	biggest_float = biggest_float + 1.0;
	biggest_float = biggest_float + 2.0;
	biggest_float = *(float *)l;
	biggest_float = *(float *)l;
	strcmp("I don't like", "DATA GENERAL.");
	biggest_float = *(float *)l;
	biggest_double = *(double *)l;
	biggest_double = *(double *)l;
	biggest_double = *(double *)l;
	biggest_double = 0.0;
	biggest_double = biggest_double + 1.0;
	biggest_double = biggest_double + 2.0;
	biggest_double = *(double *)l;
	biggest_double = *(double *)l;
	strcmp("I don't like", "DATA GENERAL.");
	biggest_double = *(double *)l;
#endif

#ifdef DBL_MAX_10_EXP
	biggest_double = DBL_MAX;
	smallest_norm_double = DBL_MIN;
	smallest_norm_float = FLT_MIN;
	biggest_float = FLT_MAX;
#endif
	
	{
	  
	  volatile double rd,dd,td,td1;
	  volatile float  rf,df,tf,tf1;
	  int i,j;
#define MAX 500
	  
	  for (rf=1.0f,df=0.5f,i=j=0;i<MAX && j<MAX && df!=1.0f;i++,df=1.0f-(0.5f*(1.0f-df)))
	    for (tf=rf,tf1=tf+1.0f,j=0;j<MAX && tf1!=1.0f;j++,rf=tf,tf*=df,tf1=tf+1.0f);
	  if (i==MAX||j==MAX)
	    printf("WARNING, cannot calculate float_epsilon: %d %d %f   %f %f %f\n",i,j,rf,df,tf,tf1);
	  float_epsilon=rf;

	  for (rf=1.0f,df=0.5f,i=j=0;i<MAX && j<MAX && df!=1.0f;i++,df=1.0f-(0.5f*(1.0f-df)))
	    for (tf=rf,tf1=1.0f-tf,j=0;j<MAX && tf1!=1.0f;j++,rf=tf,tf*=df,tf1=1.0f-tf);
	  if (i==MAX||j==MAX)
	    printf("WARNING, cannot calculate float_negative_epsilon: %d %d %f   %f %f %f\n",i,j,rf,df,tf,tf1);
	  float_negative_epsilon=rf;
	  
	  for (rd=1.0,dd=0.5,i=j=0;i<MAX && j<MAX && dd!=1.0;i++,dd=1.0-(0.5*(1.0-dd)))
	    for (td=rd,td1=td+1.0,j=0;j<MAX && td1!=1.0;j++,rd=td,td*=dd,td1=td+1.0);
	  if (i==MAX||j==MAX)
	    printf("WARNING, cannot calculate double_epsilon: %d %d %f   %f %f %f\n",i,j,rd,dd,td,td1);
	  double_epsilon=rd;

	  for (rd=1.0,dd=0.5,i=j=0;i<MAX && j<MAX && dd!=1.0;i++,dd=1.0-(0.5*(1.0-dd)))
	    for (td=rd,td1=1.0-td,j=0;j<MAX && td1!=1.0;j++,rd=td,td*=dd,td1=1.0-td);
	  if (i==MAX||j==MAX)
	    printf("WARNING, cannot calculate double_negative_epsilon: %d %d %f   %f %f %f\n",i,j,rd,dd,td,td1);
	  double_negative_epsilon=rd;
	  
	}

	
#ifdef IEEEFLOAT
	/* Maybe check for "right" answer here */
#endif

	make_constant("MOST-POSITIVE-SHORT-FLOAT",
		      make_shortfloat(biggest_float));
	make_constant("LEAST-POSITIVE-SHORT-FLOAT",
		      make_shortfloat(smallest_float));
	make_constant("LEAST-NEGATIVE-SHORT-FLOAT",
		      make_shortfloat(-smallest_float));
	make_constant("MOST-NEGATIVE-SHORT-FLOAT",
		      make_shortfloat(-biggest_float));

	make_constant("MOST-POSITIVE-SINGLE-FLOAT",
		      make_longfloat(biggest_double));
	make_constant("LEAST-POSITIVE-SINGLE-FLOAT",
		      make_longfloat(smallest_double));
	make_constant("LEAST-NEGATIVE-SINGLE-FLOAT",
		      make_longfloat(-smallest_double));
	make_constant("MOST-NEGATIVE-SINGLE-FLOAT",
		      make_longfloat(-biggest_double));

	make_constant("MOST-POSITIVE-DOUBLE-FLOAT",
		      make_longfloat(biggest_double));
	make_constant("LEAST-POSITIVE-DOUBLE-FLOAT",
		      make_longfloat(smallest_double));
	make_constant("LEAST-NEGATIVE-DOUBLE-FLOAT",
		      make_longfloat(-smallest_double));
	make_constant("MOST-NEGATIVE-DOUBLE-FLOAT",
		      make_longfloat(-biggest_double));

	make_constant("MOST-POSITIVE-LONG-FLOAT",
		      make_longfloat(biggest_double));
	make_constant("LEAST-POSITIVE-LONG-FLOAT",
		      make_longfloat(smallest_double));
	make_constant("LEAST-NEGATIVE-LONG-FLOAT",
		      make_longfloat(-smallest_double));
	make_constant("MOST-NEGATIVE-LONG-FLOAT",
		      make_longfloat(-biggest_double));

	make_constant("SHORT-FLOAT-EPSILON",
		      make_shortfloat(float_epsilon));
	make_constant("SINGLE-FLOAT-EPSILON",
		      make_longfloat(double_epsilon));
	make_constant("DOUBLE-FLOAT-EPSILON",
		      make_longfloat(double_epsilon));
	make_constant("LONG-FLOAT-EPSILON",
		      make_longfloat(double_epsilon));

	make_constant("SHORT-FLOAT-NEGATIVE-EPSILON",
		      make_shortfloat(float_negative_epsilon));
	make_constant("SINGLE-FLOAT-NEGATIVE-EPSILON",
		      make_longfloat(double_negative_epsilon));
	make_constant("DOUBLE-FLOAT-NEGATIVE-EPSILON",
		      make_longfloat(double_negative_epsilon));
	make_constant("LONG-FLOAT-NEGATIVE-EPSILON",
		      make_longfloat(double_negative_epsilon));

	/* Normalized constants added, CM */
	make_constant("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT",
		      make_shortfloat(smallest_norm_float));
	make_constant("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT",
		      make_shortfloat(-smallest_norm_float));
	make_constant("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT",
		      make_longfloat(smallest_norm_double));
	make_constant("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT",
		      make_longfloat(-smallest_norm_double));
	make_constant("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT",
		      make_longfloat(smallest_norm_double));
	make_constant("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT",
		      make_longfloat(-smallest_norm_double));
	make_constant("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT",
		      make_longfloat(smallest_norm_double));
	make_constant("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT",
		      make_longfloat(-smallest_norm_double));

	plus_half = make_ratio(small_fixnum(1), small_fixnum(2));
	enter_mark_origin(&plus_half);

	minus_half = make_ratio(small_fixnum(-1), small_fixnum(2));
	enter_mark_origin(&minus_half);

	make_function("FLOAT", Lfloat);
	make_function("NUMERATOR", Lnumerator);
	make_function("DENOMINATOR", Ldenominator);
	make_function("FLOOR", Lfloor);
	make_function("CEILING", Lceiling);
	make_function("TRUNCATE", Ltruncate);
	make_function("ROUND", Lround);
	make_function("MOD", Lmod);
	make_function("REM", Lrem);
	make_function("DECODE-FLOAT", Ldecode_float);
	make_function("SCALE-FLOAT", Lscale_float);
	make_function("FLOAT-RADIX", Lfloat_radix);
	make_function("FLOAT-SIGN", Lfloat_sign);
	make_function("FLOAT-DIGITS", Lfloat_digits);
	make_function("FLOAT-PRECISION", Lfloat_precision);
	make_function("INTEGER-DECODE-FLOAT", Linteger_decode_float);
	make_function("COMPLEX", Lcomplex);
	make_function("REALPART", Lrealpart);
	make_function("IMAGPART", Limagpart);
}
