
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
	Logical operations on number
*/
#define NEED_MP_H
#include <stdlib.h>
#include "include.h"
#include "num_include.h"

   
#ifdef GMP
#include "gmp_num_log.c"
#else
#include "pari_num_log.c"
#endif



inline object
fixnum_big_shift(fixnum x,fixnum w) {
  MPOP(return,shifti,SI_TO_MP(x,big_fixnum1),w);
}

inline object
integer_fix_shift(object x, fixnum w) { 
  if (type_of(x)==t_fixnum) {
    fixnum fx=fix(x);
    return (fx!=MOST_NEGATIVE_FIX || w<0) ? fixnum_shft(fx,w) : fixnum_big_shift(fx,w);
  }
  MPOP(return,shifti,MP(x),w);
}
	
inline object
integer_shift(object x,object y) {
  enum type tx=type_of(x),ty=type_of(y);
  if (ty==t_fixnum)
    return integer_fix_shift(x,fix(y));
  else {
    if (eql(x,make_fixnum(0)))
      return x;
    if (big_sign(y)<0)
      return make_fixnum((tx==t_fixnum ? fix(x) : big_sign(x))<0 ? -1 : 0);
    FEerror("Insufficient memory",0);
    return Cnil;
  }
}
      
inline object
integer_length(object x) {
  return make_fixnum(type_of(x)==t_fixnum ? fixnum_length(fix(x)) : MP_SIZE_IN_BASE2(MP(x)));
}

inline object
integer_count(object x) {
  return make_fixnum(type_of(x)==t_fixnum ? fixnum_count(fix(x)) : MP_BITCOUNT(MP(x)));
}

#define DEFLOG(a_,b_,c_) \
  LFD(a_)(void) {				\
	 object x;				\
	 int narg, i;				\
	 					\
	 narg = vs_top - vs_base;		\
	 for (i = 0; i < narg; i++)			\
	   check_type_integer(&vs_base[i]);		\
	 if (narg == 0) {				\
	 vs_top = vs_base;				\
	 vs_push(c_);					\
	 return;					\
	 }						\
	 if (narg == 1)					\
	   return;					\
	 x = log_op(b_);				\
	 vs_top = vs_base;				\
	 vs_push(x);					\
  }

DEFLOG(Llogior,BOOLIOR,small_fixnum(0));
DEFLOG(Llogxor,BOOLXOR,small_fixnum(0));
DEFLOG(Llogand,BOOLAND,small_fixnum(-1));
DEFLOG(Llogeqv,BOOLEQV,small_fixnum(-1));

LFD(Lboole)(void)
{
  object x;
  object o;
  
  check_arg(3);
  check_type_integer(&vs_base[0]);
  check_type_integer(&vs_base[1]);
  check_type_integer(&vs_base[2]);
  o = vs_base[0];
  
  vs_base++;
  x = log_op(fix(o));
  vs_base--;
  vs_top = vs_base;
  vs_push(x);

}

inline bool
integer_bitp(object p,object x) {
  enum type tp=type_of(p),tx=type_of(x);

  if (tp==t_fixnum) {
    if (tx==t_fixnum)
      return fixnum_bitp(fix(p),fix(x));
    else 
      return big_bitp(x,fix(p));
  } else if (big_sign(p)<0)
    return 0;
  else if (tx==t_fixnum)/*fixme integer_minusp*/
    return fix(x)<0;
  else return big_sign(x)<0;
}

LFD(Llogbitp)(void)
{
  check_arg(2);
  check_type_integer(&vs_base[0]);
  check_type_integer(&vs_base[1]);
  vs_top=vs_base;
  vs_push(integer_bitp(vs_base[0],vs_base[1])?Ct:Cnil);
}

LFD(Lash)(void) {
  check_arg(2);
  check_type_integer(&vs_base[0]);
  check_type_integer(&vs_base[1]);
  vs_top=vs_base;
  vs_push(integer_shift(vs_base[0],vs_base[1]));
}

LFD(Llogcount)(void) {
  check_arg(1);
  check_type_integer(&vs_base[0]);
  vs_base[0]=integer_count(vs_base[0]);
}

LFD(Linteger_length)(void) {
  check_arg(1);
  check_type_integer(&vs_base[0]);
  vs_base[0]=integer_length(vs_base[0]);
}

#define W_SIZE (8*sizeof(int))

static fixnum
ior_op(fixnum i, fixnum j)
{
	return(i | j);
}

static fixnum
xor_op(fixnum i, fixnum j)
{
	return(i ^ j);
}

static fixnum
and_op(fixnum i, fixnum j)
{
	return(i & j);
}

static fixnum
eqv_op(fixnum i, fixnum j)
{
	return(~(i ^ j));
}

static fixnum
nand_op(fixnum i, fixnum j)
{
	return(~(i & j));
}

static fixnum
nor_op(fixnum i, fixnum j)
{
	return(~(i | j));
}

static fixnum
andc1_op(fixnum i, fixnum j)
{
	return((~i) & j);
}

static fixnum
andc2_op(fixnum i, fixnum j)
{
	return(i & (~j));
}

static fixnum
orc1_op(fixnum i, fixnum j)
{
	return((~i) | j);
}

static fixnum
orc2_op(fixnum i, fixnum j)
{
	return(i | (~j));
}

static fixnum
b_clr_op(fixnum i, fixnum j)
{
	return(0);
}

static fixnum
b_set_op(fixnum i, fixnum j)
{
	return(-1);
}

static fixnum
b_1_op(fixnum i, fixnum j)
{
	return(i);
}

static fixnum
b_2_op(fixnum i, fixnum j)
{
	return(j);
}

static fixnum
b_c1_op(fixnum i, fixnum j)
{
	return(~i);
}

static fixnum
b_c2_op(fixnum i, fixnum j)
{
	return(~j);
}

LFD(siLbit_array_op)(void)
{
	fixnum i, j, n, d;
	object  o, x, y, r, r0=Cnil;
	fixnum (*op)()=NULL;
	bool replace = FALSE;
	fixnum xi, yi, ri;
	char *xp, *yp, *rp;
	fixnum xo, yo, ro;
	object *base = vs_base;

	check_arg(4);
	o = vs_base[0];
	x = vs_base[1];
	y = vs_base[2];
	r = vs_base[3];
	if (type_of(x) == t_bitvector) {
		d = x->bv.bv_dim;
		xp = x->bv.bv_self;
		xo = BV_OFFSET(x);
		if (type_of(y) != t_bitvector)
			goto ERROR;
		if (d != y->bv.bv_dim)
			goto ERROR;
		yp = y->bv.bv_self;
		yo = BV_OFFSET(y);
		if (r == Ct)
			r = x;
		if (r != Cnil) {
			if (type_of(r) != t_bitvector)
				goto ERROR;
			if (r->bv.bv_dim != d)
				goto ERROR;
			i = (r->bv.bv_self - xp)*8 + (BV_OFFSET(r) - xo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = Cnil;
				replace = TRUE;
				goto L1;
			}
			i = (r->bv.bv_self - yp)*8 + (BV_OFFSET(r) - yo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = Cnil;
				replace = TRUE;
			}
		}
	L1:
		if (r == Cnil) {
			vs_base = vs_top;
			vs_push(sLbit);
			vs_push(make_fixnum(d));
			vs_push(Cnil);
			vs_push(Cnil);
			vs_push(Cnil);
			vs_push(Cnil);
			vs_push(Cnil);
			siLmake_vector();
			r = vs_base[0];
		}
	} else {
		if (type_of(x) != t_array)
			goto ERROR;
		if ((enum aelttype)x->a.a_elttype != aet_bit)
			goto ERROR;
		d = x->a.a_dim;
		xp = x->bv.bv_self;
		xo = BV_OFFSET(x);
		if (type_of(y) != t_array)
			goto ERROR;
		if ((enum aelttype)y->a.a_elttype != aet_bit)
			goto ERROR;
		if (x->a.a_rank != y->a.a_rank)
			goto ERROR;
		yp = y->bv.bv_self;
		yo = BV_OFFSET(y);
		for (i = 0;  i < x->a.a_rank;  i++)
			if (x->a.a_dims[i] != y->a.a_dims[i])
				goto ERROR;
		if (r == Ct)
			r = x;
		if (r != Cnil) {
			if (type_of(r) != t_array)
				goto ERROR;
			if ((enum aelttype)r->a.a_elttype != aet_bit)
				goto ERROR;
			if (r->a.a_rank != x->a.a_rank)
				goto ERROR;
			for (i = 0;  i < x->a.a_rank;  i++)
				if (r->a.a_dims[i] != x->a.a_dims[i])
					goto ERROR;
			i = (r->bv.bv_self - xp)*8 + (BV_OFFSET(r) - xo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = Cnil;
				replace = TRUE;
				goto L2;
			} 
			i = (r->bv.bv_self - yp)*8 + (BV_OFFSET(r) - yo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = Cnil;
				replace = TRUE;
			}
		}
	L2:
		if (r == Cnil) {
		  object b;
		  struct cons *p=alloca(x->a.a_rank*sizeof(struct cons));
		  if (x->a.a_rank) {
		    object b1;

		    b=(object)p;
		    for (b1=b,i=0;i<x->a.a_rank;i++,b1=b1->c.c_cdr) {
#ifdef WIDE_CONS
		      set_type_of(b1,t_cons);
#endif
		      b1->c.c_car=/* x->a.a_dims[i]<SMALL_FIXNUM_LIMIT ?  */
			/* small_fixnum(x->a.a_dims[i]) :  */ 
			/* now done in a macro */
			make_fixnum(x->a.a_dims[i]);
		      b1->c.c_cdr=i<x->a.a_rank-1 ? (object)++p : Cnil;
		    }
		  } else
		    b=Cnil;

		  r = fSmake_array1(aet_bit,Cnil,small_fixnum(0),Cnil,0,b);

		  /* 		  object b[F_ARG_LIMIT]; */
		  /* 		  b[0]=Cnil; */
		  /* 		  for (i = 0;  i < x->a.a_rank;  i++) */
		  /* 		    b[i] = (make_fixnum(x->a.a_dims[i])); */
		  /* 		  r=Iapply_fun_n1(fSmake_array1,5,x->a.a_rank ? x->a.a_rank : 1, */
		  /* 			       aet_bit, */
		  /* 			       Cnil, */
		  /* 			       small_fixnum(0), */
		  /* 			       Cnil, */
		  /* 			       Cnil, */
		  /*				 b); */

		}
	}
	rp = r->bv.bv_self;
	ro = BV_OFFSET(r);
	switch(fixint(o)) {
		case BOOLCLR:	op = b_clr_op;	break;
		case BOOLSET:	op = b_set_op;	break;
		case BOOL1:	op = b_1_op;	break;
		case BOOL2:	op = b_2_op;	break;
		case BOOLC1:	op = b_c1_op;	break;
		case BOOLC2:	op = b_c2_op;	break;
		case BOOLAND:	op = and_op;	break;
		case BOOLIOR:	op = ior_op;	break;
		case BOOLXOR:	op = xor_op;	break;
		case BOOLEQV:	op = eqv_op;	break;
		case BOOLNAND:	op = nand_op;	break;
		case BOOLNOR:	op = nor_op;	break;
		case BOOLANDC1:	op = andc1_op;	break;
		case BOOLANDC2:	op = andc2_op;	break;
		case BOOLORC1:	op = orc1_op;	break;
		case BOOLORC2:	op = orc2_op;	break;
		default:
			FEerror("~S is an invalid logical operator.", 1, o);
	}

#define	set_high(place, nbits, value) \
	((place)=(((place)&~(-0400>>(nbits)))|((value)&(-0400>>(nbits)))))

#define	set_low(place, nbits, value) \
	((place)=(((place)&(-0400>>(8-(nbits))))|((value)&~(-0400>>(8-(nbits))))))

#define	extract_byte(integer, pointer, index, offset) \
	(integer) = (pointer)[(index)+1] & 0377; \
	(integer) = ((pointer)[index]<<(offset))|((integer)>>(8-(offset)))

#define	store_byte(pointer, index, offset, value) \
	set_low((pointer)[index], 8-(offset), (value)>>(offset)); \
	set_high((pointer)[(index)+1], offset, (value)<<(8-(offset)))

	if (xo == 0 && yo == 0 && ro == 0) {
		for (n = d/8, i = 0;  i < n;  i++)
			rp[i] = (*op)(xp[i], yp[i]);
		if ((j = d%8) > 0)
			set_high(rp[n], j, (*op)(xp[n], yp[n]));
		if (!replace) {
			vs_top = vs_base = base;
			vs_push(r);
			return;
		}
	} else {
		for (n = d/8, i = 0;  i <= n;  i++) {
			extract_byte(xi, xp, i, xo);
			extract_byte(yi, yp, i, yo);
			if (i == n) {
				if ((j = d%8) == 0)
					break;
				extract_byte(ri, rp, n, ro);
				set_high(ri, j, (*op)(xi, yi));
			} else
				ri = (*op)(xi, yi);
			store_byte(rp, i, ro, ri);
		}
		if (!replace) {
			vs_top = vs_base = base;
			vs_push(r);
			return;
		}
	}
	rp = r0->bv.bv_self;
	ro = BV_OFFSET(r0);
	for (n = d/8, i = 0;  i <= n;  i++) {
		if (i == n) {
			if ((j = d%8) == 0)
				break;
			extract_byte(ri, rp, n, ro);
			set_high(ri, j, r->bv.bv_self[n]);
		} else
			ri = r->bv.bv_self[i];
		store_byte(rp, i, ro, ri);
	}
	vs_top = vs_base = base;
	vs_push(r0);
	return;

ERROR:
	FEerror("Illegal arguments for bit-array operation.", 0);
}

void
gcl_init_num_log(void)
{
/*  	int siLbit_array_op(void); */

	make_constant("BOOLE-CLR", make_fixnum(BOOLCLR));
	make_constant("BOOLE-SET", make_fixnum(BOOLSET));
	make_constant("BOOLE-1", make_fixnum(BOOL1));
	make_constant("BOOLE-2", make_fixnum(BOOL2));
	make_constant("BOOLE-C1", make_fixnum(BOOLC1));
	make_constant("BOOLE-C2", make_fixnum(BOOLC2));
	make_constant("BOOLE-AND", make_fixnum(BOOLAND));
	make_constant("BOOLE-IOR", make_fixnum(BOOLIOR));
	make_constant("BOOLE-XOR", make_fixnum(BOOLXOR));
	make_constant("BOOLE-EQV", make_fixnum(BOOLEQV));
	make_constant("BOOLE-NAND", make_fixnum(BOOLNAND));
	make_constant("BOOLE-NOR", make_fixnum(BOOLNOR));
	make_constant("BOOLE-ANDC1", make_fixnum(BOOLANDC1));
	make_constant("BOOLE-ANDC2", make_fixnum(BOOLANDC2));
	make_constant("BOOLE-ORC1", make_fixnum(BOOLORC1));
	make_constant("BOOLE-ORC2", make_fixnum(BOOLORC2));

	make_function("LOGIOR", Llogior);
	make_function("LOGXOR", Llogxor);
	make_function("LOGAND", Llogand);
	make_function("LOGEQV", Llogeqv);
	make_function("BOOLE", Lboole);
	make_function("LOGBITP", Llogbitp);
	make_function("ASH", Lash);
	make_function("LOGCOUNT", Llogcount);
	make_function("INTEGER-LENGTH", Linteger_length);

	sLbit = make_ordinary("BIT");
	make_si_function("BIT-ARRAY-OP", siLbit_array_op);
}

