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
	Arithmetic operations
*/
#define NEED_MP_H
#include "include.h"

#include "num_include.h"

void
zero_divisor(void);

object fixnum_add(fixnum i, fixnum j)
{

  if (i>=0)
   { if (j<= (MOST_POSITIVE_FIX-i))
      { return make_fixnum(i+j);
      }
   MPOP(return,addss,i,j);
   } else { /* i < 0 */
     if ((MOST_NEGATIVE_FIX -i) <= j) {
       return make_fixnum(i+j);
     }
   MPOP(return,addss,i,j);
   }
}
/* return i - j */
object fixnum_sub(fixnum i, fixnum j)
{  

  if (i>=0)
   { if (j >= (i - MOST_POSITIVE_FIX))
      { return make_fixnum(i-j);
      }
   MPOP(return,subss,i,j);
   } else { /* i < 0 */
     if ((MOST_NEGATIVE_FIX -i) <= -j) {
       return make_fixnum(i-j);
     }
   MPOP(return,subss,i,j);
   }
}

inline object 
fixnum_times(fixnum i, fixnum j) {

#ifdef HAVE_CLZL
  if (i!=MOST_NEGATIVE_FIX && j!=MOST_NEGATIVE_FIX && fixnum_mul_safe(i,j))
#else
  if (i>=0 ? (j>=0 ? (!i || j<= (MOST_POSITIVE_FIX/i)) : (j==-1 || i<= (MOST_NEGATIVE_FIX/j))) :
      (j>=0 ? (i==-1 || j<= (MOST_NEGATIVE_FIX/i)) : (i>MOST_NEGATIVE_FIX && -i<= (MOST_POSITIVE_FIX/-j))))
#endif
      return make_fixnum(i*j);
  else
    MPOP(return,mulss,i,j);
}


static object
number_to_complex(object x)
{
	object z;

	switch (type_of(x)) {

	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_shortfloat:
	case t_longfloat:
		z = alloc_object(t_complex);
		z->cmp.cmp_real = x;
		z->cmp.cmp_imag = small_fixnum(0);
		return(z);

	case t_complex:
		return(x);

	default:
		FEwrong_type_argument(sLnumber, x);
		return(Cnil);
	}
}

object
number_plus(object x, object y)
{
	double dx, dy;
	object z;
	switch (type_of(x)) {
	case t_fixnum:
		switch(type_of(y)) {
		case t_fixnum:
		  return fixnum_add(fix(x),fix(y));
		case t_bignum:
		  MPOP(return, addsi,fix(x),MP(y));
		case t_ratio:
			z = number_plus(number_times(x, y->rat.rat_den),
					y->rat.rat_num);
			return make_ratio(z, y->rat.rat_den);
		case t_shortfloat:
			dx = (double)(fix(x));
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = (double)(fix(x));
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
		  MPOP(return,addsi,fix(y),MP(x)); 
		case t_bignum:
		  MPOP(return,addii,MP(y),MP(x)); 
		case t_ratio:
			z = number_plus(number_times(x, y->rat.rat_den), y->rat.rat_num);
			return make_ratio(z, y->rat.rat_den);
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			
			z = number_plus(x->rat.rat_num,
					number_times(x->rat.rat_den, y));
			z = make_ratio(z, x->rat.rat_den);
			return(z);
		case t_ratio:

			z = number_plus(number_times(x->rat.rat_num,y->rat.rat_den),
					number_times(x->rat.rat_den,y->rat.rat_num));
			z = make_ratio(z,number_times(x->rat.rat_den,y->rat.rat_den));
			return(z);
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			dx = (double)(sf(x));
			dy = (double)(fix(y));
			goto SHORTFLOAT;
		case t_shortfloat:
			dx = (double)(sf(x));
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = (double)(sf(x));
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			dx = (double)(sf(x));
			dy = number_to_double(y);
			goto SHORTFLOAT;
		}
	SHORTFLOAT:
		z = alloc_object(t_shortfloat);
		sf(z) = (shortfloat)(dx + dy);
		return(z);

	case t_longfloat:
		dx = lf(x);
		switch (type_of(y)) {
		case t_fixnum:
			dy = (double)(fix(y));
			goto LONGFLOAT;
		case t_shortfloat:
			dy = (double)(sf(y));
			goto LONGFLOAT;
		case t_longfloat:
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			dy = number_to_double(y);
			goto LONGFLOAT;
		}
	LONGFLOAT:
		z = alloc_object(t_longfloat);
		lf(z) = dx + dy;
		return(z);

	case t_complex:
	COMPLEX:
		x = number_to_complex(x);
		y = number_to_complex(y);
		z = make_complex(number_plus(x->cmp.cmp_real, y->cmp.cmp_real),
				 number_plus(x->cmp.cmp_imag, y->cmp.cmp_imag));
		return(z);

	default:
		FEwrong_type_argument(sLnumber, x);
		return(Cnil);
	}
}

object
one_plus(object x)
{
	double dx;
	object z;

	
	switch (type_of(x)) {

	case t_fixnum:
	  return fixnum_add(fix(x),1);
	case t_bignum:
	  MPOP(return,addsi,1,MP(x));
	case t_ratio:
		z = number_plus(x->rat.rat_num, x->rat.rat_den);
		z = make_ratio(z, x->rat.rat_den);
		return(z);

	case t_shortfloat:
		dx = (double)(sf(x));
		z = alloc_object(t_shortfloat);
		sf(z) = (shortfloat)(dx + 1.0);
		return(z);

	case t_longfloat:
		dx = lf(x);
		z = alloc_object(t_longfloat);
		lf(z) = dx + 1.0;
		return(z);

	case t_complex:
		z = make_complex(one_plus(x->cmp.cmp_real), x->cmp.cmp_imag);
		return(z);

	default:
		FEwrong_type_argument(sLnumber, x);
		return(Cnil);
	}
}

object
number_minus(object x, object y)
{
	double dx, dy;
	object z;

	
	switch (type_of(x)) {

	case t_fixnum:
		switch(type_of(y)) {
		case t_fixnum:
		  return fixnum_sub(fix(x),fix(y));
		  /* MPOP(return,subss,fix(x),fix(y)); */
		case t_bignum:
		  MPOP(return, subsi,fix(x),MP(y));
		case t_ratio:
			z = number_minus(number_times(x, y->rat.rat_den), y->rat.rat_num);
			z = make_ratio(z, y->rat.rat_den);
			return(z);
		case t_shortfloat:
			dx = (double)(fix(x));
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = (double)(fix(x));
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
		  MPOP(return,subis,MP(x),fix(y));
		case t_bignum:
		  MPOP(return,subii,MP(x),MP(y));
		case t_ratio:
			z = number_minus(number_times(x, y->rat.rat_den), y->rat.rat_num);
			z = make_ratio(z, y->rat.rat_den);
			return(z);
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = number_minus(x->rat.rat_num, number_times(x->rat.rat_den, y));
			z = make_ratio(z, x->rat.rat_den);
			return(z);
		case t_ratio:
			z = number_minus(number_times(x->rat.rat_num,y->rat.rat_den),
					 (number_times(x->rat.rat_den,y->rat.rat_num)));
			z = make_ratio(z,number_times(x->rat.rat_den,y->rat.rat_den));
			return(z);
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			dx = (double)(sf(x));
			dy = (double)(fix(y));
			goto SHORTFLOAT;
		case t_shortfloat:
			dx = (double)(sf(x));
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = (double)(sf(x));
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			dx = (double)(sf(x));
			dy = number_to_double(y);
			goto SHORTFLOAT;
		}
	SHORTFLOAT:
		z = alloc_object(t_shortfloat);
		sf(z) = (shortfloat)(dx - dy);
		return(z);

	case t_longfloat:
		dx = lf(x);
		switch (type_of(y)) {
		case t_fixnum:
			dy = (double)(fix(y));
			goto LONGFLOAT;
		case t_shortfloat:
			dy = (double)(sf(y));
			goto LONGFLOAT;
		case t_longfloat:
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			dy = number_to_double(y);
		}
	LONGFLOAT:
		z = alloc_object(t_longfloat);
		lf(z) = dx - dy;
		return(z);

	case t_complex:
	COMPLEX:
		x = number_to_complex(x);
		y = number_to_complex(y);
		z = make_complex(number_minus(x->cmp.cmp_real, y->cmp.cmp_real),
				 number_minus(x->cmp.cmp_imag, y->cmp.cmp_imag));
		return(z);

	default:
		FEwrong_type_argument(sLnumber, x);
		return(Cnil);
	}
}

object
one_minus(object x)
{
	double dx;
	object z;
	switch (type_of(x)) {

	case t_fixnum:
	  return fixnum_sub(fix(x),1);
	case t_bignum:
	  MPOP(return,addsi,-1,MP(x));
	case t_ratio:
		z = number_minus(x->rat.rat_num, x->rat.rat_den);
		z = make_ratio(z, x->rat.rat_den);
		return(z);

	case t_shortfloat:
		dx = (double)(sf(x));
		z = alloc_object(t_shortfloat);
		sf(z) = (shortfloat)(dx - 1.0);
		return(z);

	case t_longfloat:
		dx = lf(x);
		z = alloc_object(t_longfloat);
		lf(z) = dx - 1.0;
		return(z);

	case t_complex:
		z = make_complex(one_minus(x->cmp.cmp_real), x->cmp.cmp_imag);
		return(z);

	default:
		FEwrong_type_argument(sLnumber, x);
		return(Cnil);
	}
}

object
number_negate(object x)
{
	object	z, z1;

	switch (type_of(x)) {

	case t_fixnum:
		if(fix(x) == MOST_NEGATIVE_FIX)
		  return fixnum_add(1,MOST_POSITIVE_FIX);
		else
		  return(make_fixnum(-fix(x)));
	case t_bignum:
		return big_minus(x);
	case t_ratio:
		z1 = number_negate(x->rat.rat_num);
		z = alloc_object(t_ratio);
		z->rat.rat_num = z1;
		z->rat.rat_den = x->rat.rat_den;
		return(z);

	case t_shortfloat:
		z = alloc_object(t_shortfloat);
		sf(z) = -sf(x);
		return(z);

	case t_longfloat:
		z = alloc_object(t_longfloat);
		lf(z) = -lf(x);
		return(z);

	case t_complex:
		z = make_complex(number_negate(x->cmp.cmp_real),
				 number_negate(x->cmp.cmp_imag));
		return(z);

	default:
		FEwrong_type_argument(sLnumber, x);
		return(Cnil);
	}
}

object
number_times(object x, object y)
{  
	object z;
	double dx, dy;

	switch (type_of(x)) {

	case t_fixnum:
		switch (type_of(y)) {
		case t_fixnum:
		  return fixnum_times(fix(x),fix(y));
		  /* MPOP(return,mulss,fix(x),fix(y)); */
		case t_bignum:
		  MPOP(return,mulsi,fix(x),MP(y));
		case t_ratio:
			z = make_ratio(number_times(x, y->rat.rat_num), y->rat.rat_den);
			return(z);
		case t_shortfloat:
			dx = (double)(fix(x));
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = (double)(fix(x));
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
 		  MPOP(return,mulsi,fix(y),MP(x));
		case t_bignum:
		  MPOP(return,mulii,MP(y),MP(x));
		case t_ratio:
			z = make_ratio(number_times(x, y->rat.rat_num), y->rat.rat_den);
			return(z);
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = make_ratio(number_times(x->rat.rat_num, y), x->rat.rat_den);
			return(z);
		case t_ratio:
			z = make_ratio(number_times(x->rat.rat_num,y->rat.rat_num),
				       number_times(x->rat.rat_den,y->rat.rat_den));
			return(z);
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			dx = (double)(sf(x));
			dy = (double)(fix(y));
			goto SHORTFLOAT;
		case t_shortfloat:
			dx = (double)(sf(x));
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = (double)(sf(x));
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			dx = (double)(sf(x));
			dy = number_to_double(y);
			break;
		}
	SHORTFLOAT:
		z = alloc_object(t_shortfloat);
		sf(z) = (shortfloat)(dx * dy);
		return(z);

	case t_longfloat:
		dx = lf(x);
		switch (type_of(y)) {
		case t_fixnum:
			dy = (double)(fix(y));
			goto LONGFLOAT;
		case t_shortfloat:
			dy = (double)(sf(y));
			goto LONGFLOAT;
		case t_longfloat:
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			dy = number_to_double(y);
		}
	LONGFLOAT:
		z = alloc_object(t_longfloat);
		lf(z) = dx * dy;
		return(z);

	case t_complex:
	COMPLEX:
	{
		object z1, z2, z11, z12, z21, z22;

		x = number_to_complex(x);
		y = number_to_complex(y);
		z11 = number_times(x->cmp.cmp_real, y->cmp.cmp_real);
		z12 = number_times(x->cmp.cmp_imag, y->cmp.cmp_imag);
		z21 = number_times(x->cmp.cmp_imag, y->cmp.cmp_real);
		z22 = number_times(x->cmp.cmp_real, y->cmp.cmp_imag);
		z1 =  number_minus(z11, z12);
		z2 =  number_plus(z21, z22);
		z = make_complex(z1, z2);
		return(z);
	}

	default:
		FEwrong_type_argument(sLnumber, x);
		return(Cnil);
	}
}

object
number_divide(object x, object y)
{
	object z;
	double dx, dy;

	switch (type_of(x)) {

	case t_fixnum:
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			if(number_zerop(y) == TRUE)
				zero_divisor();
			if (number_minusp(y) == TRUE) {
				x = number_negate(x);
				y = number_negate(y);
			}
			z = make_ratio(x, y);
			return(z);
		case t_ratio:
			if(number_zerop(y->rat.rat_num))
				zero_divisor();
			z = make_ratio(number_times(x, y->rat.rat_den), y->rat.rat_num);
			return(z);
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			if (number_zerop(y))
				zero_divisor();
			z = make_ratio(x->rat.rat_num, number_times(x->rat.rat_den, y));
			return(z);
		case t_ratio:
			z = make_ratio(number_times(x->rat.rat_num,y->rat.rat_den),
				       number_times(x->rat.rat_den,y->rat.rat_num));
			return(z);
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_argument(sLnumber, y);
		}

	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			dx = (double)(sf(x));
			dy = (double)(fix(y));
			goto SHORTFLOAT;
		case t_shortfloat:
			dx = (double)(sf(x));
			dy = (double)(sf(y));
			goto SHORTFLOAT;
		case t_longfloat:
			dx = (double)(sf(x));
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			dx = (double)(sf(x));
			dy = number_to_double(y);
			goto LONGFLOAT;
		}
	SHORTFLOAT:
		z = alloc_object(t_shortfloat);
		if (dy == 0.0)
			zero_divisor();
		sf(z) = (shortfloat)(dx / dy);
		return(z);


	case t_longfloat:
		dx = lf(x);
		switch (type_of(y)) {
		case t_fixnum:
			dy = (double)(fix(y));
			goto LONGFLOAT;
		case t_shortfloat:
			dy = (double)(sf(y));
			goto LONGFLOAT;
		case t_longfloat:
			dy = lf(y);
			goto LONGFLOAT;
		case t_complex:
			goto COMPLEX;
		default:
			dy = number_to_double(y);
		}
	LONGFLOAT:
		z = alloc_object(t_longfloat);
		if (dy == 0.0)
			zero_divisor();
		lf(z) = dx / dy;
		return(z);

	case t_complex:
	COMPLEX:
	{
		object z1, z2, z3;

		x = number_to_complex(x);
		y = number_to_complex(y);
		z1 = number_times(y->cmp.cmp_real, y->cmp.cmp_real);
		z2 = number_times(y->cmp.cmp_imag, y->cmp.cmp_imag);
		if (number_zerop(z3 = number_plus(z1, z2)))
			zero_divisor();
		z1 = number_times(x->cmp.cmp_real, y->cmp.cmp_real);
		z2 = number_times(x->cmp.cmp_imag, y->cmp.cmp_imag);
		z1 = number_plus(z1, z2);
		z = number_times(x->cmp.cmp_imag, y->cmp.cmp_real);
		z2 = number_times(x->cmp.cmp_real, y->cmp.cmp_imag);
		z2 = number_minus(z, z2);
		z1 = number_divide(z1, z3);
		z2 = number_divide(z2, z3);
		z = make_complex(z1, z2);
		return(z);
	}

	default:
		FEwrong_type_argument(sLnumber, x);
		return(Cnil);
	}
}

object
integer_divide1(object x, object y,fixnum d) {
  object q;

  integer_quotient_remainder_1(x, y, &q, NULL,d);
  return(q);

}

object
integer_divide2(object x, object y,fixnum d,object *r) {
  object q;

  integer_quotient_remainder_1(x, y, &q, r,d);
  return(q);

}

object
get_gcd_abs(object x,object y) {

  object r;
  
  for (;;) {
    
    if (type_of(x) == t_fixnum && type_of(y) == t_fixnum)
      return make_fixnum(fixnum_gcd(fix(x),fix(y)));
    
    if (number_compare(x, y) < 0) {
      r = x;
      x = y;
      y = r;
    }
    if (type_of(y) == t_fixnum && fix(y) == 0)
      return(x);

    integer_quotient_remainder_1(x, y, NULL, &r,0);
    x = y;
    y = r;

  }

}


object
get_gcd(object x, object y) {
  
  return get_gcd_abs(number_abs(x),number_abs(y));

}

LFD(Lplus)(void)
{
        fixnum i, j;
	
	j = vs_top - vs_base;
	if (j == 0) {
		vs_push(small_fixnum(0));
		return;
	}
	for (i = 0;  i < j;  i++)
		check_type_number(&vs_base[i]);
	for (i = 1;  i < j;  i++)
		vs_base[0] = number_plus(vs_base[0], vs_base[i]);
	vs_top = vs_base+1;
}

LFD(Lminus)(void)
{
	fixnum i, j;

	j = vs_top - vs_base;
	if (j == 0)
		too_few_arguments();
	for (i = 0; i < j ; i++)
		check_type_number(&vs_base[i]);
	if (j == 1) {
		vs_base[0] = number_negate(vs_base[0]);
		return;
	}
	for (i = 1;  i < j;  i++)
		vs_base[0] = number_minus(vs_base[0], vs_base[i]);
	vs_top = vs_base+1;
}

LFD(Ltimes)(void)
{
	fixnum i, j;

	j = vs_top - vs_base;
	if (j == 0) {
		vs_push(small_fixnum(1));
		return;
	}
	for (i = 0;  i < j;  i++)
		check_type_number(&vs_base[i]);
	for (i = 1;  i < j;  i++)
		vs_base[0] = number_times(vs_base[0], vs_base[i]);
	vs_top = vs_base+1;
}

LFD(Ldivide)(void)
{
	fixnum i, j;

	j = vs_top - vs_base;
	if (j == 0)
		too_few_arguments();
	for(i = 0;  i < j;  i++)
		check_type_number(&vs_base[i]);
	if (j == 1) {
		vs_base[0] = number_divide(small_fixnum(1), vs_base[0]);
		return;
	}
	for (i = 1; i < j; i++)
		vs_base[0] = number_divide(vs_base[0], vs_base[i]);
	vs_top = vs_base+1;
}

LFD(Lone_plus)(void)
{
	
	check_arg(1);
	check_type_number(&vs_base[0]);
	vs_base[0] = one_plus(vs_base[0]);
}

LFD(Lone_minus)(void)
{
	
	check_arg(1);
	check_type_number(&vs_base[0]);
	vs_base[0] = one_minus(vs_base[0]);
}

LFD(Lconjugate)(void)
{
	object	c, i;

	check_arg(1);
	check_type_number(&vs_base[0]);
	c = vs_base[0];
	if (type_of(c) == t_complex) {
		i = number_negate(c->cmp.cmp_imag);
		vs_push(i);
		vs_base[0] = make_complex(c->cmp.cmp_real, i);
		vs_popp;
	}
}

LFD(Lgcd)(void) {

  fixnum i, narg=vs_top-vs_base;
  
  if (narg == 0) {
    vs_push(small_fixnum(0));
    return;
  }

  for (i = 0;  i < narg;  i++)
    check_type_integer(&vs_base[i]);

  vs_top=vs_base;
  vs_push(number_abs(vs_base[0]));
  
  for (i = 1;  i < narg;  i++)
    vs_base[0] = get_gcd_abs(vs_base[0], number_abs(vs_base[i]));

}

object
get_lcm_abs(object x,object y) {

  object g=get_gcd_abs(x,y);

  return number_zerop(g) ? g : number_times(x,integer_divide1(y,g,0));

}

object
get_lcm(object x,object y) {

  return get_lcm_abs(number_abs(x),number_abs(y));

}

LFD(Llcm)(void) {

  fixnum i, narg;
  
  narg = vs_top - vs_base;

  if (narg == 0)
    too_few_arguments();

  for (i = 0;  i < narg;  i++)
    check_type_integer(&vs_base[i]);

  vs_top=vs_base;
  vs_push(number_abs(vs_base[0]));

  for (i=1;i<narg && !number_zerop(vs_base[0]);i++)
    vs_base[0]=get_lcm_abs(vs_base[0],number_abs(vs_base[i]));

}

void
zero_divisor(void)
{
	FEerror("Zero divisor.", 0);
}

void
gcl_init_num_arith(void)
{
	make_function("+", Lplus);
	make_function("-", Lminus);
	make_function("*", Ltimes);
	make_function("/", Ldivide);
	make_function("1+", Lone_plus);
	make_function("1-", Lone_minus);
	make_function("CONJUGATE", Lconjugate);
	make_function("GCD", Lgcd);
	make_function("LCM", Llcm);
}
