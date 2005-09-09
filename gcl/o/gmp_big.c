  /* Copyright William F. Schelter 1991
   Bignum routines.
 

   
num_arith.c: add_int_big
num_arith.c: big_minus
num_arith.c: big_plus
num_arith.c: big_quotient_remainder
num_arith.c: big_sign
num_arith.c: big_times
num_arith.c: complement_big
num_arith.c: copy_big
num_arith.c: div_int_big
num_arith.c: mul_int_big
num_arith.c: normalize_big
num_arith.c: normalize_big_to_object
num_arith.c: stretch_big
num_arith.c: sub_int_big
num_comp.c: big_compare
num_comp.c: big_sign
num_log.c: big_sign
num_log.c: copy_to_big
num_log.c: normalize_big
num_log.c: normalize_big_to_object
num_log.c: stretch_big
num_pred.c: big_sign
number.c: big_to_double
predicate.c: big_compare
typespec.c: big_sign
print.d: big_minus
print.d: big_sign
print.d: big_zerop
print.d: copy_big
print.d: div_int_big
read.d: add_int_big
read.d: big_to_double
read.d: complement_big
read.d: mul_int_big
read.d: normalize_big
read.d: normalize_big_to_object

 */


#include <string.h>

#define DEBUG_GMP
#ifdef DEBUG_GMP
#define ABS(x) ((x) < 0 ? -(x) : (x))
/*  static object */
/*  verify_big(object big) */
/*  { int size; */
/*    if(type_of(big)!=t_bignum) FEerror("Not a bignum",0); */
/*    size = MP_SIZE(big); */
/*    if ( size ==0 ||  (MP_SELF(big))[ABS(size)-1]==0) */
/*      FEerror("badly formed",0); */
/*    return big; */
/*  } */

static object verify_big_or_zero(object big)
{ int size;
  if(type_of(big)!=t_bignum) FEerror("Not a bignum",0);
  size = MP_SIZE(big);
  if ( size && (MP_SELF(big))[ABS(size)-1]==0)
    FEerror("badly formed",0);
  return big;
}

/*  static */
/*  MP_INT* */
/*  verify_mp(MP_INT *u) */
/*  { int size = u->_mp_size; */
/*   if (size != 0 && u->_mp_d[ABS(size)] == 0) */
/*     FEerror("bad mp",0); */
/*   return u; */
/*  } */
#else
#define verify_mp(x)
#define verify_big(x)
#define verify_big_or_zero(x)
#endif


 


#ifndef GMP_USE_MALLOC
object big_gcprotect;
object big_fixnum1;

#include "gmp.c"
void
gcl_init_big1(void) {
    mp_set_memory_functions( gcl_gmp_alloc,gcl_gmp_realloc,gcl_gmp_free);
}

#else
gcl_init_big1()
{
}
#endif  

object
new_bignum(void)
{ object ans;
 {BEGIN_NO_INTERRUPT;
 ans = alloc_object(t_bignum);
 MP_SELF(ans) = 0;
 mpz_init(MP(ans));
 END_NO_INTERRUPT;
 }
 return ans;
}

/* we have to store the body of a u in a bignum object
   so that the garbage collecter will move it and save
   it, and then we can copy it back
*/   
#define GCPROTECT(u) \
 MP_INT * __u = MP(big_gcprotect); \
 (__u)->_mp_d =   (u)->_mp_d; \
 (__u)->_mp_alloc = (u)->_mp_alloc 
#define GC_PROTECTED_SELF (__u)->_mp_d
#define END_GCPROTECT (__u)->_mp_d = 0
 


static object
make_bignum(__mpz_struct *u)
{ object ans ;
 int size;
 {BEGIN_NO_INTERRUPT;
 /* make sure we follow the bignum body of u if it gets moved... */
 { GCPROTECT(u);
 ans = alloc_object(t_bignum);
 size = u->_mp_size;
 MP(ans)->_mp_d = 0;
 if (size == 0 )
   size = 1;
 else if (size < 0) size= -size;
 MP(ans)->_mp_d = (mp_ptr) gcl_gmp_alloc (size*MP_LIMB_SIZE);
 MP(ans)->_mp_alloc = size;
 MP(ans)->_mp_size = u->_mp_size;
 memcpy(MP(ans)->_mp_d,GC_PROTECTED_SELF,size*MP_LIMB_SIZE);
 END_GCPROTECT;
 }
 END_NO_INTERRUPT;
 return ans;
 }
} 

/* coerce a mpz_t to a bignum or fixnum */

object
make_integer(__mpz_struct *u)
{
  if ((u)->_mp_size == 0) return small_fixnum(0);
  if (mpz_fits_sint_p(u)) {
    return make_fixnum(mpz_get_si(u));
      }
  return make_bignum(u);
}

/* like make_integer except that the storage of u is cleared
   if it is a fixnum, and if not the storage of u is actually
   copied to the new bignum
*/
#ifdef OBSOLETE
object
make_integer_clear(u)
mpz_t u;
{ object ans;
  if ((u)->_mp_size == 0) return small_fixnum(0);
  if (mpz_fits_sint_p(u)) {
    signed long int x = mpz_get_si(u);
    mpz_clear(u);
    return make_fixnum(x);
      }
  {BEGIN_NO_INTERRUPT;
  { GCPROTECT(u);
  ans = alloc_object(t_bignum);
  MP(ans)->_mp_alloc = u->_mp_alloc;
  MP(ans)->_mp_size = u->_mp_size;
  /* the u->_mp_d may have moved */
  MP_SELF(ans) = GC_PROTECTED_SELF;
  mpz_clear(u);
  END_GCPROTECT;
  }
  END_NO_INTERRUPT;
  } 
  return ans;
}
#endif /* obsolete */

/* static int */
/* big_zerop(object x) */
/* { return (mpz_sgn(MP(x))== 0);} */

int
big_compare(object x, object y)
{return   mpz_cmp(MP(x),MP(y));
}


object
normalize_big_to_object(object x)
{
  return maybe_replace_big(x);
}


/* static void */
/* gcopy_to_big(__mpz_struct *res, object x) */
/* { */
/*   mpz_set(MP(x),res); */
/* } */

/* destructively modifies x = i - x; */
void
add_int_big(int i, object x)
{
       MPOP_DEST(x,addsi,i,MP(x));
}

/* static void */
/* sub_int_big(int i, object x) */
/* { */  /*  SI_TEMP_DECL(mpz_int_temp); */
/*   MPOP_DEST(x,subsi,i,MP(x)); */
/* } */

void
mul_int_big(int i, object x)
{ MPOP_DEST(x,mulsi,i,MP(x));
}    



/*
	Div_int_big(i, x) destructively divides non-negative bignum x
	by positive int i.
	X will hold the quotient from  the division.
	Div_int_big(i, x) returns the remainder of the division.
	I should be positive.
	X should be non-negative.
*/

/* static int */
/* div_int_big(int i, object x) */
/* { */
/*   return mpz_tdiv_q_ui(MP(x),MP(x),i); */
/* } */


/* static object */
/* big_plus(object x, object y) */
/* { */
/*   MPOP(return,addii,MP(x),MP(y)); */
/* } */

/* static object */
/* big_times(object x, object y) */
/* { */
/*  MPOP(return,mulii,MP(x),MP(y)); */

/* } */

/* x is a big, and it is coerced to a fixnum (and the big is cleared)
   or it is smashed
   
*/
object
normalize_big(object x)
{
 if (MP_SIZE(x) == 0) return small_fixnum(0);
  if (mpz_fits_sint_p(MP(x))) {
    MP_INT *u = MP(x);
    signed long int xx = mpz_get_si(u);
    return make_fixnum(xx);
      }
  else return x;
}

object
big_minus(object x)
{ object y = new_bignum();
 mpz_neg(MP(y),MP(x));
 return normalize_big(y);
}


/* static void */
/* big_quotient_remainder(object x0, object y0, object *qp, object *rp) */
/* { */
/*   object res,quot; */
/*   res = new_bignum(); */
/*   quot = new_bignum(); */
/*   mpz_tdiv_qr(MP(quot),MP(res),MP(x0),MP(y0)); */
/*   *qp = normalize_big(quot); */
/*   *rp = normalize_big(res); */
/*   return; */
/* } */

	
double
big_to_double(object x)
{
  return mpz_get_d(MP(x));
}


/* static object copy_big(object x) */
/* { */
/*   if (type_of(x)==t_bignum) */
/*     return make_bignum(MP(x)); */
/*   else FEerror("bignum expected",0); */
/*   return Cnil; */

/* } */

/* this differes from old copy_to_big in that it does not alter
   copy a bignum.
*/   
/* static object */
/* copy_to_big(object x) { */
/*  if (type_of(x) == t_fixnum) { */
/*    object ans = new_bignum(); */
/*    mpz_set_si(MP(ans),fix(x)); */
/*    return ans; */
/*  } else { */
/*    return x; */
/* } */
/* } */


/* put in to get (declare integer working with existing setup.
   should be optimized at some point, as we're just converting 
   and reconverting integer data, it appears -- CM */

int
obj_to_mpz(object x,MP_INT * y) {

  switch(type_of(x)) {
  case t_fixnum:
    mpz_set_si(y,fix(x));
    break;
  case t_bignum:
    if (abs(MP(x)->_mp_size)<=y->_mp_alloc)
      mpz_set(y,MP(x));
    else
      return abs(MP(x)->_mp_size)*sizeof(*y->_mp_d);
    break;
  default:
    FEerror("fixnum or bignum expected",0);
    break;
  }

  return 0;

}

int
obj_to_mpz1(object x,MP_INT * y,void *v) {

  switch(type_of(x)) {
  case t_fixnum:
    mpz_set_si(y,fix(x));
    break;
  case t_bignum:
    y->_mp_alloc=abs(MP(x)->_mp_size);
    y->_mp_d=v;
    mpz_set(y,MP(x));
    break;
  default:
    FEerror("fixnum or bignum expected",0);
    break;
  }

  return 0;

}

int
mpz_to_mpz(MP_INT * x,MP_INT * y) {

  if (abs(x->_mp_size)<=y->_mp_alloc)
    mpz_set(y,x);
  else
    return abs(x->_mp_size)*sizeof(*y->_mp_d);

  return 0;

}

int
mpz_to_mpz1(MP_INT * x,MP_INT * y,void *v) {

  y->_mp_alloc=abs(x->_mp_size);
  y->_mp_d=v;
  mpz_set(y,x);
  return 0;

}

void
isetq_fix(MP_INT * var,int s)
{
  mpz_set_si(var,s);
}

MP_INT *
otoi(object x) {
  if (type_of(x)==t_fixnum) {
    object y = new_bignum();
    mpz_set_si(MP(y),fix(x));
    return MP(y);
  }
  if (type_of(x)==t_bignum)
    return (MP(x));
  FEwrong_type_argument(sLinteger,x);
  return NULL;
}
/* end added section for declare integer -- CM */




/* return object like *xpt coercing to a fixnum if necessary,
   or return the actual bignum replacing it with another
*/
object
maybe_replace_big(object x)
{ 
/* note  mpz_fits_sint_p(MP(x)) returns arbitrary result if
   passed 0 in bignum form.
   bug or feature of gmp..
*/   
  if (MP_SIZE(x) == 0) return small_fixnum(0);
  if (mpz_fits_sint_p(MP(x))) {
    MP_INT *u = MP(x);
    signed long int xx = mpz_get_si(u);
    return make_fixnum(xx);
  }
  return make_bignum(MP(x));
}


object
bignum2(unsigned int h, unsigned int l)
{
  object x = new_bignum();
  mpz_set_ui(MP(x),h);
  mpz_mul_2exp(MP(x),MP(x),32);
  mpz_add_ui(MP(x),MP(x),l);
  return normalize_big(x);
}

void
integer_quotient_remainder_1(object x, object y, object *qp, object *rp)
{
  *qp = new_bignum();
  *rp = new_bignum();
  /* we may need to coerce the fixnums to MP here, and
     we use the temporary storage of the rp/qp as inputs.
     since overlap is allowed in the mpz_tdiv_qr operation..
  */    
  mpz_tdiv_qr(MP(*qp),MP(*rp),INTEGER_TO_MP(x,big_fixnum1),
	      INTEGER_TO_MP(y,big_fixnum2));
  *qp = normalize_big(*qp);
  *rp = normalize_big(*rp);
  return;
}

#define HAVE_MP_COERCE_TO_STRING
     
object
coerce_big_to_string(object x, int printbase)
{ int i;
 int sign = BIG_SIGN(x);
 int ss = mpz_sizeinbase(MP(x),printbase);
 char *p;
  object ans = alloc_simple_string(ss+2+(sign<0? 1: 0));
  ans->st.st_self=p=alloc_relblock(ans->st.st_dim);
  /*  if (sign < 0) *p++='-'; */
  mpz_get_str(p, printbase,MP(x));
  i = ans->st.st_dim-5;
  if (i <0 ) i=0;
  while(ans->st.st_self[i]) { i++;}
  ans->st.st_fillp=i;
  return ans;
}


void
gcl_init_big(void)
{
  big_gcprotect=alloc_object(t_bignum);
  MP_SELF(big_gcprotect)=0;
  MP_ALLOCATED(big_gcprotect)=0;
  big_fixnum1=new_bignum();
  big_fixnum2=new_bignum();
  enter_mark_origin(&big_fixnum1);
  enter_mark_origin(&big_gcprotect);
  enter_mark_origin(&big_fixnum2);
  gcl_init_big1();


}
