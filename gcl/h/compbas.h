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
