#include <stdarg.h>
#define _VA_LIST_DEFINED

#include <setjmp.h>
			   
#ifndef INLINE
#if (defined(__GNUC__) && __GNUC__ <= 4) && !defined __clang__
#define INLINE extern inline
#else
#define INLINE inline
#endif
#endif
