#include <stdarg.h>
#define _VA_LIST_DEFINED
#ifndef EXTER
#define EXTER extern
#endif
#ifndef INLINE
#if defined(__GNUC__) && __GNUC__ <= 4
#define INLINE extern inline
#else
#define INLINE inline
#endif
#endif
