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
	unixtime.c
*/

#define IN_UNIXTIME

#include <unistd.h>

#include "include.h"
#include <sys/types.h>
#ifdef UNIX
/* all we want from this is HZ the number of clock ticks per second
which is usually 60 maybe 100 or something else. */
#undef PAGESIZE
#ifndef NO_SYS_PARAM_H
#include <sys/param.h>
#endif
#endif

#ifndef HZ
/* #define HZ 60 */
#define HZ 100
#endif

/* #define HZ1 (HZ > 100 ? 100 : HZ) */
#define HZ1 HZ

#ifdef USE_ATT_TIME
#  undef BSD
#  define ATT
#endif

#if defined __MINGW32__ || !defined NO_SYSTEM_TIME_ZONE

#  ifdef __MINGW32__
#    include <windows.h>
#    include <time.h>
#    include <sys/timeb.h>

static struct timeb t0;
int usleep ( unsigned int microseconds );

#  endif

#endif /* __MINGW32__ or  !defined NO_SYSTEM_TIME_ZONE */

#ifdef BSD
#include <time.h>
#include <sys/timeb.h>
#ifndef NO_SYS_TIMES_H
#include <sys/times.h>
#endif
#include <sys/time.h>
/* static struct timeb beginning; */
#endif

#ifdef ATT
#include <sys/times.h>
static long beginning;
#endif

int
runtime(void)
{

#ifdef USE_INTERNAL_REAL_TIME_FOR_RUNTIME

#  ifdef __MINGW32__    
    struct timeb t;
    if ( t0.time == 0 ) {
        ftime(&t0);
    }
    ftime ( &t );
    return ( ( t.time - t0.time ) * HZ1 + ( (t.millitm) * HZ1 ) / 1000 );
#  else
#  error Need to return runtime without generating a fixnum (else GBC(t_fixnum) will loop)
#  endif
    
#else	
	{
	  struct tms buf;
	  times(&buf);
	  return(buf.tms_utime);
	}
#endif
}

object
unix_time_to_universal_time(int i)
{
	object x;
	vs_mark;

	vs_push(make_fixnum(24*60*60));
	vs_push(make_fixnum(70*365+17));
	x = number_times(vs_top[-1], vs_top[-2]);
	vs_push(x);
	vs_push(make_fixnum(i));
	x = number_plus(vs_top[-1], vs_top[-2]);
	vs_reset;
	return(x);
}

DEFUN_NEW("GET-UNIVERSAL-TIME",object,fLget_universal_time,LISP
   ,0,0,NONE,OO,OO,OO,OO,(void),"")
{
	/* 0 args */
	RETURN1(unix_time_to_universal_time(time(0)));
}

LFD(Lsleep)(void)
{
	object z;
	
	check_arg(1);
	check_type_or_rational_float(&vs_base[0]);
	if (number_minusp(vs_base[0]) == TRUE)
		FEerror("~S is not a non-negative number.", 1, vs_base[0]);
	vs_base[0]=number_times(vs_base[0],make_fixnum(1000000));
	Lround();
	z = vs_base[0];
	if (type_of(z) == t_fixnum)
		usleep(fix(z));
	else
            /* What is this for? -- MJT */
		for(;;)
#ifdef __MINGW32__
			Sleep ( 10000 );
#else                    
			sleep(1000);
#endif        
	vs_top = vs_base;
	vs_push(Cnil);
}

LFD(Lget_internal_run_time)(void)
{

#ifdef USE_INTERNAL_REAL_TIME_FOR_RUNTIME
        vs_push(fLget_internal_real_time());
	vs_push(small_fixnum(0));
	return;
#else
	struct tms buf;

	check_arg(0);
	times(&buf);
	vs_push(make_fixnum(buf.tms_utime));
	vs_push(make_fixnum(buf.tms_cutime));
	vs_push(make_fixnum(buf.tms_stime));
	vs_push(make_fixnum(buf.tms_cstime));

#endif	
	
}


DEFUN_NEW("GETTIMEOFDAY",object,fSgettimeofday,SI,0,0,NONE,OO,OO,OO,OO,(void),"Return time with maximum resolution") {
#ifdef __MINGW32__
  LARGE_INTEGER uu,ticks;
  if (QueryPerformanceFrequency(&ticks)) {
    QueryPerformanceCounter(&uu);
    return make_longfloat((longfloat)uu.QuadPart/ticks.QuadPart);
  } else {
    FEerror("microsecond timing not available",0);
    return Cnil;
   /* static struct timeb t0;  */
   /* static unsigned u;  */
   /* struct timeb t;   */
   /* ftime(&t);  */
   /* if (t.time!=t0.time || t.millitm!=t0.millitm) {t0=t;u=0;}  */
   /* u++;  */
   /* return make_longfloat(((longfloat)t.time+1.0e-3*t.millitm+1.0e-6*(u%1000))); */
  }
#endif  
#ifdef BSD
  struct timeval tzp;
  gettimeofday(&tzp,0);
  return make_longfloat((longfloat)tzp.tv_sec+1.0e-6*tzp.tv_usec);
#endif
#ifdef ATT
  return make_longfloat((longfloat)time(0));
#endif
}


DEFUN_NEW("GET-INTERNAL-REAL-TIME",object,fLget_internal_real_time,LISP,0,0,NONE,OO,OO,OO,OO,(void),"Run time relative to beginning")
     
{
#ifdef __MINGW32__
    struct timeb t;
    if ( t0.time == 0 ) {
        ftime ( &t0 );
    }
    ftime(&t);
    return ( make_fixnum ( ( t.time - t0.time ) * HZ1 + ( (t.millitm) * HZ1 ) / 1000 ) );
#endif  
#ifdef BSD
	static struct timeval begin_tzp;
	struct timeval tzp;
	if (begin_tzp.tv_sec==0)
	  gettimeofday(&begin_tzp,0);
	gettimeofday(&tzp,0);
/* the value returned will be relative to the first time this is called,
   plus the fraction of a second.  We must make it relative, so this
   will only wrap if the process lasts longer than 818 days
   */
	return make_fixnum(((tzp.tv_sec-begin_tzp.tv_sec)*HZ1
			    + ((tzp.tv_usec)*HZ1)/1000000));
#endif
#ifdef ATT
	return make_fixnum((time(0) - beginning)*HZ1);
#endif
}


void
gcl_init_unixtime(void) {
#ifdef ATT
  beginning = time(0);
#endif
#  if defined __MINGW32__
  ftime(&t0);
#  endif        
  
  make_constant("INTERNAL-TIME-UNITS-PER-SECOND", make_fixnum(HZ1));
  
  make_function("SLEEP", Lsleep);
  make_function("GET-INTERNAL-RUN-TIME", Lget_internal_run_time);

}

#ifdef __MINGW32__
int usleep ( unsigned int microseconds )
{
    unsigned int milliseconds = microseconds / 1000;
    return ( SleepEx ( milliseconds, TRUE ) );
}

#endif

DEFUN_NEW("CURRENT-TIMEZONE",object,fScurrent_timezone,SI,0,0,NONE,IO,OO,OO,OO,(void),"") {

#if defined(__MINGW32__)

  TIME_ZONE_INFORMATION tzi;
  DWORD TZResult;
  
  TZResult = GetTimeZoneInformation ( &tzi );
  
  /* Now UTC = (local time + bias), in units of minutes, so */
  /*fprintf ( stderr, "Bias = %ld\n", tzi.Bias );*/
  return (object)((tzi.Bias+tzi.DaylightBias)/60);
  
#elif defined NO_SYSTEM_TIME_ZONE
  return (object)0;
#elif defined __CYGWIN__
  struct tm gt,lt;
  fixnum _t=time(0);
  gmtime_r(&_t, &gt);
  localtime_r(&_t, &lt);
  return (object)(gt.tm_hour-lt.tm_hour+24*(gt.tm_yday!=lt.tm_yday ? (gt.tm_year>lt.tm_year||gt.tm_yday>lt.tm_yday ? 1 : -1) : 0));
#else
  fixnum _t=time(0);
  return (object)(-localtime(&_t)->tm_gmtoff/3600);
#endif
}

DEFUN_NEW("CURRENT-DSTP",object,fScurrent_dstp,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {

#if defined(__MINGW32__)

  return Cnil;

#elif defined NO_SYSTEM_TIME_ZONE /*solaris*/
  return Cnil;
#else
  fixnum _t=time(0);
  return localtime(&_t)->tm_isdst > 0 ? Ct : Cnil;
#endif
}
