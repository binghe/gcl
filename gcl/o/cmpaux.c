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

	cmpaux.c
*/

#include <sys/types.h>
#include <sys/stat.h>
     
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#define NEED_MP_H
#include "include.h"
#define dcheck_type(a,b) check_type(a,b)

#include "page.h"

#ifdef HAVE_AOUT
#undef ATT
#undef BSD
#ifndef HAVE_ELF
#ifndef HAVE_FILEHDR
#define BSD
#endif
#endif
#include HAVE_AOUT
#endif

DEFUNO_NEW("SPECIALP",object,fSspecialp,SI
       ,1,1,NONE,OO,OO,OO,OO,void,siLspecialp,(object sym),"")
{
	/* 1 args */
	if (type_of(sym) == t_symbol &&
	    (enum stype)sym->s.s_stype == stp_special)
		sym = Ct;
	else
		sym = Cnil;
	RETURN1(sym);
}

DEF_ORDINARY("DEBUGGER",sSdebugger,SI,"");

DEFUN_NEW("DEFVAR1",object,fSdefvar1,SI
       ,2,3,NONE,OO,OO,OO,OO,(object sym,object val,...),"")
{	int n=VFUN_NARGS;
	object doc;
	va_list ap;
	{ va_start(ap,val);
	  if (n>=3) doc=va_arg(ap,object);else goto LDEFAULT3;
	  goto LEND_VARARG;
	LDEFAULT3: doc = Cnil;
	LEND_VARARG: va_end(ap);}

	CHECK_ARG_RANGE(2,3);
	if(sym->s.s_dbind==0 && n > 1)
	  sym->s.s_dbind= val;
	sym->s.s_stype=(short)stp_special;
	if(n > 2)
	  putprop(sym,doc,sSvariable_documentation);
	RETURN1(sym);
      }


DEFUN_NEW("DEBUG",object,fLdebug,LISP
       ,2,2,NONE,OO,OO,OO,OO,(object sym,object val),"")
{ /* 2 args */
  putprop(sym,val,sSdebugger);
  RETURN1(sym);
}


DEFUN_NEW("SETVV",object,fSsetvv,SI
       ,2,2,NONE,OO,OO,OO,OO,(object index,object val),"")
{ /* 2 args */
  if(type_of(sSPmemory->s.s_dbind)==t_cfdata)
  sSPmemory->s.s_dbind->cfd.cfd_self[fix(index)]=val;
  else FEerror("setvv called outside %init",0);
  RETURN1(index);
}

DEF_ORDINARY("%MEMORY",sSPmemory,SI,"");
DEF_ORDINARY("%INIT",sSPinit,SI,"");

/* void Lidentity(void); */
void
gcl_init_cmpaux(void)
{


	/* real one defined in predlib.lsp, need this for bootstrap */
/* 	make_si_function("WARN-VERSION",Lidentity); */
	
}

  
/* Now inlined directly by optimizer  */
/* int */
/* ifloor(int x, int y) */
/* { */
/*   if (y == 0) { */
/*     FEerror("Zero divizor", 0); */
/*     return 0; */
/*   } */
/*   if (y > 0) { */
/*     if (x >= 0) */
/*       return(x/y); */
/*     else */
      /* FIXME, deal with possible overflow here*/
/*       return(-((-x-1))/y-1); */
/*   } */
/*   if (x >= 0) */
      /* FIXME, deal with possible overflow here*/
/*     return(-((x-1)/(-y))-1); */
/*   else */
/*     return((-x)/(-y)); */
/* } */

/* int */
/* imod(int x, int y) */
/* { */
/*   return(x - ifloor(x, y)*y); */
/* } */

/* static void */
/* set_VV(object *, int, object); */

/* static void */
/* set_VV_data(object *VV, int n, object data, char *start, int size) */
/* {set_VV(VV,n,data); */
/*  data->cfd.cfd_start=start; */
/*  data->cfd.cfd_size = size; */
/* } */

/* static void */
/* set_VV(object *VV, int n, object data) */
/* { */
/* 	object *p, *q; */

/* 	p = VV; */
/* 	q = data->v.v_self; */
/* 	while (n-- > 0) */
/* 		*p++ = *q++; */
/* 	data->cfd.cfd_self = VV; */
/* } */

/*
	Conversions to C
*/

char
object_to_char(object x)
{
	int c=0;
 	switch (type_of(x)) {
	case t_fixnum:
		c = fix(x);  break;
	case t_bignum:
	  {object *to = vs_top;
	  vs_push(x);
	  vs_push(small_fixnum(0xff));
	  Llogand();
	  x = vs_base[0];
	  vs_top = to;
	  c = (char) fix(x);
	  break;
	  }
	case t_character:
		c = char_code(x);  break;
	default:
		FEerror("~S cannot be coerce to a C char.", 1, x);
	}
	return(c);
}

int
object_to_int(object x)
{
	int i=0;

	switch (type_of(x)) {
	case t_character:
		i = char_code(x);  break;
	case t_fixnum:
		i = fix(x);  break;
	case t_bignum:
	  i = number_to_double(x);
	  break;
	case t_ratio:
		i = number_to_double(x);  break;
	case t_shortfloat:
		i = sf(x);  break;
	case t_longfloat:
		i = lf(x);  break;
	default:
		FEerror("~S cannot be coerce to a C int.", 1, x);
	}
	return(i);
}

fixnum
object_to_fixnum(object x)
{
	fixnum i=0;

	switch (type_of(x)) {
	case t_character:
	  i = char_code(x);  break;
	case t_fixnum:
	  i = fix(x);  break;
	case t_bignum:
	  i = number_to_double(x);
	  break;
	case t_ratio:
	  i = number_to_double(x);  break;
	case t_shortfloat:
	  i = sf(x);  break;
	case t_longfloat:
	  i = lf(x);  break;
	default:
	  FEerror("~S cannot be coerce to a C int.", 1, x);
	}
	return(i);
}

float 
object_to_float(object x) 
{ 
	float f=0.0; 

	switch (type_of(x)) { 
	case t_character: 
		f = char_code(x);  break; 
	case t_fixnum: 
		f = fix(x);  break; 
	case t_bignum: 
	case t_ratio: 
		f = number_to_double(x);  break; 
	case t_shortfloat: 
		f = sf(x);  break; 
	case t_longfloat: 
		f = lf(x);  break; 
	default: 
		FEerror("~S cannot be coerce to a C float.", 1, x); 
	} 
	return(f); 
} 

double 
object_to_double(object x) 
{ 
	double d=0.0; 

	switch (type_of(x)) { 
	case t_character: 
		d = char_code(x);  break; 
	case t_fixnum: 
		d = fix(x);  break; 
	case t_bignum: 
	case t_ratio: 
		d = number_to_double(x);  break; 
	case t_shortfloat: 
		d = sf(x);  break; 
	case t_longfloat: 
		d = lf(x);  break; 
	default: 
		FEerror("~S cannot be coerce to a C double.", 1, x); 
	} 
	return(d); 
} 

/* this may allocate storage.  The user can prevent this
   by providing a string will fillpointer < length and
   have a null character in the fillpointer position. */

char *
object_to_string(object x) { 

  unsigned int leng;
  char *res;

  if (type_of(x)!=t_string) FEwrong_type_argument(sLstring,x);
  leng= x->st.st_fillp;
  /* user has thoughtfully provided a null terminated string ! */
  if (leng > 0 && leng < x->st.st_dim && x->st.st_self[leng]==0)
    return x->st.st_self;

  if (x->st.st_dim == leng && leng % sizeof(object) && MAYBE_DATA_P(x->st.st_self)) {
    x->st.st_self[leng] = 0;
    return x->st.st_self;
  }  
  
  res=malloc(leng+1);
  bcopy(x->st.st_self,res,leng);
  res[leng]=0;
  return res;

}

/*  typedef int (*FUNC)(); */

/* perform the actual invocation of the init function durint a fasload
   init_address is the offset from the place in memory where the code is loaded
   in.  In most systems this will be 0.
   The new style fasl vector MUST end with an entry (si::%init f1 f2 .....)
   where f1 f2 are forms to be evaled.
*/

/* #ifdef CLEAR_CACHE */
/* static int */
/* sigh(int sig,long code,void *scp, char *addr) { */

/*     fprintf(stderr,"Received SIGILL at %p\n",((siginfo_t *)code)->si_addr); */
/*     exit(1); */
/* } */
/* #endif */


void
call_init(int init_address,object memory,object faslfile) {

  bds_bind(sSPmemory,memory);
  bds_bind(sSPinit,faslfile);
  ((FUNC)(memory->cfd.cfd_start+init_address))();
  bds_unwind1;
  bds_unwind1;

}


/* statVV is the address of some static storage, which is used by the
   cfunctions to refer to global variables,..
   Initially it holds a number of addresses.   We also have sSPmemory->s.s_dbind
   which points to a vector  of lisp constants.   We switch the
   fn addresses and lisp constants.   We follow this convoluted path,
   since we don't wish to have a separate block of data space allocated
   in the object module simply to temporarily have access to the
   actual function addresses during load. 

   */

object *min_cfd_self=NULL;

void
do_init(object *statVV) {

  object faslfile=sSPinit->s.s_dbind;
  object data=sSPmemory->s.s_dbind;
  object *p,*q,y;
  int i,n;
  object fasl_vec;
  char ch;

  ch=readc_stream(faslfile);
  unreadc_stream(ch,faslfile);

  if (ch!='\n') {
    struct fasd * fd;
    faslfile=fSopen_fasd(faslfile,sKinput,OBJNULL,Cnil);
    fd=(struct fasd *)faslfile->v.v_self;
    n=fix(fd->table_length);
    fd->table->v.v_self=alloca(n*sizeof(object));
    memset(fd->table->v.v_self,0,n*sizeof(object));
    fd->table->v.v_dim=faslfile->v.v_self[1]->v.v_fillp=n;
  }

  n=fix(READ_STREAM_OR_FASD(faslfile));
  sSPinit->s.s_dbind=fasl_vec=fSmake_vector1_1(n,aet_object,Cnil);

  /* switch SPinit to point to a vector of function addresses */

  fasl_vec->v.v_elttype = aet_fix;

  /* swap the entries */
  for (i=0,p=fasl_vec->v.v_self,q=statVV;i<n;i++) {
    y=*p;
    *p++=*q;
    *q++=y;
  }

  data->cfd.cfd_self = statVV;
  if (!min_cfd_self || data->cfd.cfd_self<min_cfd_self)
    min_cfd_self=data->cfd.cfd_self;
  data->cfd.cfd_fillp= n;
  statVV[n-1] = data;

  /* So now the fasl_vec is a fixnum array, containing random addresses of c
     functions and other stuff from the compiled code.
     data is what it wants to be for the init
  */
  /* Now we can run the forms f1 f2 in form= (%init f1 f2 ...) */

  fSload_stream(faslfile,Cnil);
  if (type_of(faslfile)!=t_stream)
    fSclose_fasd(faslfile);

}

DEFUN_NEW("MARK-MEMORY-AS-PROFILING",object,fSmark_memory_as_profiling,SI,0,0,
	  NONE,OO,OO,OO,OO,(void),"") {

  sSPmemory->s.s_dbind->cfd.cfd_prof=1;

  return Cnil;

}

#ifdef DOS
#define PATH_LIM 8
#define TYPE_LIM 3
char *
fix_path_string_dos(s)
char *s;
{char buf[200];
 char *p=s,*q=buf;
 int i=PATH_LIM;	
 while(*p)
  {
   if (IS_DIR_SEPARATOR(*p)) i=PATH_LIM;
    else if (*p == '.') i = TYPE_LIM;
    else i--;
   if (i>=0) *q++ = *p;
   p++;}
 *q = 0;
 strcpy(s,buf);
 return s;
}
	
#endif

object
new_cfdata(void) {

  object memory=alloc_object(t_cfdata);

  memory->cfd.cfd_size=0;
  memory->cfd.cfd_fillp=0;
  memory->cfd.cfd_prof=0;
  memory->cfd.cfd_self=0;
  memory->cfd.cfd_start=0;

  return memory;

}


void
gcl_init_or_load1(void (*fn)(void),const char *file) {

  if (file[strlen(file)-1]=='o') {

    object memory;
    object faslfile;
    file=FIX_PATH_STRING(file);
    
    memory=new_cfdata();
    memory->cfd.cfd_start= (char *)fn;
    printf("Initializing %s\n",file); fflush(stdout);
    faslfile=open_stream(make_simple_string(file),smm_input,Cnil,sKerror);
    SEEK_TO_END_OFILE(faslfile->sm.sm_fp);
    call_init(0,memory,faslfile);
    close_stream(faslfile);

  } else {
    printf("loading %s\n",file); 
    fflush(stdout);  
    load(file);
  }

}

DEFUN_NEW("INIT-CMP-ANON", object, fSinit_cmp_anon, SI, 0, 0,
       NONE, OO, OO, OO,OO,(void),
      "Initialize previously compiled and linked anonymous function from the \
.text section of the running executable.  This function is inherently \
dangerous, and is meant as a work-around to facilitate the production \
of an ansi GCL image on systems which must currently link using \
dlopen.  On such systems, it is imposible to compile and load \
anonymous functions as part of the initialization sequence of the lisp \
image, as is done in pcl, and preserve that function across a \
save-system call.  The approach here is to provide a flag to GCL's \
compile function which will direct the algorithm to forgo \
recompilation and loading in favor of initialization via this \
function.")
{

  int i;

  i=gcl_init_cmp_anon();
  if (i<0) 
    FEerror("No such anonymous function",0);

  return i ? Cnil : Ct;

}

object
find_init_name1(char *s,unsigned len) {

#ifdef _WIN32

  char *tmp;

  if (len) {
    tmp=alloca(len+1);
    memcpy(tmp,s,len);
    tmp[len]=0;
  } else
    tmp=s;

  return find_init_string(tmp);

#else    
/* These functions have no relevance on Windows
 * as dlopen and friends don't exist in that part of Cyberspace. */

  struct stat ss;
  char *tmp,*q;
  FILE *f;

  if (len) {
    tmp=alloca(len+1);
    memcpy(tmp,s,len);
    tmp[len]=0;
  } else
    tmp=s;
  if (stat(tmp,&ss))
    FEerror("File ~a does not exist",1,make_simple_string(tmp));
  if (!(f=fopen(tmp,"rb")))
    FEerror("Cannot open ~a for binary reading",1,make_simple_string(tmp));
  tmp=alloca(ss.st_size+1);
  if (fread(tmp,1,ss.st_size,f)!=ss.st_size)
    FEerror("Error reading binary file",0);
  fclose(f);
  for (s=tmp+1;s<tmp+ss.st_size
	 && (strncmp(s,"init_",5) || (s>tmp && (s[-1]=='_' ? (s>tmp+1 && s[-2]) : s[-1])));
       q=strstr(s+1,"init_"),s=q ? q : s+strlen(s)+1);
  if (strncmp(s,"init_",5))
    FEerror("Init name not found",0);
  return make_simple_string(s);
#endif  /* _WIN32 */

}
 

DEFUN_NEW("FIND-INIT-NAME", object, fSfind_init_name, SI, 1, 1,
	  NONE, OO, OO, OO,OO,(object namestring),"") 
{

  check_type_string(&namestring);
  return find_init_name1(namestring->st.st_self,namestring->st.st_dim);

}

DEFUN_NEW("SEEK-TO-END-OFILE",object,fSseek_to_end_ofile,SI,1,1,NONE,OO,OO,OO,OO,(object sm),"") {
  check_type_stream(&sm);
  SEEK_TO_END_OFILE(sm->sm.sm_fp);
  RETURN1(sm);
}
