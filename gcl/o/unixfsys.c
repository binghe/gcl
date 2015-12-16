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

#include <unistd.h>
#include <errno.h>
#include <time.h>

#define IN_UNIXFSYS
#include "include.h"
#include <sys/types.h>
#include <sys/stat.h>
#ifndef NO_PWD_H
#include <pwd.h>
#endif

#ifdef __MINGW32__
#  include <windows.h>        
/* Windows has no symlink, therefore no lstat.  Without symlinks lstat
   is equivalent to stat anyway.  */
#  define S_ISLNK(a) 0
#  define lstat stat
#endif

static object
get_string(object x) {
  switch(type_of(x)) {
  case t_symbol:
  case t_string:
    return x;
  case t_pathname:
    return x->pn.pn_namestring;
  case t_stream:
    switch(x->sm.sm_mode) {
    case smm_input:
    case smm_output:
    case smm_probe:
    case smm_io:
      return get_string(x->sm.sm_object1);
    case smm_file_synonym:
      return get_string(x->sm.sm_object0->s.s_dbind);
    }
  }
  return Cnil;
}

void
coerce_to_filename1(object spec, char *p,unsigned sz) {

  object namestring=get_string(spec);

  massert(namestring->st.st_fillp<sz);
  memcpy(p,namestring->st.st_self,namestring->st.st_fillp);
  p[namestring->st.st_fillp]=0;

#ifdef FIX_FILENAME
  FIX_FILENAME(spec,p);
#endif

}


#ifndef MAXPATHLEN
#define MAXPATHLEN 512
#endif

DEFUN("UID-TO-NAME",object,fSuid_to_name,SI,1,1,NONE,OI,OO,OO,OO,(fixnum uid),"") {
  struct passwd *pwent,pw;
  char *b;
  long r;

  massert((r=sysconf(_SC_GETPW_R_SIZE_MAX))>=0);
  massert(b=alloca(r));
  
  massert(!getpwuid_r(uid,&pw,b,r,&pwent));

  RETURN1(make_simple_string(pwent->pw_name));

}
  
DEFUN("HOME-NAMESTRING",object,fShome_namestring,SI,1,1,NONE,OO,OO,OO,OO,(object nm),"") {

  struct passwd *pwent,pw;
  char *b;
  long r;

  massert((r=sysconf(_SC_GETPW_R_SIZE_MAX))>=0);
  massert(b=alloca(r));
  
  if (nm->st.st_fillp==1)
    
    if ((pw.pw_dir=getenv("HOME")))
      pwent=&pw;
    else
      massert(!getpwuid_r(getuid(),&pw,b,r,&pwent));
  
  else {
    
    char *name;
    
    massert(name=alloca(nm->st.st_fillp));
    memcpy(name,nm->st.st_self+1,nm->st.st_fillp-1);
    name[nm->st.st_fillp-1]=0;
    
    massert(!getpwnam_r(name,&pw,b,r,&pwent));
    
  }
  
  massert((b=alloca(strlen(pwent->pw_dir)+2)));
  memcpy(b,pwent->pw_dir,strlen(pwent->pw_dir));
  b[strlen(pwent->pw_dir)]='/';
  b[strlen(pwent->pw_dir)+1]=0;
  RETURN1(make_simple_string(b));
  
}

FILE *
fopen_not_dir(char *filename,char * option) {

  struct stat ss;

  if (!stat(filename,&ss) && S_ISDIR(ss.st_mode))
    return NULL;
  else
    return fopen(filename,option);

}

int
file_len(FILE *fp)
{
	struct stat filestatus;

	if (fstat(fileno(fp), &filestatus)==0) 
	return(filestatus.st_size);
	else return 0;
}



DEF_ORDINARY("DIRECTORY",sKdirectory,KEYWORD,"");
DEF_ORDINARY("LINK",sKlink,KEYWORD,"");
DEF_ORDINARY("FILE",sKfile,KEYWORD,"");

DEFUNM("STAT",object,fSstat,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  object *vals=(object *)fcall.valp;
  object *base=vs_top;
  struct stat ss;
  
  check_type_string(&x);
  coerce_to_filename(x,FN1);

#ifdef __MINGW32__
  {
    char *p=FN1+strlen(FN1)-1;
    for (;p>FN1 && *p=='/';p--)
      *p=0;
  }
#endif
  if (lstat(FN1,&ss))
    RETURN1(Cnil);
  else
    RETURN4(S_ISDIR(ss.st_mode) ? sKdirectory : 
	    (S_ISLNK(ss.st_mode) ? sKlink : sKfile),
	    make_fixnum(ss.st_size),
	    make_fixnum(ss.st_ctime),
	    make_fixnum(ss.st_uid));
}

#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>

DEFUN("READLINKAT",object,fSreadlinkat,SI,2,2,NONE,OI,OO,OO,OO,(fixnum d,object s),"") {
  char *b1,*b2=NULL;
  ssize_t l,z1,z2;
  check_type_string(&s);
  /* l=s->st.st_hasfillp ? s->st.st_fillp : s->st.st_dim; */
  z1=length(s);
  massert((b1=alloca(z1+1)));
  memcpy(b1,s->st.st_self,z1);
  b1[z1]=0;
  for (l=z2=0;l>=z2;) {
    memset(b2,0,z2);
    z2+=z2+10;
    massert((b2=alloca(z2)));
    massert((l=readlinkat(d ? dirfd((DIR *)d) : AT_FDCWD,b1,b2,z2))>=0);
  }
  b2[l]=0;
  s=make_simple_string(b2);
  memset(b1,0,z1);
  memset(b2,0,z2);
  RETURN1(s);
}

DEFUN("GETCWD",object,fSgetcwd,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {
  char *b=NULL;
  size_t z;
  object s;

  for (z=0;!(errno=0) && !getcwd(b,z) && errno==ERANGE;b=memset(b,0,z),z+=z+10,massert((b=alloca(z))));
  massert((b=getcwd(b,z)));
  s=make_simple_string(b);
  memset(b,0,z);
  RETURN1(s);

}

DEFUN("SETENV",object,fSsetenv,SI,2,2,NONE,OO,OO,OO,OO,(object variable,object value),"Set environment VARIABLE to VALUE")

{

  int res = -1;
#ifdef HAVE_SETENV 
  res = setenv(object_to_string(variable),object_to_string(value),1);
#else
#ifdef HAVE_PUTENV
  {char *buf;
  char *sym=object_to_string(variable);
  char *val=object_to_string(value);
  buf = malloc(strlen(sym)+strlen(val)+5);
  sprintf(buf,"%s=%s",sym,val);
  res=putenv(buf);
  free(buf);
  }
#endif
#endif  
  RETURN1((res == 0 ? Ct : Cnil ));
}

#include <sys/types.h>
#include <dirent.h>

DEFUN("OPENDIR",fixnum,fSopendir,SI,1,1,NONE,IO,OO,OO,OO,(object x),"") {
  DIR *d;
  check_type_string(&x);
  coerce_to_filename(x,FN1);
  d=opendir(FN1);
  return (fixnum)d;
}

#ifdef HAVE_D_TYPE
  
DEFUN("D-TYPE-LIST",object,fSd_type_list,SI,0,0,NONE,OI,OO,OO,OO,(void),"") {
  RETURN1(list(8,
	       MMcons(make_fixnum(DT_BLK),make_keyword("BLOCK")),
	       MMcons(make_fixnum(DT_CHR),make_keyword("CHAR")),
	       MMcons(make_fixnum(DT_DIR),make_keyword("DIRECTORY")),
	       MMcons(make_fixnum(DT_FIFO),make_keyword("FIFO")),
	       MMcons(make_fixnum(DT_LNK),make_keyword("LINK")),
	       MMcons(make_fixnum(DT_REG),make_keyword("FILE")),
	       MMcons(make_fixnum(DT_SOCK),make_keyword("SOCKET")),
	       MMcons(make_fixnum(DT_UNKNOWN),make_keyword("UNKNOWN"))
	       ));
}
#endif

DEFUN("READDIR",object,fSreaddir,SI,3,3,NONE,OI,IO,OO,OO,(fixnum x,fixnum y,object s),"") {
  struct dirent *e;
  object z;
  long tl;
  size_t l;
  if (!x) RETURN1(Cnil);
  tl=telldir((DIR *)x);
#ifdef HAVE_D_TYPE
  for (;(e=readdir((DIR *)x)) && y!=DT_UNKNOWN && e->d_type!=y;);
#endif
  if (!e) RETURN1(Cnil);
  if (s==Cnil)
    z=make_simple_string(e->d_name);
  else {
    check_type_string(&s);
    l=strlen(e->d_name);
    if (s->st.st_dim-s->st.st_fillp>=l) {
      memcpy(s->st.st_self+s->st.st_fillp,e->d_name,l);
      s->st.st_fillp+=l;
      z=s;
    } else {
      seekdir((DIR *)x,tl);
      RETURN1(make_fixnum(l));
    }
  }
#ifdef HAVE_D_TYPE
  if (y==DT_UNKNOWN) z=MMcons(z,make_fixnum(e->d_type));
#endif
  RETURN1(z);
}

DEFUN("CLOSEDIR",object,fSclosedir,SI,1,1,NONE,OI,OO,OO,OO,(fixnum x),"") {
  closedir((DIR *)x);
  return Cnil;
}

DEFUN("RENAME",object,fSrename,SI,2,2,NONE,OO,OO,OO,OO,(object x,object y),"") {

  check_type_string(&x);
  check_type_string(&y);

  coerce_to_filename(x,FN1);
  coerce_to_filename(y,FN2);

  RETURN1(rename(FN1,FN2) ? Cnil : Ct);

}
  
DEFUN("UNLINK",object,fSunlink,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  check_type_string(&x);

  coerce_to_filename(x,FN1);

  RETURN1(unlink(FN1) ? Cnil : Ct);

}
  
DEFUN("CHDIR",object,fSchdir,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  check_type_string(&x);

  coerce_to_filename(x,FN1);

  RETURN1(chdir(FN1) ? Cnil : Ct);

}

DEFUN("MKDIR",object,fSmkdir,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  check_type_string(&x);

  coerce_to_filename(x,FN1);

  RETURN1(mkdir(FN1
#ifndef __MINGW32__		
		,01777
#endif
		) ? Cnil : Ct);

}

DEFUN("RMDIR",object,fSrmdir,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  check_type_string(&x);

  coerce_to_filename(x,FN1);

  RETURN1(rmdir(FN1) ? Cnil : Ct);

}

DEFVAR("*LOAD-WITH-FREAD*",sSAload_with_freadA,SI,Cnil,"");

#ifdef _WIN32

void *
get_mmap(FILE *fp,void **ve) {
  
  int n;
  void *st;
  size_t sz;
  HANDLE handle;

  massert((sz=file_len(fp))>0);
  if (sSAload_with_freadA->s.s_dbind==Cnil) {
    n=fileno(fp);
    massert((n=fileno(fp))>2);
    massert(handle = CreateFileMapping((HANDLE)_get_osfhandle(n), NULL, PAGE_WRITECOPY, 0, 0, NULL));
    massert(st=MapViewOfFile(handle,FILE_MAP_COPY,0,0,sz));
    CloseHandle(handle);
  } else {
    massert(st=malloc(sz));
    massert(fread(st,sz,1,fp)==1);
  }

  *ve=st+sz;

  return st;

}

int
un_mmap(void *v1,void *ve) {

  if (sSAload_with_freadA->s.s_dbind==Cnil)
    return UnmapViewOfFile(v1) ? 0 : -1;
  else {
    free(v1);
    return 0;
  }

}


#else

#include <sys/mman.h>

void *
get_mmap(FILE *fp,void **ve) {
  
  int n;
  void *v1;
  struct stat ss;

  massert((n=fileno(fp))>2);
  massert(!fstat(n,&ss));
  if (sSAload_with_freadA->s.s_dbind==Cnil) {
    massert((v1=mmap(0,ss.st_size,PROT_READ|PROT_WRITE,MAP_PRIVATE,n,0))!=(void *)-1);
  } else {
    massert(v1=malloc(ss.st_size));
    massert(fread(v1,ss.st_size,1,fp)==1);
  }

  *ve=v1+ss.st_size;
  return v1;

}
 

int
un_mmap(void *v1,void *ve) {

  if (sSAload_with_freadA->s.s_dbind==Cnil)
    return munmap(v1,ve-v1);
  else {
    free(v1);
    return 0;
  }

}

#endif


void
gcl_init_unixfsys(void) {
}
