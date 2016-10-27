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
#include <string.h>

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
    case smm_synonym:
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

}

#ifndef __MINGW32__
static char GETPW_BUF[4096];
#endif

DEFUN_NEW("UID-TO-NAME",object,fSuid_to_name,SI,1,1,NONE,OI,OO,OO,OO,(fixnum uid),"") {
#ifndef __MINGW32__
  struct passwd *pwent,pw;
  long r;

  massert((r=sysconf(_SC_GETPW_R_SIZE_MAX))>=0);
  massert(r<=sizeof(GETPW_BUF));/*FIXME maybe once at image startup*/

  massert(!getpwuid_r(uid,&pw,GETPW_BUF,r,&pwent));

  RETURN1(make_simple_string(pwent->pw_name));
#else
  RETURN1(Cnil);
#endif
}

DEFUN_NEW("HOME-NAMESTRING",object,fShome_namestring,SI,1,1,NONE,OO,OO,OO,OO,(object nm),"") {

#ifndef __MINGW32__
  struct passwd *pwent,pw;
  long r;

  massert((r=sysconf(_SC_GETPW_R_SIZE_MAX))>=0);
  massert(r<=sizeof(GETPW_BUF));/*FIXME maybe once at image startup*/

  if (nm->st.st_fillp==1)

    if ((pw.pw_dir=getenv("HOME")))
      pwent=&pw;
    else
      massert(!getpwuid_r(getuid(),&pw,GETPW_BUF,r,&pwent) && pwent);

  else {

    massert(nm->st.st_fillp<sizeof(FN2));
    memcpy(FN2,nm->st.st_self+1,nm->st.st_fillp-1);
    FN2[nm->st.st_fillp-1]=0;

    massert(!getpwnam_r(FN2,&pw,GETPW_BUF,r,&pwent) && pwent);

  }

  massert((r=strlen(pwent->pw_dir))+2<sizeof(FN3));
  memcpy(FN3,pwent->pw_dir,r);
  FN3[r]='/';
  FN3[r+1]=0;
  RETURN1(make_simple_string(FN3));
#else
  massert(snprintf(FN1,sizeof(FN1)-1,"%s%s",getenv("SystemDrive"),getenv("HOMEPATH"))>=0);
  RETURN1(make_simple_string(FN1));
#endif

}

#define FILE_EXISTS_P(a_,b_) !stat(a_,&b_) && S_ISREG(b_.st_mode)
#define DIR_EXISTS_P(a_,b_) !stat(a_,&b_) && S_ISDIR(b_.st_mode)

FILE *
fopen_not_dir(char *filename,char *option) {

  struct stat ss;

  return DIR_EXISTS_P(filename,ss) ? NULL : fopen(filename,option);

}

int
file_len(FILE *fp) {/*FIXME dir*/

  struct stat filestatus;

  return fstat(fileno(fp), &filestatus) ? 0 : filestatus.st_size;

}

bool
file_exists(object x) {

  struct stat ss;

  coerce_to_filename(x,FN1);

  return FILE_EXISTS_P(FN1,ss) ? TRUE : FALSE;

}

DEF_ORDINARY("DIRECTORY",sKdirectory,KEYWORD,"");
DEF_ORDINARY("LINK",sKlink,KEYWORD,"");
DEF_ORDINARY("FILE",sKfile,KEYWORD,"");

DEFUNM_NEW("STAT",object,fSstat,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  struct stat ss;

  if (type_of(x)==t_string) {

    coerce_to_filename(x,FN1);

#ifdef __MINGW32__
    {char *p=FN1+strlen(FN1)-1;for (;p>FN1 && *p=='/';p--) *p=0;}
#endif
    if (lstat(FN1,&ss))
      RETURN1(Cnil);
  } else if ((x=file_stream(x))!=Cnil&&x->sm.sm_fp) {
    if (fstat(fileno(x->sm.sm_fp),&ss))
      RETURN1(Cnil);
  } else
    RETURN1(Cnil);

  RETURN4(S_ISDIR(ss.st_mode) ? sKdirectory :
	  (S_ISLNK(ss.st_mode) ? sKlink : sKfile),
	  make_fixnum(ss.st_size),
	  make_fixnum(ss.st_mtime),
	  make_fixnum(ss.st_uid));

}

DEFUN_NEW("FTELL",object,fSftell,SI,1,1,NONE,IO,OO,OO,OO,(object x),"") {

  RETURN1((x=file_stream(x))!=Cnil&&x->sm.sm_fp ? (object)ftell(x->sm.sm_fp) : (object)0);

}

DEFUN_NEW("FSEEK",object,fSfseek,SI,2,2,NONE,OO,IO,OO,OO,(object x,fixnum pos),"") {

  RETURN1((x=file_stream(x))!=Cnil&&x->sm.sm_fp&&!fseek(x->sm.sm_fp,pos,SEEK_SET) ? Ct : Cnil);

}

#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>

DEFUN_NEW("READLINKAT",object,fSreadlinkat,SI,2,2,NONE,OI,OO,OO,OO,(fixnum d,object s),"") {
  ssize_t l,z1;

  check_type_string(&s);
  /* l=s->st.st_hasfillp ? s->st.st_fillp : s->st.st_dim; */
  z1=length(s);
  massert(z1<sizeof(FN1));
  memcpy(FN1,s->st.st_self,z1);
  FN1[z1]=0;
#ifndef __MINGW32__
  massert((l=readlinkat(d ? dirfd((DIR *)d) : AT_FDCWD,FN1,FN2,sizeof(FN2)))>=0 && l<sizeof(FN2));
#else
  l=0;
#endif
  FN2[l]=0;
  RETURN1(make_simple_string(FN2));

}

DEFUN_NEW("GETCWD",object,fSgetcwd,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {
  massert((getcwd(FN1,sizeof(FN1))));
  RETURN1(make_simple_string(FN1));
}

DEFUN_NEW("SETENV",object,fSsetenv,SI,2,2,NONE,OO,OO,OO,OO,(object variable,object value),"Set environment VARIABLE to VALUE")

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

DEFUN_NEW("OPENDIR",object,fSopendir,SI,1,1,NONE,IO,OO,OO,OO,(object x),"") {
  check_type_string(&x);
  coerce_to_filename(x,FN1);
  return (object)opendir(strlen(FN1) ? FN1 : "./");
}


DEFUN_NEW("D-TYPE-LIST",object,fSd_type_list,SI,0,0,NONE,OI,OO,OO,OO,(void),"") {
  RETURN1(

#ifdef HAVE_D_TYPE
	  list(8,
	       MMcons(make_fixnum(DT_BLK),make_keyword("BLOCK")),
	       MMcons(make_fixnum(DT_CHR),make_keyword("CHAR")),
	       MMcons(make_fixnum(DT_DIR),make_keyword("DIRECTORY")),
	       MMcons(make_fixnum(DT_FIFO),make_keyword("FIFO")),
	       MMcons(make_fixnum(DT_LNK),make_keyword("LINK")),
	       MMcons(make_fixnum(DT_REG),make_keyword("FILE")),
	       MMcons(make_fixnum(DT_SOCK),make_keyword("SOCKET")),
	       MMcons(make_fixnum(DT_UNKNOWN),make_keyword("UNKNOWN"))
	       )
#else
#define DT_UNKNOWN 0
#define DT_REG 1
#define DT_DIR 2
	  list(3,
	       MMcons(make_fixnum(DT_REG),make_keyword("FILE")),
	       MMcons(make_fixnum(DT_DIR),make_keyword("DIRECTORY")),
	       MMcons(make_fixnum(DT_UNKNOWN),make_keyword("UNKNOWN"))
	       )
#endif
	  );
}



DEFUN_NEW("READDIR",object,fSreaddir,SI,3,3,NONE,OI,IO,OO,OO,(fixnum x,fixnum y,object s),"") {

  struct dirent *e;
  object z;
  long tl;
  size_t l;
  long d_type=DT_UNKNOWN;
#ifdef HAVE_D_TYPE
#define get_d_type(e,s) e->d_type
#else
#define get_d_type(e,s) \
  ({struct stat ss;\
    massert(snprintf(FN1,sizeof(FN1),"%-*.*s%s",s->st.st_fillp,s->st.st_fillp,s->st.st_self,e->d_name)>=0);\
    lstat(FN1,&ss);S_ISDIR(ss.st_mode) ? DT_DIR : DT_REG;})
#endif

  if (!x) RETURN1(Cnil);

  tl=telldir((DIR *)x);

  for (;(e=readdir((DIR *)x)) && y!=DT_UNKNOWN && y!=(d_type=get_d_type(e,s)););
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

  if (y==DT_UNKNOWN) z=MMcons(z,make_fixnum(d_type));

  RETURN1(z);

}

DEFUN_NEW("CLOSEDIR",object,fSclosedir,SI,1,1,NONE,OI,OO,OO,OO,(fixnum x),"") {
  closedir((DIR *)x);
  return Cnil;
}

DEFUN_NEW("RENAME",object,fSrename,SI,2,2,NONE,OO,OO,OO,OO,(object x,object y),"") {

  check_type_string(&x);
  check_type_string(&y);

  coerce_to_filename(x,FN1);
  coerce_to_filename(y,FN2);

  RETURN1(rename(FN1,FN2) ? Cnil : Ct);

}

DEFUN_NEW("UNLINK",object,fSunlink,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  check_type_string(&x);

  coerce_to_filename(x,FN1);

  RETURN1(unlink(FN1) ? Cnil : Ct);

}


DEFUN_NEW("CHDIR1",object,fSchdir1,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  check_type_string(&x);

  coerce_to_filename(x,FN1);

  RETURN1(chdir(FN1) ? Cnil : Ct);

}

DEFUN_NEW("MKDIR",object,fSmkdir,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  check_type_string(&x);

  coerce_to_filename(x,FN1);

  RETURN1(mkdir(FN1
#ifndef __MINGW32__
		,01777
#endif
		) ? Cnil : Ct);

}

DEFUN_NEW("RMDIR",object,fSrmdir,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
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

/* export these for AXIOM */
int gcl_putenv(char *s) {return putenv(s);}
char *gcl_strncpy(char *d,const char *s,size_t z) {return strncpy(d,s,z);}
char *gcl_strncpy_chk(size_t z) {char a[10],b[10];return strncpy(a,b,z);}/*compile in __strncpy_chk with FORTIFY_SOURCE*/
#ifdef __MINGW32__
#define uid_t int
#endif
uid_t gcl_geteuid(void) {
#ifndef __MINGW32__
  return geteuid();
#else
  return 0;
#endif
}
uid_t gcl_getegid(void) {
#ifndef __MINGW32__
  return getegid();
#else
  return 0;
#endif
}
int gcl_dup2(int o,int n) {return dup2(o,n);}
char *gcl_gets(char *s,int z) {return fgets(s,z,stdin);}
int gcl_puts(const char *s) {int i=fputs(s,stdout);fflush(stdout);return i;}

int gcl_feof(void *v) {return feof(((FILE *)v));}
int gcl_getc(void *v) {return getc(((FILE *)v));}
int gcl_putc(int i,void *v) {return putc(i,((FILE *)v));}

void
gcl_init_unixfsys(void) {
}
