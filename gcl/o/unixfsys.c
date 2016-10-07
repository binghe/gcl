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

#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

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

#ifdef BSD
#define HAVE_RENAME
#endif

#ifdef NEED_GETWD
#include <sys/dir.h>


#ifndef HAVE_GETCWD
char dotdot[3*16+2] = "../../../../../../../../../../../../../../../../.";
#include <mnttab.h>
static char *getwd_buf;
static int getwd_bufp;

static char *
getwd(buffer)
char *buffer;
{
	getwd_buf = buffer;
	getwd1(0);
	if (getwd_bufp == 0)
		getwd_buf[getwd_bufp++] = '/';
	getwd_buf[getwd_bufp] = '\0';
	return(getwd_buf);
}

getwd1(n)
int n;
{
	struct stat st, dev_st;
	struct direct dir;
	ino_t ino;
	struct mnttab mnt;
	FILE *fp;
	register int i;
	char buf[BUFSIZ];
	static char dev_name[64];

	if (stat(dotdot+(16-n)*3, &st) < 0)
		FEerror("Can't get the current working directory.", 0);
	ino = st.st_ino;
	if (ino == 2)
		goto ROOT;
	getwd1(n+1);
	fp = fopen(dotdot+(16-n-1)*3, "r");
	if (fp == NULL)
		FEerror("Can't get the current working directory.", 0);
	setbuf(fp, buf);
	fread(&dir, sizeof(struct direct), 1, fp);
	fread(&dir, sizeof(struct direct), 1, fp);
	for (;;) {
		if (fread(&dir, sizeof(struct direct), 1, fp) <= 0)
			break;
		if (dir.d_ino == ino)
			goto FOUND;
	}
	fclose(fp);
	FEerror("Can't get the current working directory.", 0);

FOUND:
	fclose(fp);
	getwd_buf[getwd_bufp++] = '/';
	for (i = 0;  i < DIRSIZ && dir.d_name[i] != '\0';  i++)
		getwd_buf[getwd_bufp++] = dir.d_name[i];
	return;

ROOT:
	fp = fopen("/etc/mnttab", "r");
	if (fp == NULL)
		FEerror("Can't get the current working directory.", 0);
	setbuf(fp, buf);
	for (;;) {
		if (fread(&mnt, sizeof(struct mnttab), 1, fp) <= 0)
			break;
		if (mnt.mt_dev[0] != '/') {
			strcpy(dev_name, "/dev/dsk/");
			strcat(dev_name, mnt.mt_dev);
			stat(dev_name, &dev_st);
		} else
			stat(mnt.mt_dev, &dev_st);
		if (dev_st.st_rdev == st.st_dev)
			goto DEV_FOUND;
	}
	fclose(fp);
	getwd_bufp = 0;
	return;

DEV_FOUND:
	fclose(fp);
	getwd_bufp = 0;
	for (i = 0;  mnt.mt_filsys[i] != '\0';  i++)
		getwd_buf[i] = mnt.mt_filsys[i];
	/* BUG FIX by Grant J. Munsey */
	if (i == 1 && *getwd_buf == '/')
		i = 0;	/* don't add an empty directory name */
	/* END OF BUG FIX */
	getwd_bufp = i;
}
#endif   /* not HAVE_GETCWD */
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 512
#endif


#ifdef HAVE_GETCWD
char *
getwd(char *buffer) {
#ifndef _WIN32    
  char *getcwd(char *, size_t);
#endif
  return(getcwd(buffer, MAXPATHLEN));
}
#endif


#define pcopy(a_,b_,c_,d_) ({\
      unsigned _c=c_,_d=d_;\
      if (_c+_d>=MAXPATHLEN-16) FEerror("Can't expand pathname ~a",1,namestring);\
      bcopy(a_,b_+_c,_d);\
      b_[_c+_d]=0;\
      })

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
    case smm_synonym:
      return get_string(x->sm.sm_object0->s.s_dbind);
    }
  }
  return Cnil;
}


void
coerce_to_filename(object pathname,char *p) {

  object namestring=get_string(pathname);
  unsigned e=namestring->st.st_fillp;
  char *q=namestring->st.st_self,*qe=q+e;

  if (pathname==Cnil||namestring==Cnil)
    FEerror ( "NIL argument.", 1, pathname ); 
  
  if (*q=='~' && e) {

    unsigned m=0;
    char *s=++q,*c;

    for (;s<qe && *s!='/';s++);

    if (s==q && (c=getenv("HOME")))

      pcopy(c,p,0,m=strlen(c));

#if !defined(NO_PWD_H) && !defined(STATIC_LINKING)
    else {
#ifndef __STDC__
      extern struct passwd *getpwuid();
      extern struct passwd *getpwnam();
#endif
      struct passwd *pwent;
      
      if (s==q)
	pwent=getpwuid(getuid());
      else {
	*s=0;
	pwent=getpwnam(q);
	*s='/';
      }
      
      if (!pwent)
	FEerror("Can't expand pathname ~a",1,namestring);
      pcopy(pwent->pw_dir,p,0,m=strlen(pwent->pw_dir));
      
    }
#endif

    pcopy(s,p,m,qe-s);
    
  } else

    pcopy(q,p,0,e);
  
#ifdef FIX_FILENAME
  FIX_FILENAME(pathname,p);
#endif
    
}

object sSAallow_gzipped_fileA;

bool
file_exists(object file)
{
	char filename[MAXPATHLEN];
	struct stat filestatus;

	coerce_to_filename(file, filename);

#ifdef __MINGW32__
        {
            char *p;
            for (p = filename;  *p != '\0';  p++);
            if ( (p > filename) &&
                 ( ( *(p-1) == '/' ) || ( *(p-1) == '\\' ) ) ) {
               *(p-1) = '\0'; 
            }
        }
#endif        

	if (stat(filename, &filestatus) >= 0 && !S_ISDIR(filestatus.st_mode))
	  {
#ifdef AIX
	    /* if /tmp/foo is not a directory /tmp/foo/ should not exist */
	    if (filename[strlen(filename)-1] == '/' &&
		!( filestatus.st_mode & S_IFDIR))
		return(FALSE);
#endif	    

	    return TRUE;
	  }
	else
	  if (sSAallow_gzipped_fileA->s.s_dbind != sLnil
	      && (strcat(filename,".gz"),
		  stat(filename, &filestatus) >= 0 && !S_ISDIR(filestatus.st_mode)))
	      
	      return TRUE;

	else
		return(FALSE);
}

FILE *
fopen_not_dir(char *filename,char * option) {

  struct stat ss;

  if (!stat(filename,&ss) && S_ISDIR(ss.st_mode))
    return NULL;
  else
    return fopen(filename,option);

}

FILE *
backup_fopen(char *filename, char *option)
{
	char backupfilename[MAXPATHLEN];
	char command[MAXPATHLEN * 2];

	strcat(strcpy(backupfilename, filename), ".BAK");
	sprintf(command, "mv %s %s", filename, backupfilename);
	msystem(command);
	return(fopen(filename, option));
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



DEFUNM_NEW("STAT",object,fSstat,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

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
  DIR *d;
  char filename[MAXPATHLEN];
  check_type_string(&x);
  memcpy(filename,x->st.st_self,x->st.st_fillp);
  filename[x->st.st_fillp]=0;
  d=opendir(filename);
  return (object)d;
}

#ifdef HAVE_D_TYPE
  
DEFUN_NEW("D-TYPE-LIST",object,fSd_type_list,SI,0,0,NONE,OI,OO,OO,OO,(void),"") {
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

DEFUN_NEW("READDIR",object,fSreaddir,SI,3,3,NONE,OI,IO,OO,OO,(fixnum x,fixnum y,object s),"") {
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

DEFUN_NEW("CLOSEDIR",object,fSclosedir,SI,1,1,NONE,OI,OO,OO,OO,(fixnum x),"") {
  closedir((DIR *)x);
  return Cnil;
}

DEFUN_NEW("MKDIR",object,fSmkdir,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  char filename[MAXPATHLEN];

  check_type_string(&x);

  memcpy(filename,x->st.st_self,x->st.st_fillp);
  filename[x->st.st_fillp]=0;

#ifdef __MINGW32__
  if (mkdir(filename) < 0)
#else        
  if (mkdir(filename,01777) < 0)
#endif        
    FEerror("Cannot make the directory ~S.", 1, vs_base[0]);

  RETURN1(x);

}

#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>

DEFUN_NEW("READLINKAT",object,fSreadlinkat,SI,2,2,NONE,OI,OO,OO,OO,(fixnum d,object s),"") {
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

DEFUN_NEW("GETCWD",object,fSgetcwd,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {
  char *b=NULL;
  size_t z;
  object s;

  for (z=0;!(errno=0) && !getcwd(b,z) && errno==ERANGE;b=memset(b,0,z),z+=z+10,({massert((b=alloca(z)));}));
  massert((b=getcwd(b,z)));
  s=make_simple_string(b);
  memset(b,0,z);
  RETURN1(s);

}

DEFUN_NEW("HOME-NAMESTRING",object,fShome_namestring,SI,1,1,NONE,OO,OO,OO,OO,(object nm),"") {

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

DEFUN_NEW("RENAME",object,fSrename,SI,2,2,NONE,OO,OO,OO,OO,(object x,object y),"") {

  check_type_string(&x);
  check_type_string(&y);

  coerce_to_filename(x,FN1);
  coerce_to_filename(y,FN2);

  RETURN1(rename(FN1,FN2) ? Cnil : Ct);

}

DEFUN_NEW("UNLINK",object,fSunlink,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {

  coerce_to_filename(x,FN1);

  RETURN1(unlink(FN1) ? Cnil : Ct);

}


static void
FFN(siLchdir)(void)
{
	char filename[MAXPATHLEN];

	check_arg(1);
	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	coerce_to_filename(vs_base[0], filename);

	if (chdir(filename) < 0)
		FEerror("Can't change the current directory to ~S.",
			1, vs_base[0]);
}

void
gcl_init_unixfsys(void) {

  make_si_function("CHDIR", siLchdir);

}
