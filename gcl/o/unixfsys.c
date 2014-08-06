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

void Ldirectory(void);



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

void
coerce_to_filename(object pathname,char *p) {

  object namestring=coerce_to_namestring(pathname);
  unsigned e=namestring->st.st_fillp;
  char *q=namestring->st.st_self,*qe=q+e;;

  if (pathname==Cnil)
    FEerror ( "NIL argument.", 1, pathname ); 
  
  if (*q=='~') {

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

object
truename(object pathname)
{
	register char *p, *q;
	char filename[MAXPATHLEN];
	char truefilename[MAXPATHLEN];
	char current_directory[MAXPATHLEN];
	char directory[MAXPATHLEN];
#ifdef __MINGW32__ 
        DWORD current_directory_length =
            GetCurrentDirectory ( MAXPATHLEN, current_directory ); 
        if ( MAXPATHLEN < current_directory_length ) { 
           FEerror ( "truename got a current directory name larger than MAXPATHLEN", 1, "" ); 
        } 
        if ( 0 == current_directory_length ) { 
           FEerror ( "truename could not determine the current directory.", 1, "" ); 
        } 
#else 
        massert(current_directory==getcwd(current_directory,sizeof(current_directory))); 
#endif 
    
	coerce_to_filename(pathname, filename);
	
#ifdef S_IFLNK
 {

   struct stat filestatus;
   int islinkcount=8;

   if (lstat(filename, &filestatus) >= 0)

	while (((filestatus.st_mode&S_IFMT) == S_IFLNK) && (--islinkcount>0)) {

	  char newname[MAXPATHLEN];
	  int newlen;

	  newlen=readlink(filename,newname,MAXPATHLEN-1);
	  if (newlen < 0)
	    return((FEerror("Symlink broken at ~S.",1,pathname),Cnil));

	  for (p = filename, q = 0;  *p != '\0';  p++)
	    if (*p == '/') q = p;
	  if (q == 0 || *newname == '/')
	    q = filename;
	  else
	    q++;

	  memcpy(q,newname,newlen);
	  q[newlen]=0;
	  if (lstat(filename, &filestatus) < 0) 
	    islinkcount=0; /* It would be ANSI to do the following :
			      return(file_error("Symlink broken at ~S.",pathname));
			      but this would break DIRECTORY if a file points to nowhere */
	}
 }
#endif

	for (p = filename, q = 0;  *p != '\0';  p++)
		if (*p == '/')
			q = p;
	if (q == filename) {
		q++;
		p = "/";
	} else if (q == 0) {
		q = filename;
		p = current_directory;
	} else
#ifdef __MINGW32__
	   if ( ( q > filename ) && ( q[-1] == ':' ) ) {
	     int current = (q++, q[0]);
	     q[0]=0;
	     if (chdir(filename) < 0)
	       FEerror("Cannot get the truename of ~S.", 1, pathname);
             current_directory_length =
               GetCurrentDirectory ( MAXPATHLEN, directory );
             if ( MAXPATHLEN < current_directory_length ) { 
               FEerror ( "truename got a current directory name larger than MAXPATHLEN", 1, "" ); 
             } 
             if ( 0 == current_directory_length ) { 
               FEerror ( "truename could not determine the current directory.", 1, "" ); 
             } 
             p = directory; 
             if ( p[1]==':' && ( p[2]=='\\' || p[2]=='/' ) && p[3]==0 ) p[2]=0; 
	     q[0]=current;
          }
	  else
#endif	
	  {
		*q++ = '\0';
		if (chdir(filename) < 0)
		    FEerror("Cannot get the truename of ~S.", 1, pathname);
#ifdef __MINGW32__ 
                current_directory_length = GetCurrentDirectory ( MAXPATHLEN, directory ); 
                if ( MAXPATHLEN < current_directory_length ) { 
                    FEerror ( "truename got a current directory name larger than MAXPATHLEN", 1, "" ); 
                } 
                if ( 0 == current_directory_length ) { 
                    FEerror ( "truename could not determine the current directory.", 1, "" ); 
                } 
                p = directory; 
#else 
		p = getcwd(directory,sizeof(directory));
#endif                
	}
	if (p[0] == '/' && p[1] == '\0') {
		if (strcmp(q, "..") == 0)
			strcpy(truefilename, "/.");
		else
			sprintf(truefilename, "/%s", q);
	} else if (strcmp(q, ".") == 0)
		strcpy(truefilename, p);
	else if (strcmp(q, "..") == 0) {
		for (q = p + strlen(p);  *--q != '/';) ;
		if (p == q)
			strcpy(truefilename, "/.");
		else {
			*q = '\0';
			strcpy(truefilename, p);
			*q = '/';
		}
	} else
		sprintf(truefilename, "%s/%s", p, q);
	massert(!chdir(current_directory));
	vs_push(make_simple_string(truefilename));
	pathname = coerce_to_pathname(vs_head);
	vs_popp;
	return(pathname);
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

LFD(Ltruename)(void)
{
	check_arg(1);
	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	vs_base[0] = truename(vs_base[0]);
}

LFD(Lrename_file)(void)
{
	char filename[MAXPATHLEN];
	char newfilename[MAXPATHLEN];

	check_arg(2);
	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	check_type_or_Pathname_string_symbol(&vs_base[1]);
	coerce_to_filename(vs_base[0], filename);
	vs_base[0] = coerce_to_pathname(vs_base[0]);
	vs_base[1] = coerce_to_pathname(vs_base[1]);
	vs_base[1] = merge_pathnames(vs_base[1], vs_base[0], Cnil);
	coerce_to_filename(vs_base[1], newfilename);
#ifdef HAVE_RENAME
	if (rename(filename, newfilename) < 0)
		FEerror("Cannot rename the file ~S to ~S.",
			2, vs_base[0], vs_base[1]);
#else
	sprintf(command, "mv %s %s", filename, newfilename);
	msystem(command);
#endif
	vs_push(vs_base[1]);
	vs_push(truename(vs_base[0]));
	vs_push(truename(vs_base[1]));
	vs_base += 2;
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



DEFUN_NEW("STAT",object,fSstat,SI,1,1,NONE,OO,OO,OO,OO,(object path),"") {

  char filename[4096];
  struct stat ss;
  

  bzero(filename,sizeof(filename));
  coerce_to_filename(path,filename);
#ifdef __MINGW32__
  {
    char *p=filename+strlen(filename)-1;
    for (;p>filename && *p=='/';p--)
      *p=0;
  }
#endif
  if (lstat(filename,&ss))
    RETURN1(Cnil);
  else {/* ctime_r insufficiently portable */
    /* int j;
       ctime_r(&ss.st_ctime,filename);
       j=strlen(filename);
       if (isspace(filename[j-1]))
       filename[j-1]=0;*/
    RETURN1(list(3,S_ISDIR(ss.st_mode) ? sKdirectory : 
		 (S_ISLNK(ss.st_mode) ? sKlink : sKfile),
		 make_fixnum(ss.st_size),make_fixnum(ss.st_ctime)));
  }
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

DEFUNO_NEW("DELETE-FILE",object,fLdelete_file,LISP
   ,1,1,NONE,OO,OO,OO,OO,void,Ldelete_file,(object path),"")

{
	char filename[MAXPATHLEN];

	/* 1 args */
	check_type_or_pathname_string_symbol_stream(&path);
	coerce_to_filename(path, filename);
	if (unlink(filename) < 0 && rmdir(filename) < 0)
		FEerror("Cannot delete the file ~S: ~s.", 2, path, make_simple_string(strerror(errno)));
	path = Ct;
	RETURN1(path);
}
#ifdef STATIC_FUNCTION_POINTERS
object
fLdelete_file(object path) {
  return FFN(fLdelete_file)(path);
}
#endif

LFD(Lprobe_file)(void)
{
	check_arg(1);

	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	if (file_exists(vs_base[0]))
		vs_base[0] = truename(vs_base[0]);
	else
		vs_base[0] = Cnil;
}

LFD(Lfile_write_date)(void)
{
	char filename[MAXPATHLEN];
	struct stat filestatus;

	check_arg(1);
	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	coerce_to_filename(vs_base[0], filename);
	if (stat(filename, &filestatus) < 0 || S_ISDIR(filestatus.st_mode))
	  { vs_base[0] = Cnil; return;}
	vs_base[0] = unix_time_to_universal_time(filestatus.st_mtime);
}

LFD(Lfile_author)(void)
{
#if !defined(NO_PWD_H) && !defined(STATIC_LINKING)
	char filename[MAXPATHLEN];
	struct stat filestatus;
	struct passwd *pwent;
#ifndef __STDC__
	extern struct passwd *getpwuid();
#endif

	check_arg(1);
	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	coerce_to_filename(vs_base[0], filename);
	if (stat(filename, &filestatus) < 0 || S_ISDIR(filestatus.st_mode))
	  { vs_base[0] = Cnil; return;}
	pwent = getpwuid(filestatus.st_uid);
	vs_base[0] = make_simple_string(pwent->pw_name);
#else
	vs_base[0] = Cnil; return;
#endif	
	
}

static void
FFN(Luser_homedir_pathname)(void)
{

  char filename[MAXPATHLEN];

  coerce_to_filename(make_simple_string("~/"),filename);
  vs_base[0]=coerce_to_pathname(make_simple_string(filename));
  vs_top = vs_base+1; 
  
}


#ifdef BSD
LFD(Ldirectory)(void)
{
	char filename[MAXPATHLEN];
	char command[MAXPATHLEN * 2];
	FILE *fp;
	register int i, c;
	object *top = vs_top;
	char iobuffer[BUFSIZ];
	extern FILE *popen(const char *, const char *);

	check_arg(1);

	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	vs_base[0] = coerce_to_pathname(vs_base[0]);
	if (vs_base[0]->pn.pn_name==Cnil && vs_base[0]->pn.pn_type==Cnil) {
		coerce_to_filename(vs_base[0], filename);
		strcat(filename, "*");
	} else if (vs_base[0]->pn.pn_name==Cnil) {
		vs_base[0]->pn.pn_name = sKwild;
		coerce_to_filename(vs_base[0], filename);
		vs_base[0]->pn.pn_name = Cnil;
	} else if (vs_base[0]->pn.pn_type==Cnil) {
		coerce_to_filename(vs_base[0], filename);
		strcat(filename, "*");
	} else
		coerce_to_filename(vs_base[0], filename);
	sprintf(command, "ls -d %s 2> /dev/null", filename);
	fp = popen(command, "r");
	setbuf(fp, iobuffer);
	for (;;) {
		for (i = 0;  (c = getc(fp));  i++)
			if (c <= 0)
				goto L;
			else if (c == '\n')
				break;
			else
				filename[i] = c;
		filename[i] = '\0';
		vs_push(make_simple_string(filename));
		vs_head = truename(vs_head);
	}
L:
	pclose(fp);
	vs_push(Cnil);
	while (vs_top > top + 1)
		stack_cons();
	vs_base = top;
}
#endif


#ifdef ATT
LFD(Ldirectory)()
{
	object name, type;
	char filename[MAXPATHLEN];
	FILE *fp;
	object *top = vs_top;
	char iobuffer[BUFSIZ];
	struct direct dir;
	int i;

	check_arg(1);

	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	vs_base[0] = coerce_to_pathname(vs_base[0]);
	vs_push(vs_base[0]->pn.pn_name);
	vs_push(vs_base[0]->pn.pn_type);
	vs_base[0]->pn.pn_name = Cnil;
	vs_base[0]->pn.pn_type = Cnil;
	coerce_to_filename(vs_base[0], filename);
	type = vs_base[0]->pn.pn_type = vs_pop;
	name = vs_base[0]->pn.pn_name = vs_pop;
	i = strlen(filename);
	if (i > 1 && filename[i-1] == '/')
		filename[i-1] = '\0';
	if (i == 0)
		strcpy(filename, ".");
	fp = fopen(filename, "r");
	if (fp == NULL) {
		vs_push(make_simple_string(filename));
		FEerror("Can't open the directory ~S.", 1, vs_head);
	}
	setbuf(fp, iobuffer);
	fread(&dir, sizeof(struct direct), 1, fp);
	fread(&dir, sizeof(struct direct), 1, fp);
	filename[DIRSIZ] = '\0';
	for (;;) {
		if (fread(&dir, sizeof(struct direct), 1, fp) <=0)
			break;
		if (dir.d_ino == 0)
			continue;
		strncpy(filename, dir.d_name, DIRSIZ);
		vs_push(make_simple_string(filename));
		vs_head = coerce_to_pathname(vs_head);
		if ((name == Cnil || name == sKwild ||
		     equal(name, vs_head->pn.pn_name)) &&
		    (type == Cnil || type == sKwild ||
		     equal(type, vs_head->pn.pn_type))) {
			vs_head->pn.pn_directory
			= vs_base[0]->pn.pn_directory;
			vs_head = truename(vs_head);
		} else
			vs_pop;
	}
	fclose(fp);
	vs_push(Cnil);
	while (vs_top > top + 1)
		stack_cons();
	vs_base = top;
}
#endif


#ifdef E15
#include <sys/dir.h>

LFD(Ldirectory)()
{
	object name, type;
	char filename[MAXPATHLEN];
	FILE *fp;
	object *top = vs_top;
	char iobuffer[BUFSIZ];
	struct direct dir;
	int i;

	check_arg(1);

	check_type_or_pathname_string_symbol_stream(&vs_base[0]);
	vs_base[0] = coerce_to_pathname(vs_base[0]);
	vs_push(vs_base[0]->pn.pn_name);
	vs_push(vs_base[0]->pn.pn_type);
	vs_base[0]->pn.pn_name = Cnil;
	vs_base[0]->pn.pn_type = Cnil;
	coerce_to_filename(vs_base[0], filename);
	type = vs_base[0]->pn.pn_type = vs_pop;
	name = vs_base[0]->pn.pn_name = vs_pop;
	i = strlen(filename);
	if (i > 1 && filename[i-1] == '/')
		filename[i-1] = '\0';
	if (i == 0)
		strcpy(filename, ".");
	fp = fopen(filename, "r");
	if (fp == NULL) {
		vs_push(make_simple_string(filename));
		FEerror("Can't open the directory ~S.", 1, vs_head);
	}
	setbuf(fp, iobuffer);
	fread(&dir, sizeof(struct direct), 1, fp);
	fread(&dir, sizeof(struct direct), 1, fp);
	filename[DIRSIZ] = '\0';
	for (;;) {
		if (fread(&dir, sizeof(struct direct), 1, fp) <=0)
			break;
		if (dir.d_ino == 0)
			continue;
		strncpy(filename, dir.d_name, DIRSIZ);
		vs_push(make_simple_string(filename));
		vs_head = coerce_to_pathname(vs_head);
		if ((name == Cnil || name == sKwild ||
		     equal(name, vs_head->pn.pn_name)) &&
		    (type == Cnil || type == sKwild ||
		     equal(type, vs_head->pn.pn_type))) {
			vs_head->pn.pn_directory
			= vs_base[0]->pn.pn_directory;
			vs_head = truename(vs_head);
		} else
			vs_pop;
	}
	fclose(fp);
	vs_push(Cnil);
	while (vs_top > top + 1)
		stack_cons();
	vs_base = top;
}
#endif

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

DEFUN_NEW("READDIR",object,fSreaddir,SI,2,2,NONE,OI,IO,OO,OO,(fixnum x,fixnum y),"") {
  struct dirent *e;
  object z;
  if (!x) RETURN1(Cnil);
  e=readdir((DIR *)x);
  RETURN1(e ? make_simple_string(e->d_name) : Cnil);
#ifdef HAVE_D_TYPE
  for (;(e=readdir((DIR *)x)) && y!=DT_UNKNOWN && e->d_type!=y;);
#endif
  if (!e) RETURN1(Cnil);
  z=make_simple_string(e->d_name);
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
gcl_init_unixfsys(void)
{
	make_function("TRUENAME", Ltruename);
	make_function("RENAME-FILE", Lrename_file);
	make_function("DELETE-FILE", Ldelete_file);
	make_function("PROBE-FILE", Lprobe_file);
	make_function("FILE-WRITE-DATE", Lfile_write_date);
	make_function("FILE-AUTHOR", Lfile_author);
	make_function("USER-HOMEDIR-PATHNAME", Luser_homedir_pathname);
	make_function("DIRECTORY", Ldirectory);

	make_si_function("CHDIR", siLchdir);
}
