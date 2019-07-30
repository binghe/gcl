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

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifndef __MINGW32__
#include <sys/wait.h>
#endif

#include "include.h"

#ifndef __MINGW32__

int
vsystem(const char *command) {

  unsigned j,n=strlen(command)+1;
  char *z,*c;
  const char *x1[]={"/bin/sh","-c",NULL,NULL},*spc=" \n\t",**p1,**pp;
  int s;
  pid_t pid;

  if (strpbrk(command,"\"'$<>"))

    (p1=x1)[2]=command;

  else {

    massert(n<sizeof(FN1));
    memcpy((z=FN1),command,n);
    for (j=1,c=z;strtok(c,spc);c=NULL,j++);

    memcpy(z,command,n);
    massert(j*sizeof(*p1)<sizeof(FN2));
    p1=(void *)FN2;
    for (pp=p1,c=z;(*pp=strtok(c,spc));c=NULL,pp++);

  }

  if (!(pid=vfork())) {
    errno=0;
    execvp(*p1,(void *)p1);
    _exit(128|(errno&0x7f));
  }

  massert(pid>0);
  massert(pid==waitpid(pid,&s,0));

  if ((s>>8)&128)
    emsg("execvp failure when executing '%s': %s\n",command,strerror((s>>8)&0x7f));

  return s;

}
#endif


#ifdef ATT3B2
#include <signal.h>
int
system(command)
char *command;
{
	char buf[4];
	extern sigint();

	signal(SIGINT, SIG_IGN);
	write(4, command, strlen(command)+1);
	read(5, buf, 1);
	signal(SIGINT, sigint);
	return(buf[0]<<8);
}
#endif

#ifdef E15
#include <signal.h>
int
system(command)
char *command;
{
	char buf[4];
	extern sigint();

	signal(SIGINT, SIG_IGN);
	write(4, command, strlen(command)+1);
	read(5, buf, 1);
	signal(SIGINT, sigint);
	return(buf[0]<<8);
}
#endif

int
msystem(const char *s) {

  return psystem(s);

}

static void
FFN(siLsystem)(void)
{
	char command[32768];
	int i;

	check_arg(1);
	check_type_string(&vs_base[0]);
	if (VLEN(vs_base[0]) >= 32768)
		FEerror("Too long command line: ~S.", 1, vs_base[0]);
	for (i = 0;  i < VLEN(vs_base[0]);  i++)
		command[i] = vs_base[0]->st.st_self[i];
	command[i] = '\0';
	{int old = signals_allowed;
	 int res;
	 signals_allowed = sig_at_read;
	 res = msystem(command) ;
	 signals_allowed = old;
	 vs_base[0] = make_fixnum(res >> 8);
	 vs_base[1] = make_fixnum((res & 0xff));
	 vs_top++;
       }
}

DEFUN("GETPID",object,fSgetpid,SI,0,0,NONE,OO,OO,OO,OO,(void),
      "getpid  returns  the  process  ID  of the current process") {
  return make_fixnum(getpid());
}


void
gcl_init_unixsys(void)
{
	make_si_function("SYSTEM", siLsystem);
}
