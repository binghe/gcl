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


#include <dlfcn.h>
#include <sys/param.h>
#include <time.h>
#ifdef HAVE_AOUT
#include HAVE_AOUT
#endif
#if defined(HAVE_ELF_H)
#include <elf.h>
#elif defined(HAVE_ELF_ABI_H)
#include <elf_abi.h>
#endif

#include "ptable.h"

struct name_list { 
  struct name_list *next;
  char name[1];
};
static struct name_list *loaded_files;

static void
unlink_loaded_files(void) { 

  while(loaded_files) { 
    unlink(loaded_files->name);
    loaded_files= loaded_files->next;
  }

}

int
fasload(object faslfile) {

  void *dlp ;
  int (*fptr)();
  static int count;
  object memory,data,faslstream;
  struct name_list *nl;
  object x;


  coerce_to_filename(faslfile, FN1);
  if (!count)
    count=time(0);
  massert(snprintf(FN2,sizeof(FN2),"/tmp/ufas%dxXXXXXX",count++)>0);
  massert(mkstemp(FN2)>=0);

  massert((nl=(void *) malloc(strlen(FN2)+1+sizeof(nl))));
  massert(loaded_files || !atexit(unlink_loaded_files));
  nl->next = loaded_files;
  loaded_files = nl;
  strcpy(nl->name,FN2);

  /* faslstream = open_stream(faslfile, smm_input, Cnil, sKerror); */
  massert(snprintf(FN3,sizeof(FN3),"cc -shared %s -o %s",FN1,FN2)>0);
  massert(!psystem(FN3));

  if (!(dlp = dlopen(FN2,RTLD_NOW))) {
    emsg(dlerror());
    FEerror("Cannot open for dynamic link ~a",1,make_simple_string(faslfile));
  }
  

  x=find_init_name1(FN2,0);
  coerce_to_filename(x,FN3);
  if (!(fptr=dlsym(dlp,FN3))) {
    fputs(dlerror(),stderr);
    FEerror("Cannot lookup ~a in ~a",2,make_simple_string(b),make_simple_string(faslfile));
  }

  faslstream = open_stream(faslfile, smm_input, Cnil, sKerror);
  SEEK_TO_END_OFILE(faslstream->sm.sm_fp);

  data = read_fasl_vector(faslstream);
  memory=new_cfdata();
  memory->cfd.cfd_name=faslfile->sm.sm_object1;

  if(symbol_value(sLAload_verboseA)!=Cnil) {
    printf(" ;; start address (dynamic) %p ",fptr);
    fflush(stdout);
  }

  call_init(0,memory,data,fptr);

  unlink(FN2);
  close_stream(faslstream);

  return memory->cfd.cfd_size;

}

#include "sfasli.c"
