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
	pathname.d
	IMPLEMENTATION-DEPENTENT

	This file contains those functions that interpret namestrings.
*/

#include <string.h>
#include "include.h"

DEFUN_NEW("C-SET-T-TT",object,fSc_set_t_tt,SI,2,2,NONE,OO,IO,OO,OO,(object x,fixnum y),"") {
  x->d.tt=y;
  RETURN1(x);
}


DEFUN_NEW("C-T-TT",object,fSc_t_tt,SI,1,1,NONE,IO,OO,OO,OO,(object x),"") {
  RETURN1((object)(fixnum)x->d.tt);
}


DEFUN_NEW("C-SET-PATHNAME-NAMESTRING",object,fSc_set_pathname_namestring,SI,2,2,NONE,OO,OO,OO,OO,(object x,object y),"") {
  check_type_pathname(&x);
  x->pn.pn_namestring=y;
  RETURN1(x);
}

DEFUN_NEW("C-PATHNAME-HOST",object,fSc_pathname_host,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  check_type_pathname(&x);
  RETURN1(x->pn.pn_host);
}
DEFUN_NEW("C-PATHNAME-DEVICE",object,fSc_pathname_device,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  check_type_pathname(&x);
  RETURN1(x->pn.pn_device);
}
DEFUN_NEW("C-PATHNAME-DIRECTORY",object,fSc_pathname_directory,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  check_type_pathname(&x);
  RETURN1(x->pn.pn_directory);
}
DEFUN_NEW("C-PATHNAME-NAME",object,fSc_pathname_name,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  check_type_pathname(&x);
  RETURN1(x->pn.pn_name);
}
DEFUN_NEW("C-PATHNAME-TYPE",object,fSc_pathname_type,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  check_type_pathname(&x);
  RETURN1(x->pn.pn_type);
}
DEFUN_NEW("C-PATHNAME-VERSION",object,fSc_pathname_version,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  check_type_pathname(&x);
  RETURN1(x->pn.pn_version);
}
DEFUN_NEW("C-PATHNAME-NAMESTRING",object,fSc_pathname_namestring,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  check_type_pathname(&x);
  RETURN1(x->pn.pn_namestring);
}


DEFUN_NEW("C-STREAM-OBJECT0",object,fSc_stream_object0,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  RETURN1(x->sm.sm_object0);
}

DEFUN_NEW("C-STREAM-OBJECT1",object,fSc_stream_object1,SI,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  RETURN1(x->sm.sm_object1);
}

DEFUN_NEW("C-SET-STREAM-OBJECT0",object,fSc_set_stream_object0,SI,2,2,NONE,OO,OO,OO,OO,(object x,object y),"") {
  x->sm.sm_object0=y;
  RETURN1(x);
}

DEFUN_NEW("C-SET-STREAM-OBJECT1",object,fSc_set_stream_object1,SI,2,2,NONE,OO,OO,OO,OO,(object x,object y),"") {
  x->sm.sm_object1=y;
  RETURN1(x);
}

DEFUN_NEW("INIT-PATHNAME",object,fSinit_pathname,SI,7,7,NONE,OO,OO,OO,OO,
      (object host,object device,object directory,object name,object type,object version,object namestring),"") {

  object x=alloc_object(t_pathname);

  x->pn.pn_host=host;
  x->pn.pn_device=device;
  x->pn.pn_directory=directory;
  x->pn.pn_name=name;
  x->pn.pn_type=type;
  x->pn.pn_version=version;
  x->pn.pn_namestring=namestring;

  RETURN1(x);

}

DEFUN_NEW("PATHNAMEP",object,fLpathnamep,LISP,1,1,NONE,OO,OO,OO,OO,(object x),"") {
  RETURN1(type_of(x)==t_pathname ? Ct : Cnil);
}

void
gcl_init_pathname(void) {

}

void
gcl_init_pathname_function(void) {

}
