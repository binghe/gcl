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

#define IN_UNIXFASL
#include "include.h"

#ifdef UNIXFASL
#include UNIXFASL
#else

#ifdef HAVE_AOUT
#undef BSD
#undef ATT
#define BSD
#include HAVE_AOUT
#endif

#ifdef COFF_ENCAPSULATE
#undef BSD
#undef ATT
#define BSD
#include "a.out.encap.h"
#endif

#ifdef ATT
#include <filehdr.h>
#include <scnhdr.h>
#include <syms.h>
#endif

#ifdef E15
#include <a.out.h>
#define exec		bhdr
#define a_text		tsize
#define a_data		dsize
#define a_bss		bsize
#define a_syms		ssize
#define a_trsize	rtsize
#define a_drsize	rdsize
#endif

#ifdef BSD
#define	textsize	header.a_text
#define	datasize	header.a_data
#define	bsssize		header.a_bss
#ifdef COFF_ENCAPSULATE
#define	textstart	sizeof(header) +sizeof(struct coffheader)
#else
#define	textstart	sizeof(header)
#endif
#define	newbsssize	newheader.a_bss
#endif

#ifndef HEADER_SEEK
#define HEADER_SEEK
#endif

#ifndef MAXPATHLEN
#  define	MAXPATHLEN	1024
#endif

#ifndef SFASL
#error must define SFASL
#endif /* ifndef SFASL */

#ifndef __svr4__
#ifdef BSD

#define FASLINK
#ifndef PRIVATE_FASLINK

DEFUN_NEW("FASLINK-INT",object,fSfaslink_int,SI,2,2,NONE,II,OO,OO,OO,(object faslfile, object ldargstring),"") {
#if defined(__ELF__) || defined(DARWIN)
  FEerror("faslink() not supported for ELF or DARWIN yet",0);
  return 0;
#else
	struct exec header, faslheader;
	object memory, data, tempfile;
	FILE *fp;
	char filename[MAXPATHLEN];
	char ldargstr[MAXPATHLEN];
	char tempfilename[32];
	char command[MAXPATHLEN * 2];
	char buf[BUFSIZ];
	int i;
	object *old_vs_base = vs_base;
	object *old_vs_top = vs_top;

	coerce_to_filename(ldargstring, ldargstr);
	coerce_to_filename(faslfile, filename);

	sprintf(tempfilename, "/tmp/fasltemp%d", getpid());
	LD_COMMAND(command,
		kcl_self,
		(int)core_end,
		filename,
		ldargstr,
		tempfilename);

	if (system(command) != 0)
		FEerror("The linkage editor failed.", 0);

	fp = fopen(tempfilename, "r");
	setbuf(fp, buf);
	fread(&header, sizeof(header), 1, fp);
	{BEGIN_NO_INTERRUPT;
	  memory=new_cfdata();
	  memory->cfd.cfd_size = textsize + datasize + bsssize;
	  vs_push(memory);
	  memory->cfd.cfd_start = ALLOC_ALIGNED(alloc_contblock,
					      memory->cfd.cfd_size,
					      sizeof(double));
	END_NO_INTERRUPT;}
	fclose(fp);

	faslfile = open_stream(faslfile, smm_input, Cnil, sKerror);
	vs_push(faslfile);
#ifdef SEEK_TO_END_OFILE
SEEK_TO_END_OFILE(faslfile->sm.sm_fp);
#else  
	fp = faslfile->sm.sm_fp;
	fread(&faslheader, sizeof(faslheader), 1, fp);
	fseek(fp,
	      faslheader.a_text+faslheader.a_data+
	      faslheader.a_syms+faslheader.a_trsize+faslheader.a_drsize,
	      1);
	fread(&i, sizeof(i), 1, fp);
	fseek(fp, i - sizeof(i), 1);
#endif
	data = read_fasl_vector(faslfile);
	vs_push(data);
	close_stream(faslfile);
        LD_COMMAND(command,
		   kcl_self,
		   memory->cfd.cfd_start,
		   filename,
		   ldargstr,
		   tempfilename);
	 if(symbol_value(sLAload_verboseA)!=Cnil)	
        printf("start address -T %x ",memory->cfd.cfd_start);
	if (system(command) != 0)
		FEerror("The linkage editor failed.", 0);

	tempfile = make_simple_string(tempfilename);
	vs_push(tempfile);
	tempfile = open_stream(tempfile, smm_input, Cnil, sKerror);
	vs_push(tempfile);
	fp = tempfile->sm.sm_fp;

	if (fseek(fp, textstart, 0) < 0)
		error("file seek error");

	fread(memory->cfd.cfd_start, textsize + datasize, 1, fp);

	close_stream(tempfile);

	unlink(tempfilename);

	call_init(0,memory,data,0);

	vs_base = old_vs_base;
	vs_top = old_vs_top;

	return(memory->cfd.cfd_size);
#endif
}

#endif

#endif
#endif/*  svr4 */
#endif /* UNIXFASL */

void
gcl_init_unixfasl(void) {
}
