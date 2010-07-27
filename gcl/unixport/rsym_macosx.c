/*
    File rsym_macosx.c
    
    Build an executable rsym for Mac OS X (31 July 2003).
    
    Grab only the external symbols from a Mach-O object file, and put them
    in a simple format. This information will be used for relocation.
    
    To compile in standalone mode:
        gcc -DDEBUG -DSTANDALONE -I../h -o rsym_macosx rsym_macosx.c
    
    Aurelien Chanudet (aurelienDOTchanudetATm4xDOTorg)
*/

#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach/mach.h>

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#define IN_RSYM 1

#include "ext_sym.h"

#define massert(a_) if (!(a_)) {fprintf(stderr,"The assertion %s on line %d of %s in function %s failed", \
					#a_,__LINE__,__FILE__,__FUNCTION__);exit(-1);}

int 
main(int argc,char * argv[],char **envp) {

  struct stat ss;
  struct mach_header *mh;
  struct load_command *lc;
  struct symtab_command *st=NULL;
  struct nlist *sym1,*sym,*syme;
  struct lsymbol_table tab;
  char *strtab;
  void *addr;
  int i,l;
  FILE *f;
  
  massert(!stat(argv[1],&ss));
  massert((l=open(argv[1],O_RDONLY,0))>0);
  massert((addr=mmap(0,ss.st_size,PROT_READ|PROT_WRITE,MAP_PRIVATE,l,0))!=(void *)-1);

  mh=addr;
  lc=addr+sizeof(*mh);
  
  for (i=0;i<mh->ncmds;i++,lc=(void *)lc+lc->cmdsize)
    if (lc->cmd==LC_SYMTAB) {
      st=(void *) lc;
      break;
    }
    
  massert(st);
  sym1=addr+st->symoff;
  syme=sym1+st->nsyms;
  strtab=addr+st->stroff;
  
  tab.n_symbols=0;
  tab.tot_leng=0;

  massert(f=fopen (argv[2], "wb"));
  fseek(f,sizeof(tab),0);

  for (sym=sym1;sym<syme;sym++) {
    
    char *name=sym->n_un.n_strx + strtab;
    
    if (sym->n_type & N_STAB)
      continue;
    if (!(sym->n_type & N_EXT))
      continue;

    fwrite (&sym->n_value,sizeof(sym->n_value),1,f);
    tab.n_symbols++;

    fprintf(f,"%s",name);
    putc (0, f);
    tab.tot_leng+=strlen(name)+1;

  }
  
  fseek (f, 0, 0);
  fwrite (&tab, sizeof(tab), 1, f);
  fclose (f);

  munmap(addr,ss.st_size);
  close (l);

  return 0;

}
