#include "include.h"
#include "page.h"
#include "ptable.h"


static unsigned long gprof_on;

DEFUN_NEW("MCLEANUP",object,fSmcleanup,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {

  extern void _mcleanup(void);

  if (!gprof_on)
    return Cnil;

  massert(getcwd(FN1,sizeof(FN1)));
  massert(!chdir(P_tmpdir));
  _mcleanup();
  massert(!chdir(FN1));
  gprof_on=0;
  massert(snprintf(FN1,sizeof(FN1),"%s/gmon.out",P_tmpdir)>0);
  return make_simple_string(FN1);
}

static inline int
my_monstartup(unsigned long start,unsigned long end) {

  extern void monstartup(unsigned long,unsigned long);

  monstartup(start,end);

  return 0;

}

DEFUN_NEW("MONSTARTUP",object,fSmonstartup,SI,2,2,NONE,OI,IO,OO,OO,(ufixnum start,ufixnum end),"") {

  if (gprof_on)
    return Cnil;

  writable_malloc_wrap(my_monstartup,int,start,end);
  gprof_on=1;

  return Ct;

}

void
gprof_cleanup(void) {

  FFN(fSmcleanup)();
  /*rename gmon?*/

}

DEFUNM_NEW("GPROF-ADDRESSES",object,fSgprof_addresses,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {

  void *min=heap_end,*max=data_start,*c;
  static void *mintext;
  struct pageinfo *v;
  object x;
  fixnum i;
  struct typemanager *tm=tm_of(t_cfdata);

  for (v=cell_list_head;v;v=v->next)
    if (v->type==tm->tm_type)
      for (c=pagetochar(page(v)),i=0;i<tm->tm_nppage;i++,c+=tm->tm_size)
	if (!is_free((x=c)) && type_of(x)==t_cfdata && x->cfd.cfd_prof) {
	  min=(void *)x->cfd.cfd_start<min ? x->cfd.cfd_start : min;
	  max=(void *)x->cfd.cfd_start+x->cfd.cfd_size>max ? x->cfd.cfd_start+x->cfd.cfd_size : max;
	}

  if (max<min)
    min=max;

  if (!mintext) {

    mintext=data_start;

#ifdef GCL_GPROF
    for (i=0;i<c_table.length;i++)
      mintext=(void *)c_table.ptable[i].address<mintext ? (void *)c_table.ptable[i].address : mintext;
    for (i=0;i<c_table.local_length;i++)
      mintext=(void *)c_table.local_ptable[i].address<mintext ? (void *)c_table.local_ptable[i].address : mintext;
#endif

  }

  if (mintext<data_start)
    min=mintext;

  RETURN2(make_fixnum((fixnum)min),make_fixnum((fixnum)max));

}

DEFUN_NEW("KCL-SELF",object,fSkcl_self,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {

  return make_simple_string(kcl_self);

}

DEFUN_NEW("WRITE-SYMTAB",object,fSwrite_symtab,SI,3,3,NONE,OO,II,OO,OO,
     (object symtab,ufixnum start,ufixnum end),"") {

  struct package *p;
  object l,s,f,*b,*be;
  FILE *pp;
  ufixnum i;

  coerce_to_filename(symtab,FN1);
  pp=fopen(FN1,"w");
  fprintf(pp,"%016lx T GCL_MONSTART\n",start);
  for (p=pack_pointer;p;p=p->p_link)
    for (i=0,b=p->p_internal,be=b+p->p_internal_size;b;
	 b=i ? NULL : p->p_external,be=b+p->p_external_size,i=1)
      for (;b<be;b++)
	for (l=*b;consp(l);l=l->c.c_cdr)
	  if ((f=(s=l->c.c_car)->s.s_gfdef)!=OBJNULL && s->s.s_hpack==(object)p)
	    switch(type_of(f)) {
	    case t_cfun:case t_sfun:case t_vfun:case t_afun:case t_gfun:
	      if ((ufixnum)f->cf.cf_self>=start && (ufixnum)f->cf.cf_self<end)
		fprintf(pp,"%016lx T %-.*s::%-.*s\n",
			(ufixnum)f->cf.cf_self,
			p->p_name->st.st_fillp,p->p_name->st.st_self,
			s->st.st_fillp,s->st.st_self);
	      break;
	    }
  fprintf(pp,"%016lx T GCL_MONEND\n",end);

  for (i=0;i<c_table.length;i++)
    fprintf(pp,"%016lx T %s\n",c_table.ptable[i].address,c_table.ptable[i].string);
  for (i=0;i<c_table.local_length;i++)
    fprintf(pp,"%016lx t %s\n",c_table.local_ptable[i].address,c_table.local_ptable[i].string);
  fclose(pp);

  return symtab;

}
