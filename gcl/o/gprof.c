#include "include.h"
#include "page.h"
#include "ptable.h"


static unsigned long gprof_on;

#ifdef DARWIN
void _mcleanup() {}
#endif

DEFUN_NEW("MCLEANUP",object,fSmcleanup,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {

  extern void _mcleanup(void);

  if (!gprof_on)
    return Cnil;

  massert((_mcleanup(),1));
  gprof_on=0;

  return make_simple_string("gmon.out");

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

}

DEFUN_NEW("GPROF-ADDRESSES",object,fSgprof_addresses,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {

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
    for (i=0;i<c_table.alloc_length;i++)
      mintext=(void *)c_table.ptable[i].address<mintext ? (void *)c_table.ptable[i].address : mintext;
#endif

  }

  if (mintext<data_start)
    min=mintext;

  return MMcons(make_fixnum((fixnum)min),make_fixnum((fixnum)max));

}

DEFUN_NEW("KCL-SELF",object,fSkcl_self,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {

  return make_simple_string(kcl_self);

}

DEFUN_NEW("PTABLE-ALLOC-LENGTH",object,fSptable_alloc_length,SI,0,0,NONE,OO,OO,OO,OO,(void),"") {
  return make_fixnum(c_table.alloc_length);
}

DEFUNM_NEW("PTABLE",object,fSptable,SI,2,2,NONE,OI,OO,OO,OO,(ufixnum i,object s),"") {
  check_type_string(&s);
  massert(i<c_table.alloc_length);
  s->st.st_self=(void *)c_table.ptable[i].string;
  s->st.st_fillp=s->st.st_dim=strlen(s->st.st_self);
  RETURN2(make_fixnum(c_table.ptable[i].address),s);
}
