static int tramp[]={0x0c00f240,  /*movw	r12, #0*/
		    0x0c00f2c0,  /*movt	r12, #0*/
		    0xbf004760}; /*bx r12   nop*/
static ul tz=sizeof(tramp)/sizeof(ul);

static ul *
next_plt_entry(ul *p,ul *pe) {

  ul l0=0xe5bef000,/*ldr pc,[ip,#]*/
     l1=0xe5bcf000;/*ldr pc,[lr,#]*/

  for (;p<pe && (*p&l0)!=l0 && (*p&l1)!=l1;p++);
  if ((*p&l0)==l0) p++;

  return p+1;

}

static int
find_special_params(void *v,Shdr *sec1,Shdr *sece,const char *sn,
		    const char *st1,Sym *ds1,Sym *dse,Sym *sym,Sym *syme) {

  Shdr *sec,*psec;
  Rel *r;
  ul *p,*pe;
  void *ve;

  /*plt entries are not of uniform size*/

  massert(psec=get_section(".plt",sec1,sece,sn));
  p=(void *)psec->sh_addr;
  pe=(void *)p+psec->sh_size;

  massert((sec=get_section( ".rel.plt",sec1,sece,sn)) ||
	  (sec=get_section(".rela.plt",sec1,sece,sn)));

  v+=sec->sh_offset;
  ve=v+sec->sh_size;

  p=next_plt_entry(p,pe);/*plt0*/

  for (r=v;v<ve && p<pe;v+=sec->sh_entsize,r=v,p=next_plt_entry(p,pe)) {
    if (!ds1[ELF_R_SYM(r->r_info)].st_value)
      ds1[ELF_R_SYM(r->r_info)].st_value=(ul)p;
  }

  massert(p==pe);
  massert(v==ve);

  return 0;

}

static int
label_got_symbols(void *v1,Shdr *sec1,Shdr *sece,Sym *sym1,Sym *syme,const char *st1,const char *sn,ul *gs) {

  Rel *r;
  Sym *sym;
  Shdr *sec;
  void *v,*ve;

  for (sym=sym1;sym<syme;sym++)
    sym->st_size=0;

  for (*gs=0,sec=sec1;sec<sece;sec++)
    if (sec->sh_type==SHT_REL)
      for (v=v1+sec->sh_offset,ve=v+sec->sh_size,r=v;v<ve;v+=sec->sh_entsize,r=v)
	if (
#define R_ARM_THM_CALL        10
	    ELF_R_TYPE(r->r_info)==R_ARM_THM_CALL ||
	    ELF_R_TYPE(r->r_info)==R_ARM_THM_JUMP24
	    ) {

	  sym=sym1+ELF_R_SYM(r->r_info);

	  if (!sym->st_size)
	    sym->st_size=++*gs;

	}

  (*gs)*=tz;

  return 0;

}
