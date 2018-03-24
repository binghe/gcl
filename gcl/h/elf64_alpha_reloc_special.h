static ul ggot1,ggote,gotoff,mcount;

static int
write_stub_mcount(ul s,ul *gote) {

  unsigned int *goti;

  /*mcount calls written using at register, address not available in stub*/
  /*mcount guaranteed to be within 32bits*/
  *gote=(ul)(goti=(void *)(gote+1));
  *goti++=(0x9<<26)|(0x1b<<21)|(0x1f<<16)|((s-(short)s)>>16); /*ldah	t12,(symhigh)(zero)*/
  *goti++=(0x8<<26)|(0x1b<<21)|(0x1b<<16)|(s&MASK(16));       /*lda	t12,(symlow)(t12)*/
  *goti++=(0x29<<26)|(0x1b<<21)|(0x1b<<16)|0;                 /*ldq	t12,0(t12)*/
  *goti++=(0x1a<<26)|(0x1f<<21)|(0x1b<<16)|0x4000;            /*jsr	zero,(t12),$pc+4*/
  *goti++=0;                                                  /*halt*/
  *goti++=0;                                                  /*halt*/

  return 0;

}

static int
write_stub(ul s,ul *gote) {

  unsigned int *goti;

  if (s==mcount)
    return write_stub_mcount(mcount,gote);

  *gote=(ul)(goti=(void *)(gote+2));
  *++gote=s;
  *goti++=(0x29<<26)|(0x1b<<21)|(0x1b<<16)|0xfff8; /*ldq	t12,-8(t12)*/
  *goti++=(0x29<<26)|(0x1b<<21)|(0x1b<<16)|0;      /*ldq	t12,0(t12)*/
  *goti++=(0x1a<<26)|(0x1f<<21)|(0x1b<<16)|0x4000; /*jsr	zero,(t12),$pc+4*/
  *goti++=0;                                       /*halt*/

  return 0;
  
}

static int
make_got_room_for_stub(Shdr *sec1,Shdr *sece,Sym *sym,const char *st1,ul *gs) {

  Shdr *ssec=sec1+sym->st_shndx;
  struct node *a;

  if ((ssec>=sece || !ALLOC_SEC(ssec)) && 
      (a=find_sym_ptable(st1+sym->st_name)) &&
      a->address>=ggot1 && a->address<ggote)
    (*gs)+=3;

  return 0;

}

static int
find_special_params(void *v,Shdr *sec1,Shdr *sece,const char *sn,
		    const char *st1,Sym *ds1,Sym *dse,Sym *sym1,Sym *syme) {

  Sym *sym;
  Shdr *sec;
  Rela *r;
  void *ve,*dst1;

  massert((sec=get_section(".got",sec1,sece,sn)));

  ggot1=sec->sh_addr;
  ggote=ggot1+sec->sh_size;

  massert(sec=get_section(".dynstr",sec1,sece,sn));/*FIXME pass as parameter*/
  dst1=v+sec->sh_offset;

  massert((sec=get_section(".rel.dyn",sec1,sece,sn))||
	  (sec=get_section(".rela.dyn",sec1,sece,sn)));

  v+=sec->sh_offset;
  ve=v+sec->sh_size;

  for (r=v;v<ve;v+=sec->sh_entsize,r=v) 
    if (ELF_R_TYPE(r->r_info) && !(sym=ds1+ELF_R_SYM(r->r_info))->st_value) {
      sym->st_value=r->r_offset;
      if (!strncmp("_mcount",dst1+sym->st_name,7))
	mcount=sym->st_value;
    }

  return 0;

}

static int
label_got_symbols(void *v1,Shdr *sec1,Shdr *sece,Sym *sym1,Sym *syme,const char *st1,const char *sn,ul *gs) {

  Sym *sym,*fsym=sym1;
  Rela *r;
  Shdr *sec;
  void *v,*ve;
  ul q,gotp;

  for (sym=sym1;sym<syme;sym++)
    sym->st_size=0;

  for (*gs=gotp=0,sec=sec1;sec<sece;sec++)
    if (sec->sh_type==SHT_RELA)
      for (v=v1+sec->sh_offset,ve=v+sec->sh_size,r=v;v<ve;v+=sec->sh_entsize,r=v)

	switch(ELF_R_TYPE(r->r_info)) {

	case R_ALPHA_LITERAL:

	  if (!r->r_addend) {

	    sym=sym1+ELF_R_SYM(r->r_info);
	    q=(gotp-sym->st_size)*sizeof(*gs);

	    if (!sym->st_size || q!=(short)q) {
	      sym->st_size=++*gs;
	      massert(!make_got_room_for_stub(sec1,sece,sym,st1,gs));
	    }

	    q=sym->st_size;

	  } else

	    q=++*gs;

	  if (r->r_addend>>32)
	    fprintf(stderr,"zeroing high addend %lx\n",r->r_addend>>32);
	  r->r_addend&=0xffffffff;
	  massert((q&0xffffffff)==q);
	  r->r_addend|=(q<<32);

	  q=(q-gotp)*sizeof(*gs);
	  massert(q==(short)q);

	  break;

	case R_ALPHA_GPDISP:

	  for (sym=fsym;sym<syme && (sym->st_shndx!=1 || sym->st_value!=r->r_offset);sym++);

	  if (sym<syme) {
	    fsym=sym;
	    gotp=*gs+1;
	  }

	  if (r->r_addend>>32)
	    fprintf(stderr,"zeroing high addend %lx\n",r->r_addend>>32);
	  r->r_addend&=0xffffffff;
	  massert((gotp&0xffffffff)==gotp);
	  r->r_addend|=(gotp<<32);

	  break;

	}

  return 0;
  
}
