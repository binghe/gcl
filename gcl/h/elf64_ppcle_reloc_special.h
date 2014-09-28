static Sym *toc;

/* static int tramp[]={0,0, */
/* 		    (((0x3a<<10)|(0x9<<5)|0xc)<<16)|0xfff8,/\*ld      r9,-8(r12)*\/ */
/* 		    ((0x3a<<10)|(0x9<<5)|0x9)<<16,         /\*ld      r9,0(r9)*\/ */
/* 		    0x7d2c4b78,                            /\*mr      r12,r9 *\/ */
/* 		    0x7d8903a6,                            /\*mtctr   r12*\/ */
/* 		    0x4e800420                             /\*bctrl*\/ */
/* }; */

static int
find_special_params(void *v,Shdr *sec1,Shdr *sece,const char *sn,
		    const char *st1,Sym *ds1,Sym *dse,Sym *sym,Sym *syme) {
  
  Shdr *sec;
  Rela *r;
  void *ve;
  ul j,*u;


  massert((sec=get_section(".rela.plt",sec1,sece,sn)));

  v+=sec->sh_offset;
  ve=v+sec->sh_size;

  for (r=v;v<ve;v+=sec->sh_entsize,r=v) 
    if (ELF_R_TYPE(r->r_info) && !ds1[ELF_R_SYM(r->r_info)].st_value)
	ds1[ELF_R_SYM(r->r_info)].st_value=*(ul *)r->r_offset;

  return 0;


  /* massert((sec=get_section(".rel.dyn",sec1,sece,sn))|| */
  /* 	  (sec=get_section(".rela.dyn",sec1,sece,sn))); */

  /* v+=sec->sh_offset; */
  /* ve=v+sec->sh_size; */

  /* for (j=0,r=v;v<ve;v+=sec->sh_entsize,r=v)  */
  /*   if (ELF_R_TYPE(r->r_info) && !ds1[ELF_R_SYM(r->r_info)].st_value) */
  /*     j++; */

  /* massert(u=malloc(j*sizeof(tramp))); */

  /* v=ve-sec->sh_size; */
  /* for (r=v;v<ve;v+=sec->sh_entsize,r=v)  */
  /*   if (ELF_R_TYPE(r->r_info) && !ds1[ELF_R_SYM(r->r_info)].st_value) { */
  /*     memcpy(u,tramp,sizeof(tramp)); */
  /*     *u++=r->r_offset; */
  /*     ds1[ELF_R_SYM(r->r_info)].st_value=(ul)u; */
  /*     u=((void *)(u-1)+sizeof(tramp)); */
  /*   } */

  /* return 0; */

}

static int
label_got_symbols(void *v1,Shdr *sec1,Shdr *sece,Sym *sym1,Sym *syme,const char *st1,const char *sn,ul *gs) {

  Shdr *sec;
  Sym *sym;
  
  massert(sec=get_section(".toc",sec1,sece,sn));

  for (sym=sym1;sym<syme;sym++) {
    const char *s=st1+sym->st_name;
    if (!strcmp(s,".TOC.") || !strcmp(s,".toc.")) {
      toc=sym;
      toc->st_info=ELF_ST_INFO(STB_LOCAL,ELF_ST_TYPE(sym->st_info));
      toc->st_shndx=sec-sec1;
    }
  }

  return 0;
  
}
