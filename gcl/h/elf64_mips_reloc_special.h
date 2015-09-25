static Rela *hr;

#undef ELF_R_SYM 
#define ELF_R_SYM(a_) (a_&0xffffffff) 
#undef ELF_R_TYPE 
#define ELF_R_TYPE(a_) (((a_>>40)&0xff) ? ((a_>>40)&0xff) : ((a_>>56)&0xff)) 
#define ELF_R_FTYPE(a_) ((a_>>56)&0xff)

static int
find_special_params(void *v,Shdr *sec1,Shdr *sece,const char *sn,
		    const char *st1,Sym *ds1,Sym *dse,Sym *sym,Sym *syme) {
  
  return 0;

}

static int
label_got_symbols(void *v1,Shdr *sec1,Shdr *sece,Sym *sym1,Sym *syme,const char *st1,const char *sn,ul *gs) {

  Rela *r;
  Sym *sym;
  Shdr *sec;
  void *v,*ve;
  ul q=0,a,b;

  for (sym=sym1;sym<syme;sym++)
    sym->st_size=0;

  for (*gs=0,sec=sec1;sec<sece;sec++)
    if (sec->sh_type==SHT_RELA)
      for (v=v1+sec->sh_offset,ve=v+sec->sh_size,r=v;v<ve;v+=sec->sh_entsize,r=v)
	if (ELF_R_TYPE(r->r_info)==R_MIPS_CALL16||
	    ELF_R_TYPE(r->r_info)==R_MIPS_GOT_DISP||
	    ELF_R_TYPE(r->r_info)==R_MIPS_GOT_PAGE) {

	  sym=sym1+ELF_R_SYM(r->r_info);

	  a=r->r_addend>>15;

	  if (2*a>=sizeof(sym->st_size) || !((sym->st_size>>(a*16))&0xffff)) {

	    q=++*gs;
	    if (2*a<sizeof(sym->st_size)) {
	      massert(q<=0xffff);
	      sym->st_size|=(q<<(a*16));
	    }
	    
	  }

	  b=sizeof(r->r_addend)*4; 
	  massert(!(r->r_addend>>b)); 
	  q=2*a>=sizeof(sym->st_size) ? q : (sym->st_size>>(a*16))&0xffff; 
	  r->r_addend|=(q<<=b); 

	}
  
  return 0;
  
}
