static ul gpd; static Rel *hr;

static int
find_special_params(void *v,Shdr *sec1,Shdr *sece,const char *sn,
		    const char *st1,Sym *ds1,Sym *dse,Sym *sym,Sym *syme) {
  
  return 0;

}

static int
label_got_symbols(void *v1,Shdr *sec1,Shdr *sece,Sym *sym1,Sym *syme,const char *st1,const char *sn,ul *gs) {

  Rel *r;
  Sym *sym;
  Shdr *sec;
  void *v,*ve;
  ul q;

  for (q=0,sym=sym1;sym<syme;sym++) {
    const char *s=st1+sym->st_name;
    if ((sym->st_other=strcmp(s,"_gp_disp") ? (strcmp(s,"__gnu_local_gp") ? 0 : 2) : 1)) {
      q++;
      sym->st_info=ELF_ST_INFO(STB_LOCAL,ELF_ST_TYPE(sym->st_info));
    }
  }
  massert(q<=1);
  
  for (sym=sym1;sym<syme;sym++)
    sym->st_size=0;

  for (*gs=0,sec=sec1;sec<sece;sec++)
    if (sec->sh_type==SHT_REL)
      for (v=v1+sec->sh_offset,ve=v+sec->sh_size,r=v;v<ve;v+=sec->sh_entsize,r=v)

	if (ELF_R_TYPE(r->r_info)==R_MIPS_CALL16||
	    ELF_R_TYPE(r->r_info)==R_MIPS_GOT16) {

	  sym=sym1+ELF_R_SYM(r->r_info);

	  if (!sym->st_size)
	    sym->st_size=++*gs; 

	}
  
  return 0;
  
}

#define FIX_HIDDEN_SYMBOLS(st1_,a_,sym1_,sym_,syme_)				\
  ({Sym *p;const char *n=(st1_)+(sym_)->st_name,*s=".pic.",*q;ul z=strlen(s);	\
    if (ELF_ST_VISIBILITY((sym_)->st_other)==STV_HIDDEN) {		\
      for (p=(sym1_);p<(syme_);p++)					\
	if (!strncmp(s,(q=(st1_)+p->st_name),z) && !strcmp(n,q+z)) {	\
	  (*(a_))->address=p->st_value;					\
	  break;							\
	}}})
