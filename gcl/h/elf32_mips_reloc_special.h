static ul gpd,cgp,stub1,stube,gotsym,locgotno,ggot; static Rel *hr;

#ifdef __mips__

static int
write_stub(ul s,ul *got) {

  ul ogp=cgp;
  
  ogp+=((ogp&0x8000)<<1);

  s=((ul *)s)[3]&MASK(16);
  s+=locgotno-gotsym;
  s*=sizeof(*got);
  s+=ggot-cgp;
  
  *got=(ul)(got+1);
  *++got=0x3c190000|(ogp>>16);
  *++got=0x27390000|(ogp&0xffff);
  *++got=0x8f390000|(s&MASK(16));
  *++got=0x03200008;
  *++got=0x00200825;

  return 0;
  
}

static int
make_got_room_for_stub(Shdr *sec1,Shdr *sece,Sym *sym,const char *st1,ul *gs) {

  Shdr *ssec=sec1+sym->st_shndx;
  struct node *a;
  if ((ssec>=sece || !ALLOC_SEC(ssec)) && 
      (a=find_sym_ptable(st1+sym->st_name)) &&
      a->address>=stub1 && a->address<stube)
    (*gs)+=5;

  return 0;

}

static int
find_global_mips_params(Shdr *sec1,Shdr *sece,const char *sn,const char *st1,
			Sym *ds1,Sym *dse,Sym *sym,Sym *syme) {

  Shdr *sec;
  ul *q;
  void *p,*pe;

  for (;sym<syme && strcmp("_gp",st1+sym->st_name);sym++);
  massert(sym<syme);
  cgp=sym->st_value;

  massert(sec=get_section(".dynamic",sec1,sece,sn));
  for (p=(void *)sec->sh_addr,pe=p+sec->sh_size;p<pe;p+=sec->sh_entsize) {
    q=p;
    if (q[0]==DT_MIPS_GOTSYM)
      gotsym=q[1];
    if (q[0]==DT_MIPS_LOCAL_GOTNO)
      locgotno=q[1];
    
  }
  massert(gotsym && locgotno);

  massert(sec=get_section(".got",sec1,sece,sn));
  ggot=sec->sh_addr;

  massert(sec=get_section(".MIPS.stubs",sec1,sece,sn));
  stub1=sec->sh_addr;
  stube=stub1+sec->sh_size;
      
  return 0;

}
#endif
