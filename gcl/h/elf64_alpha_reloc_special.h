static ul ggot1,ggote;

static int
write_stub(ul s,ul *got) {

  unsigned int *goti;

  goti=(void *)(got+1);
  *got=(ul)goti;
  *goti++=(0x29<<26)|(0x1b<<21)|(0x1b<<16)|16;
  *goti++=(0x29<<26)|(0x1b<<21)|(0x1b<<16)|0;
  *goti++=(0x1a<<26)|(0x1f<<21)|(0x1b<<16)|0x4000;
  *goti++=0;
  got=(void *)goti;
  *got=s;

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

  Shdr *sec;
  Rela *r;
  void *ve;

  massert((sec=get_section(".got",sec1,sece,sn)));

  ggot1=sec->sh_addr;
  ggote=ggot1+sec->sh_size;

  massert((sec=get_section(".rel.dyn",sec1,sece,sn))||
	  (sec=get_section(".rela.dyn",sec1,sece,sn)));

  v+=sec->sh_offset;
  ve=v+sec->sh_size;

  for (r=v;v<ve;v+=sec->sh_entsize,r=v) 
    if (ELF_R_TYPE(r->r_info) && !ds1[ELF_R_SYM(r->r_info)].st_value)
      ds1[ELF_R_SYM(r->r_info)].st_value=r->r_offset;

  return 0;

}
