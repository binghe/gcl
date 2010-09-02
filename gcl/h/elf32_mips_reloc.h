    case R_MIPS_JALR:
      break;
    case R_MIPS_32:
      if (!sym1[ELF_R_SYM(r->r_info)].st_shndx)
	s=*(ul *)(cgp+s);
      add_val(where,~0L,s);
      break;
    case R_MIPS_GOT16:
    case R_MIPS_CALL16:
      if (!sym1[ELF_R_SYM(r->r_info)].st_shndx) { 
        add_valsc(where,MASK(16),s);
        break;
      }
      massert(ELF_R_TYPE(r->r_info)==R_MIPS_GOT16);
      store_val(where,0xffe00000,0x3c000000); 
      r->r_info=ELF_R_INFO(ELF_R_SYM(r->r_info),R_MIPS_HI16);
    case R_MIPS_HI16:
      massert(!s || sym1[ELF_R_SYM(r->r_info)].st_shndx);
      if (!s) s=gpd=cgp-(ul)where;
      if (!hr) hr=r;
      if (a) add_vals(where,MASK(16),(s>>16)+a);
      break;
    case R_MIPS_LO16:
      massert(!s || sym1[ELF_R_SYM(r->r_info)].st_shndx);
      if (!s) s=gpd;
      a=*where&MASK(16);
      if (a&0x8000) a|=0xffff0000; 
      a+=s&MASK(16);
      a+=(a&0x8000)<<1; 
      store_val(where,MASK(16),a);
      a=0x10000|(a>>16);
      for (hr=hr ? hr : r;--r>=hr && ELF_R_TYPE(r->r_info)==R_MIPS_HI16;)
        relocate(sym1,r,a,start,got,gote);
      hr=NULL;gpd=0;
      break;