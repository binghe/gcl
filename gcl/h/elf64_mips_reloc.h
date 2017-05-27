    case R_MIPS_JALR:
      break;
    case R_MIPS_GPREL32:
      recurse(s+a-(ul)got);
      add_val(where,MASK(32),s+a-(ul)got);
      break;
    case R_MIPS_GPREL16:
      recurse(s+a-(ul)got);
      add_val(where,MASK(16),s+a-(ul)got);
      break;
    case R_MIPS_SUB:
      recurse(-(s+a));
      break;/*???*/
    case R_MIPS_64:
      recurse(s+a);
      add_val(where,~0L,s+a);
      break;
    case R_MIPS_32:
      recurse(s+a);
      add_val(where,MASK(32),s+a);
      break;
    case R_MIPS_GOT_DISP:
    case R_MIPS_CALL16:
    case R_MIPS_GOT_PAGE:
    case R_MIPS_GOT_HI16:
    case R_MIPS_GOT_LO16:
    case R_MIPS_CALL_HI16:
    case R_MIPS_CALL_LO16:
      recurse(s+a);
      gote=got+(a>>32)-1;
      a&=MASK(32);
      if (s>=ggot && s<ggote) {
        massert(!write_stub(s,got,gote));
      } else
        *gote=s+(a&~MASK(16))+((a&0x8000)<<1);
      ((Rela *)r)->r_addend=((void *)gote-(void *)got)-s;
      switch(tp) {
        case R_MIPS_GOT_HI16:
        case R_MIPS_CALL_HI16:
	  r->r_info=((ul)R_MIPS_HI16<<56)|(r->r_info&MASK(32));
	  relocate(sym1,r,((Rela *)r)->r_addend,start,got,gote);
	  break;
        case R_MIPS_GOT_LO16:
        case R_MIPS_CALL_LO16:
	  r->r_info=((ul)R_MIPS_LO16<<56)|(r->r_info&MASK(32));
	  relocate(sym1,r,((Rela *)r)->r_addend,start,got,gote);
	  break;
        default:
	  store_val(where,MASK(16),((void *)gote-(void *)got));
	  break;
      }
      break;
    case R_MIPS_GOT_OFST:
      recurse(s+a);
      store_val(where,MASK(16),a);
      break;
    case R_MIPS_HI16:
      recurse(s+a);
      if (!hr) hr=(void *)r;
      if (lr)/*==(Rela *)r*/
	add_vals(where,MASK(16),(s+a+la)>>16);
      break;
    case R_MIPS_LO16:
      recurse(s+a);
      s+=a;
      a=*where&MASK(16);
      if (a&0x8000) a|=0xffffffffffff0000; 
      a+=s&MASK(16);
      a+=(a&0x8000)<<1; 
      store_val(where,MASK(16),a);
      for (la=a&~MASK(16),lr=(Rela *)r,hr=hr ? hr : lr;--lr>=hr;)
        if (ELF_R_TYPE1(lr->r_info)==R_MIPS_HI16||
            ELF_R_TYPE2(lr->r_info)==R_MIPS_HI16||
            ELF_R_TYPE3(lr->r_info)==R_MIPS_HI16)
          relocate(sym1,lr,lr->r_addend,start,got,gote);
      hr=lr=NULL;
      break;
