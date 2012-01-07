/* #define R_ARM_THM_CALL        10 */
#define R_ARM_CALL 28
#define R_ARM_V4BX 40
#define R_ARM_THM_MOVW_ABS_NC 47
#define R_ARM_THM_MOVW_ABS    48
    /* case R_ARM_THM_CALL: */
    /*   s+=a; */
    /*   if (ELF_ST_TYPE(sym->st_info)==STT_FUNC) s|=1; */
    /*   s-=p; */
    /*   s-=4;  */
    /*   s>>=1; */
    /*   s&=(~1); */
    /*   store_val(where,MASK(1)<<28,0); */
    /*   /\* massert(!(s&0xffc00000)); *\/ */
    /*   /\* s=((s&0x7ff)<<16)|((s>>11)&0x7ff); *\/ */
    /*   store_val(where,MASK(12),s>>12); */
    /*   store_val(where,MASK(12)<<16,(s&0xfff)<<16); */
    /*   /\* add_vals(where,~0L,s); *\/ */
    /*   break; */
    case R_ARM_THM_MOVW_ABS_NC:
      s+=a;
      if (ELF_ST_TYPE(sym->st_info)==STT_FUNC) s|=1;
      s&=0xffff;
      s=((s>>12)&0xf)|(((s>>11)&0x1)<<10)|((s&0xff)<<16)|(((s>>8)&0x7)<<28);
      add_vals(where,~0L,s);
      break;
    case R_ARM_THM_MOVW_ABS:
      s+=a;
      s>>=16;
      s=((s>>12)&0xf)|(((s>>11)&0x1)<<10)|((s&0xff)<<16)|(((s>>8)&0x7)<<28);
      add_vals(where,~0L,s);
      break;
    case R_ARM_CALL:
      add_vals(where,MASK(24),((long)(s+a-p))>>2);
      break;
    case R_ARM_ABS32:
      add_val(where,~0L,s+a);
      break;
    case R_ARM_V4BX:
      add_val(where,~0L,s+a);
      break;
