    case R_PPC_ADDR16_HA:
      s+=a;
      s+=s&0x8000 ? 1<<16 : 0;
      store_val(where,~MASK(16),s&0xffff0000);
      break;
    case R_PPC_ADDR16_LO:
      store_val(where,~MASK(16),(s+a)<<16);
      break;
    case R_PPC_ADDR32:
      store_val(where,~0L,s+a);
      break;
