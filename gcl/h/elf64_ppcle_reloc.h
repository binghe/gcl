#define ha(x_) ((((x_) >> 16) + (((x_) & 0x8000) ? 1 : 0)) & 0xffff)
#define lo(x_) ((x_) & 0xffff)

    case R_PPC64_REL16_HA: 
      store_val(where,MASK(16),ha(s+a-p));
      break;
    case R_PPC64_TOC16_HA: 
      store_val(where,MASK(16),ha(s+a-toc->st_value));
      break;
    case R_PPC64_TOC16_LO_DS: 
      store_val(where,MASK(16),lo(s+a-toc->st_value));/*>>2*/
      break;
    case R_PPC64_REL16_LO:
      store_val(where,MASK(16),lo(s+a-p));
      break;
    case R_PPC64_TOC16_LO:
      store_val(where,MASK(16),lo(s+a-toc->st_value));
      break;
    case R_PPC64_ADDR64:
      store_val(where,~0L,(s+a));
      break;
    case R_PPC64_TOC:
      store_val(where,~0L,toc->st_value);
      break;
    case R_PPC64_REL32:
      store_val(where,MASK(32),(s+a-p));
      break;
