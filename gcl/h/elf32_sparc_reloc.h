  case     R_SPARC_WDISP30:
    /* v-disp30*/
    store_val(where,MASK(30),(s+a-p) >> 2);
    break;
    
  case R_SPARC_HI22:
    /* t-sim22 */
    store_val(where,MASK(22),(s+a)>>10);
    break;
    
  case R_SPARC_32:
    store_val(where,~0,s+a);
    break;
    
  case R_SPARC_UA32:
    store_val(where,0xffffffff,s+a);
    break;
    
  case R_SPARC_LO10:
    /* val = (s+a) & MASK(10); */
    /* *(short *)((void *)where +2) |= val; */
    or_val(where,MASK(10),s+a);
    break;
