  case R_SPARC_WDISP30:
    /* v-disp30*/
    store_valu(where,MASK(30),(s+a-p)>>2);
    break;
    
  case R_SPARC_HI22:
    /* t-sim22 */
    store_valu(where,MASK(22),(s+a)>>10);
    break;
    
  case R_SPARC_LO10:
    or_val(where,MASK(10),(s+a)&MASK(10));
    break;

  case R_SPARC_13:
    store_valu(where,MASK(13),s+a);
    break;
    
  case R_SPARC_32:
  case R_SPARC_UA32:
    store_valu(where,MASK(32),s+a);
    break;
    
  case R_SPARC_64:
  case R_SPARC_UA64:
    store_valu(where,~0L,s+a);
    break;
