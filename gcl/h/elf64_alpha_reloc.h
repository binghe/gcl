    case R_ALPHA_GPDISP:
      s=(ul)got;
      s-=p; 
      s+=(s&0x8000)<<1;
      store_val(where,MASK(16),s>>16); 
      where=(void *)where+a; 
      store_val(where,MASK(16),s); 
      break;
    case R_ALPHA_SREL32:
      store_val(where,MASK(32),s+a-p);
      break;
    case R_ALPHA_GPREL32:
      store_val(where,MASK(32),s+a-(ul)got);
      break;
    case R_ALPHA_LITUSE:
    case R_ALPHA_HINT:
      break;
    case R_ALPHA_REFQUAD:
      store_val(where,~0L,s+a);
      break;
    case R_ALPHA_REFLONG:
      store_val(where,MASK(32),s+a);
      break;
    case R_ALPHA_LITERAL:
      gote=got+(a>>32)-1;
      a&=MASK(32);
      if (s>=ggot1 && s<ggote) {
        massert(!write_stub(s,got,gote));
      } else 
	*gote=s+a;
      s=(gote-got)*sizeof(*got);
      massert(!(s&~MASK(15)));
      store_val(where,MASK(16),s);
      break;
    case R_ALPHA_GPRELHIGH:
      s+=a-(ul)got;
      s+=(s&0x8000)<<1;      
      store_val(where,MASK(16),s>>16);
      break;
    case R_ALPHA_GPRELLOW:
      store_val(where,MASK(16),s+a-(ul)got);
      break;
    case R_ALPHA_TLS_GD_HI:
      store_vals(where,MASK(21),((long)(s+a-(p+4)))>>2);
      break;
