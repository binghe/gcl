    case R_AARCH64_ABS64: /* .xword: (S+A) */
      store_val(where,~0L,s+a);
      break;
    case R_AARCH64_ABS32: /* .word:  (S+A) */
      store_val(where,MASK(32),s+a);
      break;
    case R_AARCH64_JUMP26: /* B:      ((S+A-P) >> 2) & 0x3ffffff.  */
    case R_AARCH64_CALL26: /* BL:     ((S+A-P) >> 2) & 0x3ffffff.  */
      {
	long x=((long)(s+a-p))/4;
	if (abs(x)&(~MASK(26))) {
	  *(ul *)tramp=s+a;
	  got+=gotp;
	  gotp+=sizeof(tramp)/sizeof(*got);
	  memcpy(got,tramp,sizeof(tramp));
	  x=((long)(got+1))/4;
	}
	store_vals(where,MASK(26),x);
      }
      break;
    case R_AARCH64_ADR_PREL_PG_HI21: /* ADRH:   ((PG(S+A)-PG(P)) >> 12) & 0x1fffff */
#define PG(x) ((x) & ~0xfff)
      s = ((long)(PG(s+a)-PG(p))) / 0x1000;
      store_val(where,MASK(2) << 29, (s & 0x3) << 29);
      store_val(where,MASK(19) << 5, (s & 0x1ffffc) << 3);
#undef PG
      break;
    case R_AARCH64_ADD_ABS_LO12_NC: /* ADD:    (S+A) & 0xfff */
      store_val(where,MASK(12) << 10,(s+a) << 10);
      break;
    case R_AARCH64_LDST32_ABS_LO12_NC: /* LD/ST32: (S+A) & 0xffc */
      store_val(where,MASK(12) << 10,((s+a) & 0xffc) << 8);
      break;
    case R_AARCH64_LDST64_ABS_LO12_NC: /* LD/ST64: (S+A) & 0xff8 */
      store_val(where,MASK(12) << 10,((s+a) & 0xff8) << 7);
      break;
