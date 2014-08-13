/* .xword: (S+A) */
#define R_AARCH64_ABS64 257
/* .word:  (S+A) */
#define R_AARCH64_ABS32 258
/* B:      ((S+A-P) >> 2) & 0x3ffffff.  */
#define R_AARCH64_JUMP26 282
/* BL:     ((S+A-P) >> 2) & 0x3ffffff.  */
#define R_AARCH64_CALL26 283
/* ADRH:   ((PG(S+A)-PG(P)) >> 12) & 0x1fffff */
#define R_AARCH64_ADR_PREL_PG_HI21 275
/* ADD:    (S+A) & 0xfff */
#define R_AARCH64_ADD_ABS_LO12_NC 277
/* LD/ST32: (S+A) & 0xffc */
#define R_AARCH64_LDST32_ABS_LO12_NC 285
/* LD/ST64: (S+A) & 0xff8 */
#define R_AARCH64_LDST64_ABS_LO12_NC 286

    case R_AARCH64_ABS64:
      store_val(where,~0L,s+a);
      break;
    case R_AARCH64_ABS32:
      store_val(where,MASK(32),s+a);
      break;
    case R_AARCH64_JUMP26:
    case R_AARCH64_CALL26:
      store_vals(where,MASK(26),((long)(s+a-p)) / 4);
      break;
    case R_AARCH64_ADR_PREL_PG_HI21:
#define PG(x) ((x) & ~0xfff)
      s = ((long)(PG(s+a)-PG(p))) / 0x1000;
      store_val(where,MASK(2) << 29, (s & 0x3) << 29);
      store_val(where,MASK(19) << 5, (s & 0x1ffffc) << 3);
#undef PG
      break;
    case R_AARCH64_ADD_ABS_LO12_NC:
      store_val(where,MASK(12) << 10,(s+a) << 10);
      break;
    case R_AARCH64_LDST32_ABS_LO12_NC:
      store_val(where,MASK(12) << 10,((s+a) & 0xffc) << 8);
      break;
    case R_AARCH64_LDST64_ABS_LO12_NC:
      store_val(where,MASK(12) << 10,((s+a) & 0xff8) << 7);
      break;
