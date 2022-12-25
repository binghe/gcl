#define riscv_high(a_) ((a_)+(((a_)&0x800) ? (1<<12) : 0))

    case R_RISCV_HI20:
      store_val(where,MASK(20)<<12,riscv_high(s+a));
      break;
    case R_RISCV_RELAX:/*FIXME figure out how to delete instructions efficiently*/
      break;
    case R_RISCV_LO12_I:
      store_val(where,MASK(12)<<20,(s+a)<<20);
      break;
    case R_RISCV_LO12_S:
      store_val(where,MASK(5)<<7,(s+a)<<7);
      store_val(where,MASK(7)<<25,(s+a)<<20);
      break;
    case R_RISCV_CALL:
    case R_RISCV_CALL_PLT:
      store_val(where,MASK(20)<<12,riscv_high(s+a-p));
      store_val((void *)where+4,MASK(12)<<20,(s+a-p)<<20);
      break;
    case R_RISCV_BRANCH:
    case R_RISCV_RVC_BRANCH:
    case R_RISCV_RVC_JUMP:
    case R_RISCV_JAL:
      break;
    case R_RISCV_64:
      store_val(where,MASK(64),(s+a));
      break;
    case R_RISCV_32:
      store_val(where,MASK(32),(s+a));
      break;
