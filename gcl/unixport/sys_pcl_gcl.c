#define FLAVOR "pcl_"

#include "sys.c"


void
gcl_init_init()
{

  build_symbol_table();

  lsp_init("../lsp/gcl_export.lsp");

  ar_init(gcl_defmacro);
  ar_init(gcl_evalmacros);
  ar_init(gcl_top);
  ar_init(gcl_module);

  lsp_init("../lsp/gcl_autoload.lsp");

}

void
gcl_init_system(object no_init)
{

  if (type_of(no_init)!=t_symbol)
    error("Supplied no_init is not of type symbol\n");

  ar_check_init(gcl_predlib,no_init);
  ar_check_init(gcl_setf,no_init);
  ar_check_init(gcl_arraylib,no_init);
  ar_check_init(gcl_assert,no_init);
  ar_check_init(gcl_defstruct,no_init);
  ar_check_init(gcl_restart,no_init);
  ar_check_init(gcl_describe,no_init);
#ifdef HAVE_JAPI_H
  ar_check_init(gcl_japi,no_init);
#endif
  ar_check_init(gcl_iolib,no_init);
  ar_check_init(gcl_listlib,no_init);
  ar_check_init(gcl_mislib,no_init);
  ar_check_init(gcl_numlib,no_init);
  ar_check_init(gcl_packlib,no_init);
  ar_check_init(gcl_seq,no_init);
  ar_check_init(gcl_seqlib,no_init);
  ar_check_init(gcl_trace,no_init);
  ar_check_init(gcl_sloop,no_init);
  ar_check_init(gcl_serror,no_init);
  ar_check_init(gcl_destructuring_bind,no_init);
  ar_check_init(gcl_loop,no_init);
  ar_check_init(gcl_defpackage,no_init);
  ar_check_init(gcl_make_defpackage,no_init);
  ar_check_init(gcl_sharp,no_init);
  ar_check_init(gcl_fpe,no_init);

	
  ar_check_init(gcl_cmpinline,no_init);
  ar_check_init(gcl_cmputil,no_init);

  ar_check_init(gcl_debug,no_init);
  ar_check_init(gcl_info,no_init);

  ar_check_init(gcl_cmptype,no_init);
  ar_check_init(gcl_cmpbind,no_init);
  ar_check_init(gcl_cmpblock,no_init);
  ar_check_init(gcl_cmpcall,no_init);
  ar_check_init(gcl_cmpcatch,no_init);
  ar_check_init(gcl_cmpenv,no_init);
  ar_check_init(gcl_cmpeval,no_init);
  ar_check_init(gcl_cmpflet,no_init);
  ar_check_init(gcl_cmpfun,no_init);
  ar_check_init(gcl_cmpif,no_init);
  ar_check_init(gcl_cmplabel,no_init);
  ar_check_init(gcl_cmplam,no_init);
  ar_check_init(gcl_cmplet,no_init);
  ar_check_init(gcl_cmploc,no_init);
  ar_check_init(gcl_cmpmap,no_init);
  ar_check_init(gcl_cmpmulti,no_init);
  ar_check_init(gcl_cmpspecial,no_init);
  ar_check_init(gcl_cmptag,no_init);
  ar_check_init(gcl_cmptop,no_init);
  ar_check_init(gcl_cmpvar,no_init);
  ar_check_init(gcl_cmpvs,no_init);
  ar_check_init(gcl_cmpwt,no_init);
  ar_check_init(gcl_cmpmain,no_init);

#ifdef HAVE_XGCL
  lsp_init("../xgcl-2/sysdef.lisp");
  ar_check_init(gcl_Xlib,no_init);
  ar_check_init(gcl_Xutil,no_init);
  ar_check_init(gcl_X,no_init);
  ar_check_init(gcl_XAtom,no_init);
  ar_check_init(gcl_defentry_events,no_init);
  ar_check_init(gcl_Xstruct,no_init);
  ar_check_init(gcl_XStruct_l_3,no_init);
  ar_check_init(gcl_general,no_init);
  ar_check_init(gcl_keysymdef,no_init);
  ar_check_init(gcl_X10,no_init);
  ar_check_init(gcl_Xinit,no_init);
  ar_check_init(gcl_dwtrans,no_init);
  ar_check_init(gcl_tohtml,no_init);
  ar_check_init(gcl_index,no_init);
#endif
  
  ar_check_init(gcl_pcl_pkg,no_init);
  ar_check_init(gcl_pcl_walk,no_init);
  ar_check_init(gcl_pcl_iterate,no_init);
  ar_check_init(gcl_pcl_macros,no_init);
  ar_check_init(gcl_pcl_low,no_init);
  ar_check_init(gcl_pcl_impl_low,no_init);
  ar_check_init(gcl_pcl_fin,no_init);
  ar_check_init(gcl_pcl_defclass,no_init);
  ar_check_init(gcl_pcl_defs,no_init);
  ar_check_init(gcl_pcl_fngen,no_init);
  ar_check_init(gcl_pcl_cache,no_init);
  ar_check_init(gcl_pcl_dlisp,no_init);
  ar_check_init(gcl_pcl_dlisp2,no_init);
  ar_check_init(gcl_pcl_boot,no_init);
  ar_check_init(gcl_pcl_vector,no_init);
  ar_check_init(gcl_pcl_slots_boot,no_init);
  ar_check_init(gcl_pcl_combin,no_init);
  ar_check_init(gcl_pcl_dfun,no_init);
  ar_check_init(gcl_pcl_fast_init,no_init);
  ar_check_init(gcl_pcl_braid,no_init);
  ar_check_init(gcl_pcl_generic_functions,no_init);
  ar_check_init(gcl_pcl_slots,no_init);
  ar_check_init(gcl_pcl_init,no_init);
  ar_check_init(gcl_pcl_std_class,no_init);
  ar_check_init(gcl_pcl_cpl,no_init);
  ar_check_init(gcl_pcl_fsc,no_init);
  ar_check_init(gcl_pcl_methods,no_init);
  ar_check_init(gcl_pcl_fixup,no_init);
  ar_check_init(gcl_pcl_defcombin,no_init);
  ar_check_init(gcl_pcl_ctypes,no_init);
  ar_check_init(gcl_pcl_env,no_init);
  ar_check_init(gcl_pcl_compat,no_init);
  ar_check_init(gcl_pcl_precom1,no_init);
  ar_check_init(gcl_pcl_precom2,no_init);

}

static int ngazonk;

int
gcl_init_cmp_anon(void) {

  int i=0;

  switch(ngazonk++) {
  case 0:
    ar_check_init(gcl_pcl_gazonk0,Cnil);
    break;
  case 1:
    ar_check_init(gcl_pcl_gazonk1,Cnil);
    break;
  case 2:
    ar_check_init(gcl_pcl_gazonk2,Cnil);
    break;
  case 3:
    ar_check_init(gcl_pcl_gazonk3,Cnil);
    break;
  case 4:
    ar_check_init(gcl_pcl_gazonk4,Cnil);
    break;
  case 5:
    ar_check_init(gcl_pcl_gazonk5,Cnil);
    break;
  case 6:
    ar_check_init(gcl_pcl_gazonk6,Cnil);
    break;
  case 7:
    ar_check_init(gcl_pcl_gazonk7,Cnil);
    i=1;
    break;
  default:
    i=-1;
    break;
  }

  return i;

}
