#define FLAVOR ""

#include "sys.c"

void
gcl_init_init() {

  build_symbol_table();

  lsp_init("../lsp/gcl_export.lsp");

  ar_init(gcl_defmacro);
  ar_init(gcl_evalmacros);
  ar_init(gcl_top);
  ar_init(gcl_module);

  lsp_init("../lsp/gcl_autoload.lsp");

}

void
gcl_init_system(object no_init) {

  if (type_of(no_init)!=t_symbol)
    error("Supplied no_init is not of type symbol\n");

  ar_check_init(gcl_arraylib,no_init);
  ar_check_init(gcl_predlib,no_init);
  ar_check_init(gcl_setf,no_init);
  ar_check_init(gcl_assert,no_init);
  ar_check_init(gcl_defstruct,no_init);
  ar_check_init(gcl_restart,no_init);
  ar_check_init(gcl_describe,no_init);
#ifdef HAVE_JAPI_H
  ar_check_init(gcl_japi,no_init);
#endif
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

  ar_check_init(gcl_sharp_uv,no_init);
  ar_check_init(gcl_namestring,no_init);
  ar_check_init(gcl_logical_pathname_translations,no_init);
  ar_check_init(gcl_make_pathname,no_init);
  ar_check_init(gcl_parse_namestring,no_init);
  ar_check_init(gcl_translate_pathname,no_init);
  ar_check_init(gcl_directory,no_init);
  ar_check_init(gcl_merge_pathnames,no_init);
  ar_check_init(gcl_truename,no_init);
  ar_check_init(gcl_rename_file,no_init);
  ar_check_init(gcl_wild_pathname_p,no_init);
  ar_check_init(gcl_pathname_match_p,no_init);
	
  ar_check_init(gcl_iolib,no_init);
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
  lsp_init("../xgcl-2/package.lisp");
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
  
}

int
gcl_init_cmp_anon(void) {

  return 1;

}
