#include "sys.c"

void
gcl_init_init()
{

  build_symbol_table();

  lsp_init("../lsp/gcl_export.lsp");

  lsp_init("../lsp/gcl_defmacro.lsp");
  lsp_init("../lsp/gcl_evalmacros.lsp");
  lsp_init("../lsp/gcl_top.lsp");
  lsp_init("../lsp/gcl_module.lsp");

  lsp_init("../lsp/gcl_autoload.lsp");

}

void
gcl_init_system(object no_init)
{

  if (type_of(no_init)!=t_symbol)
    error("Supplied no_init is not of type symbol\n");

  lsp_init("../lsp/gcl_listlib.lsp");
  lsp_init("../lsp/gcl_predlib.lsp");
  lsp_init("../lsp/gcl_setf.lsp");
  lsp_init("../lsp/gcl_arraylib.lsp");
  lsp_init("../lsp/gcl_assert.lsp");
  lsp_init("../lsp/gcl_defstruct.lsp");
  lsp_init("../lsp/gcl_restart.lsp");
  lsp_init("../lsp/gcl_describe.lsp");
#ifdef HAVE_JAPI_H
  lsp_init("../lsp/gcl_japi.lsp");
#endif
  lsp_init("../lsp/gcl_iolib.lsp");
/*   lsp_init("../lsp/gcl_listlib.lsp"); */
  lsp_init("../lsp/gcl_mislib.lsp");
  lsp_init("../lsp/gcl_numlib.lsp");
  lsp_init("../lsp/gcl_packlib.lsp");
  lsp_init("../lsp/gcl_seq.lsp");
  lsp_init("../lsp/gcl_seqlib.lsp");
  lsp_init("../lsp/gcl_trace.lsp");
  lsp_init("../lsp/gcl_sloop.lsp");
  lsp_init("../lsp/gcl_serror.lsp");
  lsp_init("../lsp/gcl_destructuring_bind.lsp");
  lsp_init("../lsp/gcl_loop.lsp");
  lsp_init("../lsp/gcl_defpackage.lsp");
  lsp_init("../lsp/gcl_make_defpackage.lsp");
  lsp_init("../lsp/gcl_sharp.lsp");
  lsp_init("../lsp/gcl_fpe.lsp");

  lsp_init("../cmpnew/gcl_cmpinline.lsp");
  lsp_init("../cmpnew/gcl_cmputil.lsp");

  lsp_init("../lsp/gcl_debug.lsp");
  lsp_init("../lsp/gcl_info.lsp");

  lsp_init("../cmpnew/gcl_cmptype.lsp");
  lsp_init("../cmpnew/gcl_cmpbind.lsp");
  lsp_init("../cmpnew/gcl_cmpblock.lsp");
  lsp_init("../cmpnew/gcl_cmpcall.lsp");
  lsp_init("../cmpnew/gcl_cmpcatch.lsp");
  lsp_init("../cmpnew/gcl_cmpenv.lsp");
  lsp_init("../cmpnew/gcl_cmpeval.lsp");
  lsp_init("../cmpnew/gcl_cmpflet.lsp");
  lsp_init("../cmpnew/gcl_cmpfun.lsp");
  lsp_init("../cmpnew/gcl_cmpif.lsp");
  lsp_init("../cmpnew/gcl_cmplabel.lsp");
  lsp_init("../cmpnew/gcl_cmplam.lsp");
  lsp_init("../cmpnew/gcl_cmplet.lsp");
  lsp_init("../cmpnew/gcl_cmploc.lsp");
  lsp_init("../cmpnew/gcl_cmpmap.lsp");
  lsp_init("../cmpnew/gcl_cmpmulti.lsp");
  lsp_init("../cmpnew/gcl_cmpspecial.lsp");
  lsp_init("../cmpnew/gcl_cmptag.lsp");
  lsp_init("../cmpnew/gcl_cmptop.lsp");
  lsp_init("../cmpnew/gcl_cmpvar.lsp");
  lsp_init("../cmpnew/gcl_cmpvs.lsp");
  lsp_init("../cmpnew/gcl_cmpwt.lsp");

  
}

int
gcl_init_cmp_anon(void) {

  return 1;

}
