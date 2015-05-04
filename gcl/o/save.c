#ifndef FIRSTWORD
#include "include.h"
#endif

static void
memory_save(char *original_file, char *save_file)
{
#ifdef DO_BEFORE_SAVE
  DO_BEFORE_SAVE ;
#endif    
  
  unexec(save_file,original_file,0,0,0);
}

#ifdef USE_CLEANUP
extern void _cleanup();
#endif

LFD(siLsave)(void) {

  char filename[256];
  extern char *kcl_self;
  
  check_arg(1);
  check_type_or_pathname_string_symbol_stream(&vs_base[0]);
  coerce_to_filename(vs_base[0], filename);

  gcl_cleanup(1);
  
#ifdef MEMORY_SAVE
  MEMORY_SAVE(kcl_self,filename);
#else	  
  memory_save(kcl_self, filename);
#endif	

  /*  no return  */
  exit(0);

}
