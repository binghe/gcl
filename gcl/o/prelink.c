#define NO_PRELINK_UNEXEC_DIVERSION

#include "include.h"

void
prelink_init(void) {
  
  my_stdin=stdin;
  my_stdout=stdout;
  my_stderr=stderr;
#ifdef HAVE_READLINE
  my_rl_completion_entry_function_ptr=(void *)&rl_completion_entry_function;
  my_rl_readline_name_ptr=(void *)&rl_readline_name;
#endif

}

