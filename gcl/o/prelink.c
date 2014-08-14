#define NO_PRELINK_UNEXEC_DIVERSION

#include "include.h"

void
prelink_init(void) {
  
  my_stdin=stdin;
  my_stdout=stdout;
  my_stderr=stderr;
#ifdef HAVE_READLINE
  my_rl_instream=rl_instream;
  my_rl_completion_entry_function=rl_completion_entry_function;
  my_rl_readline_name=rl_readline_name;
  my_rl_line_buffer=rl_line_buffer;
#endif

}

