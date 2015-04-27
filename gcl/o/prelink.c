#define NO_PRELINK_UNEXEC_DIVERSION

#include "include.h"

extern FILE *stdin __attribute__((weak));
extern FILE *stderr __attribute__((weak));
extern FILE *stdout __attribute__((weak));
extern rl_compentry_func_t *rl_completion_entry_function __attribute__((weak));
extern const char *rl_readline_name __attribute__((weak));

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

