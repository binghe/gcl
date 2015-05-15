#define NO_PRELINK_UNEXEC_DIVERSION

#include "include.h"

#if !defined(__MINGW32__) && !defined(__CYGWIN__)
extern FILE *stdin __attribute__((weak));
extern FILE *stderr __attribute__((weak));
extern FILE *stdout __attribute__((weak));

#if RL_READLINE_VERSION < 0x0600
extern Function		*rl_completion_entry_function __attribute__((weak));
extern char		*rl_readline_name __attribute__((weak));
#else
extern rl_compentry_func_t *rl_completion_entry_function __attribute__((weak));
extern const char *rl_readline_name __attribute__((weak));
#endif
#endif

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

