#define NO_PRELINK_UNEXEC_DIVERSION

#include "include.h"

#ifdef NEED_STACK_CHK_GUARD
unsigned long __stack_chk_guard;
#endif

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

#ifdef NEED_STACK_CHK_GUARD
  if (!raw_image) {
    object y;
    vs_top=vs_base;
    vs_push(Ct);
    Lmake_random_state();
    y=vs_pop;
    vs_push(number_negate(find_symbol(make_simple_string("MOST-NEGATIVE-FIXNUM"),system_package)->s.s_dbind));
    vs_push(y);
    Lrandom();
    __stack_chk_guard=fixint(vs_pop);
  }
#endif

}

