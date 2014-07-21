#ifdef NO_PRELINK_UNEXEC_DIVERSION
#define PRELINK_EXTER
#else
#define PRELINK_EXTER extern

#undef stdin
#define stdin my_stdin
#undef stdout
#define stdout my_stdout
#undef stderr
#define stderr my_stderr

#ifdef HAVE_READLINE
#undef rl_instream
#define rl_instream my_rl_instream
#undef rl_completion_entry_function
#define rl_completion_entry_function my_rl_completion_entry_function
#undef rl_readline_name
#define rl_readline_name my_rl_readline_name
#undef rl_line_buffer
#define rl_line_buffer my_rl_line_buffer
#endif
#endif

PRELINK_EXTER void *my_stdin;
PRELINK_EXTER void *my_stdout;
PRELINK_EXTER void *my_stderr;

#ifdef HAVE_READLINE
PRELINK_EXTER void *my_rl_instream;
PRELINK_EXTER void *my_rl_completion_entry_function;
PRELINK_EXTER const char *my_rl_readline_name;
PRELINK_EXTER char *my_rl_line_buffer;
#endif
