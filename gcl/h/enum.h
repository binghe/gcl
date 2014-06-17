#ifndef NEW_LISP
#define t_doublefloat t_longfloat
#endif

enum signals_allowed_values {
  sig_none,
  sig_normal,
  sig_try_to_delay,
  sig_safe,
  sig_at_read,
  sig_use_signals_allowed_value

};

  
