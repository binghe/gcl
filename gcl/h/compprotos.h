bool eql1(object,object);
bool equal1(object,object);
bool equalp1(object,object);
bool file_exists(object);
bool integer_bitp(object,object);
double big_to_double(object);
frame_ptr frs_sch_catch(object);
frame_ptr frs_sch(object);
int length(object);
int number_compare(object,object);
int number_evenp(object);
int number_minusp(object);
int number_oddp(object);
int number_plusp(object);
int number_zerop(object);
long int fixint(object);
object alloc_object(enum type);
object call_proc_new(object,void **,int,object,va_list);
object coerce_to_string();
object elt(object,int);
object fixnum_big_shift(fixnum,fixnum);
object fixnum_times(fixnum,fixnum);
object fSsputprop(object,object,object);
object get(object,object,object);
object get_gcd(object,object);
object get_lcm(object,object);
object integer_count(object);
object integer_length(object);
object integer_shift(object,object);
object listA(int,...);
object list(int,...);
object log_op2(fixnum,object,object);
object make_cons(object,object);
object make_fixnum1(long);
object make_list(int);
object make_longfloat(longfloat);
object make_shortfloat(double);
object make_simple_string(const char *);
object number_abs(object);
object number_divide(object, object);
object number_dpb(object,object,object);
object number_dpf(object,object,object);
object number_ldb(object,object);
object number_ldbt(object,object);
object number_minus(object,object);
object number_negate(object);
object number_plus(object,object);
object number_signum(object);
object number_times(object,object);
object princ(object,object);
object read_char1(object,object);
object structure_ref(object,object,fixnum);
object structure_set(object,object,fixnum,object);
object symbol_function(object);
object symbol_name(object);
object symbol_value(object);
object terpri(object);
object vs_overflow(void);
void bds_overflow(void);
void bds_unwind(bds_ptr);
void do_init(object *);
void frs_overflow(void);
void intdivrem(object,object,fixnum,object *,object *);
void princ_char(int,object);
void princ_str(char *,object);
void princ_str(char *,object);
void sethash(object,object,object);
void setq(object,object);
void super_funcall_no_event(object);
void unwind(frame_ptr,object) NO_RETURN;
int object_to_int(object);
fixnum object_to_fixnum(object);
char object_to_char(object);
void not_a_symbol(object);
object number_expt(object,object);
object fLrow_major_aref(object,fixnum);
object car(object);
object cdr(object);
object caar(object);
object cadr(object);
object cdar(object);
object cddr(object);
object caaar(object);
object caadr(object);
object cadar(object);
object caddr(object);
object cdaar(object);
object cdadr(object);
object cddar(object);
object cdddr(object);
object caaaar(object);
object caaadr(object);
object caadar(object);
object caaddr(object);
object cadaar(object);
object cadadr(object);
object caddar(object);
object cadddr(object);
object cdaaar(object);
object cdaadr(object);
object cdadar(object);
object cdaddr(object);
object cddaar(object);
object cddadr(object);
object cdddar(object);
object cddddr(object);
object fcalln1(object,...);
object append(object,object);
object aset1(object,fixnum,object);
void call_or_link(object,void **);
object call_proc0(object,void *);
object call_vproc_new(object,void *,object,va_list);
void check_arg_failed (int);
void check_other_key (object,int, ...);
object elt_set(object,int,object);
void FEerror(char *,int,...);
void FEwrong_type_argument(object,object);
void funcall(object);
object getf(object,object,object);
struct htent * gethash(object,object);
void invalid_macro_call(void);
long labs(long);
object list_vector_new(int,object,va_list);
object make_cclosure_new(void (*)(),object,object,object);
object nconc(object,object);
object nreverse(object);
object one_plus(object);
object one_minus(object);
void parse_key(object *,bool,bool,int,...); 
int parse_key_new_new();/* (int,object *,void *,object,va_list); */
int parse_key_rest_new(); /* (object,int,object *,void *,object,va_list); */
object prin1(object,object);
object print(object,object);
object putprop(object,object,object);
object remprop(object,object);
object reverse(object);
object simple_symlispcall(object,object *,int);
object sputprop(object,object,object);
void symlispcall(object,object *,int);
void too_few_arguments(void);
void too_many_arguments(void);
object wrong_type_argument(object,object);
bool oeql(object,object);
void call_or_link_closure(object,void **,void **);
void check_alist(object);
void lispcall(object *,int);
object make_cclosure(void (*)(),object,object,object,char *,int);
object simple_lispcall(object *,int);
object sublis1(object,object,bool(*)());
void turbo_closure(object);
char * object_to_string(object);
object on_stack_cons(object,object);
object on_stack_list(int,...);
object on_stack_list_vector_new(int,object,va_list);
object on_stack_make_list(int);
object read_byte1(object,object);
int not_a_variable(object);
object cmod(object);
object ctimes(object,object);
object cdifference(object,object);
object cplus(object,object);
void funcall_with_catcher(object,object);
void check_type_symbol(object *);
void ck_larg_exactly(int, object);
double cos(double);
double sin(double);
double sqrt(double);
double tan(double);
#ifdef CMPINCLUDE
void * alloca(unsigned long);
int feof(void *);
int getc(void *);
int putc(int,void *);
#endif
void vfun_wrong_number_of_args(object);
void ihs_overflow (void);
double object_to_double(object);
void gcl_init_or_load1(void (*)(void),const char *);
