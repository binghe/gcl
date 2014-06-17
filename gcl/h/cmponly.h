#define CMPcar(x)	(x)->c.c_car
#define CMPcdr(x)	(x)->c.c_cdr
#define CMPcaar(x)	(x)->c.c_car->c.c_car
#define CMPcadr(x)	(x)->c.c_cdr->c.c_car
#define CMPcdar(x)	(x)->c.c_car->c.c_cdr
#define CMPcddr(x)	(x)->c.c_cdr->c.c_cdr
#define CMPcaaar(x)	(x)->c.c_car->c.c_car->c.c_car
#define CMPcaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car
#define CMPcadar(x)	(x)->c.c_car->c.c_cdr->c.c_car
#define CMPcaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car
#define CMPcdaar(x)	(x)->c.c_car->c.c_car->c.c_cdr
#define CMPcdadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr
#define CMPcddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr
#define CMPcdddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr
#define CMPcaaaar(x)	(x)->c.c_car->c.c_car->c.c_car->c.c_car
#define CMPcaaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car->c.c_car
#define CMPcaadar(x)	(x)->c.c_car->c.c_cdr->c.c_car->c.c_car
#define CMPcaaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car->c.c_car
#define CMPcadaar(x)	(x)->c.c_car->c.c_car->c.c_cdr->c.c_car
#define CMPcadadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr->c.c_car
#define CMPcaddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr->c.c_car
#define CMPcadddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr->c.c_car
#define CMPcdaaar(x)	(x)->c.c_car->c.c_car->c.c_car->c.c_cdr
#define CMPcdaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car->c.c_cdr
#define CMPcdadar(x)	(x)->c.c_car->c.c_cdr->c.c_car->c.c_cdr
#define CMPcdaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car->c.c_cdr
#define CMPcddaar(x)	(x)->c.c_car->c.c_car->c.c_cdr->c.c_cdr
#define CMPcddadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr->c.c_cdr
#define CMPcdddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr->c.c_cdr
#define CMPcddddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr->c.c_cdr
#define EQ(x,y) ((x)==(y))
#define	CMPmake_fixnum(x) make_fixnum(x)
#define ALLOCA_CONS(n) (alloca_val=alloca((n)*sizeof(struct cons))) 
#define ON_STACK_CONS(x,y) (alloca_val=alloca(sizeof(struct cons)), on_stack_cons(x,y)) 
#define ON_STACK_LIST on_stack_list
#define ON_STACK_LIST_VECTOR_NEW on_stack_list_vector_new
#define ON_STACK_MAKE_LIST on_stack_make_list
#define KEYTYPE void *
#define IDECL(a,b,c) mp_limb_t *c=(mp_limb_t *)alloca(1*sizeof(mp_limb_t));MP_INT b={1,1,c}; a = &b 
#define SETQ_IO(var,alloc,val,af) { object _xx = (val); \
                                    int _n; \
                                    if ((_n=obj_to_mpz(_xx,(var)))) {\
                                      obj_to_mpz1(_xx,(var),af(_n));}}
#define SETQ_II(var,alloc,val,af) { MP_INT * _xx = (val); \
                                    int _n; \
                                    if ((_n=mpz_to_mpz(_xx,(var)))) {\
                                      mpz_to_mpz1(_xx,(var),af(_n));}}
#define ISETQ_FIX(a,b,c) isetq_fix(a,c)

#define save_avma 
#define restore_avma 
#define stp_ordinary 0 /*FIXME maxima*/
