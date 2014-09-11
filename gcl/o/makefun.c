#include "include.h"
#include "funlink.h"

#define PADDR(i) ((void *)(sSPinit->s.s_dbind->fixa.fixa_self[Mfix(i)]))
/* eg:
MakeAfun(addr,F_ARGD(min,max,flags,ARGTYPES(a,b,c,d)),0);
MakeAfun(addr,F_ARGD(2,3,NONE,ARGTYPES(OO,OO,OO,OO)),0);
*/
object MakeAfun(object (*addr)(object,object), unsigned int argd, object data)
{int type = (F_ARG_FLAGS_P(argd,F_requires_fun_passed) ? t_closure : t_afun);
  object x = alloc_object(type);
  x->sfn.sfn_name = Cnil;
  x->sfn.sfn_self = addr;
  x->sfn.sfn_argd = argd;
  if (type == t_closure)
    { x->cl.cl_env = 0;
      x->cl.cl_envdim=0;}
  x->sfn.sfn_data = data;
  return x;
}
 

static object
fSmakefun(object sym, object (*addr) (/* ??? */), unsigned int argd)
{object ans = MakeAfun(addr,argd,
		      (sSPmemory && sSPmemory->s.s_dbind &&
		       type_of(sSPmemory->s.s_dbind)==t_cfdata) ?
		      sSPmemory->s.s_dbind : 0);
 ans->sfn.sfn_name = sym;
 return ans;
}

/* static object */
/* ImakeClosure(object (*addr)(),int argd,int n,...) */
/* { object x = fSmakefun(Cnil,addr,argd); */
/*   va_list ap; */
/*   va_start(ap,n); */
/*   IsetClosure(x,n,ap); */
/*   va_end(ap); */
/*   return x; */
/* } */

static void     
IsetClosure(object x, int n, va_list ap)
{  /* this will change so that we can allocate 'hunks' which will be little
   blocks the size of an array header say with only one header word.   This
   will be more economical. Because of gc, we can't allocate relblock, it
   might move while in the closure.  */
  object *p;
  if (type_of(x) != t_closure)
    { FEerror("Not a closure",0);}
  if (x->cl.cl_envdim < n)
    {BEGIN_NO_INTERRUPT; x->cl.cl_env = (object *)alloc_relblock(n);
     x->cl.cl_envdim = n;
     END_NO_INTERRUPT;
   }
  p = x->cl.cl_env;
  while (--n >= 0)
    { *p++ = va_arg(ap,object);
    }
}

DEFUN_NEW("INITFUN",object,fSinitfun,SI,3,ARG_LIMIT,NONE,OO,OO,OO,OO,
      (object sym,object addr_ind,object argd,...),
      "Store a compiled function on SYMBOL whose body is in the VV array at \
INDEX, and whose argd descriptor is ARGD.  If more arguments IND1, IND2,.. \
are supplied these are indices in the VV array for the environment of this \
closure.")
{ int nargs = F_NARGS(VFUN_NARGS) -3;
  va_list ap;
  object fun = fSmakefun(IisSymbol(sym),PADDR(addr_ind),Mfix(argd));
  if (nargs > 0)
    { va_start(ap,argd);
      IsetClosure(fun,nargs,ap);
      while (--nargs >= 0)
	/* the things put in by IsetClosure were only the indices
	   of the closure variables not the actual variables */
	{ fun->cl.cl_env[nargs]= (object) PADDR(fun->cl.cl_env[nargs]);}
      va_end(ap);
    }
  fSfset(sym,fun);
  return sym;
}

#include "apply_n.h"

DEFUN_NEW("INITMACRO",object,fSinitmacro,SI,4,ARG_LIMIT,NONE,OO,OO,OO,OO,(object first,...),
      "Like INITFUN, but makes then sets the 'macro' flag on this symbol")
{va_list ap;
 object res;
 int n = VFUN_NARGS;
 object *new;
 va_start(ap,first);
 COERCE_VA_LIST_NEW(new,first,ap,n);
 res= c_apply_n_f((void *)FFN(fSinitfun),n,new,3,ARG_LIMIT);
 va_end(ap);
 res->s.s_mflag = 1;
 return res;
}

DEFUN_NEW("SET-KEY-STRUCT",object,fSset_key_struct,SI,1,1,NONE,OO,OO,OO,OO,(object key_struct_ind),
      "Called inside the loader.  The keystruct is set up in the file with \
   indexes rather than the actual entries.  We change these indices to \
   the objects")
{ set_key_struct(PADDR(key_struct_ind),sSPmemory->s.s_dbind);
  return Cnil;
}
     

#define collect(top_,next_,val_) ({object _x=MMcons(val_,Cnil);\
                                   if (top_==Cnil) top_=next_=_x; \
                                   else next_=next_->c.c_cdr=_x;})


static void
put_fn_procls(object sym,fixnum argd,fixnum oneval,object def,object rdef) {

  unsigned int atypes=F_TYPES(argd) >> F_TYPE_WIDTH;
  unsigned int minargs=F_MIN_ARGS(argd);
  unsigned int maxargs=F_MAX_ARGS(argd);
  unsigned int rettype=F_RESULT_TYPE(argd);
  unsigned int i;
  object ta=Cnil,na=Cnil;

  for (i=0;i<minargs;i++,atypes >>=F_TYPE_WIDTH) 
    switch(maxargs!=minargs ? F_object : atypes & MASK_RANGE(0,F_TYPE_WIDTH)) {
    case F_object:
      collect(ta,na,def);
      break;
    case F_int:
      collect(ta,na,sLfixnum);
      break;
    case F_shortfloat:
      collect(ta,na,sLshort_float);
      break;
    case F_double_ptr:
      collect(ta,na,sLlong_float);
      break;
    default:
      FEerror("Bad sfn declaration",0);
      break;
    }
  if (maxargs!=minargs)
    collect(ta,na,sLA);
  putprop(sym,ta,sSproclaimed_arg_types);
  ta=na=Cnil;
  if (oneval) 
    switch(rettype) {
    case F_object:
      ta=rdef;
      break;
    case F_int:
      ta=sLfixnum;
      break;
    case F_shortfloat:
      ta=sLshort_float;
      break;
    case F_double_ptr:
      ta=sLlong_float;
      break;
    default:
      FEerror("Bad sfn declaration",0);
      break;
    }
  else
/*     ta=MMcons(sLA,Cnil); */
    ta=sLA;
  putprop(sym,ta,sSproclaimed_return_type);
  if (oneval)
    putprop(sym,Ct,sSproclaimed_function);

}

void
SI_makefun(char *strg, void *fn, unsigned int argd)
{ object sym = make_si_ordinary(strg);
 fSfset(sym, fSmakefun(sym,fn,argd));
 put_fn_procls(sym,argd,1,Ct,Ct);
}

void
LISP_makefun(char *strg, void *fn, unsigned int argd)
{ object sym = make_ordinary(strg);
 fSfset(sym, fSmakefun(sym,fn,argd));
 put_fn_procls(sym,argd,1,Ct,Ct);
}

void
SI_makefunm(char *strg, void *fn, unsigned int argd)
{ object sym = make_si_ordinary(strg);
 fSfset(sym, fSmakefun(sym,fn,argd));
 put_fn_procls(sym,argd,0,Ct,Ct);
}

void
LISP_makefunm(char *strg, void *fn, unsigned int argd)
{ object sym = make_ordinary(strg);
 fSfset(sym, fSmakefun(sym,fn,argd));
 put_fn_procls(sym,argd,0,Ct,Ct);
}


/* static object  */
/* MakeClosure(int n,int argd,object data,object (*fn)(),...) */
/* { object x; */
/*   va_list ap; */
/*   x = alloc_object(t_closure); */
/*   x->cl.cl_name = Cnil; */
/*   x->cl.cl_self = fn; */
/*   x->cl.cl_data = data; */
/*   x->cl.cl_argd = argd; */
/*   x->cl.cl_env = 0; */
/*   x->cl.cl_env = (object *)alloc_contblock(n*sizeof(object)); */
/*   x->cl.cl_envdim=n; */
/*   va_start(ap,fn); */
/*   { object *p = x->cl.cl_env; */
/*   while (--n>= 0) */
/*     { *p++ = va_arg(ap,object);} */
/*   va_end(ap); */
/*   } */
/*   return x; */
/* } */
      
DEFUN_NEW("INVOKE",object,fSinvoke,SI,1,ARG_LIMIT,NONE,OO,OO,OO,OO,(object x),
      "Invoke a C function whose body is at INDEX in the VV array")
{ int (*fn)();
  fn = (void *) PADDR(x);
  (*fn)();
  return Cnil;
}
  
