#define ALLOCATE(n) (*gcl_gmp_allocfun)(n)

void *gcl_gmp_alloc(size_t size)
{
   return (void *) ALLOCATE(size);
}

static void *gcl_gmp_realloc(void *oldmem, size_t oldsize, size_t newsize)
{
  unsigned int *old,*new;
  if (!jmp_gmp) { /* No gc in alloc if jmp_gmp */
    if (MP_SELF(big_gcprotect)) abort();
    MP_SELF(big_gcprotect)=oldmem;
    MP_ALLOCATED(big_gcprotect)=oldsize/MP_LIMB_SIZE;
  }
  new = (void *)ALLOCATE(newsize);
  old = jmp_gmp ? oldmem : MP_SELF(big_gcprotect);
  MP_SELF(big_gcprotect)=0;
  bcopy(old,new,oldsize);
/* SGC contblock pages: Its possible this is on an old page CM 20030827 */
/*   if (inheap(oldmem))  */
/* #ifdef SGC */
/*     insert_maybe_sgc_contblock(oldmem,oldsize); */
/* #else */
/*     insert_contblock(oldmem,oldsize); */
/* #endif */

  return new;
}

static void gcl_gmp_free(void *old, size_t oldsize)
{
}

