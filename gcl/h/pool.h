#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>

static int pool=-1;
static struct pool {
  ufixnum n;
  ufixnum s;
} *Pool;

static void
lock_pool(void) {
  massert(!lockf(pool,F_LOCK,0));
}

static void
unlock_pool(void) {
  massert(!lockf(pool,F_ULOCK,0));
}

static ufixnum
data_pages(void) {

  return page(2*(rb_end-rb_start)+((void *)heap_end-data_start));

}
  
static void
register_pool(int s) {
  lock_pool();
  Pool->n+=s;
  Pool->s+=s*data_pages();
  unlock_pool();
}
  
void
close_pool(void) {

  ufixnum n;
  
  if (pool!=-1) {
    register_pool(-1);
    lock_pool();
    n=Pool->n;
    unlock_pool();
    massert(!close(pool));
    massert(!munmap(Pool,sizeof(struct pool)));
    massert(n || !unlink("/tmp/gcl_pool"));
    pool=-1;
  }

}

static void
open_pool(void) {

  if (pool==-1) {
    if ((pool=open("/tmp/gcl_pool",O_CREAT|O_EXCL|O_RDWR,0644))!=-1) {
      massert(!ftruncate(pool,sizeof(struct pool)));
    } else
      massert((pool=open("/tmp/gcl_pool",O_RDWR))>=0);
    massert((Pool=mmap(NULL,sizeof(struct pool),PROT_READ|PROT_WRITE,MAP_SHARED,pool,0))!=(void *)-1);
    register_pool(1);
    massert(!atexit(close_pool));
  }
}

static void
update_pool(fixnum val) {

  open_pool();
  lock_pool();
  Pool->s+=val;
  unlock_pool();

}

static ufixnum
get_pool(void) {

  ufixnum s;
  
  open_pool();
  lock_pool();
  s=Pool->s;
  unlock_pool();

  return s;
  
}


static void
pool_check(void) {

  /* if (pool!=-1) */
  /*   massert(get_pool()==data_pages() */
  /* 	    ||!fprintf(stderr,"%lu %lu %lu\n",get_pool(),page((void *)heap_end-data_start),page(((rb_end-rb_start))))); */

}
