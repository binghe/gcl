#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>

static int pool=-1;
static struct pool {
  ufixnum pid;
  ufixnum n;
  ufixnum s;
} *Pool;

static struct flock pl;

static void
lock_pool(void) {

  pl.l_type=F_WRLCK;
  massert(!fcntl(pool,F_SETLK,&pl));

}

static void
unlock_pool(void) {

  pl.l_type=F_UNLCK;
  massert(!fcntl(pool,F_SETLK,&pl));

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

    struct flock f;

    massert((pool=open("/tmp/gcl_pool",O_CREAT|O_RDWR,0644))!=-1);
    massert(!ftruncate(pool,sizeof(struct pool)));
    massert((Pool=mmap(NULL,sizeof(struct pool),PROT_READ|PROT_WRITE,MAP_SHARED,pool,0))!=(void *)-1);

    pl.l_type=F_WRLCK;
    pl.l_whence=SEEK_SET;
    pl.l_start=sizeof(Pool->pid);;
    pl.l_len=0;

    f=pl;
    f.l_start=0;
    f.l_len=sizeof(Pool->pid);
    
    if (!fcntl(pool,F_SETLK,&f)) {

      Pool->pid=getpid();

      lock_pool();
      Pool->n=0;
      Pool->s=0;
      unlock_pool();

      f.l_type=F_UNLCK;
      massert(!fcntl(pool,F_SETLK,&f));

      fprintf(stderr,"Initializing pool\n");
      fflush(stderr);

    }

    f.l_type=F_RDLCK;
    massert(!fcntl(pool,F_SETLK,&f));

    register_pool(1);
    massert(!atexit(close_pool));

  }

}

static void
update_pool(fixnum val) {

  if (use_pool) {
    open_pool();
    lock_pool();
    Pool->s+=val;
    unlock_pool();
  }

}

static ufixnum
get_pool(void) {

  ufixnum s;

  if (use_pool) {

    open_pool();
    lock_pool();
    s=Pool->s;
    unlock_pool();
    
  } else

    s=data_pages();

  return s;
  
}


static void
pool_check(void) {

  /* if (pool!=-1) */
  /*   massert(get_pool()==data_pages() */
  /* 	    ||!fprintf(stderr,"%lu %lu %lu\n",get_pool(),page((void *)heap_end-data_start),page(((rb_end-rb_start))))); */

}
