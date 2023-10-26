#include <stdio.h>

#define NO_PRELINK_UNEXEC_DIVERSION
char *rb_end=NULL,*rb_start=NULL,*heap_end=NULL;
void *data_start=NULL;
int multiprocess_memory_pool=1;

#include "include.h"
#include "page.h"
#include "pool.h"

/*lintian*/
void
assert_error(const char *a,unsigned l,const char *f,const char *n) {
  update_pool(0);
  get_pool();
  pool_check();
}

int
main(int argc,char * argv[],char * envp[]) {

  int s=3;

  if (argc>1) sscanf(argv[1],"%d",&s);
  open_pool();
  for (;;) {
    lock_pool();
    fprintf(stderr,"master pid %lu %lu processess %lu pages\n",Pool->pid,Pool->n,Pool->s);
    fflush(stderr);
    unlock_pool();
    sleep(s);
  }
  return 0;
}
