#include <stdio.h>

#define NO_PRELINK_UNEXEC_DIVERSION
char *rb_end=NULL,*rb_start=NULL,*heap_end=NULL;

#include "include.h"
#include "page.h"
#include "pool.h"

void
assert_error(const char *a,unsigned l,const char *f,const char *n) {
}

int
main(int argc,char * argv[],char * envp[]) {

  int s;

  sscanf(argv[1],"%d",&s);
  open_pool();
  for (;;) {
    lock_pool();
    fprintf(stderr,"%lu processess %lu pages\n",Pool->n,Pool->s);
    fflush(stderr);
    unlock_pool();
    sleep(s);
  }
  return 0;
}
