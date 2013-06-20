#include <sys/personality.h>
#include <syscall.h>
#include <unistd.h>
#include <alloca.h>
#include <errno.h>


{
  errno=0;

  {

    long pers = personality(0xffffffffUL);
    long flag = ADDR_NO_RANDOMIZE;

    if (sizeof(long)==4) flag|=ADDR_LIMIT_3GB|ADDR_COMPAT_LAYOUT;

    if (pers==-1) {printf("personality failure %d\n",errno);exit(-1);}
    if (!(pers & flag) && !getenv("GCL_UNRANDOMIZE")) {
      errno=0;
      if (personality(pers | flag) != -1 && personality(0xffffffffUL) & flag) {
	int i;
	char **n;
	for (i=0;envp[i];i++);
	n=alloca((i+2)*sizeof(*n));
	n[i+1]=0;
	n[i--]="GCL_UNRANDOMIZE=t";
	for (;i>=0;i--)
	  n[i]=envp[i];
#ifdef GCL_GPROF
	gprof_cleanup();
#endif
	errno=0;
	execve(*argv,argv,n);
	printf("execve failure %d\n",errno);
	exit(-1);
      } else {
	printf("personality change failure %d\n",errno);
	exit(-1);
      }
    }
  }
}
