/*  Copyright (C) 2024 Camm Maguire */

#include <string.h>
#include <stdio.h>

int
main() {

  char buf[4096],*c,*d=(void *)-1,*e;

  for (;fgets(buf,sizeof(buf),stdin);) {

    if (!strchr(buf,'\n')) {
      fprintf(stderr,"Line too long, %s\n",buf);
      return -1;
    }

    for (c=buf;(c=(!d&&*c&&*c!='\n') || !strncmp("DEF",c,3) ? c : strstr(c," DEF"));c=e) {

      c=c==buf ? c : c+1;
      d=strstr(c,"\")");
      e=d ? d+2 : buf+strlen(buf)-1;
      printf("%-.*s\n",e-c,c);

    }

  }

}
