/*  Copyright (C) 2024 Camm Maguire */

#include <string.h>
#include <stdio.h>

int
main() {

  char buf[4096],*c,*d=(void *)-1;

  for (;fgets(buf,sizeof(buf),stdin);) {

    if (!strchr(buf,'\n')) {
      fprintf(stderr,"Line too long, %s\n",buf);
      return -1;
    }

    if (!(c=!d || !strncmp("DEF",buf,3) ? buf : strstr(buf," DEF")))
      continue;

    c=c==buf ? c : c+1;
    d=strstr(c,"\")");
    printf("%-.*s\n",(d ? d+2 : buf+strlen(buf)-1)-c,c);

  }

}
