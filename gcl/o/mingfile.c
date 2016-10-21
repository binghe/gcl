#include "include.h"
#include "winsock2.h"
#include "windows.h"

int
mingwlisten(FILE *fp) {

  int c = 0;
  ioctlsocket(fileno(fp), FIONREAD, (void *)&c);
  if (c<=0)
    return 1;
  return 0;
}
