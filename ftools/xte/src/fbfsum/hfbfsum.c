/*
 filename: hfbfsum.c
 purpose:  c - wrapper for host FBFSUM task
*/

#include <stdio.h>

#ifdef unix
#define FBFSUM Fbfsum
#endif
#ifdef vms
#define FBFSUM Fbfsum
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
  void FBFSUM();

  OpenDefaultPF(argc, argv);
  FBFSUM();
  CloseDefaultPF();
  return(RETURN);
}
