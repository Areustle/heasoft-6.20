/*
 filename: hfbssum.c
 purpose:  c - wrapper for host FBSSUM task
*/

#include <stdio.h>

#ifdef unix
#define FBSSUM Fbssum
#endif
#ifdef vms
#define FBSSUM Fbssum
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
  void FBSSUM();

  OpenDefaultPF(argc, argv);
  FBSSUM();
  CloseDefaultPF();
  return(RETURN);
}
