/*
 filename: hfbsub.c
 purpose:  c - wrapper for host FBSUB task
*/

#include <stdio.h>

#ifdef unix
#define FBSUB Fbsub
#endif
#ifdef vms
#define FBSUB Fbsub
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
  void FBSUB();

  OpenDefaultPF(argc, argv);
  FBSUB();
  CloseDefaultPF();
  return(RETURN);
}
