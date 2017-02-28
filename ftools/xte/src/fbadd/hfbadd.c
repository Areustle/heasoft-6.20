/*
 filename: hfbadd.c
 purpose:  c - wrapper for host FBADD task
*/

#include <stdio.h>

#ifdef unix
#define FBADD Fbadd
#endif
#ifdef vms
#define FBADD Fbadd
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
  void FBADD();

  OpenDefaultPF(argc, argv);
  FBADD();
  CloseDefaultPF();
  return(RETURN);
}
