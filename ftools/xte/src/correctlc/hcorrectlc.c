/*
 filename: hcorrectlc.c
 purpose:  c - wrapper for CORRECTLC task
*/

#include <stdio.h>

#ifdef unix
#define CORRECTLC Correctlc
#endif
#ifdef vms
#define CORRECTLC Correctlc
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
  void CORRECTLC();

  OpenDefaultPF(argc, argv);
  CORRECTLC();
  CloseDefaultPF();
  return(RETURN);
}
