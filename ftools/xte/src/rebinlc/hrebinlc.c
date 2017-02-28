/*
 filename: hrebinlc.c
 purpose:  c - wrapper for REBINLC task
 author:   M. Tripicco
*/

#include <stdio.h>

#ifdef unix
#define REBINLC Rebinlc
#endif
#ifdef vms
#define REBINLC Rebinlc
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
  void REBINLC();

  OpenDefaultPF(argc, argv);
  REBINLC();
  CloseDefaultPF();
  return(RETURN);
}
