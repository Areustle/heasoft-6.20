/*
 filename: hhxtdead.c
 purpose:  c - wrapper for host HXTDEAD task
 author:   Tom Gasaway
 derived from TRANS2FITS wrapper by
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define HXTDEAD hxtdead
#endif
#ifdef vms
#define HXTDEAD hxtdead
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
  void HXTDEAD();

  OpenDefaultPF(argc, argv);
  HXTDEAD();
  CloseDefaultPF();
  return(RETURN);
}
