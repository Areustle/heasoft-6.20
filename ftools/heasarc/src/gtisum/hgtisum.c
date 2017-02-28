/*
 filename: hgtisum.c
 purpose:  c - wrapper for host GTISUM task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define GTISUM Gtisum
#endif
#ifdef vms
#define GTISUM Gtisum
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
  void GTISUM();

  OpenDefaultPF(argc, argv);
  GTISUM();
  CloseDefaultPF();
  return(RETURN);
}
