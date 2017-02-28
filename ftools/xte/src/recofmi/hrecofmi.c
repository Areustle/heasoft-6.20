/*
 filename: hrecofmi.c
 purpose:  c - wrapper for host RECOFMI task
*/

#include <stdio.h>

#ifdef unix
#define RECOFMI Recofmi
#endif
#ifdef vms
#define RECOFMI Recofmi
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
  void RECOFMI();

  OpenDefaultPF(argc, argv);
  RECOFMI();
  CloseDefaultPF();
  return(RETURN);
}
